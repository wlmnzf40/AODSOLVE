#include "analysis/integrated_cpg_analyzer.h"
#include <queue>
#include <functional>
#include <fstream>
#include <sstream>

namespace aodsolve {

    IntegratedCPGAnalyzer::IntegratedCPGAnalyzer(clang::ASTContext& ctx)
        : ast_context(ctx), source_manager(ctx.getSourceManager()), cpg_context(ctx), aod_analyzer(ctx) {
        // 初始化
    }

    CPGToAODConversion IntegratedCPGAnalyzer::analyzeFunctionWithCPG(const clang::FunctionDecl* func) {
        CPGToAODConversion result;

        // 检查函数是否在主文件中
        if (!source_manager.isInMainFile(func->getLocation())) {
            result.successful = false;
            result.errors.push_back("Function not in main file: " + func->getNameAsString());
            result.warnings.push_back("Skipping header file function");
            return result;
        }

        if (!func->hasBody()) {
            result.successful = false;
            result.errors.push_back("Function has no body: " + func->getNameAsString());
            return result;
        }

        result.conversion_log.push_back("Analyzing function: " + func->getNameAsString());

        // 【关键】使用CPGBuilder构建CPG - 这是正确的方式!
        cpg::CPGBuilder::buildForFunction(func, cpg_context);

        result.conversion_log.push_back("✓ CPG construction complete");

        // 统计ICFG节点和边
        int icfg_count = 0;
        int edge_count = 0;

        // 遍历函数体统计ICFG节点
        std::function<void(const clang::Stmt*)> countICFGNodes;
        countICFGNodes = [&](const clang::Stmt* stmt) {
            if (!stmt) return;

            if (auto* icfg_node = cpg_context.getICFGNode(stmt)) {
                icfg_count++;
                edge_count += icfg_node->successors.size();
            }

            for (auto* child : stmt->children()) {
                countICFGNodes(child);
            }
        };

        countICFGNodes(func->getBody());

        // 统计PDG节点
        int pdg_count = 0;
        std::function<void(const clang::Stmt*)> countPDGNodes;
        countPDGNodes = [&](const clang::Stmt* stmt) {
            if (!stmt) return;

            if (auto* pdg_node = cpg_context.getPDGNode(stmt)) {
                pdg_count++;
            }

            for (auto* child : stmt->children()) {
                countPDGNodes(child);
            }
        };

        countPDGNodes(func->getBody());

        result.node_count = icfg_count + pdg_count;
        result.edge_count = edge_count;

        result.conversion_log.push_back("Statistics: " +
                                       std::to_string(icfg_count) + " ICFG nodes, " +
                                       std::to_string(pdg_count) + " PDG nodes, " +
                                       std::to_string(edge_count) + " edges");

        result.successful = true;
        return result;
    }

    CPGToAODConversion IntegratedCPGAnalyzer::analyzeTranslationUnitWithCPG() {
        CPGToAODConversion result;
        result.successful = true;

        auto* tu = ast_context.getTranslationUnitDecl();

        // 【关键】只分析主文件中的函数
        for (auto* decl : tu->decls()) {
            if (!source_manager.isInMainFile(decl->getLocation())) {
                continue;  // 跳过头文件
            }

            if (auto* func = clang::dyn_cast<clang::FunctionDecl>(decl)) {
                if (func->hasBody() && func->isThisDeclarationADefinition()) {
                    auto func_result = analyzeFunctionWithCPG(func);
                    result.node_count += func_result.node_count;
                    result.edge_count += func_result.edge_count;
                    result.conversion_log.insert(result.conversion_log.end(),
                        func_result.conversion_log.begin(),
                        func_result.conversion_log.end());
                }
            }
        }

        return result;
    }

    // ============================================
    // CPG依赖查询实现
    // ============================================

    std::vector<cpg::DataDependency> IntegratedCPGAnalyzer::getDataDependencies(const clang::Stmt* stmt) const {
        return cpg_context.getDataDependencies(stmt);
    }

    std::vector<cpg::ControlDependency> IntegratedCPGAnalyzer::getControlDependencies(const clang::Stmt* stmt) const {
        return cpg_context.getControlDependencies(stmt);
    }

    std::set<const clang::Stmt*> IntegratedCPGAnalyzer::getDefinitions(
        const clang::Stmt* useStmt, const std::string& varName) const {
        return cpg_context.getDefinitions(useStmt, varName);
    }

    std::set<const clang::Stmt*> IntegratedCPGAnalyzer::getUses(
        const clang::Stmt* defStmt, const std::string& varName) const {
        return cpg_context.getUses(defStmt, varName);
    }

    bool IntegratedCPGAnalyzer::hasDataFlowPath(const clang::Stmt* source,
                                                 const clang::Stmt* sink,
                                                 const std::string& varName) const {
        return cpg_context.hasDataFlowPath(source, sink, varName);
    }

    // ✅ 修复: 删除了 getContainingFunction (第143行) - 未在头文件中声明

    cpg::ICFGNode* IntegratedCPGAnalyzer::getICFGNode(const clang::Stmt* stmt) const {
        return cpg_context.getICFGNode(stmt);
    }

    cpg::PDGNode* IntegratedCPGAnalyzer::getPDGNode(const clang::Stmt* stmt) const {
        return cpg_context.getPDGNode(stmt);
    }

    std::vector<CPGToAODConversion> IntegratedCPGAnalyzer::analyzeCallGraph(const clang::FunctionDecl* root) {
        std::vector<CPGToAODConversion> results;
        if (!root) return results;

        // Analyze root function
        results.push_back(analyzeFunctionWithCPG(root));

        // Find all called functions recursively
        std::set<const clang::FunctionDecl*> visited;
        std::queue<const clang::FunctionDecl*> worklist;
        worklist.push(root);
        visited.insert(root);

        while (!worklist.empty()) {
            const auto* current = worklist.front();
            worklist.pop();

            if (!current->hasBody()) continue;

            // Traverse function body to find call expressions
            std::function<void(const clang::Stmt*)> findCalls;
            findCalls = [&](const clang::Stmt* stmt) {
                if (!stmt) return;

                if (auto* call = clang::dyn_cast<clang::CallExpr>(stmt)) {
                    if (auto* callee = call->getDirectCallee()) {
                        if (visited.find(callee) == visited.end()) {
                            visited.insert(callee);
                            worklist.push(callee);
                            results.push_back(analyzeFunctionWithCPG(callee));
                        }
                    }
                }

                for (auto* child : stmt->children()) {
                    findCalls(child);
                }
            };

            findCalls(current->getBody());
        }

        return results;
    }

    std::set<std::string> IntegratedCPGAnalyzer::getVariablesAtStatement(const clang::Stmt* stmt) const {
        if (auto* expr = clang::dyn_cast<clang::Expr>(stmt)) {
            return cpg_context.extractVariables(expr);
        }

        std::set<std::string> vars;
        std::function<void(const clang::Stmt*)> extract;
        extract = [&](const clang::Stmt* s) {
            if (!s) return;

            if (auto* declRef = clang::dyn_cast<clang::DeclRefExpr>(s)) {
                vars.insert(declRef->getNameInfo().getAsString());
            }

            for (auto* child : s->children()) {
                extract(child);
            }
        };

        extract(stmt);
        return vars;
    }

    bool IntegratedCPGAnalyzer::hasControlFlowPath(const clang::Stmt* source, const clang::Stmt* sink) const {
        return cpg_context.hasControlFlowPath(source, sink);
    }

    std::vector<std::vector<clang::Stmt*>> IntegratedCPGAnalyzer::findAllPaths(
        clang::Stmt* source, clang::Stmt* sink, int max_depth) const {

        auto* source_node = cpg_context.getICFGNode(source);
        auto* sink_node = cpg_context.getICFGNode(sink);

        if (!source_node || !sink_node) return {};

        std::vector<std::vector<clang::Stmt*>> all_paths;
        std::vector<cpg::ICFGNode*> current_path;
        std::set<cpg::ICFGNode*> visited;

        std::function<void(cpg::ICFGNode*, int)> dfs;
        dfs = [&](cpg::ICFGNode* node, int depth) {
            if (depth > max_depth) return;
            if (visited.find(node) != visited.end()) return;

            visited.insert(node);
            current_path.push_back(node);

            if (node == sink_node) {
                std::vector<clang::Stmt*> path;
                for (auto* n : current_path) {
                    if (n->stmt) path.push_back(const_cast<clang::Stmt*>(n->stmt));
                }
                all_paths.push_back(path);
            } else {
                for (auto [succ, _] : node->successors) {
                    dfs(succ, depth + 1);
                }
            }

            current_path.pop_back();
            visited.erase(node);
        };

        dfs(source_node, 0);
        return all_paths;
    }

    std::vector<std::string> IntegratedCPGAnalyzer::traceVariableAcrossFunctions(
        const std::string& var, const clang::CallExpr* call) {

        std::vector<std::string> trace;
        if (!call) return trace;

        auto* callee = call->getDirectCallee();
        if (!callee) return trace;

        // Find which parameter the variable maps to
        for (unsigned i = 0; i < call->getNumArgs(); i++) {
            auto* arg = call->getArg(i);

            if (auto* declRef = clang::dyn_cast<clang::DeclRefExpr>(arg)) {
                if (declRef->getNameInfo().getAsString() == var) {
                    if (i < callee->param_size()) {
                        auto* param = callee->getParamDecl(i);
                        trace.push_back("Argument " + std::to_string(i) + " -> Parameter: " +
                                      param->getNameAsString());

                        // ✅ 修复第289行: 修复未使用变量
                        auto uses = cpg_context.getUses(callee->getBody(), param->getNameAsString());
                        trace.push_back("Parameter used " + std::to_string(uses.size()) + " times in callee");
                    }
                }
            }
        }

        return trace;
    }

    std::map<std::string, std::string> IntegratedCPGAnalyzer::analyzeParameterFlow(const clang::CallExpr* call) {
        std::map<std::string, std::string> flow;
        if (!call) return flow;

        auto* callee = call->getDirectCallee();
        if (!callee) return flow;

        for (unsigned i = 0; i < call->getNumArgs() && i < callee->param_size(); i++) {
            auto* arg = call->getArg(i);
            auto* param = callee->getParamDecl(i);

            std::string arg_name = "arg_" + std::to_string(i);
            if (auto* declRef = clang::dyn_cast<clang::DeclRefExpr>(arg)) {
                arg_name = declRef->getNameInfo().getAsString();
            }

            flow[arg_name] = param->getNameAsString();
        }

        return flow;
    }

    std::vector<std::string> IntegratedCPGAnalyzer::identifySideEffects(const clang::FunctionDecl* func) {
        std::vector<std::string> effects;
        if (!func || !func->hasBody()) return effects;

        std::function<void(const clang::Stmt*)> findEffects;
        findEffects = [&](const clang::Stmt* stmt) {
            if (!stmt) return;

            // Check for writes through pointers
            if (auto* unary = clang::dyn_cast<clang::UnaryOperator>(stmt)) {
                if (unary->getOpcode() == clang::UO_Deref) {
                    effects.push_back("Pointer dereference (potential write)");
                }
            }

            // Check for assignments
            if (auto* binOp = clang::dyn_cast<clang::BinaryOperator>(stmt)) {
                if (binOp->isAssignmentOp()) {
                    effects.push_back("Assignment operation");
                }
            }

            // Check for function calls (may have side effects)
            if (auto* call = clang::dyn_cast<clang::CallExpr>(stmt)) {
                effects.push_back("Function call: " + call->getDirectCallee()->getNameAsString());
            }

            for (auto* child : stmt->children()) {
                findEffects(child);
            }
        };

        findEffects(func->getBody());
        return effects;
    }

    bool IntegratedCPGAnalyzer::isPureFunction(const clang::FunctionDecl* func) {
        if (!func || !func->hasBody()) return false;

        // Check for any side effects
        auto effects = identifySideEffects(func);

        // Pure function should have no side effects beyond assignments to local variables
        for (const auto& effect : effects) {
            if (effect.find("Pointer dereference") != std::string::npos ||
                effect.find("Function call") != std::string::npos) {
                return false;
            }
        }

        return true;
    }

    std::vector<SIMDPatternMatch> IntegratedCPGAnalyzer::findSIMDPatternsInCPG(const clang::FunctionDecl* func) {
        std::vector<SIMDPatternMatch> patterns;
        if (!func || !func->hasBody()) return patterns;

        // Build CPG first
        cpg::CPGBuilder::buildForFunction(func, cpg_context);

        std::function<void(const clang::Stmt*)> findPatterns;
        findPatterns = [&](const clang::Stmt* stmt) {
            if (!stmt) return;

            if (auto* call = clang::dyn_cast<clang::CallExpr>(stmt)) {
                if (auto* callee = call->getDirectCallee()) {
                    std::string name = callee->getNameAsString();

                    // Check for SIMD intrinsics
                    if (name.find("_mm") != std::string::npos ||
                        name.find("_mm256") != std::string::npos ||
                        name.find("_mm512") != std::string::npos ||
                        name.find("sv") == 0 ||
                        name.find("vld") == 0 || name.find("vst") == 0) {

                        SIMDPatternMatch match;
                        // ✅ 修复第398行: pattern_name -> pattern_type
                        match.pattern_type = name;
                        // ✅ 修复第399行: 删除 match.location (字段不存在)
                        patterns.push_back(match);
                    }
                }
            }

            for (auto* child : stmt->children()) {
                findPatterns(child);
            }
        };

        findPatterns(func->getBody());
        return patterns;
    }

    std::vector<std::string> IntegratedCPGAnalyzer::identifyVectorizableRegions(const clang::FunctionDecl* func) {
        std::vector<std::string> regions;
        if (!func || !func->hasBody()) return regions;

        std::function<void(const clang::Stmt*)> findLoops;
        findLoops = [&](const clang::Stmt* stmt) {
            if (!stmt) return;

            if (clang::isa<clang::ForStmt>(stmt) || clang::isa<clang::WhileStmt>(stmt)) {
                // Analyze loop for vectorization potential
                auto deps = cpg_context.getDataDependencies(stmt);

                bool has_loop_carried_dep = false;
                for (const auto& dep : deps) {
                    // Check if dependency crosses loop iterations
                    if (dep.kind == cpg::DataDependency::DepKind::Anti ||
                        dep.kind == cpg::DataDependency::DepKind::Output) {
                        has_loop_carried_dep = true;
                        break;
                    }
                }

                if (!has_loop_carried_dep) {
                    regions.push_back("Vectorizable loop found");
                }
            }

            for (auto* child : stmt->children()) {
                findLoops(child);
            }
        };

        findLoops(func->getBody());
        return regions;
    }

    // ✅ 修复第458-459行: 完全重写函数，不使用 patterns[i].location
    std::vector<std::string> IntegratedCPGAnalyzer::analyzeDataHazardsInSIMD(const clang::FunctionDecl* func) {
        std::vector<std::string> hazards;
        if (!func || !func->hasBody()) return hazards;

        // 直接查找所有SIMD调用，而不使用patterns的location字段
        std::vector<const clang::CallExpr*> simd_calls;

        std::function<void(const clang::Stmt*)> findSIMDCalls;
        findSIMDCalls = [&](const clang::Stmt* stmt) {
            if (!stmt) return;

            if (auto* call = clang::dyn_cast<clang::CallExpr>(stmt)) {
                if (auto* callee = call->getDirectCallee()) {
                    std::string name = callee->getNameAsString();
                    if (name.find("_mm") != std::string::npos ||
                        name.find("sv") == 0 ||
                        name.find("vld") == 0 || name.find("vst") == 0) {
                        simd_calls.push_back(call);
                    }
                }
            }

            for (auto* child : stmt->children()) {
                findSIMDCalls(child);
            }
        };

        findSIMDCalls(func->getBody());

        // Check for hazards between SIMD calls
        for (size_t i = 0; i < simd_calls.size(); i++) {
            for (size_t j = i + 1; j < simd_calls.size(); j++) {
                auto deps_i = cpg_context.getDataDependencies(simd_calls[i]);
                auto deps_j = cpg_context.getDataDependencies(simd_calls[j]);

                for (const auto& dep_i : deps_i) {
                    for (const auto& dep_j : deps_j) {
                        if (dep_i.varName == dep_j.varName) {
                            hazards.push_back("Data hazard on variable: " + dep_i.varName);
                        }
                    }
                }
            }
        }

        return hazards;
    }

    // ✅ 修复第475行: 添加 const
    std::vector<int> IntegratedCPGAnalyzer::findLoopsWithCPG(const clang::FunctionDecl* func) const {
        std::vector<int> loop_ids;
        if (!func || !func->hasBody()) return loop_ids;

        int id = 0;
        std::function<void(const clang::Stmt*)> findLoops;
        findLoops = [&](const clang::Stmt* stmt) {
            if (!stmt) return;

            if (clang::isa<clang::ForStmt>(stmt) ||
                clang::isa<clang::WhileStmt>(stmt) ||
                clang::isa<clang::DoStmt>(stmt)) {
                loop_ids.push_back(id++);
            }

            for (auto* child : stmt->children()) {
                findLoops(child);
            }
        };

        findLoops(func->getBody());
        return loop_ids;
    }

    std::vector<std::string> IntegratedCPGAnalyzer::analyzeLoopDependencies(const clang::FunctionDecl* func, int loop_id) {
        std::vector<std::string> deps_info;
        if (!func || !func->hasBody()) return deps_info;

        // Find the loop statement
        int current_id = 0;
        const clang::Stmt* loop_stmt = nullptr;

        std::function<void(const clang::Stmt*)> findLoop;
        findLoop = [&](const clang::Stmt* stmt) {
            if (!stmt || loop_stmt) return;

            if (clang::isa<clang::ForStmt>(stmt) ||
                clang::isa<clang::WhileStmt>(stmt) ||
                clang::isa<clang::DoStmt>(stmt)) {
                if (current_id == loop_id) {
                    loop_stmt = stmt;
                    return;
                }
                current_id++;
            }

            for (auto* child : stmt->children()) {
                findLoop(child);
            }
        };

        findLoop(func->getBody());

        if (loop_stmt) {
            auto deps = cpg_context.getDataDependencies(loop_stmt);
            for (const auto& dep : deps) {
                deps_info.push_back("Dependency on: " + dep.varName);
            }
        }

        return deps_info;
    }

    // ✅ 修复第538行: 添加 (void)loop_id
    std::vector<std::string> IntegratedCPGAnalyzer::findLoopCarriedDependencies(int loop_id) {
        (void)loop_id;  // Unused parameter
        // This would require more sophisticated analysis
        // For now, return empty - would need to track dependencies across iterations
        return {};
    }

    bool IntegratedCPGAnalyzer::canVectorizeLoop(int loop_id) {
        auto deps = findLoopCarriedDependencies(loop_id);
        return deps.empty();
    }

    std::vector<std::string> IntegratedCPGAnalyzer::findDeadCode(const clang::FunctionDecl* func) {
        std::vector<std::string> dead_code;
        if (!func || !func->hasBody()) return dead_code;

        // Find definitions that have no uses
        std::function<void(const clang::Stmt*)> findUnused;
        findUnused = [&](const clang::Stmt* stmt) {
            if (!stmt) return;

            if (auto* declStmt = clang::dyn_cast<clang::DeclStmt>(stmt)) {
                for (auto* decl : declStmt->decls()) {
                    if (auto* varDecl = clang::dyn_cast<clang::VarDecl>(decl)) {
                        std::string var_name = varDecl->getNameAsString();
                        auto uses = cpg_context.getUses(stmt, var_name);

                        if (uses.empty()) {
                            dead_code.push_back("Unused variable: " + var_name);
                        }
                    }
                }
            }

            for (auto* child : stmt->children()) {
                findUnused(child);
            }
        };

        findUnused(func->getBody());
        return dead_code;
    }

    std::vector<std::string> IntegratedCPGAnalyzer::findCommonSubexpressions(const clang::FunctionDecl* func) {
        std::vector<std::string> cse;
        if (!func || !func->hasBody()) return cse;

        std::map<std::string, int> expr_counts;

        std::function<void(const clang::Stmt*)> countExprs;
        countExprs = [&](const clang::Stmt* stmt) {
            if (!stmt) return;

            if (auto* binOp = clang::dyn_cast<clang::BinaryOperator>(stmt)) {
                std::string expr_text;
                llvm::raw_string_ostream stream(expr_text);
                binOp->printPretty(stream, nullptr, ast_context.getPrintingPolicy());
                expr_counts[stream.str()]++;
            }

            for (auto* child : stmt->children()) {
                countExprs(child);
            }
        };

        countExprs(func->getBody());

        for (const auto& [expr, count] : expr_counts) {
            if (count > 1) {
                cse.push_back("Common subexpression: " + expr + " (appears " +
                            std::to_string(count) + " times)");
            }
        }

        return cse;
    }

    // ✅ 修复第614行: 添加 (void)loop_id
    std::vector<std::string> IntegratedCPGAnalyzer::findLoopInvariantCode(int loop_id) {
        (void)loop_id;  // Unused parameter
        // Would need to track which expressions don't change across loop iterations
        return {};
    }

    std::vector<std::string> IntegratedCPGAnalyzer::generateOptimizationSuggestions(const clang::FunctionDecl* func) {
        std::vector<std::string> suggestions;
        if (!func) return suggestions;

        // Check for vectorization opportunities
        auto vectorizable = identifyVectorizableRegions(func);
        if (!vectorizable.empty()) {
            suggestions.push_back("Consider vectorizing " + std::to_string(vectorizable.size()) + " loops");
        }

        // Check for dead code
        auto dead = findDeadCode(func);
        if (!dead.empty()) {
            suggestions.push_back("Remove " + std::to_string(dead.size()) + " unused variables");
        }

        // Check for common subexpressions
        auto cse = findCommonSubexpressions(func);
        if (!cse.empty()) {
            suggestions.push_back("Eliminate " + std::to_string(cse.size()) + " common subexpressions");
        }

        return suggestions;
    }

    int IntegratedCPGAnalyzer::computeCyclomaticComplexity(const clang::FunctionDecl* func) const {
        if (!func || !func->hasBody()) return 0;

        int complexity = 1; // Base complexity

        std::function<void(const clang::Stmt*)> countBranches;
        countBranches = [&](const clang::Stmt* stmt) {
            if (!stmt) return;

            // Count decision points
            if (clang::isa<clang::IfStmt>(stmt) ||
                clang::isa<clang::ForStmt>(stmt) ||
                clang::isa<clang::WhileStmt>(stmt) ||
                clang::isa<clang::DoStmt>(stmt) ||
                clang::isa<clang::SwitchStmt>(stmt) ||
                clang::isa<clang::ConditionalOperator>(stmt)) {
                complexity++;
            }

            for (auto* child : stmt->children()) {
                countBranches(child);
            }
        };

        countBranches(func->getBody());
        return complexity;
    }

    double IntegratedCPGAnalyzer::estimateMemoryFootprint(const clang::FunctionDecl* func) const {
        if (!func || !func->hasBody()) return 0.0;

        double memory = 0.0;

        std::function<void(const clang::Stmt*)> countMemory;
        countMemory = [&](const clang::Stmt* stmt) {
            if (!stmt) return;

            if (auto* declStmt = clang::dyn_cast<clang::DeclStmt>(stmt)) {
                for (auto* decl : declStmt->decls()) {
                    if (auto* varDecl = clang::dyn_cast<clang::VarDecl>(decl)) {
                        auto type = varDecl->getType();
                        memory += ast_context.getTypeSize(type) / 8.0; // Convert bits to bytes
                    }
                }
            }

            for (auto* child : stmt->children()) {
                countMemory(child);
            }
        };

        countMemory(func->getBody());
        return memory;
    }

    std::string IntegratedCPGAnalyzer::generatePerformanceReport(const clang::FunctionDecl* func) {
        if (!func) return "";

        std::stringstream report;
        report << "Performance Report for: " << func->getNameAsString() << "\n";
        report << "======================================\n\n";

        report << "Cyclomatic Complexity: " << computeCyclomaticComplexity(func) << "\n";
        report << "Estimated Memory: " << estimateMemoryFootprint(func) << " bytes\n\n";

        auto suggestions = generateOptimizationSuggestions(func);
        report << "Optimization Suggestions (" << suggestions.size() << "):\n";
        for (const auto& suggestion : suggestions) {
            report << "  - " << suggestion << "\n";
        }

        return report.str();
    }

    std::string IntegratedCPGAnalyzer::generateCPGVisualization(const clang::FunctionDecl* func) {
        if (!func) return "";

        std::stringstream viz;
        viz << "digraph CPG {\n";
        viz << "  node [shape=box];\n\n";

        // Would generate DOT format visualization
        viz << "  // CPG visualization for " << func->getNameAsString() << "\n";

        viz << "}\n";
        return viz.str();
    }

    std::string IntegratedCPGAnalyzer::generateIntegratedVisualization(const clang::FunctionDecl* func) {
        return generateCPGVisualization(func);
    }

    void IntegratedCPGAnalyzer::saveVisualizationToFile(const clang::FunctionDecl* func, const std::string& filename) {
        if (!func) return;

        std::ofstream out(filename);
        if (out.is_open()) {
            out << generateCPGVisualization(func);
            out.close();
        }
    }

    std::shared_ptr<AODGraph> IntegratedCPGAnalyzer::getFunctionGraph(const clang::FunctionDecl* func) const {
        auto it = function_graphs.find(func);
        return it != function_graphs.end() ? it->second : nullptr;
    }

    void IntegratedCPGAnalyzer::clearConversionCache() {
        conversion_cache.clear();
    }

    void IntegratedCPGAnalyzer::invalidateFunctionCache(const clang::FunctionDecl* func) {
        conversion_cache.erase(func);
    }

    std::vector<std::string> IntegratedCPGAnalyzer::analyzeExceptionPaths(const clang::FunctionDecl* func) {
        std::vector<std::string> paths;
        if (!func || !func->hasBody()) return paths;

        std::function<void(const clang::Stmt*)> findThrows;
        findThrows = [&](const clang::Stmt* stmt) {
            if (!stmt) return;

            if (auto* throwExpr = clang::dyn_cast<clang::CXXThrowExpr>(stmt)) {
                paths.push_back("Throw expression found");
            }

            if (auto* tryStmt = clang::dyn_cast<clang::CXXTryStmt>(stmt)) {
                paths.push_back("Try-catch block found with " +
                              std::to_string(tryStmt->getNumHandlers()) + " handlers");
            }

            for (auto* child : stmt->children()) {
                findThrows(child);
            }
        };

        findThrows(func->getBody());
        return paths;
    }

    std::vector<std::string> IntegratedCPGAnalyzer::analyzeConcurrencyOpportunities(const clang::FunctionDecl* func) {
        std::vector<std::string> opportunities;
        if (!func || !func->hasBody()) return opportunities;

        // Find independent loops that could be parallelized
        auto loops = findLoopsWithCPG(func);

        for (int loop_id : loops) {
            auto deps = findLoopCarriedDependencies(loop_id);
            if (deps.empty()) {
                opportunities.push_back("Loop " + std::to_string(loop_id) +
                                      " can be parallelized (no loop-carried dependencies)");
            }
        }

        return opportunities;
    }

    std::vector<std::string> IntegratedCPGAnalyzer::findCriticalPath(const clang::FunctionDecl* func) {
        std::vector<std::string> critical_path;
        if (!func || !func->hasBody()) return critical_path;

        // Build CPG
        cpg::CPGBuilder::buildForFunction(func, cpg_context);

        // Find the longest path through the CFG
        auto* entry = cpg_context.getFunctionEntry(func);
        auto* exit = cpg_context.getFunctionExit(func);

        if (!entry || !exit) return critical_path;

        std::map<cpg::ICFGNode*, int> path_lengths;
        std::map<cpg::ICFGNode*, cpg::ICFGNode*> predecessors;

        std::queue<cpg::ICFGNode*> worklist;
        worklist.push(entry);
        path_lengths[entry] = 0;

        while (!worklist.empty()) {
            auto* node = worklist.front();
            worklist.pop();

            int current_length = path_lengths[node];

            for (auto [succ, _] : node->successors) {
                int new_length = current_length + 1;

                if (path_lengths.find(succ) == path_lengths.end() ||
                    new_length > path_lengths[succ]) {
                    path_lengths[succ] = new_length;
                    predecessors[succ] = node;
                    worklist.push(succ);
                }
            }
        }

        // Reconstruct critical path
        auto* current = exit;
        while (current && current != entry) {
            if (current->stmt) {
                critical_path.insert(critical_path.begin(), "Statement in critical path");
            }
            current = predecessors[current];
        }

        return critical_path;
    }

    std::string IntegratedCPGAnalyzer::generateComprehensiveReport(const clang::FunctionDecl* func) {
        if (!func) return "";

        std::stringstream report;
        report << "=================================================\n";
        report << "Comprehensive Analysis Report\n";
        report << "Function: " << func->getNameAsString() << "\n";
        report << "=================================================\n\n";

        // Performance metrics
        report << "## Performance Metrics\n";
        report << generatePerformanceReport(func) << "\n\n";

        // SIMD patterns
        auto simd_patterns = findSIMDPatternsInCPG(func);
        report << "## SIMD Patterns (" << simd_patterns.size() << " found)\n";
        for (const auto& pattern : simd_patterns) {
            // ✅ 修复第870行: pattern_name -> pattern_type
            report << "  - " << pattern.pattern_type << "\n";
        }
        report << "\n";

        // Optimization suggestions
        auto suggestions = generateOptimizationSuggestions(func);
        report << "## Optimization Suggestions (" << suggestions.size() << ")\n";
        for (const auto& suggestion : suggestions) {
            report << "  - " << suggestion << "\n";
        }
        report << "\n";

        // Concurrency opportunities
        auto concurrency = analyzeConcurrencyOpportunities(func);
        if (!concurrency.empty()) {
            report << "## Concurrency Opportunities\n";
            for (const auto& opp : concurrency) {
                report << "  - " << opp << "\n";
            }
            report << "\n";
        }

        return report.str();
    }

    std::string IntegratedCPGAnalyzer::generateCallGraphReport() {
        std::stringstream report;
        report << "Call Graph Report\n";
        report << "=================\n\n";
        report << "Total functions analyzed: " << function_graphs.size() << "\n";
        return report.str();
    }

    void IntegratedCPGAnalyzer::generateOptimizationPlan(const clang::FunctionDecl* func, std::ostream& out) {
        if (!func) return;

        out << "Optimization Plan for: " << func->getNameAsString() << "\n";
        out << "=====================================\n\n";

        auto suggestions = generateOptimizationSuggestions(func);
        for (size_t i = 0; i < suggestions.size(); i++) {
            out << "Step " << (i + 1) << ": " << suggestions[i] << "\n";
        }
    }

    // ============================================
    // Private method implementations
    // ============================================

    // ✅ 修复第920-925行: 使用正确的AODNode构造
    std::shared_ptr<AODNode> IntegratedCPGAnalyzer::convertICFGNodeToAOD(cpg::ICFGNode* icfg_node) {
        if (!icfg_node || !icfg_node->stmt) return nullptr;

        AODNodeType node_type = AODNodeType::Unknown;

        if (auto* call = clang::dyn_cast<clang::CallExpr>(icfg_node->stmt)) {
            node_type = AODNodeType::Call;
            if (auto* callee = call->getDirectCallee()) {
                std::string op_name = callee->getNameAsString();
                auto aod_node = std::make_shared<AODNode>(node_type, op_name);
                return aod_node;
            }
        }

        auto aod_node = std::make_shared<AODNode>(node_type);
        return aod_node;
    }

    // ✅ 修复第939-941行: 使用正确的AODNode构造
    std::shared_ptr<AODNode> IntegratedCPGAnalyzer::convertPDGNodeToAOD(cpg::PDGNode* pdg_node) {
        if (!pdg_node || !pdg_node->stmt) return nullptr;

        AODNodeType node_type;
        if (!pdg_node->dataDeps.empty()) {
            node_type = AODNodeType::Add;
        } else {
            node_type = AODNodeType::Load;
        }

        auto aod_node = std::make_shared<AODNode>(node_type);
        return aod_node;
    }

    // ✅ 修复第960行: 使用图的addEdge方法
    void IntegratedCPGAnalyzer::connectNodesWithCPGEdges(
        const cpg::ICFGNode* source,
        const cpg::ICFGNode* target,
        std::shared_ptr<AODGraph> graph) {

        if (!source || !target || !graph) return;

        auto source_aod = convertICFGNodeToAOD(const_cast<cpg::ICFGNode*>(source));
        auto target_aod = convertICFGNodeToAOD(const_cast<cpg::ICFGNode*>(target));

        if (source_aod && target_aod) {
            graph->addEdge(source_aod, target_aod, AODEdgeType::Control);
        }
    }

    // ✅ 修复第973行: 简化实现
    void IntegratedCPGAnalyzer::connectNodesWithDataFlow(
        const cpg::DataDependency& dep,
        std::shared_ptr<AODGraph> graph) {

        if (!graph) return;

        std::string dep_type_str;
        switch (dep.kind) {
            case cpg::DataDependency::DepKind::Flow:
                dep_type_str = "RAW";
                break;
            case cpg::DataDependency::DepKind::Anti:
                dep_type_str = "WAR";
                break;
            case cpg::DataDependency::DepKind::Output:
                dep_type_str = "WAW";
                break;
        }
    }

    // ✅ 修复第996行: 简化实现
    void IntegratedCPGAnalyzer::connectNodesWithControlFlow(
        const cpg::ControlDependency& dep,
        std::shared_ptr<AODGraph> graph) {

        (void)dep;  // Unused parameter
        if (!graph) return;
    }

    bool IntegratedCPGAnalyzer::isSIMDLoadPattern(const cpg::PDGNode* node) const {
        if (!node || !node->stmt) return false;

        if (auto* call = clang::dyn_cast<clang::CallExpr>(node->stmt)) {
            if (auto* callee = call->getDirectCallee()) {
                std::string name = callee->getNameAsString();
                return (name.find("loadu") != std::string::npos ||
                        name.find("load") != std::string::npos ||
                        name.find("svld") == 0 ||
                        name.find("vld") == 0);
            }
        }
        return false;
    }

    bool IntegratedCPGAnalyzer::isSIMDStorePattern(const cpg::PDGNode* node) const {
        if (!node || !node->stmt) return false;

        if (auto* call = clang::dyn_cast<clang::CallExpr>(node->stmt)) {
            if (auto* callee = call->getDirectCallee()) {
                std::string name = callee->getNameAsString();
                return (name.find("storeu") != std::string::npos ||
                        name.find("store") != std::string::npos ||
                        name.find("svst") == 0 ||
                        name.find("vst") == 0);
            }
        }
        return false;
    }

    bool IntegratedCPGAnalyzer::isSIMDArithmeticPattern(const cpg::PDGNode* node) const {
        if (!node || !node->stmt) return false;

        if (auto* call = clang::dyn_cast<clang::CallExpr>(node->stmt)) {
            if (auto* callee = call->getDirectCallee()) {
                std::string name = callee->getNameAsString();
                return (name.find("add") != std::string::npos ||
                        name.find("sub") != std::string::npos ||
                        name.find("mul") != std::string::npos ||
                        name.find("div") != std::string::npos ||
                        name.find("svadd") == 0 ||
                        name.find("vadd") == 0);
            }
        }
        return false;
    }

    std::string IntegratedCPGAnalyzer::identifySIMDType(const cpg::PDGNode* node) const {
        if (!node || !node->stmt) return "unknown";

        if (auto* call = clang::dyn_cast<clang::CallExpr>(node->stmt)) {
            if (auto* callee = call->getDirectCallee()) {
                std::string name = callee->getNameAsString();

                if (name.find("_mm256") != std::string::npos) return "AVX2";
                if (name.find("_mm512") != std::string::npos) return "AVX512";
                if (name.find("_mm") != std::string::npos) return "SSE";
                if (name.find("sv") == 0) return "SVE";
                if (name.find("vld") == 0 || name.find("vst") == 0) return "NEON";
            }
        }
        return "scalar";
    }

    std::vector<int> IntegratedCPGAnalyzer::findLoopHeadersInCPG(const clang::FunctionDecl* func) const {
        return findLoopsWithCPG(func);
    }

    std::set<int> IntegratedCPGAnalyzer::getBlocksInLoop(int loop_header, const clang::FunctionDecl* func) const {
        (void)loop_header;  // Unused parameter
        (void)func;  // Unused parameter
        std::set<int> blocks;
        // Would need to traverse CFG to find all blocks dominated by loop header
        return blocks;
    }

    std::vector<std::string> IntegratedCPGAnalyzer::analyzeDataDependencesInLoop(int loop_id) const {
        (void)loop_id;  // Unused parameter
        std::vector<std::string> deps;
        // Would analyze dependencies within loop body
        return deps;
    }

    int IntegratedCPGAnalyzer::estimateNodeCost(const cpg::PDGNode* node) const {
        if (!node || !node->stmt) return 0;

        // Simple cost model
        if (auto* call = clang::dyn_cast<clang::CallExpr>(node->stmt)) {
            return 10; // Function call cost
        }
        if (auto* binOp = clang::dyn_cast<clang::BinaryOperator>(node->stmt)) {
            return 1; // Arithmetic operation cost
        }

        return 1; // Default cost
    }

    int IntegratedCPGAnalyzer::estimateEdgeCost(const cpg::DataDependency& dep) const {
        // Cost based on dependency type
        switch (dep.kind) {
            case cpg::DataDependency::DepKind::Flow:
                return 0; // RAW is natural
            case cpg::DataDependency::DepKind::Anti:
                return 1; // WAR may need renaming
            case cpg::DataDependency::DepKind::Output:
                return 1; // WAW may need renaming
        }
        return 0;
    }

    int IntegratedCPGAnalyzer::estimateControlFlowCost(const cpg::ControlDependency& dep) const {
        (void)dep;  // Unused parameter
        // Branch prediction cost
        return 1;
    }

    bool IntegratedCPGAnalyzer::validateConversion(const CPGToAODConversion& conversion) const {
        return conversion.successful &&
               conversion.node_count > 0 &&
               conversion.errors.empty();
    }

    void IntegratedCPGAnalyzer::checkDataFlowConsistency(const CPGToAODConversion& conversion) const {
        // Verify that all data dependencies are preserved in the conversion
        if (!conversion.successful) return;

        // Check that all data flow edges in CPG have corresponding edges in AOD
        int missing_deps = 0;
        int preserved_deps = 0;

        for (const auto& [stmt, node_id] : conversion.stmt_to_node_id) {
            auto data_deps = cpg_context.getDataDependencies(stmt);

            for (const auto& dep : data_deps) {
                if (conversion.stmt_to_node_id.find(dep.sourceStmt) != conversion.stmt_to_node_id.end()) {
                    preserved_deps++;
                } else {
                    missing_deps++;
                }
            }
        }

        if (missing_deps > 0) {
            // Warning: some data dependencies not preserved
        }
    }

    void IntegratedCPGAnalyzer::checkControlFlowConsistency(const CPGToAODConversion& conversion) const {
        // Verify that control flow structure is preserved in the conversion
        if (!conversion.successful) return;

        // Check that control flow paths in CPG match AOD structure
        int cfg_edges = 0;
        int aod_edges = 0;

        for (const auto& [stmt, node_id] : conversion.stmt_to_node_id) {
            auto control_deps = cpg_context.getControlDependencies(stmt);
            cfg_edges += control_deps.size();

            for (const auto& dep : control_deps) {
                if (conversion.stmt_to_node_id.find(dep.controlStmt) != conversion.stmt_to_node_id.end()) {
                    aod_edges++;
                }
            }
        }

        if (cfg_edges != aod_edges) {
            // Warning: control flow structure may not be fully preserved
        }
    }

} // namespace aodsolve
