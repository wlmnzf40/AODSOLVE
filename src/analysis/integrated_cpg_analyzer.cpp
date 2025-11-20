#include "analysis/integrated_cpg_analyzer.h"

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

} // namespace aodsolve