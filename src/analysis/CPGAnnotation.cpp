// CPGAnnotation_v2.cpp - 改进版实现
#include "analysis/CPGAnnotation.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Analysis/CFG.h"
#include "clang/Lex/Lexer.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/raw_ostream.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Basic/SourceLocation.h"

#include <queue>
#include <iostream>
#include <fstream>
#include <sstream>
#include <algorithm>
#include "clang/AST/ParentMapContext.h"

namespace cpg {

// ============================================
// ICFGNode实现
// ============================================

std::string ICFGNode::getLabel() const {
    std::ostringstream oss;
    switch (kind) {
        case ICFGNodeKind::Entry:
            oss << "Entry: " << (func ? func->getNameAsString() : "?");
            break;
        case ICFGNodeKind::Exit:
            oss << "Exit: " << (func ? func->getNameAsString() : "?");
            break;
        case ICFGNodeKind::CallSite:
            oss << "Call: " << (callee ? callee->getNameAsString() : "?");
            break;
        case ICFGNodeKind::ReturnSite:
            oss << "Return from: " << (callee ? callee->getNameAsString() : "?");
            break;
        case ICFGNodeKind::FormalIn:
            oss << "FormalIn[" << paramIndex << "]";
            break;
        case ICFGNodeKind::FormalOut:
            oss << "FormalOut[" << paramIndex << "]";
            break;
        case ICFGNodeKind::ActualIn:
            oss << "ActualIn[" << paramIndex << "]";
            break;
        case ICFGNodeKind::ActualOut:
            oss << "ActualOut[" << paramIndex << "]";
            break;
        case ICFGNodeKind::Statement:
            if (stmt) {
                oss << stmt->getStmtClassName();
            }
            break;
    }
    return oss.str();
}

void ICFGNode::dump(const clang::SourceManager* SM) const {
    llvm::outs() << "[ICFGNode] " << getLabel();
    if (stmt && SM) {
        clang::PresumedLoc loc = SM->getPresumedLoc(stmt->getBeginLoc());
        if (loc.isValid()) {
            llvm::outs() << " @Line:" << loc.getLine();
        }
    }
    llvm::outs() << "\n";

    if (!successors.empty()) {
        llvm::outs() << "  Successors: ";
        for (const auto& [succ, kind] : successors) {
            llvm::outs() << succ->getLabel() << " (";
            switch (kind) {
                case ICFGEdgeKind::Intraprocedural: llvm::outs() << "intra"; break;
                case ICFGEdgeKind::Call: llvm::outs() << "call"; break;
                case ICFGEdgeKind::Return: llvm::outs() << "ret"; break;
                case ICFGEdgeKind::ParamIn: llvm::outs() << "pin"; break;
                case ICFGEdgeKind::ParamOut: llvm::outs() << "pout"; break;
                case ICFGEdgeKind::True: llvm::outs() << "T"; break;
                case ICFGEdgeKind::False: llvm::outs() << "F"; break;
                case ICFGEdgeKind::Unconditional: llvm::outs() << "ε"; break;
            }
            llvm::outs() << "), ";
        }
        llvm::outs() << "\n";
    }
}

// ============================================
// PDGNode实现
// ============================================

void PDGNode::dump(const clang::SourceManager* SM) const {
    llvm::outs() << "[PDGNode] ";
    if (stmt) {
        llvm::outs() << stmt->getStmtClassName();
        if (SM) {
            clang::PresumedLoc loc = SM->getPresumedLoc(stmt->getBeginLoc());
            if (loc.isValid()) {
                llvm::outs() << " @Line:" << loc.getLine();
            }
        }
    }
    llvm::outs() << "\n";

    if (!dataDeps.empty()) {
        llvm::outs() << "  Data Dependencies:\n";
        for (const auto& dep : dataDeps) {
            llvm::outs() << "    " << dep.varName << " <- ";
            switch (dep.kind) {
                case DataDependency::DepKind::Flow: llvm::outs() << "Flow"; break;
                case DataDependency::DepKind::Anti: llvm::outs() << "Anti"; break;
                case DataDependency::DepKind::Output: llvm::outs() << "Output"; break;
            }
            llvm::outs() << "\n";
        }
    }

    if (!controlDeps.empty()) {
        llvm::outs() << "  Control Dependencies:\n";
        for (const auto& dep : controlDeps) {
            llvm::outs() << "    Controlled by: " << dep.controlStmt->getStmtClassName()
                        << " [" << (dep.branchValue ? "T" : "F") << "]\n";
        }
    }
}

// ============================================
// CallContext实现
// ============================================

std::string CallContext::toString() const {
    std::ostringstream oss;
    oss << "[";
    for (size_t i = 0; i < callStack.size(); ++i) {
        if (i > 0) oss << " -> ";
        // oss << callStack[i]->getDirectCallee()->getNameAsString();
    }
    oss << "]";
    return oss.str();
}

// ============================================
// PathCondition实现
// ============================================

bool PathCondition::isFeasible() const {
    // 预留：实现路径可行性检查（可以使用约束求解器）
    return true;
}

std::string PathCondition::toString() const {
    std::ostringstream oss;
    oss << "Path[";
    for (size_t i = 0; i < conditions.size(); ++i) {
        if (i > 0) oss << ", ";
        oss << (conditions[i].second ? "T" : "F");
    }
    oss << "]";
    return oss.str();
}

// ============================================
// CPGContext实现
// ============================================

CPGContext::CPGContext(clang::ASTContext& ctx) : astContext(ctx) {}

// ---------- ICFG接口实现 ----------

ICFGNode* CPGContext::getICFGNode(const clang::Stmt* stmt) const {
    auto it = stmtToICFGNode.find(stmt);
    return it != stmtToICFGNode.end() ? it->second : nullptr;
}

ICFGNode* CPGContext::getFunctionEntry(const clang::FunctionDecl* func) const {
    auto it = funcEntries.find(func);
    return it != funcEntries.end() ? it->second : nullptr;
}

ICFGNode* CPGContext::getFunctionExit(const clang::FunctionDecl* func) const {
    auto it = funcExits.find(func);
    return it != funcExits.end() ? it->second : nullptr;
}

std::vector<ICFGNode*> CPGContext::getSuccessors(ICFGNode* node) const {
    std::vector<ICFGNode*> result;
    for (const auto& [succ, _] : node->successors) {
        result.push_back(succ);
    }
    return result;
}

std::vector<ICFGNode*> CPGContext::getPredecessors(ICFGNode* node) const {
    std::vector<ICFGNode*> result;
    for (const auto& [pred, _] : node->predecessors) {
        result.push_back(pred);
    }
    return result;
}

std::vector<std::pair<ICFGNode*, ICFGEdgeKind>>
CPGContext::getSuccessorsWithEdgeKind(ICFGNode* node) const {
    return node->successors;
}

// ---------- PDG接口实现 ----------

PDGNode* CPGContext::getPDGNode(const clang::Stmt* stmt) const {
    auto it = pdgNodes.find(stmt);
    return it != pdgNodes.end() ? it->second.get() : nullptr;
}

std::vector<DataDependency> CPGContext::getDataDependencies(const clang::Stmt* stmt) const {
    auto* node = getPDGNode(stmt);
    return node ? node->dataDeps : std::vector<DataDependency>();
}

std::vector<ControlDependency> CPGContext::getControlDependencies(const clang::Stmt* stmt) const {
    auto* node = getPDGNode(stmt);
    return node ? node->controlDeps : std::vector<ControlDependency>();
}

std::set<const clang::Stmt*> CPGContext::getDefinitions(
    const clang::Stmt* useStmt, const std::string& varName) const {

    auto* func = getContainingFunction(useStmt);
    if (!func) return {};

    auto it = reachingDefsMap.find(func);
    if (it == reachingDefsMap.end()) return {};

    const auto& reachInfo = it->second;
    auto reachIt = reachInfo.reachingDefs.find(useStmt);
    if (reachIt == reachInfo.reachingDefs.end()) return {};

    auto varIt = reachIt->second.find(varName);
    if (varIt == reachIt->second.end()) return {};

    return varIt->second;
}

std::set<const clang::Stmt*> CPGContext::getUses(
    const clang::Stmt* defStmt, const std::string& varName) const {

    std::set<const clang::Stmt*> uses;

    // 遍历所有PDG节点找到使用该定义的语句
    for (const auto& [stmt, node] : pdgNodes) {
        for (const auto& dep : node->dataDeps) {
            if (dep.sourceStmt == defStmt && dep.varName == varName) {
                uses.insert(stmt);
            }
        }
    }

    return uses;
}

// ---------- 路径查询实现 ----------

bool CPGContext::hasDataFlowPath(const clang::Stmt* source,
                                  const clang::Stmt* sink,
                                  const std::string& varName) const {
    // 使用BFS查找数据流路径
    std::queue<const clang::Stmt*> worklist;
    std::set<const clang::Stmt*> visited;

    worklist.push(source);
    visited.insert(source);

    while (!worklist.empty()) {
        auto* current = worklist.front();
        worklist.pop();

        if (current == sink) return true;

        // 获取当前语句定义的所有变量
        auto definedVars = getDefinedVars(current);

        for (const auto& var : definedVars) {
            if (!varName.empty() && var != varName) continue;

            // 找到所有使用该变量的语句
            auto uses = getUses(current, var);
            for (auto* use : uses) {
                if (visited.find(use) == visited.end()) {
                    worklist.push(use);
                    visited.insert(use);
                }
            }
        }
    }

    return false;
}

bool CPGContext::hasControlFlowPath(const clang::Stmt* source,
                                     const clang::Stmt* sink) const {
    auto* sourceNode = getICFGNode(source);
    auto* sinkNode = getICFGNode(sink);

    if (!sourceNode || !sinkNode) return false;

    std::queue<ICFGNode*> worklist;
    std::set<ICFGNode*> visited;

    worklist.push(sourceNode);
    visited.insert(sourceNode);

    while (!worklist.empty()) {
        auto* current = worklist.front();
        worklist.pop();

        if (current == sinkNode) return true;

        for (auto* succ : getSuccessors(current)) {
            if (visited.find(succ) == visited.end()) {
                worklist.push(succ);
                visited.insert(succ);
            }
        }
    }

    return false;
}

std::vector<std::vector<ICFGNode*>>
CPGContext::findAllPaths(ICFGNode* source, ICFGNode* sink, int maxDepth) const {
    std::vector<std::vector<ICFGNode*>> allPaths;
    std::vector<ICFGNode*> currentPath;
    std::set<ICFGNode*> visited;

    std::function<void(ICFGNode*, int)> dfs = [&](ICFGNode* node, int depth) {
        if (depth > maxDepth) return;

        currentPath.push_back(node);
        visited.insert(node);

        if (node == sink) {
            allPaths.push_back(currentPath);
        } else {
            for (auto* succ : getSuccessors(node)) {
                if (visited.find(succ) == visited.end()) {
                    dfs(succ, depth + 1);
                }
            }
        }

        visited.erase(node);
        currentPath.pop_back();
    };

    dfs(source, 0);
    return allPaths;
}

// ---------- 辅助功能实现 ----------

const clang::FunctionDecl* CPGContext::getContainingFunction(const clang::Stmt* stmt) const {
    for (const auto& [func, nodes] : icfgNodes) {
        for (const auto& node : nodes) {
            if (node->stmt == stmt) {
                return func;
            }
        }
    }
    return nullptr;
}

const clang::CFG* CPGContext::getCFG(const clang::FunctionDecl* func) const {
    auto it = cfgCache.find(func);
    return it != cfgCache.end() ? it->second.get() : nullptr;
}

// ---------- 可视化实现 ----------

void CPGContext::dumpICFG(const clang::FunctionDecl* func) const {
    llvm::outs() << "\n========== ICFG: " << func->getNameAsString() << " ==========\n";

    auto it = icfgNodes.find(func);
    if (it == icfgNodes.end()) {
        llvm::outs() << "No ICFG found\n";
        return;
    }

    const clang::SourceManager& SM = astContext.getSourceManager();
    for (const auto& node : it->second) {
        node->dump(&SM);
    }

    llvm::outs() << "===============================================\n\n";
}

void CPGContext::dumpPDG(const clang::FunctionDecl* func) const {
    llvm::outs() << "\n========== PDG: " << func->getNameAsString() << " ==========\n";

    int count = 0;
    const clang::SourceManager& SM = astContext.getSourceManager();
    for (const auto& [stmt, node] : pdgNodes) {
        if (getContainingFunction(stmt) == func) {
            llvm::outs() << "[" << count++ << "] ";
            node->dump(&SM);
        }
    }

    llvm::outs() << "===============================================\n\n";
}

void CPGContext::dumpCPG(const clang::FunctionDecl* func) const {
    llvm::outs() << "\n========== CPG: " << func->getNameAsString() << " ==========\n";
    dumpICFG(func);
    dumpPDG(func);
}

void CPGContext::dumpNode(ICFGNode* node) const {
    if (node) {
        const clang::SourceManager& SM = astContext.getSourceManager();
        node->dump(&SM);
    }
}

void CPGContext::dumpNode(PDGNode* node) const {
    if (node) {
        const clang::SourceManager& SM = astContext.getSourceManager();
        node->dump(&SM);
    }
}

// ---------- 统计信息 ----------

void CPGContext::printStatistics() const {
    llvm::outs() << "\n=== CPG Statistics ===\n";

    int totalICFGNodes = 0;
    for (const auto& [_, nodes] : icfgNodes) {
        totalICFGNodes += nodes.size();
    }

    llvm::outs() << "Functions: " << icfgNodes.size() << "\n";
    llvm::outs() << "ICFG nodes: " << totalICFGNodes << "\n";
    llvm::outs() << "PDG nodes: " << pdgNodes.size() << "\n";
    llvm::outs() << "Cached CFGs: " << cfgCache.size() << "\n";
    llvm::outs() << "======================\n\n";
}

// ---------- 构建接口 ----------

void CPGContext::buildCPG(const clang::FunctionDecl* func) {
    if (!func || !func->hasBody()) return;

    llvm::outs() << "Building CPG for function: " << func->getNameAsString() << "\n";

    // 1. 构建ICFG
    buildICFG(func);

    // 2. 计算Reaching Definitions
    computeReachingDefinitions(func);

    // 3. 构建PDG（基于ICFG和Reaching Definitions）
    buildPDG(func);

    llvm::outs() << "CPG construction completed for: " << func->getNameAsString() << "\n";
}

void CPGContext::buildICFGForTranslationUnit() {
    llvm::outs() << "Building global ICFG...\n";

    // 1. 为每个函数构建内部ICFG
    for (auto* decl : astContext.getTranslationUnitDecl()->decls()) {
        if (auto* func = llvm::dyn_cast<clang::FunctionDecl>(decl)) {
            if (func->hasBody() && func->isThisDeclarationADefinition()) {
                buildICFG(func);
            }
        }
    }

    // 2. 构建调用图
    buildCallGraph();

    // 3. 连接调用点
    linkCallSites();

    llvm::outs() << "Global ICFG construction completed\n";
}

// ---------- 内部构建方法 ----------

void CPGContext::buildICFG(const clang::FunctionDecl* func) {
    // 1. 构建CFG
    clang::CFG::BuildOptions options;
    auto cfg = clang::CFG::buildCFG(func, func->getBody(), &astContext, options);

    if (!cfg) {
        llvm::errs() << "Failed to build CFG for: " << func->getNameAsString() << "\n";
        return;
    }

    cfgCache[func] = std::move(cfg);
    const clang::CFG* cfgPtr = cfgCache[func].get();

    // 2. 创建入口和出口节点
    auto* entryNode = createICFGNode(ICFGNodeKind::Entry, func);
    auto* exitNode = createICFGNode(ICFGNodeKind::Exit, func);

    funcEntries[func] = entryNode;
    funcExits[func] = exitNode;

    // 3. 为每个CFG块和语句创建ICFG节点
    std::map<const clang::CFGBlock*, ICFGNode*> blockFirstNode;
    std::map<const clang::CFGBlock*, ICFGNode*> blockLastNode;

    for (const auto* block : *cfgPtr) {
        if (!block) continue;

        ICFGNode* prevNode = nullptr;

        for (const auto& elem : *block) {
            if (auto stmt = elem.getAs<clang::CFGStmt>()) {
                const clang::Stmt* s = stmt->getStmt();

                // 检查是否是函数调用
                ICFGNodeKind nodeKind = ICFGNodeKind::Statement;
                const clang::CallExpr* callExpr = nullptr;

                if (auto* call = llvm::dyn_cast<clang::CallExpr>(s)) {
                    nodeKind = ICFGNodeKind::CallSite;
                    callExpr = call;
                }

                auto* node = createICFGNode(nodeKind, func);
                node->stmt = s;
                node->cfgBlock = block;
                node->callExpr = callExpr;

                stmtToICFGNode[s] = node;

                // 连接节点
                if (prevNode) {
                    addICFGEdge(prevNode, node, ICFGEdgeKind::Intraprocedural);
                } else {
                    blockFirstNode[block] = node;
                }

                prevNode = node;
            }
        }

        if (prevNode) {
            blockLastNode[block] = prevNode;
        }
    }

    // 4. 连接CFG块之间的边
    for (const auto* block : *cfgPtr) {
        if (!block) continue;

        auto* lastNode = blockLastNode[block];
        if (!lastNode) continue;

        // 处理后继块
        int succCount = 0;
        for (auto it = block->succ_begin(); it != block->succ_end(); ++it) {
            const auto* succBlock = it->getReachableBlock();
            if (!succBlock) continue;

            auto* firstSuccNode = blockFirstNode[succBlock];
            if (!firstSuccNode) continue;

            // 判断边类型
            ICFGEdgeKind edgeKind = ICFGEdgeKind::Unconditional;
            if (block->getTerminatorStmt()) {
                if (llvm::isa<clang::IfStmt>(block->getTerminatorStmt()) ||
                    llvm::isa<clang::WhileStmt>(block->getTerminatorStmt())) {
                    edgeKind = (succCount == 0) ? ICFGEdgeKind::True : ICFGEdgeKind::False;
                }
            }

            addICFGEdge(lastNode, firstSuccNode, edgeKind);
            succCount++;
        }
    }

    // 5. 连接入口和出口
    if (auto* entryBlock = &cfgPtr->getEntry()) {
        if (auto* firstNode = blockFirstNode[entryBlock]) {
            addICFGEdge(entryNode, firstNode, ICFGEdgeKind::Intraprocedural);
        }
    }

    if (auto* exitBlock = &cfgPtr->getExit()) {
        // 找到所有前驱块的最后一个节点
        for (auto it = exitBlock->pred_begin(); it != exitBlock->pred_end(); ++it) {
            const auto* predBlock = it->getReachableBlock();
            if (predBlock && blockLastNode.count(predBlock)) {
                addICFGEdge(blockLastNode[predBlock], exitNode, ICFGEdgeKind::Intraprocedural);
            }
        }
    }
}

void CPGContext::buildCallGraph() {
    // 收集所有的调用点和目标
    class CallGraphBuilder : public clang::RecursiveASTVisitor<CallGraphBuilder> {
    public:
        CPGContext& ctx;

        explicit CallGraphBuilder(CPGContext& c) : ctx(c) {}

        bool VisitCallExpr(clang::CallExpr* call) {
            if (auto* callee = call->getDirectCallee()) {
                ctx.callTargets[call] = callee;

                // 记录调用点
                for (const auto& [func, nodes] : ctx.icfgNodes) {
                    for (const auto& node : nodes) {
                        if (node->callExpr == call) {
                            ctx.callSites[func].insert(call);
                            break;
                        }
                    }
                }
            }
            return true;
        }
    };

    CallGraphBuilder builder(*this);
    builder.TraverseDecl(astContext.getTranslationUnitDecl());
}

void CPGContext::linkCallSites() {
    // 为每个调用点创建参数传递节点
    for (const auto& [caller, calls] : callSites) {
        for (const auto* callExpr : calls) {
            auto* callNode = stmtToICFGNode[callExpr];
            if (!callNode) continue;

            auto* callee = callTargets[callExpr];
            if (!callee || !callee->hasBody()) continue;

            // 创建返回点
            auto* returnNode = createICFGNode(ICFGNodeKind::ReturnSite, caller);
            returnNode->callExpr = callExpr;
            returnNode->callee = callee;

            // 连接: CallSite -> Callee Entry
            auto* calleeEntry = getFunctionEntry(callee);
            if (calleeEntry) {
                addICFGEdge(callNode, calleeEntry, ICFGEdgeKind::Call);
            }

            // 连接: Callee Exit -> ReturnSite
            auto* calleeExit = getFunctionExit(callee);
            if (calleeExit) {
                addICFGEdge(calleeExit, returnNode, ICFGEdgeKind::Return);
            }

            // 创建参数传递节点
            int numArgs = callExpr->getNumArgs();
            for (int i = 0; i < numArgs; ++i) {
                // Actual-In节点
                auto* actualIn = createICFGNode(ICFGNodeKind::ActualIn, caller);
                actualIn->paramIndex = i;
                actualIn->callExpr = callExpr;

                // Formal-In节点
                auto* formalIn = createICFGNode(ICFGNodeKind::FormalIn, callee);
                formalIn->paramIndex = i;

                addICFGEdge(callNode, actualIn, ICFGEdgeKind::ParamIn);
                addICFGEdge(actualIn, formalIn, ICFGEdgeKind::ParamIn);
            }
        }
    }
}

ICFGNode* CPGContext::createICFGNode(ICFGNodeKind kind, const clang::FunctionDecl* func) {
    auto node = std::make_unique<ICFGNode>(kind);
    node->func = func;
    auto* nodePtr = node.get();
    icfgNodes[func].push_back(std::move(node));
    return nodePtr;
}

void CPGContext::addICFGEdge(ICFGNode* from, ICFGNode* to, ICFGEdgeKind kind) {
    from->successors.push_back({to, kind});
    to->predecessors.push_back({from, kind});
}

void CPGContext::buildPDG(const clang::FunctionDecl* func) {
    // 1. 计算数据依赖
    computeDataDependencies(func);

    // 2. 计算控制依赖
    computeControlDependencies(func);
}

void CPGContext::computeReachingDefinitions(const clang::FunctionDecl* func) {
    // 使用数据流分析计算reaching definitions
    auto* cfg = getCFG(func);
    if (!cfg) return;

    ReachingDefsInfo& info = reachingDefsMap[func];

    // 收集所有语句的定义和使用
    for (const auto* block : *cfg) {
        if (!block) continue;

        for (const auto& elem : *block) {
            if (auto stmt = elem.getAs<clang::CFGStmt>()) {
                const clang::Stmt* s = stmt->getStmt();

                info.definitions[s] = getDefinedVars(s);
                info.uses[s] = getUsedVars(s);
            }
        }
    }

    // 迭代计算reaching definitions（简化版本）
    // 实际应该使用工作列表算法
    std::map<const clang::CFGBlock*, std::map<std::string, std::set<const clang::Stmt*>>> blockOut;

    bool changed = true;
    int iterations = 0;
    const int maxIterations = 100;

    while (changed && iterations < maxIterations) {
        changed = false;
        iterations++;

        for (const auto* block : *cfg) {
            if (!block) continue;

            // 计算block的IN集合（所有前驱的OUT集合的并集）
            std::map<std::string, std::set<const clang::Stmt*>> blockIn;

            for (auto it = block->pred_begin(); it != block->pred_end(); ++it) {
                const auto* predBlock = it->getReachableBlock();
                if (!predBlock) continue;

                for (const auto& [var, defs] : blockOut[predBlock]) {
                    blockIn[var].insert(defs.begin(), defs.end());
                }
            }

            // 计算block的OUT集合
            auto oldOut = blockOut[block];
            blockOut[block] = blockIn;

            // 处理block中的每个语句
            for (const auto& elem : *block) {
                if (auto stmt = elem.getAs<clang::CFGStmt>()) {
                    const clang::Stmt* s = stmt->getStmt();

                    // 记录当前语句的reaching definitions
                    info.reachingDefs[s] = blockOut[block];

                    // Kill-Gen分析
                    for (const auto& def : info.definitions[s]) {
                        blockOut[block][def].clear();  // Kill
                        blockOut[block][def].insert(s);  // Gen
                    }
                }
            }

            // 检查是否有变化
            if (blockOut[block] != oldOut) {
                changed = true;
            }
        }
    }
}

void CPGContext::computeDataDependencies(const clang::FunctionDecl* func) {
    auto it = reachingDefsMap.find(func);
    if (it == reachingDefsMap.end()) return;

    const auto& reachInfo = it->second;

    // 为每个语句创建PDG节点并计算数据依赖
    for (const auto& [stmt, usedVars] : reachInfo.uses) {
        if (pdgNodes.find(stmt) == pdgNodes.end()) {
            pdgNodes[stmt] = std::make_unique<PDGNode>(stmt, func);
        }

        auto* pdgNode = pdgNodes[stmt].get();

        // 对于每个使用的变量，查找其定义
        for (const auto& var : usedVars) {
            auto defs = getDefinitions(stmt, var);

            for (auto* defStmt : defs) {
                // 创建数据依赖：defStmt -> stmt (Flow dependency)
                DataDependency dep(defStmt, stmt, var, DataDependency::DepKind::Flow);
                pdgNode->addDataDep(dep);
            }
        }
    }
}

void CPGContext::computeControlDependencies(const clang::FunctionDecl* func) {
    // 使用后支配树计算控制依赖
    std::map<const clang::CFGBlock*, std::set<const clang::CFGBlock*>> postDom;
    computePostDominators(func, postDom);

    auto* cfg = getCFG(func);
    if (!cfg) return;

    // 对于每个有条件分支的块，计算控制依赖
    for (const auto* block : *cfg) {
        if (!block) continue;

        auto* term = block->getTerminatorStmt();
        if (!term) continue;

        // 只处理条件语句
        if (!llvm::isa<clang::IfStmt>(term) &&
            !llvm::isa<clang::WhileStmt>(term)) continue;

        // 找到此块控制的所有块（不被后支配的后继）
        int branchIdx = 0;
        for (auto it = block->succ_begin(); it != block->succ_end(); ++it, ++branchIdx) {
            const auto* succBlock = it->getReachableBlock();
            if (!succBlock) continue;

            bool branchValue = (branchIdx == 0);  // 简化：假设第一个后继是true分支

            // 找到所有被此分支控制但不被block后支配的块
            std::set<const clang::CFGBlock*> visited;
            std::queue<const clang::CFGBlock*> worklist;
            worklist.push(succBlock);
            visited.insert(succBlock);

            while (!worklist.empty()) {
                const auto* current = worklist.front();
                worklist.pop();

                // 如果current被block后支配，则停止
                if (postDom[current].count(block)) continue;

                // current被block控制
                for (const auto& elem : *current) {
                    if (auto stmt = elem.getAs<clang::CFGStmt>()) {
                        const clang::Stmt* s = stmt->getStmt();

                        if (pdgNodes.find(s) == pdgNodes.end()) {
                            pdgNodes[s] = std::make_unique<PDGNode>(s, func);
                        }

                        ControlDependency dep(term, s, branchValue);
                        pdgNodes[s]->addControlDep(dep);
                    }
                }

                // 继续遍历后继
                for (auto sit = current->succ_begin(); sit != current->succ_end(); ++sit) {
                    const auto* nextBlock = sit->getReachableBlock();
                    if (nextBlock && visited.find(nextBlock) == visited.end()) {
                        worklist.push(nextBlock);
                        visited.insert(nextBlock);
                    }
                }
            }
        }
    }
}

void CPGContext::computePostDominators(
    const clang::FunctionDecl* func,
    std::map<const clang::CFGBlock*, std::set<const clang::CFGBlock*>>& postDom) {

    auto* cfg = getCFG(func);
    if (!cfg) return;

    // 初始化：exit的后支配集合只包含自己，其他块包含所有块
    std::set<const clang::CFGBlock*> allBlocks;
    for (const auto* block : *cfg) {
        if (block) allBlocks.insert(block);
    }

    const auto* exitBlock = &cfg->getExit();
    postDom[exitBlock] = {exitBlock};

    for (const auto* block : *cfg) {
        if (block && block != exitBlock) {
            postDom[block] = allBlocks;
        }
    }

    // 迭代计算后支配集合
    bool changed = true;
    int iterations = 0;
    const int maxIterations = 100;

    while (changed && iterations < maxIterations) {
        changed = false;
        iterations++;

        // 逆向遍历CFG
        for (auto it = cfg->rbegin(); it != cfg->rend(); ++it) {
            const auto* block = *it;
            if (!block || block == exitBlock) continue;

            // PostDom(B) = {B} ∪ (∩ PostDom(S) for all successors S of B)
            std::set<const clang::CFGBlock*> newPostDom = {block};

            bool firstSucc = true;
            for (auto sit = block->succ_begin(); sit != block->succ_end(); ++sit) {
                const auto* succBlock = sit->getReachableBlock();
                if (!succBlock) continue;

                if (firstSucc) {
                    newPostDom.insert(postDom[succBlock].begin(), postDom[succBlock].end());
                    firstSucc = false;
                } else {
                    std::set<const clang::CFGBlock*> intersection;
                    std::set_intersection(
                        newPostDom.begin(), newPostDom.end(),
                        postDom[succBlock].begin(), postDom[succBlock].end(),
                        std::inserter(intersection, intersection.begin())
                    );
                    newPostDom = intersection;
                    newPostDom.insert(block);
                }
            }

            if (newPostDom != postDom[block]) {
                postDom[block] = newPostDom;
                changed = true;
            }
        }
    }
}

// ---------- 可视化辅助方法 ----------

std::string CPGContext::getStmtSource(const clang::Stmt* stmt) const {
    if (!stmt) return "<null>";

    clang::SourceRange range = stmt->getSourceRange();
    if (range.isInvalid()) return "<invalid>";

    clang::CharSourceRange charRange = clang::CharSourceRange::getTokenRange(range);
    std::string source = clang::Lexer::getSourceText(
        charRange,
        astContext.getSourceManager(),
        astContext.getLangOpts()
    ).str();

    // 清理
    std::replace(source.begin(), source.end(), '\n', ' ');
    std::replace(source.begin(), source.end(), '\t', ' ');

    if (source.length() > 50) {
        source = source.substr(0, 47) + "...";
    }

    return source;
}

std::string CPGContext::escapeForDot(const std::string& str) const {
    std::string result;
    for (char c : str) {
        switch (c) {
            case '"': result += "\\\""; break;
            case '\\': result += "\\\\"; break;
            case '\n': result += "\\n"; break;
            case '<': result += "\\<"; break;
            case '>': result += "\\>"; break;
            case '{': result += "\\{"; break;
            case '}': result += "\\}"; break;
            case '|': result += "\\|"; break;
            default: result += c; break;
        }
    }
    return result;
}

void CPGContext::visualizeICFG(const clang::FunctionDecl* func, const std::string& outputPath) const {
    std::string filename = outputPath + "/" + func->getNameAsString() + "_icfg.dot";
    exportICFGDotFile(func, filename);
    llvm::outs() << "✓ ICFG saved to: " << filename << "\n";
}

void CPGContext::visualizePDG(const clang::FunctionDecl* func, const std::string& outputPath) const {
    std::string filename = outputPath + "/" + func->getNameAsString() + "_pdg.dot";
    exportPDGDotFile(func, filename);
    llvm::outs() << "✓ PDG saved to: " << filename << "\n";
}

void CPGContext::visualizeCPG(const clang::FunctionDecl* func, const std::string& outputPath) const {
    std::string filename = outputPath + "/" + func->getNameAsString() + "_cpg.dot";
    exportCPGDotFile(func, filename);
    llvm::outs() << "✓ CPG saved to: " << filename << "\n";
}

void CPGContext::exportICFGDotFile(const clang::FunctionDecl* func, const std::string& filename) const {
    std::error_code EC;
    llvm::raw_fd_ostream out(filename, EC);
    if (EC) {
        llvm::errs() << "Cannot create file: " << filename << "\n";
        return;
    }

    out << "digraph ICFG {\n";
    out << "  rankdir=TB;\n";
    out << "  node [shape=box, fontname=\"Courier\", fontsize=10];\n\n";

    auto it = icfgNodes.find(func);
    if (it == icfgNodes.end()) return;

    std::map<ICFGNode*, int> nodeIds;
    int id = 0;

    // 输出节点
    for (const auto& node : it->second) {
        nodeIds[node.get()] = id;

        out << "  n" << id << " [label=\"";
        out << escapeForDot(node->getLabel());

        if (node->stmt) {
            out << "\\n" << escapeForDot(getStmtSource(node->stmt));
        }

        // 根据节点类型设置颜色
        out << "\", style=filled, fillcolor=";
        switch (node->kind) {
            case ICFGNodeKind::Entry: out << "lightgreen"; break;
            case ICFGNodeKind::Exit: out << "lightblue"; break;
            case ICFGNodeKind::CallSite: out << "yellow"; break;
            case ICFGNodeKind::ReturnSite: out << "orange"; break;
            default: out << "white"; break;
        }
        out << "];\n";

        id++;
    }

    // 输出边
    out << "\n";
    for (const auto& node : it->second) {
        int fromId = nodeIds[node.get()];

        for (const auto& [succ, kind] : node->successors) {
            if (nodeIds.count(succ)) {
                int toId = nodeIds[succ];
                out << "  n" << fromId << " -> n" << toId << " [";

                switch (kind) {
                    case ICFGEdgeKind::Call:
                        out << "label=\"call\", color=red, style=bold";
                        break;
                    case ICFGEdgeKind::Return:
                        out << "label=\"ret\", color=blue, style=dashed";
                        break;
                    case ICFGEdgeKind::True:
                        out << "label=\"T\", color=green";
                        break;
                    case ICFGEdgeKind::False:
                        out << "label=\"F\", color=red";
                        break;
                    default:
                        out << "color=black";
                        break;
                }
                out << "];\n";
            }
        }
    }

    out << "}\n";
}

void CPGContext::exportPDGDotFile(const clang::FunctionDecl* func, const std::string& filename) const {
    std::error_code EC;
    llvm::raw_fd_ostream out(filename, EC);
    if (EC) {
        llvm::errs() << "Cannot create file: " << filename << "\n";
        return;
    }

    out << "digraph PDG {\n";
    out << "  rankdir=TB;\n";
    out << "  node [shape=box, fontname=\"Courier\", fontsize=10];\n\n";

    std::map<const clang::Stmt*, int> nodeIds;
    int id = 0;

    // 输出节点
    for (const auto& [stmt, node] : pdgNodes) {
        if (getContainingFunction(stmt) != func) continue;

        nodeIds[stmt] = id;
        out << "  n" << id << " [label=\"";
        out << escapeForDot(getStmtSource(stmt));
        out << "\"];\n";
        id++;
    }

    // 输出数据依赖边
    out << "\n  // Data dependencies\n";
    for (const auto& [stmt, node] : pdgNodes) {
        if (getContainingFunction(stmt) != func) continue;
        if (!nodeIds.count(stmt)) continue;

        int toId = nodeIds[stmt];
        for (const auto& dep : node->dataDeps) {
            if (nodeIds.count(dep.sourceStmt)) {
                int fromId = nodeIds[dep.sourceStmt];
                out << "  n" << fromId << " -> n" << toId
                    << " [label=\"" << escapeForDot(dep.varName)
                    << "\", color=blue, style=dashed];\n";
            }
        }
    }

    // 输出控制依赖边
    out << "\n  // Control dependencies\n";
    for (const auto& [stmt, node] : pdgNodes) {
        if (getContainingFunction(stmt) != func) continue;
        if (!nodeIds.count(stmt)) continue;

        int toId = nodeIds[stmt];
        for (const auto& dep : node->controlDeps) {
            if (nodeIds.count(dep.controlStmt)) {
                int fromId = nodeIds[dep.controlStmt];
                out << "  n" << fromId << " -> n" << toId
                    << " [label=\"" << (dep.branchValue ? "T" : "F")
                    << "\", color=red, style=dotted];\n";
            }
        }
    }

    out << "}\n";
}

void CPGContext::exportCPGDotFile(const clang::FunctionDecl* func, const std::string& filename) const {
    // CPG是ICFG和PDG的组合
    // 这里简化实现，实际可以在同一个图中显示所有信息
    exportICFGDotFile(func, filename);
}

// ---------- 辅助函数 ----------

std::set<std::string> CPGContext::getUsedVars(const clang::Stmt* stmt) const {
    std::set<std::string> vars;

    class VarCollector : public clang::RecursiveASTVisitor<VarCollector> {
    public:
        std::set<std::string>& vars;
        explicit VarCollector(std::set<std::string>& v) : vars(v) {}

        bool VisitDeclRefExpr(clang::DeclRefExpr* expr) {
            if (auto* var = llvm::dyn_cast<clang::VarDecl>(expr->getDecl())) {
                vars.insert(var->getNameAsString());
            }
            return true;
        }
    };

    VarCollector collector(vars);
    collector.TraverseStmt(const_cast<clang::Stmt*>(stmt));

    return vars;
}

std::set<std::string> CPGContext::getDefinedVars(const clang::Stmt* stmt) const {
    std::set<std::string> vars;

    if (auto* binOp = llvm::dyn_cast<clang::BinaryOperator>(stmt)) {
        if (binOp->isAssignmentOp()) {
            if (auto* lhs = llvm::dyn_cast<clang::DeclRefExpr>(
                    binOp->getLHS()->IgnoreParenImpCasts())) {
                if (auto* var = llvm::dyn_cast<clang::VarDecl>(lhs->getDecl())) {
                    vars.insert(var->getNameAsString());
                }
            }
        }
    } else if (auto* declStmt = llvm::dyn_cast<clang::DeclStmt>(stmt)) {
        for (auto* decl : declStmt->decls()) {
            if (auto* var = llvm::dyn_cast<clang::VarDecl>(decl)) {
                vars.insert(var->getNameAsString());
            }
        }
    }

    return vars;
}

// ---------- 预留：上下文敏感和路径敏感接口 ----------

PDGNode* CPGContext::getPDGNodeInContext(const clang::Stmt* stmt,
                                          const CallContext& context) const {
    // 预留实现
    // 可以根据调用上下文返回不同的PDG节点
    return getPDGNode(stmt);
}

std::vector<DataDependency>
CPGContext::getDataDependenciesOnPath(const clang::Stmt* stmt,
                                      const PathCondition& path) const {
    // 预留实现
    // 可以根据路径条件过滤数据依赖
    return getDataDependencies(stmt);
}

void CPGContext::traverseCallGraphContextSensitive(
    const clang::FunctionDecl* entry,
    CallGraphVisitor visitor,
    int maxDepth) const {

    // 预留实现：上下文敏感的调用图遍历
    std::function<void(const clang::FunctionDecl*, CallContext, int)> dfs;

    dfs = [&](const clang::FunctionDecl* func, CallContext context, int depth) {
        if (depth > maxDepth) return;

        visitor(func, context);

        // 遍历所有调用点
        auto it = callSites.find(func);
        if (it != callSites.end()) {
            for (const auto* call : it->second) {
                auto targetIt = callTargets.find(call);
                if (targetIt != callTargets.end()) {
                    CallContext newContext = context;
                    newContext.callStack.push_back(call);
                    dfs(targetIt->second, newContext, depth + 1);
                }
            }
        }
    };

    CallContext initialContext;
    dfs(entry, initialContext, 0);
}

// ============================================
// CPGBuilder实现
// ============================================

void CPGBuilder::buildForTranslationUnit(clang::ASTContext& astCtx, CPGContext& cpgCtx) {
    // 构建全局ICFG
    cpgCtx.buildICFGForTranslationUnit();

    // 为每个函数构建PDG
    for (auto* decl : astCtx.getTranslationUnitDecl()->decls()) {
        if (auto* func = llvm::dyn_cast<clang::FunctionDecl>(decl)) {
            if (func->hasBody() && func->isThisDeclarationADefinition()) {
                cpgCtx.computeReachingDefinitions(func);
                cpgCtx.buildPDG(func);
            }
        }
    }
}

void CPGBuilder::buildForFunction(const clang::FunctionDecl* func, CPGContext& cpgCtx) {
    cpgCtx.buildCPG(func);
}

    // 在 namespace cpg 的最后添加这些实现

std::set<std::string> CPGContext::extractVariables(const clang::Expr* expr) const {
    std::set<std::string> vars;

    class VarExtractor : public clang::RecursiveASTVisitor<VarExtractor> {
    public:
        std::set<std::string>& vars;
        explicit VarExtractor(std::set<std::string>& v) : vars(v) {}

        bool VisitDeclRefExpr(clang::DeclRefExpr* ref) {
            if (auto* var = llvm::dyn_cast<clang::VarDecl>(ref->getDecl())) {
                vars.insert(var->getNameAsString());
            }
            return true;
        }
    };

    VarExtractor extractor(vars);
    extractor.TraverseStmt(const_cast<clang::Expr*>(expr));
    return vars;
}

const clang::Stmt* CPGContext::getContainingStmt(const clang::Expr* expr) const {
    if (!expr) return nullptr;

    auto parents = astContext.getParents(*expr);

    while (!parents.empty()) {
        const auto& parent = parents[0];

        if (auto* stmt = parent.get<clang::Stmt>()) {
            if (stmtToICFGNode.count(stmt)) {
                return stmt;
            }
            parents = astContext.getParents(*stmt);
        } else {
            break;
        }
    }

    return nullptr;
}

std::vector<const clang::Stmt*> CPGContext::traceVariableDefinitions(
    const clang::Expr* expr,
    int maxDepth) const {

    std::vector<const clang::Stmt*> result;
    if (!expr) return result;

    auto vars = extractVariables(expr);
    if (vars.empty()) return result;

    const clang::Stmt* containingStmt = getContainingStmt(expr);
    if (!containingStmt) {
        containingStmt = expr;
    }

    auto* func = getContainingFunction(containingStmt);
    if (!func) return result;

    std::set<const clang::Stmt*> visited;
    std::queue<std::pair<const clang::Stmt*, int>> worklist;

    worklist.push({containingStmt, 0});
    visited.insert(containingStmt);

    for (const auto& varName : vars) {
        while (!worklist.empty()) {
            auto [current, depth] = worklist.front();
            worklist.pop();

            if (depth >= maxDepth) continue;

            auto defs = getDefinitions(current, varName);

            for (auto* defStmt : defs) {
                if (visited.find(defStmt) == visited.end()) {
                    result.push_back(defStmt);
                    visited.insert(defStmt);

                    auto usedVars = getUsedVars(defStmt);
                    if (!usedVars.empty()) {
                        worklist.push({defStmt, depth + 1});
                    }
                }
            }
        }
    }

    return result;
}

// ============================================
// 跨函数数据流分析实现
// ============================================

// 获取调用点传入的实参表达式
const clang::Expr* CPGContext::getArgumentAtCallSite(
    const clang::CallExpr* callExpr,
    unsigned paramIndex) const {

    if (!callExpr || paramIndex >= callExpr->getNumArgs()) {
        return nullptr;
    }

    return callExpr->getArg(paramIndex);
}

// 获取形参在被调函数中的使用
std::vector<const clang::Stmt*> CPGContext::getParameterUsages(
    const clang::ParmVarDecl* param) const {

    std::vector<const clang::Stmt*> usages;
    if (!param) return usages;

    const clang::FunctionDecl* func = llvm::dyn_cast<clang::FunctionDecl>(param->getDeclContext());
    if (!func || !func->hasBody()) return usages;

    // 遍历函数体查找参数使用
    class ParamUsageFinder : public clang::RecursiveASTVisitor<ParamUsageFinder> {
    public:
        const clang::ParmVarDecl* targetParam;
        std::vector<const clang::Stmt*> foundUsages;

        explicit ParamUsageFinder(const clang::ParmVarDecl* p) : targetParam(p) {}

        bool VisitDeclRefExpr(clang::DeclRefExpr* DRE) {
            if (DRE->getDecl() == targetParam) {
                // 获取包含这个DeclRefExpr的顶层语句
                foundUsages.push_back(DRE);
            }
            return true;
        }
    };

    ParamUsageFinder finder(param);
    finder.TraverseStmt(func->getBody());

    return finder.foundUsages;
}

// 跨函数追踪变量的定义链
std::vector<const clang::Stmt*> CPGContext::traceVariableDefinitionsInterprocedural(
    const clang::Expr* expr,
    int maxDepth) const {

    std::vector<const clang::Stmt*> result;
    if (!expr) return result;

    // 提取表达式中使用的变量
    auto vars = extractVariables(expr);
    if (vars.empty()) return result;

    // 获取包含该表达式的语句
    const clang::Stmt* containingStmt = getContainingStmt(expr);
    if (!containingStmt) {
        containingStmt = expr;
    }

    // 获取包含该语句的函数
    auto* func = getContainingFunction(containingStmt);
    if (!func) return result;

    // 用于追踪的工作队列：(语句, 深度, 函数上下文)
    struct WorkItem {
        const clang::Stmt* stmt;
        int depth;
        const clang::FunctionDecl* function;
        std::string varName;
    };

    std::set<const clang::Stmt*> visited;
    std::queue<WorkItem> worklist;

    // 初始化工作队列
    for (const auto& varName : vars) {
        worklist.push({containingStmt, 0, func, varName});
    }
    visited.insert(containingStmt);

    while (!worklist.empty()) {
        auto [current, depth, currentFunc, varName] = worklist.front();
        worklist.pop();

        if (depth >= maxDepth) continue;

        // 1. 首先在当前函数内查找定义
        auto defs = getDefinitions(current, varName);

        for (auto* defStmt : defs) {
            if (visited.find(defStmt) == visited.end()) {
                result.push_back(defStmt);
                visited.insert(defStmt);

                // 继续向上追踪这个定义语句中使用的变量
                auto usedVars = getUsedVars(defStmt);
                for (const auto& usedVar : usedVars) {
                    worklist.push({defStmt, depth + 1, currentFunc, usedVar});
                }
            }
        }

        // 2. 检查该变量是否为函数参数
        // 如果是参数，需要追踪到调用点
        if (auto* DRE = llvm::dyn_cast<clang::DeclRefExpr>(expr)) {
            if (auto* paramDecl = llvm::dyn_cast<clang::ParmVarDecl>(DRE->getDecl())) {
                // 这是一个参数引用，需要找到调用点

                // 获取参数索引
                unsigned paramIndex = paramDecl->getFunctionScopeIndex();

                // 查找所有调用当前函数的调用点
                for (const auto& [caller, callExprs] : callSites) {
                    for (const auto* callExpr : callExprs) {
                        // 检查这个调用是否调用了当前函数
                        auto it = callTargets.find(callExpr);
                        if (it != callTargets.end() && it->second == currentFunc) {
                            // 找到调用点，获取对应的实参
                            const clang::Expr* arg = getArgumentAtCallSite(callExpr, paramIndex);
                            if (arg) {
                                llvm::outs() << "🔗 发现跨函数数据流: 从调用点 "
                                           << caller->getNameAsString()
                                           << " 的实参传递到参数 " << varName << "\n";

                                // 提取实参中的变量
                                auto argVars = extractVariables(arg);

                                if (!argVars.empty()) {
                                    // 将实参表达式标记为一个传递点
                                    if (visited.find(arg) == visited.end()) {
                                        result.push_back(arg);
                                        visited.insert(arg);
                                    }

                                    // 关键修复：需要找到包含这个调用的语句，然后在调用者函数中继续追踪
                                    // 获取调用表达式所在的语句
                                    const clang::Stmt* callStmt = getContainingStmt(callExpr);
                                    if (!callStmt) {
                                        callStmt = callExpr;
                                    }

                                    // 在调用者函数中继续追踪实参中的变量
                                    for (const auto& argVar : argVars) {
                                        llvm::outs() << "   → 在调用者函数 " << caller->getNameAsString()
                                                   << " 中继续追踪变量: " << argVar << "\n";
                                        worklist.push({callStmt, depth + 1, caller, argVar});
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        // 3. 额外处理：如果当前语句本身就包含DeclRefExpr指向参数
        class ParamRefFinder : public clang::RecursiveASTVisitor<ParamRefFinder> {
        public:
            std::vector<std::pair<clang::ParmVarDecl*, clang::DeclRefExpr*>> paramRefs;

            bool VisitDeclRefExpr(clang::DeclRefExpr* DRE) {
                if (auto* paramDecl = llvm::dyn_cast<clang::ParmVarDecl>(DRE->getDecl())) {
                    paramRefs.push_back({paramDecl, DRE});
                }
                return true;
            }
        };

        ParamRefFinder paramFinder;
        paramFinder.TraverseStmt(const_cast<clang::Stmt*>(current));

        for (const auto& [paramDecl, declRefExpr] : paramFinder.paramRefs) {
            if (paramDecl->getName() == varName) {
                unsigned paramIndex = paramDecl->getFunctionScopeIndex();

                // 查找调用点
                for (const auto& [caller, callExprs] : callSites) {
                    for (const auto* callExpr : callExprs) {
                        auto it = callTargets.find(callExpr);
                        if (it != callTargets.end() && it->second == currentFunc) {
                            const clang::Expr* arg = getArgumentAtCallSite(callExpr, paramIndex);
                            if (arg) {
                                llvm::outs() << "🔗 发现跨函数数据流: 从调用点 "
                                           << caller->getNameAsString()
                                           << " 的实参传递到参数 " << varName << "\n";

                                auto argVars = extractVariables(arg);

                                if (!argVars.empty()) {
                                    if (visited.find(arg) == visited.end()) {
                                        result.push_back(arg);
                                        visited.insert(arg);
                                    }

                                    // 获取调用表达式所在的语句
                                    const clang::Stmt* callStmt = getContainingStmt(callExpr);
                                    if (!callStmt) {
                                        callStmt = callExpr;
                                    }

                                    for (const auto& argVar : argVars) {
                                        llvm::outs() << "   → 在调用者函数 " << caller->getNameAsString()
                                                   << " 中继续追踪变量: " << argVar << "\n";
                                        worklist.push({callStmt, depth + 1, caller, argVar});
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    return result;
}

} // namespace cpg
