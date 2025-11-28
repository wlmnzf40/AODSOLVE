#include "conversion/enhanced_cpg_to_aod_converter.h"
#include <sstream>
#include <algorithm>
#include <iostream>
#include <clang/AST/RecursiveASTVisitor.h>
#include <clang/AST/Stmt.h>
#include <clang/AST/Expr.h>
#include <clang/AST/ExprCXX.h> // 必需：用于 CXXConstructExpr

namespace aodsolve {

EnhancedCPGToAODConverter::EnhancedCPGToAODConverter(clang::ASTContext& ctx, IntegratedCPGAnalyzer& a)
    : source_manager(ctx.getSourceManager()), analyzer(&a) {
    (void)ctx;
}

ConversionResult EnhancedCPGToAODConverter::convertWithOperators(
    const clang::FunctionDecl* func,
    const std::string&, const std::string& target_arch) {

    ConversionResult result;
    result.aod_graph = std::make_shared<AODGraph>(func->getNameAsString());
    stmt_to_node_map.clear();

    try {
        buildFullAODGraph(func, *result.aod_graph);

        bool enable_autovec = (target_arch == "NEON" || target_arch == "SVE");
        if (enable_autovec) {
            for (auto& node : result.aod_graph->getNodes()) {
                if (node->getType() == AODNodeType::Control && node->getName().find("ForStmt") != std::string::npos) {
                    if (auto* forStmt = llvm::dyn_cast_or_null<clang::ForStmt>(node->getAstStmt())) {
                        if (auto* inc = forStmt->getInc()) {
                           if (llvm::isa<clang::UnaryOperator>(inc) || llvm::isa<clang::CompoundAssignOperator>(inc))
                               node->setProperty("vectorize", "true");
                        }
                    }
                }
                if (node->getProperty("op_name") == "bf16_dot") {
                    node->setProperty("vectorize", "true");
                    if (target_arch == "NEON") node->setProperty("vector_width", "4");
                }
                if (node->getType() == AODNodeType::GenericStmt && node->getName().find("ScalarOp") != std::string::npos) {
                    node->setProperty("vectorize", "true");
                    if (target_arch == "NEON") node->setProperty("vector_width", "4");
                }
            }
        }

        connectDataFlow(func, *result.aod_graph);

        result.successful = true;
        result.stmt_to_node_ptr_map = stmt_to_node_map;
        result.converted_node_count = result.aod_graph->getNodeCount();

    } catch (const std::exception& e) {
        result.successful = false;
        result.error_message = std::string("Conversion Error: ") + e.what();
        std::cerr << "[AODSOLVE Error] " << result.error_message << std::endl;
    }

    return result;
}

void EnhancedCPGToAODConverter::buildFullAODGraph(const clang::FunctionDecl* func, AODGraph& graph) {
    if (!func || !func->hasBody()) return;
    traverseAndBuild(func->getBody(), graph, true);
}

// [核心修复] 强健的数组基地址提取逻辑
// 能够穿透: CallExpr, ImplicitCast, ExplicitCast, MaterializeTemporary, CXXBindTemporary, CXXConstructExpr
const clang::Expr* digForArrayBase(const clang::Expr* expr) {
    if (!expr) return nullptr;

    const clang::Expr* curr = expr;

    // 循环剥离包装层，直到找到 ArraySubscriptExpr 或无法继续
    while (true) {
        curr = curr->IgnoreParenImpCasts(); // 处理 () 和 隐式转换 (LValueToRValue等)

        // Case 1: ArraySubscriptExpr (x[i]) -> 成功找到
        if (auto* ase = llvm::dyn_cast<clang::ArraySubscriptExpr>(curr)) {
            return ase->getBase()->IgnoreParenImpCasts();
        }

        // Case 2: CallExpr (convert(x[i])) -> 递归处理参数
        if (auto* call = llvm::dyn_cast<clang::CallExpr>(curr)) {
            if (call->getNumArgs() > 0) {
                curr = call->getArg(0);
                continue;
            }
        }

        // Case 3: CXXConstructExpr (结构体按值传递时的拷贝构造)
        // 你的 AST Dump 显示 ArraySubscriptExpr 被包裹在这里面
        if (auto* cce = llvm::dyn_cast<clang::CXXConstructExpr>(curr)) {
            if (cce->getNumArgs() > 0) {
                curr = cce->getArg(0);
                continue;
            }
        }

        // Case 4: MaterializeTemporaryExpr (右值转左值)
        if (auto* mte = llvm::dyn_cast<clang::MaterializeTemporaryExpr>(curr)) {
            curr = mte->getSubExpr();
            continue;
        }

        // Case 5: CXXBindTemporaryExpr (临时对象析构绑定)
        if (auto* bte = llvm::dyn_cast<clang::CXXBindTemporaryExpr>(curr)) {
            curr = bte->getSubExpr();
            continue;
        }

        // Case 6: 显式转换 (float)(...)
        if (auto* cast = llvm::dyn_cast<clang::CastExpr>(curr)) {
            curr = cast->getSubExpr();
            continue;
        }

        // Case 7: UnaryOperator (*(x+i)) -> 处理指针运算形式的数组访问
        if (auto* uo = llvm::dyn_cast<clang::UnaryOperator>(curr)) {
            if (uo->getOpcode() == clang::UO_Deref) {
                 auto* sub = uo->getSubExpr()->IgnoreParenImpCasts();
                 if (auto* bo = llvm::dyn_cast<clang::BinaryOperator>(sub)) {
                     // x + i
                     if (bo->getLHS()->getType()->isPointerType()) {
                         return bo->getLHS()->IgnoreParenImpCasts();
                     }
                     if (bo->getRHS()->getType()->isPointerType()) {
                         return bo->getRHS()->IgnoreParenImpCasts();
                     }
                 }
            }
        }

        // 无法继续深入
        break;
    }
    return nullptr;
}

std::string getArrayBaseName(const clang::Expr* expr) {
    auto* base = digForArrayBase(expr);
    if (base) {
        if (auto* dre = llvm::dyn_cast<clang::DeclRefExpr>(base)) {
            return dre->getDecl()->getNameAsString();
        }
    }
    return "";
}

bool isBF16Convert(const clang::Expr* expr) {
    if (!expr) return false;
    auto e = expr->IgnoreParenCasts();
    if (auto* call = llvm::dyn_cast<clang::CallExpr>(e)) {
        if (auto* func = call->getDirectCallee()) {
            return func->getNameAsString() == "ggml_compute_bf16_to_fp32";
        }
    }
    return false;
}

void EnhancedCPGToAODConverter::traverseAndBuild(const clang::Stmt* stmt, AODGraph& graph, bool is_top_level) {
    if (!stmt) return;

    // [Case 6] 模式融合：BF16 Dot Product
    if (auto* compOp = llvm::dyn_cast<clang::CompoundAssignOperator>(stmt)) {
        if (compOp->getOpcode() == clang::BO_AddAssign) {
            auto* rhs = compOp->getRHS()->IgnoreParenCasts();
            // 穿透显式/隐式 Cast
            if (auto* cast = llvm::dyn_cast<clang::ExplicitCastExpr>(rhs)) rhs = cast->getSubExpr()->IgnoreParenCasts();
            if (auto* cast = llvm::dyn_cast<clang::ImplicitCastExpr>(rhs)) rhs = cast->getSubExpr()->IgnoreParenCasts();

            if (auto* bo = llvm::dyn_cast<clang::BinaryOperator>(rhs)) {
                if (bo->getOpcode() == clang::BO_Mul) {
                    if (isBF16Convert(bo->getLHS()) && isBF16Convert(bo->getRHS())) {

                        auto node = std::make_shared<AODNode>(AODNodeType::SIMD_Intrinsic, "bf16_dot");
                        node->setProperty("op_name", "bf16_dot");
                        node->setAstStmt(stmt);
                        node->setIsStatement(true);

                        if (auto* lhs = llvm::dyn_cast<clang::DeclRefExpr>(compOp->getLHS()->IgnoreParenCasts())) {
                            node->setProperty("accum_var", lhs->getDecl()->getNameAsString());
                        }

                        // 使用增强后的 getArrayBaseName
                        node->setProperty("base_x", getArrayBaseName(bo->getLHS()));
                        node->setProperty("base_y", getArrayBaseName(bo->getRHS()));

                        graph.addNode(node);
                        stmt_to_node_map[stmt] = node;
                        return;
                    }
                }
            }
        }
    }

    // ... (后续逻辑保持不变) ...
    bool is_simd = isSIMDIntrinsic(stmt);
    bool is_container = llvm::isa<clang::CompoundStmt>(stmt);
    bool is_control = llvm::isa<clang::IfStmt>(stmt) || llvm::isa<clang::WhileStmt>(stmt) || llvm::isa<clang::ForStmt>(stmt);

    std::shared_ptr<AODNode> node;

    if (is_container) {
        for (const auto* child : stmt->children()) traverseAndBuild(child, graph, true);
        graph.addNode(std::make_shared<AODNode>(AODNodeType::BlockEnd, "}"));
        return;
    }

    if (is_control) {
        node = std::make_shared<AODNode>(AODNodeType::Control, stmt->getStmtClassName());
        node->setAstStmt(stmt);
        node->setIsStatement(is_top_level);
        graph.addNode(node);
        stmt_to_node_map[stmt] = node;

        if (auto* ws = llvm::dyn_cast<clang::WhileStmt>(stmt)) {
            traverseExpressionTree(ws->getCond(), graph);
            traverseAndBuild(ws->getBody(), graph, true);
        } else if (auto* fs = llvm::dyn_cast<clang::ForStmt>(stmt)) {
            traverseExpressionTree(fs->getInit(), graph);
            traverseExpressionTree(fs->getCond(), graph);
            traverseExpressionTree(fs->getInc(), graph);
            traverseAndBuild(fs->getBody(), graph, true);
        } else if (auto* is = llvm::dyn_cast<clang::IfStmt>(stmt)) {
            traverseExpressionTree(is->getCond(), graph);
            traverseAndBuild(is->getThen(), graph, true);
            if (is->getElse()) traverseAndBuild(is->getElse(), graph, true);
        }
        return;
    }

    if (auto* declStmt = llvm::dyn_cast<clang::DeclStmt>(stmt)) {
        if (declStmt->isSingleDecl()) {
            if (auto* var = llvm::dyn_cast<clang::VarDecl>(declStmt->getSingleDecl())) {
                node = std::make_shared<AODNode>(AODNodeType::GenericStmt, "DeclStmt");
                node->setProperty("op_name", "define");
                node->setProperty("var_name", var->getNameAsString());
                node->setAstStmt(stmt);
                node->setIsStatement(is_top_level);
                if (var->getInit() && isSIMDIntrinsic(var->getInit()->IgnoreParenCasts())) node = createSIMDNode(stmt);
                graph.addNode(node);
                stmt_to_node_map[stmt] = node;
                if (var->getInit()) traverseExpressionTree(var->getInit(), graph);
                return;
            }
        }
    }

    if (is_simd) {
        node = createSIMDNode(stmt);
        node->setIsStatement(is_top_level);
    } else {
        node = std::make_shared<AODNode>(AODNodeType::GenericStmt, stmt->getStmtClassName());
        node->setAstStmt(stmt);
        node->setIsStatement(is_top_level);
    }
    graph.addNode(node);
    stmt_to_node_map[stmt] = node;

    for (const auto* child : stmt->children()) traverseExpressionTree(child, graph);
}

void EnhancedCPGToAODConverter::traverseExpressionTree(const clang::Stmt* stmt, AODGraph& graph) {
    if (!stmt) return;
    const clang::Expr* expr = llvm::dyn_cast<clang::Expr>(stmt);
    const clang::Expr* expr_clean = expr ? expr->IgnoreParenCasts() : nullptr;

    bool is_simd = isSIMDIntrinsic(stmt);
    bool is_scalar_op = isVectorizableScalarOp(stmt);

    if (is_simd || is_scalar_op) {
        std::shared_ptr<AODNode> node = nullptr;
        if (stmt_to_node_map.count(expr_clean ? expr_clean : stmt)) {
        } else {
            if (is_simd) {
                node = createSIMDNode(stmt);
            } else if (is_scalar_op) {
                if (auto* bo = llvm::dyn_cast<clang::BinaryOperator>(expr_clean)) {
                    node = std::make_shared<AODNode>(AODNodeType::GenericStmt, "ScalarOp");
                    node->setProperty("op_name", bo->getOpcodeStr().str());
                    node->setAstStmt(stmt);
                    node->setIsStatement(false);
                }
            } else {
                node = createAODNodeFromStmt(stmt, false);
            }
            if (node) {
                graph.addNode(node);
                stmt_to_node_map[stmt] = node;
                if (expr_clean) stmt_to_node_map[expr_clean] = node;
            }
        }
    }

    for (const auto* child : stmt->children()) traverseExpressionTree(child, graph);
}

bool EnhancedCPGToAODConverter::isSIMDIntrinsic(const clang::Stmt* stmt) {
    if (!stmt) return false;
    const clang::Expr* expr = llvm::dyn_cast<clang::Expr>(stmt);
    if (!expr) return false;
    expr = expr->IgnoreParenCasts();
    if (auto* call = llvm::dyn_cast<clang::CallExpr>(expr)) {
        if (auto* func = call->getDirectCallee()) {
            return func->getNameAsString().find("_mm") != std::string::npos;
        }
    }
    return false;
}

bool EnhancedCPGToAODConverter::isVectorizableScalarOp(const clang::Stmt* stmt) {
    const clang::Expr* expr = llvm::dyn_cast<clang::Expr>(stmt);
    if (!expr) return false;
    expr = expr->IgnoreParenCasts();
    if (auto* bo = llvm::dyn_cast<clang::BinaryOperator>(expr)) {
        if (bo->getType()->isFloatingType()) return true;
        auto op = bo->getOpcode();
        return op == clang::BO_Add || op == clang::BO_Sub || op == clang::BO_Mul || op == clang::BO_Div;
    }
    return false;
}

std::shared_ptr<AODNode> EnhancedCPGToAODConverter::createSIMDNode(const clang::Stmt* stmt) {
    auto node = std::make_shared<AODNode>(AODNodeType::SIMD_Intrinsic, "SIMD_Op");
    node->setAstStmt(stmt);
    const clang::Expr* expr = llvm::dyn_cast<clang::Expr>(stmt);
    if (expr) expr = expr->IgnoreParenCasts();
    if (expr) {
        if (auto* call = llvm::dyn_cast<clang::CallExpr>(expr)) {
            if (auto* func = call->getDirectCallee()) {
                node->setProperty("op_name", func->getNameAsString());
                node->setIsStatement(false);
            }
        }
    }
    if (auto* declStmt = llvm::dyn_cast<clang::DeclStmt>(stmt)) {
        if (auto* var = llvm::dyn_cast<clang::VarDecl>(declStmt->getSingleDecl())) {
            node->setProperty("op_name", "define");
            node->setProperty("var_name", var->getNameAsString());
            node->setIsStatement(true);
        }
    }
    return node;
}

std::shared_ptr<AODNode> EnhancedCPGToAODConverter::createAODNodeFromStmt(const clang::Stmt* stmt, bool is_stmt) {
    std::string name = stmt->getStmtClassName();
    auto node = std::make_shared<AODNode>(AODNodeType::Unknown, name);
    node->setAstStmt(stmt);
    node->setIsStatement(is_stmt);
    return node;
}

AODNodeType EnhancedCPGToAODConverter::mapStmtToNodeType(const clang::Stmt* stmt) {
    if (isSIMDIntrinsic(stmt)) return AODNodeType::SIMD_Intrinsic;
    if (llvm::isa<clang::CompoundStmt>(stmt)) return AODNodeType::Control;
    return AODNodeType::GenericStmt;
}

void EnhancedCPGToAODConverter::connectDataFlow(const clang::FunctionDecl*, AODGraph& graph) {
    cpg::CPGContext& cpg_ctx = const_cast<cpg::CPGContext&>(analyzer->getCPGContext());

    for (auto& node : graph.getNodes()) {
        // Case 6 bf16_dot 已处理，跳过
        if (node->getProperty("op_name") == "bf16_dot") continue;

        const clang::Stmt* stmt = node->getAstStmt();
        if (!stmt) continue;

        // 1. DeclStmt -> Init
        if (node->getProperty("op_name") == "define") {
            if (auto* declStmt = llvm::dyn_cast<clang::DeclStmt>(stmt)) {
                if (auto* var = llvm::dyn_cast<clang::VarDecl>(declStmt->getSingleDecl())) {
                    if (auto* init = var->getInit()) {
                        if (auto* init_expr = llvm::dyn_cast<clang::Expr>(init)) {
                            auto init_clean = init_expr->IgnoreParenCasts();
                            if (stmt_to_node_map.count(init_clean)) {
                                auto src = stmt_to_node_map[init_clean];
                                if (src != node) try { graph.addEdge(src, node, AODEdgeType::Data, "init"); } catch(...) {}
                            }
                        }
                    }
                }
            }
        }

        // 2. CallExpr / BinaryOp
        const clang::Expr* expr = llvm::dyn_cast<clang::Expr>(stmt);
        const clang::Expr* expr_clean = expr ? expr->IgnoreParenCasts() : nullptr;

        if (expr_clean) {
            if (auto* call = llvm::dyn_cast<clang::CallExpr>(expr_clean)) {
                int arg_idx = 0;
                for (const auto* arg : call->arguments()) {
                    auto arg_expr = llvm::dyn_cast<clang::Expr>(arg);
                    if (!arg_expr) continue;
                    auto arg_clean = arg_expr->IgnoreParenCasts();

                    if (stmt_to_node_map.count(arg_clean)) {
                        auto src = stmt_to_node_map[arg_clean];
                        try { graph.addEdge(src, node, AODEdgeType::Data, "arg_" + std::to_string(arg_idx)); } catch(...) {}
                    } else if (auto* dre = llvm::dyn_cast<clang::DeclRefExpr>(arg_clean)) {
                        std::string var_name = dre->getDecl()->getNameAsString();
                        for (auto& s : graph.getNodes()) {
                            if (s->getProperty("op_name")=="define" && s->getProperty("var_name")==var_name) {
                                try { graph.addEdge(s, node, AODEdgeType::Data, "arg_"+std::to_string(arg_idx)); } catch(...) {}
                                break;
                            }
                        }
                    }
                    arg_idx++;
                }
            } else if (auto* bo = llvm::dyn_cast<clang::BinaryOperator>(expr_clean)) {
                 auto linkOp = [&](const clang::Expr* op, int idx) {
                    auto op_clean = op->IgnoreParenCasts();
                    if (stmt_to_node_map.count(op_clean)) {
                        try { graph.addEdge(stmt_to_node_map[op_clean], node, AODEdgeType::Data, "arg_" + std::to_string(idx)); } catch(...) {}
                    } else if (auto* dre = llvm::dyn_cast<clang::DeclRefExpr>(op_clean)) {
                        std::string var_name = dre->getDecl()->getNameAsString();
                        for (auto& kv : stmt_to_node_map) {
                            if (kv.second->getProperty("op_name") == "define" && kv.second->getProperty("var_name") == var_name) {
                                try { graph.addEdge(kv.second, node, AODEdgeType::Data, "arg_" + std::to_string(idx)); } catch(...) {}
                                break;
                            }
                        }
                    }
                    else if (auto* ase = llvm::dyn_cast<clang::ArraySubscriptExpr>(op_clean)) {
                        if (auto* base = llvm::dyn_cast<clang::DeclRefExpr>(ase->getBase()->IgnoreParenCasts())) {
                            std::string var_name = base->getDecl()->getNameAsString();
                            for (auto& kv : stmt_to_node_map) {
                                if (kv.second->getProperty("op_name") == "define" && kv.second->getProperty("var_name") == var_name) {
                                    try { graph.addEdge(kv.second, node, AODEdgeType::Data, "arg_" + std::to_string(idx)); } catch(...) {}
                                    break;
                                }
                            }
                        }
                    }
                 };
                 linkOp(bo->getLHS(), 0);
                 linkOp(bo->getRHS(), 1);
             }
        }

        auto deps = cpg_ctx.getDataDependencies(stmt);
        for (const auto& dep : deps) {
            if (stmt_to_node_map.count(dep.sourceStmt)) {
                auto source_node = stmt_to_node_map[dep.sourceStmt];
                if (source_node != node && source_node->getId() != node->getId()) {
                     try { graph.addEdge(source_node, node, AODEdgeType::Data, dep.varName); } catch(...) {}
                }
            }
        }
    }
}

} // namespace aodsolve
