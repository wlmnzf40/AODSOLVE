#include "analysis/loop_vectorization_analyzer.h"
#include <clang/AST/RecursiveASTVisitor.h>

namespace aodsolve {

// ============================================================================
// FunctionInlineAnalyzer 实现
// ============================================================================

FunctionInlineCandidate FunctionInlineAnalyzer::analyzeFunctionInlineability(
    const clang::FunctionDecl* func) {

    FunctionInlineCandidate candidate;
    candidate.func = func;
    candidate.function_name = func->getNameAsString();
    candidate.is_inline = func->isInlineSpecified();
    candidate.is_pure = isPureFunction(func);
    candidate.has_control_flow = false;
    candidate.is_small_function = false;
    candidate.has_simd_equivalent = false;
    candidate.can_be_inlined = false;

    if (!func->hasBody()) {
        return candidate;
    }

    // 分析函数大小
    class SizeAnalyzer : public clang::RecursiveASTVisitor<SizeAnalyzer> {
    public:
        int stmt_count = 0;

        bool VisitStmt(clang::Stmt* /*s*/) {
            stmt_count++;
            return true;
        }
    };

    SizeAnalyzer size_analyzer;
    size_analyzer.TraverseStmt(const_cast<clang::Stmt*>(func->getBody()));
    candidate.is_small_function = (size_analyzer.stmt_count < 20);

    // 分析控制流
    class ControlFlowAnalyzer : public clang::RecursiveASTVisitor<ControlFlowAnalyzer> {
    public:
        bool has_control_flow = false;

        bool VisitIfStmt(clang::IfStmt* /*stmt*/) {
            has_control_flow = true;
            return true;
        }

        bool VisitForStmt(clang::ForStmt* /*stmt*/) {
            has_control_flow = true;
            return true;
        }

        bool VisitWhileStmt(clang::WhileStmt* /*stmt*/) {
            has_control_flow = true;
            return true;
        }
    };

    ControlFlowAnalyzer cf_analyzer;
    cf_analyzer.TraverseStmt(const_cast<clang::Stmt*>(func->getBody()));
    candidate.has_control_flow = cf_analyzer.has_control_flow;

    // 分析变量使用
    class VariableAnalyzer : public clang::RecursiveASTVisitor<VariableAnalyzer> {
    public:
        std::set<std::string> read_vars;
        std::set<std::string> modified_vars;

        bool VisitDeclRefExpr(clang::DeclRefExpr* expr) {
            if (auto* var_decl = clang::dyn_cast<clang::VarDecl>(expr->getDecl())) {
                read_vars.insert(var_decl->getNameAsString());
            }
            return true;
        }

        bool VisitBinaryOperator(clang::BinaryOperator* op) {
            if (op->isAssignmentOp()) {
                if (auto* lhs = clang::dyn_cast<clang::DeclRefExpr>(op->getLHS()->IgnoreImpCasts())) {
                    if (auto* var_decl = clang::dyn_cast<clang::VarDecl>(lhs->getDecl())) {
                        modified_vars.insert(var_decl->getNameAsString());
                    }
                }
            }
            return true;
        }
    };

    VariableAnalyzer var_analyzer;
    var_analyzer.TraverseStmt(const_cast<clang::Stmt*>(func->getBody()));
    candidate.read_variables = var_analyzer.read_vars;
    candidate.modified_variables = var_analyzer.modified_vars;

    // 判断是否可以内联
    candidate.can_be_inlined =
        candidate.is_small_function &&
        candidate.is_pure &&
        !candidate.has_control_flow;

    return candidate;
}

std::vector<FunctionCallContext> FunctionInlineAnalyzer::analyzeFunctionCalls(
    const clang::ForStmt* loop) {

    std::vector<FunctionCallContext> contexts;

    if (!loop || !loop->getBody()) {
        return contexts;
    }

    class CallFinder : public clang::RecursiveASTVisitor<CallFinder> {
    public:
        std::vector<FunctionCallContext> contexts;

        bool VisitCallExpr(clang::CallExpr* /*call*/) {
            // 这里需要填充FunctionCallContext的详细信息
            FunctionCallContext context;
            // context.call_site = call;
            // context.callee = call->getDirectCallee();
            // ... 提取参数等
            contexts.push_back(context);
            return true;
        }
    };

    CallFinder finder;
    finder.TraverseStmt(const_cast<clang::Stmt*>(loop->getBody()));

    return finder.contexts;
}

bool FunctionInlineAnalyzer::isPureFunction(const clang::FunctionDecl* func) {
    if (!func || !func->hasBody()) {
        return false;
    }

    // 检查是否有副作用
    class PurityChecker : public clang::RecursiveASTVisitor<PurityChecker> {
    public:
        bool is_pure = true;

        // 检查函数调用
        bool VisitCallExpr(clang::CallExpr* call) {
            // 如果调用了外部函数，可能有副作用
            if (call->getDirectCallee()) {
                // 简化版：假设所有外部调用都有副作用
                is_pure = false;
            }
            return true;
        }

        // 检查赋值给全局变量
        bool VisitDeclRefExpr(clang::DeclRefExpr* expr) {
            if (auto* var_decl = clang::dyn_cast<clang::VarDecl>(expr->getDecl())) {
                if (var_decl->hasGlobalStorage()) {
                    is_pure = false;
                }
            }
            return true;
        }
    };

    PurityChecker checker;
    checker.TraverseStmt(const_cast<clang::Stmt*>(func->getBody()));

    return checker.is_pure;
}

} // namespace aodsolve