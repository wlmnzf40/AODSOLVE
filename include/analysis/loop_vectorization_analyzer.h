#pragma once

#include "conversion/enhanced_cpg_to_aod_converter.h"
#include <regex>

namespace aodsolve {

// 数组访问模式
struct ArrayAccess {
    std::string array_name;
    std::string index_expr;
    bool is_read;
    bool is_sequential;
    const clang::Expr* ast_expr;
};

// 标量操作
struct ScalarOperation {
    std::string op_type;
    std::vector<std::string> operands;
    const clang::Expr* ast_expr;
};

// 循环向量化模式
struct LoopVectorizationPattern {
    const clang::ForStmt* loop;
    std::string iterator_name;
    int64_t start_value;
    std::string end_variable;
    int64_t step;
    std::vector<ArrayAccess> array_accesses;
    std::vector<ScalarOperation> operations;
    bool has_loop_dependencies;
    bool is_vectorizable;
    bool is_reduction;
    std::string reduction_op;
    std::string reduction_var;
    std::string data_type;
    int element_size;
};

// 函数调用上下文
struct FunctionCallContext {
    const clang::CallExpr* call_site;
    const clang::FunctionDecl* callee;
    std::vector<const clang::Expr*> arguments;
    std::map<std::string, std::string> param_to_arg_map;
};

// 函数内联候选
struct FunctionInlineCandidate {
    const clang::FunctionDecl* func;
    std::string function_name;
    bool is_inline;
    bool is_small_function;
    bool has_simd_equivalent;
    bool is_pure;
    bool can_be_inlined;
    std::string simd_pattern;
    std::map<std::string, std::string> simd_mapping;
    std::set<std::string> modified_variables;
    std::set<std::string> read_variables;
    bool has_control_flow;
};

// 向量化代码生成器（前向声明）
class VectorizedCodeGenerator {
public:
    std::string generateInitialization(
        const LoopVectorizationPattern& pattern,
        const std::string& target_arch);

    std::string generateMainLoop(
        const LoopVectorizationPattern& pattern,
        const std::string& target_arch);

    std::string generateTailLoop(
        const LoopVectorizationPattern& pattern,
        const std::string& target_arch);

    std::string generateReduction(
        const LoopVectorizationPattern& pattern,
        const std::string& target_arch);
};

// 循环向量化分析器
class LoopVectorizationAnalyzer {
private:
    clang::ASTContext& ast_context;
    cpg::CPGContext* cpg_context;

    // 内部辅助方法
    bool extractLoopControl(
        const clang::ForStmt* loop,
        LoopVectorizationPattern& pattern);

    std::vector<ArrayAccess> analyzeArrayAccesses(
        const clang::Stmt* body,
        const std::string& iterator);

    bool detectReductionPattern(
        const clang::ForStmt* loop,
        LoopVectorizationPattern& pattern);

    bool hasLoopCarriedDependencies(
        const LoopVectorizationPattern& pattern);

    bool isVectorizable(
        const LoopVectorizationPattern& pattern);

public:
    explicit LoopVectorizationAnalyzer(clang::ASTContext& ctx, cpg::CPGContext* cpg = nullptr)
        : ast_context(ctx), cpg_context(cpg) {}

    // 主要分析接口
    LoopVectorizationPattern analyzeLoopVectorizability(const clang::ForStmt* loop);

    // 分析整个函数中的所有循环
    std::vector<LoopVectorizationPattern> analyzeFunction(const clang::FunctionDecl* func);

    // 分析单个循环
    LoopVectorizationPattern analyzeLoop(const clang::ForStmt* loop);

    // 分析循环体中的操作
    std::vector<ScalarOperation> analyzeOperations(const clang::Stmt* body);

    // 生成向量化代码
    std::string generateVectorizedCode(
        const LoopVectorizationPattern& pattern,
        const std::string& target_arch);
};

// 函数内联分析器
class FunctionInlineAnalyzer {
private:
    clang::ASTContext& ast_context;
    cpg::CPGContext* cpg_context;

public:
    explicit FunctionInlineAnalyzer(clang::ASTContext& ctx, cpg::CPGContext* cpg)
        : ast_context(ctx), cpg_context(cpg) {}

    FunctionInlineCandidate analyzeFunctionInlineability(const clang::FunctionDecl* func);
    std::vector<FunctionCallContext> analyzeFunctionCalls(const clang::ForStmt* loop);
    bool isPureFunction(const clang::FunctionDecl* func);
};

} // namespace aodsolve