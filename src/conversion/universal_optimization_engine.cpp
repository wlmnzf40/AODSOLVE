#include "aod/optimization_rule_system.h"
#include "analysis/loop_vectorization_analyzer.h"
#include "analysis/integrated_cpg_analyzer.h"
#include "aod/function_inline_rules.h"
#include "aod/simd_instruction_rules.h"

#include "tools/aodsolve_main_analyzer.h"
#include "conversion/enhanced_cpg_to_aod_converter.h"

#include <clang/AST/RecursiveASTVisitor.h>
#include <sstream>
#include <algorithm>

namespace aodsolve {

// ============================================================================
// 通用优化引擎 - 基于CPG/AOD图的模式匹配和代码生成
// ============================================================================

class UniversalOptimizationEngine {
public:
    UniversalOptimizationEngine()
        : pipeline(&rule_db) {  // 初始化pipeline
        initializeRules();
    }

    /**
     * 核心优化接口
     *
     * @param cpg_graph CPG图(包含控制流、数据流等)
     * @param aod_graph AOD图(算子依赖图)
     * @param target_arch 目标架构(SVE, NEON, AVX512等)
     * @return 优化后的代码
     */
    std::string optimize(
        void* cpg_graph,
        void* aod_graph,
        const std::string& target_arch);

    /**
     * 指定优化类别
     */
    std::string optimizeWithCategories(
        void* cpg_graph,
        void* aod_graph,
        const std::string& target_arch,
        const std::vector<std::string>& categories);

    /**
     * 获取优化统计
     */
    std::map<std::string, int> getOptimizationStats() const {
        return optimization_stats;
    }

    /**
     * 获取应用的规则列表
     */
    std::vector<std::string> getAppliedRules() const {
        return applied_rules;
    }

private:
    RuleDatabase rule_db;
    OptimizationPipeline pipeline;

    std::map<std::string, int> optimization_stats;
    std::vector<std::string> applied_rules;

    void initializeRules();
};

// ============================================================================
// 初始化所有规则
// ============================================================================

void UniversalOptimizationEngine::initializeRules() {
    // 注意：这里需要实际的规则构建器实现
    // 这些构建器应该在对应的头文件中定义

    // 1. 加载循环向量化规则
    // LoopVectorizationRuleBuilder loop_builder(&rule_db);
    // loop_builder.buildAllRules();

    // 2. 加载函数内联规则
    // FunctionInlineRuleBuilder inline_builder(&rule_db);
    // inline_builder.buildAllRules();

    // 3. 加载SIMD指令转换规则
    // SIMDInstructionRuleBuilder simd_builder(&rule_db);
    // simd_builder.buildAllRules();

    std::cout << "✓ Initialized rule database with "
              << rule_db.getRuleCount() << " rules" << std::endl;

    auto stats = rule_db.getCategoryStatistics();
    for (const auto& [category, count] : stats) {
        std::cout << "  - " << category << ": " << count << " rules" << std::endl;
    }
}

// ============================================================================
// 核心优化流程
// ============================================================================

std::string UniversalOptimizationEngine::optimize(
    void* cpg_graph,
    void* aod_graph,
    const std::string& target_arch) {

    std::vector<std::string> all_categories = {
        "loop_vectorization",
        "function_inline",
        "simd_conversion"
    };

    return optimizeWithCategories(cpg_graph, aod_graph, target_arch, all_categories);
}

std::string UniversalOptimizationEngine::optimizeWithCategories(
    void* cpg_graph,
    void* /*aod_graph*/,
    const std::string& target_arch,
    const std::vector<std::string>& categories) {

    std::stringstream optimized_code;

    // 运行优化管道
    std::string result = pipeline.runOptimization(
        cpg_graph,
        target_arch,
        categories
    );

    optimized_code << result;

    // 获取优化报告
    std::string report = pipeline.getOptimizationReport();

    return optimized_code.str();
}

// ============================================================================
// CPG-based Vectorization Optimizer
// ============================================================================

class CPGBasedVectorizationOptimizer {
public:
    CPGBasedVectorizationOptimizer(
        clang::ASTContext* ast_ctx,
        IntegratedCPGAnalyzer* cpg_analyzer)
        : ast_context_(ast_ctx), cpg_analyzer_(cpg_analyzer) {}

    /**
     * 分析并优化函数
     */
    std::string optimizeFunction(
        const clang::FunctionDecl* func,
        const std::string& target_arch);

    /**
     * 为函数构建CPG
     */
    void* buildCPGForFunction(const clang::FunctionDecl* func);

    /**
     * 从CPG构建AOD
     */
    void* buildAODForFunction(const clang::FunctionDecl* func);

private:
    clang::ASTContext* ast_context_;
    IntegratedCPGAnalyzer* cpg_analyzer_;
};

std::string CPGBasedVectorizationOptimizer::optimizeFunction(
    const clang::FunctionDecl* func,
    const std::string& target_arch) {

    if (!func || !func->hasBody()) {
        return "// Error: Invalid function\n";
    }

    std::stringstream code;

    code << "// Optimized version for " << target_arch << "\n";
    code << "// Function: " << func->getNameAsString() << "\n\n";

    // 1. 构建CPG
    void* cpg = buildCPGForFunction(func);

    // 2. 构建AOD
    void* aod = buildAODForFunction(func);

    // 3. 应用优化
    UniversalOptimizationEngine engine;
    std::string optimized = engine.optimize(cpg, aod, target_arch);

    code << optimized;

    return code.str();
}

void* CPGBasedVectorizationOptimizer::buildCPGForFunction(
    const clang::FunctionDecl* /*func*/) {
    // 使用CPG分析器构建CPG
    if (cpg_analyzer_) {
        return const_cast<cpg::CPGContext*>(&cpg_analyzer_->getCPGContext());
    }
    return nullptr;
}

void* CPGBasedVectorizationOptimizer::buildAODForFunction(
    const clang::FunctionDecl* /*func*/) {
    // TODO: 实现AOD构建
    return nullptr;
}

} // namespace aodsolve