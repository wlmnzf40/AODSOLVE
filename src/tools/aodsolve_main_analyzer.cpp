#include "tools/aodsolve_main_analyzer.h"
#include "aod/optimization_rule_system.h"
#include "aod/simd_instruction_rules.h"
#include "aod/function_inline_rules.h"
#include <iostream>
#include <sstream>

namespace aodsolve {

// ============================================================================
// 辅助函数：生成目标架构的函数签名
// ============================================================================
// 自动处理参数类型映射（例如将 __m256i 映射为 int8_t* 以适应通用接口）
// 并为 SVE 生成必要的谓词初始化代码
std::string generateFuncSignature(const clang::FunctionDecl* func, const std::string& suffix) {
    std::string sig = "void " + func->getNameAsString() + "_" + suffix + "(";
    bool first = true;
    for (auto param : func->parameters()) {
        if (!first) sig += ", ";
        std::string type = param->getType().getAsString();

        // 简单的类型映射逻辑：将 AVX 类型映射为指针，以便 SVE/NEON 处理
        // 实际项目中可能需要更复杂的类型系统支持
        if (type.find("__m256i") != std::string::npos) {
            type = "int8_t*";
        } else if (type.find("float") != std::string::npos && type.find("*") != std::string::npos) {
            // 保持 float* 不变，或者根据需要添加 volatile
        }

        sig += type + " " + param->getNameAsString();
        first = false;
    }
    sig += ") {\n";

    // 如果是 SVE，自动添加全真谓词初始化，这是 SVE 编程的惯例
    if (suffix == "SVE") sig += "    svbool_t pg = svptrue_b8();\n";
    return sig;
}

// ============================================================================
// AODSolveMainAnalyzer 实现
// ============================================================================

AODSolveMainAnalyzer::AODSolveMainAnalyzer(clang::ASTContext& ctx)
    : ast_context(ctx), source_manager(ctx.getSourceManager()) {
    target_architecture = "SVE"; // 默认目标架构，可通过 setTargetArchitecture 修改
    initializeComponents();
}

void AODSolveMainAnalyzer::initializeComponents() {
    // 初始化核心组件：CPG分析器、AOD转换器、代码生成器
    cpg_analyzer = std::make_unique<IntegratedCPGAnalyzer>(ast_context);
    converter = std::make_unique<EnhancedCPGToAODConverter>(ast_context, *cpg_analyzer);
    code_generator = std::make_unique<EnhancedCodeGenerator>(ast_context);

    // 初始化全局规则库 (单例模式，避免重复加载规则，提高效率)
    static RuleDatabase global_rule_db;
    static bool rules_loaded = false;
    if (!rules_loaded) {
        // 1. 加载 SIMD 指令转换规则 (AVX -> SVE, Scalar -> NEON)
        SIMDInstructionRuleBuilder simd_builder(&global_rule_db);
        simd_builder.buildAllRules();

        // 2. 加载函数内联规则 (用于支持跨函数向量化)
        FunctionInlineRuleBuilder inline_builder(&global_rule_db);
        inline_builder.buildAllRules();

        rules_loaded = true;
    }

    // 将规则库注入代码生成器，使其能够基于规则生成代码
    code_generator->setRuleDatabase(&global_rule_db);
}

ComprehensiveAnalysisResult AODSolveMainAnalyzer::analyzeFunction(const clang::FunctionDecl* func) {
    ComprehensiveAnalysisResult result;

    // 只分析主文件中的函数，跳过系统头文件中的定义
    if (!source_manager.isInMainFile(func->getLocation())) return result;

    std::cout << "\n=== AODSOLVE Analysis: " << func->getNameAsString() << " ===" << std::endl;

    try {
        // 1. 构建代码属性图 (CPG)
        // 这一步进行控制流和数据流分析，为后续的依赖连接提供基础
        cpg_analyzer->analyzeFunctionWithCPG(func);

        // 2. 构建全量 AOD 图 (Algorithm Operator Dependency)
        // 这一步将 AST 转换为包含标量语句、控制流和 SIMD 算子的混合图
        // 并利用 CPG 建立算子间的数据依赖边
        // 传入 "AVX2" 作为源架构提示，target_architecture 作为目标架构提示
        auto conversion_res = converter->convertWithOperators(func, "AVX2", target_architecture);

        if (!conversion_res.successful) {
            throw std::runtime_error(conversion_res.error_message);
        }

        // 3. 设置目标架构并生成代码
        code_generator->setTargetArchitecture(target_architecture);

        // 使用新的 generate 接口：
        // 遍历 AST 结构保证控制流的正确性（如 if/while/for 的嵌套结构）
        // 同时查询 AOD 映射表，对识别出的算子应用向量化规则进行局部替换
        // 未识别的语句则通过 AST Fallback 机制原样保留
        auto gen_res = code_generator->generate(func, conversion_res);

        if (!gen_res.successful) {
            throw std::runtime_error("Code generation failed");
        }

        // 4. 输出完整的、封装好的代码
        std::cout << "\n// Generated " << target_architecture << " Code:\n";
        std::cout << generateFuncSignature(func, target_architecture); // 函数头
        std::cout << gen_res.generated_code;                           // 函数体
        std::cout << "}\n";                                            // 结束符

        result.successful = true;
        // 保存代码生成结果到 result 中，以便后续处理或报告生成
        result.code_results[func] = gen_res;

    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << std::endl;
        result.successful = false;
        result.errors.push_back(e.what());
    }
    return result;
}

// 空实现 (占位符)
// 这些函数是为了满足类的接口定义，防止链接错误
// 在当前的 demo 场景中主要使用 analyzeFunction
ComprehensiveAnalysisResult AODSolveMainAnalyzer::analyzeTranslationUnit() { return {}; }
ComprehensiveAnalysisResult AODSolveMainAnalyzer::analyzeFile(const std::string&) { return {}; }
std::string AODSolveMainAnalyzer::generateComprehensiveReport(const ComprehensiveAnalysisResult&) { return ""; }
std::string AODSolveMainAnalyzer::generatePerformanceReport(const ComprehensiveAnalysisResult&) { return ""; }

} // namespace aodsolve
