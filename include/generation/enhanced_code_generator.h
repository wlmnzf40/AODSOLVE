#pragma once
#include "aod/enhanced_aod_graph.h"
#include "analysis/integrated_cpg_analyzer.h"
#include "conversion/enhanced_cpg_to_aod_converter.h"
#include "aod/optimization_rule_system.h"

#include <memory>
#include <map>
#include <set>
#include <vector>
#include <string>
#include <sstream>

namespace aodsolve {

struct CodeGenerationResult {
    bool successful = false;
    std::string generated_code;
    std::vector<std::string> warnings;
    std::vector<std::string> info_messages;
    std::vector<std::string> optimization_applied;
    std::map<std::string, std::string> variable_mapping;
    double estimated_speedup = 0.0;
    std::string target_architecture;
    int vector_operations = 0;
    int total_operations = 0;
    double memory_access_efficiency = 0.0;
    int lines_of_code = 0;
    int functions_generated = 0;
    int simd_intrinsics = 0;
    int loops_optimized = 0;
    int dead_code_eliminated = 0;
};

class EnhancedCodeGenerator {
private:
    std::string target_architecture;
    int optimization_level;
    bool enable_vectorization;
    bool enable_interprocedural_optimization;
    bool preserve_debug_info;

    std::map<std::string, std::function<std::string(const std::shared_ptr<AODNode>&)>> node_templates;
    std::map<AODNodeType, std::string> node_type_to_template;
    std::map<std::string, std::string> intrinsic_mappings;

    std::map<std::string, std::string> variable_renaming;
    std::set<std::string> reserved_names;
    int variable_counter = 0;

    std::stringstream current_code;
    int indent_level = 0;
    bool in_function = false;
    std::string current_function_name;

    // ===== 新增: 规则库指针 =====
    RuleDatabase* rule_db = nullptr;

public:
    explicit EnhancedCodeGenerator(const std::string& arch = "AVX2", int opt_level = 2);
    explicit EnhancedCodeGenerator(clang::ASTContext& ctx);
    ~EnhancedCodeGenerator() = default;

    // 设置规则库
    void setRuleDatabase(RuleDatabase* db) { rule_db = db; }

    CodeGenerationResult generateCodeFromGraph(const AODGraphPtr& graph);
    CodeGenerationResult generateCodeFromConversion(const ConversionResult& conversion);

    std::shared_ptr<AODGraph> buildSimpleAODGraphFromBindings(
    const std::map<std::string, std::string>& bindings);

    std::string generateLoopFromTemplate(
    const std::map<std::string, std::string>& bindings,
    const std::string& target_arch);

    // ===== 核心方法: 从AOD图生成向量化循环 =====
    std::string generateVectorizedLoopFromAOD(
    const AODGraphPtr& aod_graph,
    const std::map<std::string, std::string>& loop_context,
    const std::string& target_arch);

    // // ===== 从模板生成循环(用于demo) =====
    // std::string generateLoopFromTemplate(
    //     const std::map<std::string, std::string>& bindings,
    //     const std::string& target_arch);
    //     const AODGraphPtr& aod_graph,
    //     const std::map<std::string, std::string>& loop_context,
    //     const std::string& target_arch);

    // ===== 从算子生成SIMD指令 =====
    std::string generateSIMDInstructionForOperator(
        std::shared_ptr<AODNode> op_node,
        const std::map<std::string, std::string>& bindings,
        const std::string& target_arch);

    void setTargetArchitecture(const std::string& arch) { target_architecture = arch; }
    void setOptimizationLevel(int level) { optimization_level = level; }

    std::string formatCode(const std::string& code);
    // ===== 从AOD节点推断算子类型 =====
    // std::string inferOperatorType(const std::shared_ptr<AODNode>& node);
    std::string inferOperatorType(std::shared_ptr<AODNode> node);

private:
    void initializeNodeTemplates();
    void initializeIntrinsicMappings();



    // ===== 查找匹配的SIMD规则 =====
    OptimizationRule* findMatchingSIMDRule(
        const std::string& operator_type,
        const std::string& target_arch);
};

} // namespace aodsolve