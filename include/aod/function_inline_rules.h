#pragma once

#include "optimization_rule_system.h"

namespace aodsolve {

/**
 * 函数内联规则构建器
 * * 不直接分析函数,而是定义"什么样的函数调用可以内联并向量化"的规则
 */
class FunctionInlineRuleBuilder {
public:
    FunctionInlineRuleBuilder(RuleDatabase* db) : rule_db(db) {}

    // 构建所有函数内联规则
    void buildAllRules();

private:
    RuleDatabase* rule_db;

    // 各种函数模式的内联规则
    void buildSimpleArithmeticFunctionRule();
    void buildMinMaxFunctionRule();
    void buildClampFunctionRule();
    void buildAbsFunctionRule();
    void buildSaturateAddFunctionRule();
};

/**
 * min/max函数内联规则
 */
inline void FunctionInlineRuleBuilder::buildMinMaxFunctionRule() {
    OptimizationRule rule;
    rule.rule_id = "minmax_function_inline";
    rule.rule_name = "Min/Max Function Inlining and Vectorization";
    rule.category = "function_inline";

    // === 源模式 ===
    CodePattern& pattern = rule.source_pattern;
    pattern.pattern_id = "minmax_call";
    pattern.description = "Call to min() or max() function";

    pattern.required_node_types = {"CallExpr"};
    pattern.required_operations = {"min", "max", "fmin", "fmax"};
    pattern.constraints["num_params"] = "2";
    pattern.constraints["is_pure"] = "true";

    // === SVE转换模板 ===
    TransformTemplate sve_tmpl;
    sve_tmpl.template_id = "minmax_sve";
    sve_tmpl.target_architecture = "SVE";

    sve_tmpl.code_template = R"(
sv{{element_type}}_t {{output}}_vec = sv{{operation}}_{{element_type}}_z({{predicate}}, {{input_0}}_vec, {{input_1}}_vec);
)";

    sve_tmpl.placeholders = {
        {"{{operation}}", "min or max"},
        {"{{element_type}}", "Element type (f32, s32, etc)"},
        {"{{predicate}}", "SVE predicate (pg)"},
        {"{{input_0}}_vec", "First input vector"},
        {"{{input_1}}_vec", "Second input vector"},
        {"{{output}}_vec", "Output vector"}
    };

    sve_tmpl.required_headers = {"<arm_sve.h>"};

    // === NEON转换模板 ===
    TransformTemplate neon_tmpl;
    neon_tmpl.template_id = "minmax_neon";
    neon_tmpl.target_architecture = "NEON";

    neon_tmpl.code_template = R"(
{{neon_type}} {{output}}_vec = v{{operation}}q_{{suffix}}({{input_0}}_vec, {{input_1}}_vec);
)";

    neon_tmpl.placeholders = {
        {"{{neon_type}}", "NEON vector type (float32x4_t, int32x4_t)"},
        {"{{suffix}}", "Type suffix (f32, s32, etc)"}
    };

    neon_tmpl.required_headers = {"<arm_neon.h>"};

    rule.target_templates["SVE"] = sve_tmpl;
    rule.target_templates["NEON"] = neon_tmpl;

    rule_db->addRule(rule);
}

/**
 * clamp函数内联规则
 * * 模式: clamp(x, min, max) => min(max(x, min), max)
 */
inline void FunctionInlineRuleBuilder::buildClampFunctionRule() {
    OptimizationRule rule;
    rule.rule_id = "clamp_function_inline";
    rule.rule_name = "Clamp Function Inlining and Vectorization";
    rule.category = "function_inline";

    CodePattern& pattern = rule.source_pattern;
    pattern.pattern_id = "clamp_call";
    pattern.description = "Call to clamp/clip/saturate function";

    pattern.required_node_types = {"CallExpr"};
    pattern.required_operations = {"clamp", "clip", "saturate"};
    pattern.constraints["num_params"] = "3";

    // === SVE转换模板 ===
    TransformTemplate sve_tmpl;
    sve_tmpl.template_id = "clamp_sve";
    sve_tmpl.target_architecture = "SVE";

    sve_tmpl.code_template = R"(
// clamp(x, min, max) = min(max(x, min), max)
sv{{element_type}}_t temp_vec = svmax_{{element_type}}_z({{predicate}}, {{input}}_vec, {{min_value}}_vec);
sv{{element_type}}_t {{output}}_vec = svmin_{{element_type}}_z({{predicate}}, temp_vec, {{max_value}}_vec);
)";

    sve_tmpl.placeholders = {
        {"{{input}}_vec", "Input vector"},
        {"{{min_value}}_vec", "Minimum value vector"},
        {"{{max_value}}_vec", "Maximum value vector"},
        {"{{output}}_vec", "Output vector"}
    };

    // === NEON转换模板 ===
    TransformTemplate neon_tmpl;
    neon_tmpl.template_id = "clamp_neon";
    neon_tmpl.target_architecture = "NEON";

    neon_tmpl.code_template = R"(
{{neon_type}} temp_vec = vmaxq_{{suffix}}({{input}}_vec, {{min_value}}_vec);
{{neon_type}} {{output}}_vec = vminq_{{suffix}}(temp_vec, {{max_value}}_vec);
)";

    rule.target_templates["SVE"] = sve_tmpl;
    rule.target_templates["NEON"] = neon_tmpl;

    rule_db->addRule(rule);
}

/**
 * abs函数内联规则
 */
inline void FunctionInlineRuleBuilder::buildAbsFunctionRule() {
    OptimizationRule rule;
    rule.rule_id = "abs_function_inline";
    rule.rule_name = "Absolute Value Function Inlining";
    rule.category = "function_inline";

    CodePattern& pattern = rule.source_pattern;
    pattern.pattern_id = "abs_call";
    pattern.description = "Call to abs/fabs/absolute function";

    pattern.required_operations = {"abs", "fabs", "absolute"};
    pattern.constraints["num_params"] = "1";

    // === SVE转换模板 ===
    TransformTemplate sve_tmpl;
    sve_tmpl.template_id = "abs_sve";
    sve_tmpl.target_architecture = "SVE";

    sve_tmpl.code_template = R"(
sv{{element_type}}_t {{output}}_vec = svabs_{{element_type}}_z({{predicate}}, {{input}}_vec);
)";

    // === NEON转换模板 ===
    TransformTemplate neon_tmpl;
    neon_tmpl.template_id = "abs_neon";
    neon_tmpl.target_architecture = "NEON";

    neon_tmpl.code_template = R"(
{{neon_type}} {{output}}_vec = vabsq_{{suffix}}({{input}}_vec);
)";

    rule.target_templates["SVE"] = sve_tmpl;
    rule.target_templates["NEON"] = neon_tmpl;

    rule_db->addRule(rule);
}

/**
 * saturate_add函数内联规则
 */
inline void FunctionInlineRuleBuilder::buildSaturateAddFunctionRule() {
    OptimizationRule rule;
    rule.rule_id = "saturate_add_inline";
    rule.rule_name = "Saturating Add Function Inlining";
    rule.category = "function_inline";

    CodePattern& pattern = rule.source_pattern;
    pattern.pattern_id = "saturate_add_call";
    pattern.description = "Call to saturating add function";

    pattern.required_operations = {"saturate_add", "qadd", "sat_add"};
    pattern.constraints["num_params"] = "2";

    // === SVE转换模板 ===
    TransformTemplate sve_tmpl;
    sve_tmpl.template_id = "saturate_add_sve";
    sve_tmpl.target_architecture = "SVE";

    sve_tmpl.code_template = R"(
sv{{element_type}}_t {{output}}_vec = svqadd_{{element_type}}({{input_0}}_vec, {{input_1}}_vec);
)";

    sve_tmpl.required_headers = {"<arm_sve.h>"};

    // === NEON转换模板 ===
    TransformTemplate neon_tmpl;
    neon_tmpl.template_id = "saturate_add_neon";
    neon_tmpl.target_architecture = "NEON";

    neon_tmpl.code_template = R"(
{{neon_type}} {{output}}_vec = vqaddq_{{suffix}}({{input_0}}_vec, {{input_1}}_vec);
)";

    neon_tmpl.required_headers = {"<arm_neon.h>"};

    rule.target_templates["SVE"] = sve_tmpl;
    rule.target_templates["NEON"] = neon_tmpl;

    rule_db->addRule(rule);
}

/**
 * 简单算术函数内联规则
 * * 例如: float calc(float a, float b) { return a * 2.0f + b; }
 */
inline void FunctionInlineRuleBuilder::buildSimpleArithmeticFunctionRule() {
    OptimizationRule rule;
    rule.rule_id = "simple_arithmetic_inline";
    rule.rule_name = "Simple Arithmetic Function Inlining";
    rule.category = "function_inline";

    CodePattern& pattern = rule.source_pattern;
    pattern.pattern_id = "arithmetic_function";
    pattern.description = "Function with simple arithmetic operations";

    pattern.required_node_types = {"FunctionDecl", "BinaryOperator"};
    pattern.constraints["has_return"] = "true";
    pattern.constraints["num_statements"] = "1";  // 只有一个return语句
    pattern.constraints["is_pure"] = "true";

    // 这个规则需要更复杂的模板替换
    // 因为需要直接展开函数体

    TransformTemplate sve_tmpl;
    sve_tmpl.template_id = "arithmetic_inline_sve";
    sve_tmpl.target_architecture = "SVE";

    sve_tmpl.code_template = R"(
// 内联函数体(向量化版本)
{{inlined_body_vectorized}}
)";

    sve_tmpl.placeholders = {
        {"{{inlined_body_vectorized}}", "Vectorized version of function body"}
    };

    rule.target_templates["SVE"] = sve_tmpl;

    rule_db->addRule(rule);
}

inline void FunctionInlineRuleBuilder::buildAllRules() {
    buildMinMaxFunctionRule();
    buildClampFunctionRule();
    buildAbsFunctionRule();
    buildSaturateAddFunctionRule();
    buildSimpleArithmeticFunctionRule();
}

} // namespace aodsolve
