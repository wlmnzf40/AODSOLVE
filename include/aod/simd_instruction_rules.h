#pragma once

#include "optimization_rule_system.h"

namespace aodsolve {

/**
 * SIMD指令转换规则构建器
 * 
 * 定义如何将AVX2指令转换为SVE/NEON指令的规则
 */
class SIMDInstructionRuleBuilder {
public:
    SIMDInstructionRuleBuilder(RuleDatabase* db) : rule_db(db) {}
    
    // 构建所有SIMD指令转换规则
    void buildAllRules();
    
private:
    RuleDatabase* rule_db;
    
    // 各类指令的转换规则
    void buildLoadStoreRules();
    void buildArithmeticRules();
    void buildComparisonRules();
    void buildLogicalRules();
    void buildShuffleRules();
    void buildSpecialRules();
};

/**
 * 加载/存储指令规则
 */
inline void SIMDInstructionRuleBuilder::buildLoadStoreRules() {
    // === _mm256_loadu_si256 规则 ===
    {
        OptimizationRule rule;
        rule.rule_id = "avx2_loadu_si256";
        rule.rule_name = "AVX2 Unaligned Load Conversion";
        rule.category = "simd_instruction";
        
        CodePattern& pattern = rule.source_pattern;
        pattern.pattern_id = "mm256_loadu_si256";
        pattern.required_operations = {"_mm256_loadu_si256"};
        
        // SVE转换
        TransformTemplate sve_tmpl;
        sve_tmpl.code_template = "sv{{element_type}}_t {{output}} = svld1_{{element_type}}({{predicate}}, {{address}});";
        sve_tmpl.placeholders = {
            {"{{output}}", "Output vector variable"},
            {"{{address}}", "Memory address to load from"},
            {"{{element_type}}", "Element type (u8, s32, f32, etc)"},
            {"{{predicate}}", "SVE predicate"}
        };
        sve_tmpl.required_headers = {"<arm_sve.h>"};
        
        // NEON转换(需要拆分为2x128-bit)
        TransformTemplate neon_tmpl;
        neon_tmpl.code_template = R"(
{{neon_type}} {{output}}_lo = vld1q_{{suffix}}((const {{scalar_type}}*){{address}});
{{neon_type}} {{output}}_hi = vld1q_{{suffix}}((const {{scalar_type}}*)({{address}} + {{vector_width}}));
)";
        neon_tmpl.placeholders = {
            {"{{neon_type}}", "NEON vector type"},
            {"{{suffix}}", "Type suffix"},
            {"{{scalar_type}}", "Scalar type"},
            {"{{vector_width}}", "Vector width (16 for uint8x16_t)"}
        };
        
        rule.target_templates["SVE"] = sve_tmpl;
        rule.target_templates["NEON"] = neon_tmpl;
        
        rule_db->addRule(rule);
    }
    
    // === _mm256_storeu_si256 规则 ===
    {
        OptimizationRule rule;
        rule.rule_id = "avx2_storeu_si256";
        rule.rule_name = "AVX2 Unaligned Store Conversion";
        rule.category = "simd_instruction";
        
        CodePattern& pattern = rule.source_pattern;
        pattern.required_operations = {"_mm256_storeu_si256"};
        
        TransformTemplate sve_tmpl;
        sve_tmpl.code_template = "svst1_{{element_type}}({{predicate}}, {{address}}, {{input}});";
        sve_tmpl.placeholders = {
            {"{{input}}", "Input vector to store"},
            {"{{address}}", "Memory address"}
        };
        
        TransformTemplate neon_tmpl;
        neon_tmpl.code_template = R"(
vst1q_{{suffix}}(({{scalar_type}}*){{address}}, {{input}}_lo);
vst1q_{{suffix}}(({{scalar_type}}*)({{address}} + {{vector_width}}), {{input}}_hi);
)";
        
        rule.target_templates["SVE"] = sve_tmpl;
        rule.target_templates["NEON"] = neon_tmpl;
        
        rule_db->addRule(rule);
    }
}

/**
 * 算术指令规则
 */
inline void SIMDInstructionRuleBuilder::buildArithmeticRules() {
    // === _mm256_add_epi8/16/32 规则 ===
    {
        OptimizationRule rule;
        rule.rule_id = "avx2_add";
        rule.rule_name = "AVX2 Add Conversion";
        rule.category = "simd_instruction";
        
        CodePattern& pattern = rule.source_pattern;
        pattern.required_operations = {"_mm256_add_epi8", "_mm256_add_epi16", "_mm256_add_epi32"};
        
        TransformTemplate sve_tmpl;
        sve_tmpl.code_template = "sv{{element_type}}_t {{output}} = svadd_{{element_type}}_z({{predicate}}, {{input_0}}, {{input_1}});";
        
        TransformTemplate neon_tmpl;
        neon_tmpl.code_template = R"(
{{neon_type}} {{output}}_lo = vaddq_{{suffix}}({{input_0}}_lo, {{input_1}}_lo);
{{neon_type}} {{output}}_hi = vaddq_{{suffix}}({{input_0}}_hi, {{input_1}}_hi);
)";
        
        rule.target_templates["SVE"] = sve_tmpl;
        rule.target_templates["NEON"] = neon_tmpl;
        
        rule_db->addRule(rule);
    }
    
    // === _mm256_sub_epi8/16/32 规则 ===
    {
        OptimizationRule rule;
        rule.rule_id = "avx2_sub";
        rule.rule_name = "AVX2 Subtract Conversion";
        rule.category = "simd_instruction";
        
        TransformTemplate sve_tmpl;
        sve_tmpl.code_template = "sv{{element_type}}_t {{output}} = svsub_{{element_type}}_z({{predicate}}, {{input_0}}, {{input_1}});";
        
        TransformTemplate neon_tmpl;
        neon_tmpl.code_template = R"(
{{neon_type}} {{output}}_lo = vsubq_{{suffix}}({{input_0}}_lo, {{input_1}}_lo);
{{neon_type}} {{output}}_hi = vsubq_{{suffix}}({{input_0}}_hi, {{input_1}}_hi);
)";
        
        rule.target_templates["SVE"] = sve_tmpl;
        rule.target_templates["NEON"] = neon_tmpl;
        
        rule_db->addRule(rule);
    }
}

/**
 * 比较指令规则
 */
inline void SIMDInstructionRuleBuilder::buildComparisonRules() {
    // === _mm256_cmpgt_epi8 规则 ===
    {
        OptimizationRule rule;
        rule.rule_id = "avx2_cmpgt";
        rule.rule_name = "AVX2 Compare Greater Than";
        rule.category = "simd_instruction";
        
        CodePattern& pattern = rule.source_pattern;
        pattern.required_operations = {"_mm256_cmpgt_epi8", "_mm256_cmpgt_epi16", "_mm256_cmpgt_epi32"};
        
        TransformTemplate sve_tmpl;
        sve_tmpl.code_template = "svbool_t {{output}} = svcmpgt_{{element_type}}({{predicate}}, {{input_0}}, {{input_1}});";
        sve_tmpl.placeholders["{{output}}"] = "Output predicate";
        
        TransformTemplate neon_tmpl;
        neon_tmpl.code_template = R"(
{{uint_type}} {{output}}_lo = vcgtq_{{suffix}}({{input_0}}_lo, {{input_1}}_lo);
{{uint_type}} {{output}}_hi = vcgtq_{{suffix}}({{input_0}}_hi, {{input_1}}_hi);
)";
        neon_tmpl.placeholders["{{uint_type}}"] = "Unsigned vector type for mask";
        
        rule.target_templates["SVE"] = sve_tmpl;
        rule.target_templates["NEON"] = neon_tmpl;
        
        rule_db->addRule(rule);
    }
}

/**
 * 逻辑指令规则
 */
inline void SIMDInstructionRuleBuilder::buildLogicalRules() {
    // === _mm256_and_si256 规则 ===
    {
        OptimizationRule rule;
        rule.rule_id = "avx2_and";
        rule.rule_name = "AVX2 Bitwise AND";
        rule.category = "simd_instruction";
        
        TransformTemplate sve_tmpl;
        sve_tmpl.code_template = "sv{{element_type}}_t {{output}} = svand_{{element_type}}_z({{predicate}}, {{input_0}}, {{input_1}});";
        
        TransformTemplate neon_tmpl;
        neon_tmpl.code_template = R"(
{{neon_type}} {{output}}_lo = vandq_{{suffix}}({{input_0}}_lo, {{input_1}}_lo);
{{neon_type}} {{output}}_hi = vandq_{{suffix}}({{input_0}}_hi, {{input_1}}_hi);
)";
        
        rule.target_templates["SVE"] = sve_tmpl;
        rule.target_templates["NEON"] = neon_tmpl;
        
        rule_db->addRule(rule);
    }
    
    // === _mm256_or_si256 规则 ===
    {
        OptimizationRule rule;
        rule.rule_id = "avx2_or";
        rule.rule_name = "AVX2 Bitwise OR";
        rule.category = "simd_instruction";
        
        TransformTemplate sve_tmpl;
        sve_tmpl.code_template = "sv{{element_type}}_t {{output}} = svorr_{{element_type}}_z({{predicate}}, {{input_0}}, {{input_1}});";
        
        TransformTemplate neon_tmpl;
        neon_tmpl.code_template = R"(
{{neon_type}} {{output}}_lo = vorrq_{{suffix}}({{input_0}}_lo, {{input_1}}_lo);
{{neon_type}} {{output}}_hi = vorrq_{{suffix}}({{input_0}}_hi, {{input_1}}_hi);
)";
        
        rule.target_templates["SVE"] = sve_tmpl;
        rule.target_templates["NEON"] = neon_tmpl;
        
        rule_db->addRule(rule);
    }
}

/**
 * Shuffle/Permute指令规则
 */
inline void SIMDInstructionRuleBuilder::buildShuffleRules() {
    // === _mm256_shuffle_epi8 规则 ===
    {
        OptimizationRule rule;
        rule.rule_id = "avx2_shuffle_epi8";
        rule.rule_name = "AVX2 Byte Shuffle";
        rule.category = "simd_instruction";
        
        CodePattern& pattern = rule.source_pattern;
        pattern.required_operations = {"_mm256_shuffle_epi8"};
        
        // SVE: 使用 svtbl
        TransformTemplate sve_tmpl;
        sve_tmpl.code_template = "svuint8_t {{output}} = svtbl_u8({{input}}, {{indices}});";
        sve_tmpl.placeholders = {
            {"{{input}}", "Input vector"},
            {"{{indices}}", "Shuffle indices vector"}
        };
        
        // NEON: 使用 vtbl 系列
        TransformTemplate neon_tmpl;
        neon_tmpl.code_template = R"(
// NEON vtbl 需要特殊处理
uint8x16_t {{output}}_lo = vqtbl1q_u8({{input}}_lo, {{indices}}_lo);
uint8x16_t {{output}}_hi = vqtbl1q_u8({{input}}_hi, {{indices}}_hi);
)";
        
        rule.target_templates["SVE"] = sve_tmpl;
        rule.target_templates["NEON"] = neon_tmpl;
        
        rule_db->addRule(rule);
    }
    
    // === _mm256_setr_epi8 规则(32个参数的常量设置) ===
    {
        OptimizationRule rule;
        rule.rule_id = "avx2_setr_epi8";
        rule.rule_name = "AVX2 Set Reverse EPI8";
        rule.category = "simd_instruction";
        
        TransformTemplate sve_tmpl;
        sve_tmpl.code_template = R"(
// SVE: 使用svindex创建索引或者直接加载常量数组
const uint8_t {{const_array}}[32] = {{{values}}};
svuint8_t {{output}} = svld1_u8({{predicate}}, {{const_array}});
)";
        sve_tmpl.placeholders["{{values}}"] = "32 comma-separated values";
        
        TransformTemplate neon_tmpl;
        neon_tmpl.code_template = R"(
// NEON: 拆分为两个128-bit向量
uint8x16_t {{output}}_lo = {{{values_lo}}};
uint8x16_t {{output}}_hi = {{{values_hi}}};
)";
        neon_tmpl.placeholders = {
            {"{{values_lo}}", "First 16 values"},
            {"{{values_hi}}", "Last 16 values"}
        };
        
        rule.target_templates["SVE"] = sve_tmpl;
        rule.target_templates["NEON"] = neon_tmpl;
        
        rule_db->addRule(rule);
    }
}

/**
 * 特殊指令规则
 */
inline void SIMDInstructionRuleBuilder::buildSpecialRules() {
    // === _mm256_movemask_epi8 规则 ===
    {
        OptimizationRule rule;
        rule.rule_id = "avx2_movemask_epi8";
        rule.rule_name = "AVX2 Move Mask EPI8";
        rule.category = "simd_instruction";
        
        CodePattern& pattern = rule.source_pattern;
        pattern.required_operations = {"_mm256_movemask_epi8"};
        
        // SVE: 没有直接等价,需要手动实现
        TransformTemplate sve_tmpl;
        sve_tmpl.code_template = R"(
// SVE: 通过比较创建mask,然后转换为整数
svbool_t mask = svcmplt_s8({{predicate}}, {{input}}, svdup_n_s8(0));
uint32_t {{output}} = 0;
// 需要手动提取每个bit(SVE没有直接等价指令)
// 这通常需要循环或者特殊处理
)";
        
        // NEON: 使用 vshrn + vmovn 系列
        TransformTemplate neon_tmpl;
        neon_tmpl.code_template = R"(
// NEON: 需要多步骤操作
// 1. 提取符号位
uint8x16_t sign_lo = vshrq_n_u8(vreinterpretq_u8_s8({{input}}_lo), 7);
uint8x16_t sign_hi = vshrq_n_u8(vreinterpretq_u8_s8({{input}}_hi), 7);

// 2. 压缩为掩码
// 这需要复杂的位操作,通常需要查找表或多次移位+or
uint32_t {{output}} = /* 复杂的位打包操作 */;
)";
        
        rule.target_templates["SVE"] = sve_tmpl;
        rule.target_templates["NEON"] = neon_tmpl;
        
        rule_db->addRule(rule);
    }
    
    // === _mm256_set1_epi8 规则 ===
    {
        OptimizationRule rule;
        rule.rule_id = "avx2_set1_epi8";
        rule.rule_name = "AVX2 Set1 EPI8 (Broadcast)";
        rule.category = "simd_instruction";
        
        TransformTemplate sve_tmpl;
        sve_tmpl.code_template = "svuint8_t {{output}} = svdup_n_u8({{value}});";
        
        TransformTemplate neon_tmpl;
        neon_tmpl.code_template = R"(
uint8x16_t {{output}}_lo = vdupq_n_u8({{value}});
uint8x16_t {{output}}_hi = vdupq_n_u8({{value}});
)";
        
        rule.target_templates["SVE"] = sve_tmpl;
        rule.target_templates["NEON"] = neon_tmpl;
        
        rule_db->addRule(rule);
    }
}

inline void SIMDInstructionRuleBuilder::buildAllRules() {
    buildLoadStoreRules();
    buildArithmeticRules();
    buildComparisonRules();
    buildLogicalRules();
    buildShuffleRules();
    buildSpecialRules();
}

} // namespace aodsolve