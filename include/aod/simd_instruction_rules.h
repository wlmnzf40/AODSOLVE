#pragma once

#include "optimization_rule_system.h"

namespace aodsolve {

class SIMDInstructionRuleBuilder {
    RuleDatabase* rule_db;
public:
    SIMDInstructionRuleBuilder(RuleDatabase* db) : rule_db(db) {}

    void buildAllRules() {
        auto add_common_cleanups = [](TransformTemplate& t) {
            t.arg_replacements["(__m256i *)"] = "(int8_t *)";
            t.arg_replacements["(__m256i*)"] = "(int8_t *)";
            t.arg_replacements["const __m256i"] = "const int8_t";
        };

        // === AVX2 -> SVE Rules ===
        {
            OptimizationRule rule;
            rule.rule_id = "avx2_set1_epi8";
            rule.category = "simd_instruction";
            rule.source_pattern.required_operations = {"_mm256_set1_epi8"};
            TransformTemplate sve;
            sve.target_architecture = "SVE";
            sve.code_template = "svdup_s8({{input_0}})";
            sve.performance_hints["return_type"] = "svint8_t";
            add_common_cleanups(sve);
            rule.target_templates["SVE"] = sve;
            rule_db->addRule(rule);
        }
        {
            OptimizationRule rule;
            rule.rule_id = "avx2_loadu_si256";
            rule.category = "simd_instruction";
            rule.source_pattern.required_operations = {"_mm256_loadu_si256"};
            TransformTemplate sve;
            sve.target_architecture = "SVE";
            sve.code_template = "svld1_s8(pg, (const int8_t*){{input_0}})";
            sve.performance_hints["return_type"] = "svint8_t";
            add_common_cleanups(sve);
            rule.target_templates["SVE"] = sve;
            rule_db->addRule(rule);
        }
        {
            OptimizationRule rule;
            rule.rule_id = "avx2_storeu_si256";
            rule.category = "simd_instruction";
            rule.source_pattern.required_operations = {"_mm256_storeu_si256"};
            TransformTemplate sve;
            sve.target_architecture = "SVE";
            sve.code_template = "svst1_s8(pg, (int8_t*){{input_0}}, {{input_1}})";
            sve.performance_hints["return_type"] = "void";
            add_common_cleanups(sve);
            rule.target_templates["SVE"] = sve;
            rule_db->addRule(rule);
        }
        {
            OptimizationRule rule;
            rule.rule_id = "avx2_cmpgt_epi8";
            rule.category = "simd_instruction";
            rule.source_pattern.required_operations = {"_mm256_cmpgt_epi8"};
            TransformTemplate sve;
            sve.target_architecture = "SVE";
            sve.code_template = "svcmpgt_s8(pg, {{input_0}}, {{input_1}})";
            sve.performance_hints["return_type"] = "svbool_t";
            add_common_cleanups(sve);
            rule.target_templates["SVE"] = sve;
            rule_db->addRule(rule);
        }
        {
            OptimizationRule rule;
            rule.rule_id = "avx2_and_si256";
            rule.category = "simd_instruction";
            rule.source_pattern.required_operations = {"_mm256_and_si256"};
            TransformTemplate sve;
            sve.target_architecture = "SVE";
            sve.code_template = "svand_s8_z(pg, {{input_0}}, {{input_1}})";
            sve.performance_hints["return_type"] = "svint8_t";
            sve.input_adaptations["cmp"] = "svsel_s8({{input}}, svdup_s8(0xFF), svdup_s8(0x00))";
            add_common_cleanups(sve);
            rule.target_templates["SVE"] = sve;
            rule_db->addRule(rule);
        }
        {
            OptimizationRule rule;
            rule.rule_id = "avx2_add_epi8";
            rule.category = "simd_instruction";
            rule.source_pattern.required_operations = {"_mm256_add_epi8"};
            TransformTemplate sve;
            sve.target_architecture = "SVE";
            sve.code_template = "svadd_s8_z(pg, {{input_0}}, {{input_1}})";
            sve.performance_hints["return_type"] = "svint8_t";
            add_common_cleanups(sve);
            rule.target_templates["SVE"] = sve;
            rule_db->addRule(rule);
        }

        // === Scalar -> NEON Rules ===
        {
            OptimizationRule rule;
            rule.rule_id = "scalar_add_float";
            rule.category = "scalar_vectorization";
            rule.source_pattern.required_operations = {"+"};
            TransformTemplate neon;
            neon.target_architecture = "NEON";
            neon.code_template = "vaddq_f32({{input_0}}, {{input_1}})";
            neon.performance_hints["return_type"] = "float32x4_t";
            rule.target_templates["NEON"] = neon;
            rule_db->addRule(rule);
        }
        {
            OptimizationRule rule;
            rule.rule_id = "scalar_sub_float";
            rule.category = "scalar_vectorization";
            rule.source_pattern.required_operations = {"-"};
            TransformTemplate neon;
            neon.target_architecture = "NEON";
            neon.code_template = "vsubq_f32({{input_0}}, {{input_1}})";
            neon.performance_hints["return_type"] = "float32x4_t";
            rule.target_templates["NEON"] = neon;
            rule_db->addRule(rule);
        }
        {
            OptimizationRule rule;
            rule.rule_id = "scalar_mul_float";
            rule.category = "scalar_vectorization";
            rule.source_pattern.required_operations = {"*"};
            TransformTemplate neon;
            neon.target_architecture = "NEON";
            neon.code_template = "vmulq_f32({{input_0}}, {{input_1}})";
            neon.performance_hints["return_type"] = "float32x4_t";
            rule.target_templates["NEON"] = neon;
            rule_db->addRule(rule);
        }

        // === Case 6: BF16 Dot Product Fusion ===
        {
            OptimizationRule rule;
            rule.rule_id = "neon_bf16_dot";
            rule.category = "scalar_vectorization";
            rule.source_pattern.required_operations = {"bf16_dot"};

            TransformTemplate neon;
            neon.target_architecture = "NEON";

            // 移除首尾换行符，防止生成格式错误
            neon.code_template =
                "{{accum_var}} = vbfdotq_f32({{accum_var}}, "
                "vld1q_bf16((bfloat16_t *)({{input_0}} + i)), "
                "vld1q_bf16((bfloat16_t *)({{input_1}} + i)))";

            neon.performance_hints["return_type"] = "void";
            rule.target_templates["NEON"] = neon;
            rule_db->addRule(rule);
        }
    }
};

} // namespace aodsolve
