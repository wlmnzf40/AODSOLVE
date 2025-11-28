#pragma once

#include <string>
#include <vector>
#include <map>
#include <set>
#include <memory>
#include <functional>

namespace aodsolve {

enum class OperandType { VARIABLE, CONSTANT, ARRAY_ACCESS, FUNCTION_CALL, EXPRESSION };

struct CodePattern {
    std::string pattern_id;
    std::string description;
    std::vector<std::string> required_node_types;
    std::vector<std::string> required_operations;
    std::map<std::string, std::string> constraints;
    std::vector<std::pair<std::string, std::string>> data_dependencies;
    std::vector<std::string> control_structures;
    int priority = 0;
};

struct TransformTemplate {
    std::string template_id;
    std::string target_architecture;
    std::string code_template;

    std::map<std::string, std::string> performance_hints;
    std::map<std::string, std::string> input_adaptations;
    std::map<std::string, std::string> arg_replacements;
    std::map<std::string, std::string> placeholders;

    std::vector<std::string> required_headers;
    std::vector<std::string> auxiliary_vars;
};

struct OptimizationRule {
    std::string rule_id;
    std::string rule_name;
    std::string category;
    CodePattern source_pattern;
    std::map<std::string, TransformTemplate> target_templates;
    std::function<bool(void*)> applicability_check;
    int estimated_speedup = 1;
    int code_size_impact = 0;
};

class RuleDatabase {
public:
    void addRule(const OptimizationRule& rule) {
        rules[rule.rule_id] = rule;
        category_index[rule.category].push_back(rule.rule_id);
    }

    std::vector<OptimizationRule*> queryRules(const std::string& category) {
        std::vector<OptimizationRule*> res;
        if (category_index.count(category)) {
            for(const auto& id : category_index.at(category)) res.push_back(&rules[id]);
        }
        return res;
    }

    OptimizationRule* findRuleForOp(const std::string& op_name, const std::string& category = "") {
        for (auto& kv : rules) {
            if (!category.empty() && kv.second.category != category) continue;
            for (const auto& req : kv.second.source_pattern.required_operations) {
                if (req == op_name) return &kv.second;
            }
        }
        return nullptr;
    }

    std::vector<OptimizationRule*> queryRulesByPattern(const CodePattern&) { return {}; }
    OptimizationRule* getRuleById(const std::string& id) { return rules.count(id) ? &rules[id] : nullptr; }
    void loadRulesFromJSON(const std::string&) {}
    void loadRulesFromYAML(const std::string&) {}
    void exportRulesToJSON(const std::string&) {}
    std::map<std::string, int> getCategoryStatistics() const { return {}; }

private:
    std::map<std::string, OptimizationRule> rules;
    std::map<std::string, std::vector<std::string>> category_index;
};

// 占位符类
class PatternMatcher {
public:
    static std::vector<std::pair<CodePattern*, void*>> matchInCPG(void*, void*) { return {}; }
    static std::vector<std::pair<CodePattern*, void*>> matchInAOD(void*, void*) { return {}; }
};

class UniversalCodeGenerator {
public:
    static std::string generateFromTemplate(const TransformTemplate&, const std::map<std::string, std::string>&) { return ""; }
};

class OptimizationPipeline {
public:
    OptimizationPipeline(RuleDatabase*) {}
    std::string runOptimization(void*, const std::string&, const std::vector<std::string>&) { return ""; }
};

} // namespace aodsolve
