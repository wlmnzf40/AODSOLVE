#pragma once

#include <string>
#include <vector>
#include <map>
#include <set>
#include <memory>
#include <functional>

namespace aodsolve {

// ============================================================================
// 通用优化规则系统 - 不针对特定优化类型,而是基于CPG/AOD模式匹配
// ============================================================================

/**
 * 操作数类型
 */
enum class OperandType {
    VARIABLE,           // 变量
    CONSTANT,           // 常量
    ARRAY_ACCESS,       // 数组访问
    FUNCTION_CALL,      // 函数调用
    EXPRESSION          // 表达式
};

/**
 * 操作数描述
 */
struct OperandDescriptor {
    std::string name;                   // 操作数名称或占位符
    OperandType type;                   // 操作数类型
    std::string data_type;              // 数据类型(int, float, etc)
    bool is_loop_variant;               // 是否随循环变化
    std::string access_pattern;         // 访问模式(sequential, strided, random)
    
    OperandDescriptor() : type(OperandType::VARIABLE), is_loop_variant(false) {}
};

/**
 * 代码模式 - 在CPG/AOD图中匹配的结构化模式
 */
struct CodePattern {
    std::string pattern_id;                         // 模式ID
    std::string description;                        // 模式描述
    
    // 结构匹配条件
    std::vector<std::string> required_node_types;   // 必须包含的节点类型
    std::vector<std::string> required_operations;   // 必须包含的操作
    std::map<std::string, std::string> constraints; // 约束条件
    
    // 数据流模式
    std::vector<std::pair<std::string, std::string>> data_dependencies;  // 数据依赖关系
    
    // 控制流模式
    std::vector<std::string> control_structures;    // 控制结构(if, for, while)
    
    // 匹配优先级
    int priority;
    
    CodePattern() : priority(0) {}
};

/**
 * 代码转换模板 - 将匹配的模式转换为目标代码
 */
struct TransformTemplate {
    std::string template_id;                        // 模板ID
    std::string target_architecture;                // 目标架构(SVE, NEON, AVX2)
    
    // 代码模板(使用占位符)
    std::string code_template;
    
    // 占位符说明
    std::map<std::string, std::string> placeholders; // 占位符 -> 描述
    
    // 需要的头文件
    std::vector<std::string> required_headers;
    
    // 需要的辅助变量
    std::vector<std::string> auxiliary_vars;
    
    // 性能特性
    std::map<std::string, std::string> performance_hints;
};

/**
 * 优化规则 - 连接模式匹配和代码转换
 */
struct OptimizationRule {
    std::string rule_id;                            // 规则ID
    std::string rule_name;                          // 规则名称
    std::string category;                           // 分类(vectorization, inlining, loop_opt, etc)
    
    // 匹配模式
    CodePattern source_pattern;                     // 源代码模式
    
    // 转换模板
    std::map<std::string, TransformTemplate> target_templates;  // 架构 -> 转换模板
    
    // 应用条件
    std::function<bool(void*)> applicability_check; // 适用性检查函数
    
    // 优化效果估计
    int estimated_speedup;                          // 预估加速比
    int code_size_impact;                           // 代码大小影响
    
    OptimizationRule() : estimated_speedup(1), code_size_impact(0) {}
};

/**
 * 规则库 - 管理所有优化规则
 */
class RuleDatabase {
public:
    RuleDatabase() = default;
    
    // 添加规则
    void addRule(const OptimizationRule& rule);
    
    // 查询规则
    std::vector<OptimizationRule*> queryRules(const std::string& category);
    std::vector<OptimizationRule*> queryRulesByPattern(const CodePattern& pattern);
    OptimizationRule* getRuleById(const std::string& rule_id);
    
    // 加载规则(从配置文件)
    void loadRulesFromJSON(const std::string& json_file);
    void loadRulesFromYAML(const std::string& yaml_file);
    
    // 导出规则
    void exportRulesToJSON(const std::string& json_file);
    
    // 统计
    size_t getRuleCount() const { return rules.size(); }
    std::map<std::string, int> getCategoryStatistics() const;
    
private:
    std::map<std::string, OptimizationRule> rules;
    std::map<std::string, std::vector<std::string>> category_index;  // 分类索引
};

/**
 * 模式匹配器 - 在CPG/AOD图中查找匹配的模式
 */
class PatternMatcher {
public:
    PatternMatcher(RuleDatabase* db) : rule_db(db) {}
    
    // 在CPG中匹配模式
    std::vector<std::pair<CodePattern*, void*>> matchInCPG(
        void* cpg_graph,
        const std::string& category = "");
    
    // 在AOD图中匹配模式
    std::vector<std::pair<CodePattern*, void*>> matchInAOD(
        void* aod_graph,
        const std::string& category = "");
    
    // 检查单个模式是否匹配
    bool checkPatternMatch(const CodePattern& pattern, void* graph_node);
    
    // 提取匹配的子图
    void* extractSubgraph(void* graph, void* match_location);
    
private:
    RuleDatabase* rule_db;
    
    // 辅助函数
    bool matchNodeTypes(const std::vector<std::string>& required, void* graph);
    bool matchDataDependencies(const std::vector<std::pair<std::string, std::string>>& deps, void* graph);
};

/**
 * 代码生成器 - 根据转换模板生成目标代码
 */
class UniversalCodeGenerator {
public:
    UniversalCodeGenerator() = default;
    
    // 从模板生成代码
    std::string generateFromTemplate(
        const TransformTemplate& tmpl,
        const std::map<std::string, std::string>& bindings);
    
    // 应用优化规则
    std::string applyRule(
        const OptimizationRule& rule,
        void* matched_subgraph,
        const std::string& target_arch);
    
    // 批量应用规则
    std::string applyRules(
        const std::vector<OptimizationRule*>& rules,
        void* graph,
        const std::string& target_arch);
    
    // 格式化代码
    std::string formatCode(const std::string& code);
    
private:
    // 占位符替换
    std::string replacePlaceholders(
        const std::string& template_str,
        const std::map<std::string, std::string>& bindings);
    
    // 生成辅助变量
    std::string generateAuxiliaryVars(const std::vector<std::string>& aux_vars);
    
    // 插入头文件
    std::string insertHeaders(const std::vector<std::string>& headers);
};

/**
 * 优化管道 - 协调整个优化过程
 */
class OptimizationPipeline {
public:
    OptimizationPipeline(RuleDatabase* db) 
        : rule_db(db), matcher(db), generator() {}
    
    // 运行优化管道
    std::string runOptimization(
        void* input_graph,          // CPG或AOD图
        const std::string& target_arch,
        const std::vector<std::string>& enabled_categories);
    
    // 设置优化级别
    void setOptimizationLevel(int level);  // 0-3
    
    // 启用/禁用特定规则
    void enableRule(const std::string& rule_id);
    void disableRule(const std::string& rule_id);
    
    // 获取优化报告
    std::string getOptimizationReport() const;
    
private:
    RuleDatabase* rule_db;
    PatternMatcher matcher;
    UniversalCodeGenerator generator;
    
    std::set<std::string> enabled_rules;
    std::set<std::string> disabled_rules;
    int optimization_level = 2;
    
    // 应用的规则记录
    std::vector<std::string> applied_rules;
};

} // namespace aodsolve