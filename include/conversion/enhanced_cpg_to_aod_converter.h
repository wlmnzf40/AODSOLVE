#pragma once

#include "analysis/integrated_cpg_analyzer.h"
#include "aod/enhanced_aod_node.h"
#include "aod/enhanced_aod_graph.h"
#include "analysis/CPGAnnotation.h"

#include <memory>
#include <map>
#include <set>
#include <vector>
#include <string>
#include <queue>
#include <functional>

namespace aodsolve {

// ============================================
// è½¬æ¢å™¨ç»“æžœ
// ============================================

// 转换规则结构 (规则驱动引擎的核心)
struct TransformRule {
    std::string source_intrinsic;                              // 源指令名
    std::map<std::string, std::string> target_templates;       // 目标平台 -> 代码模板
    std::map<std::string, std::string> return_types;           // 目标平台 -> 返回类型
    std::map<std::string, bool> requires_predicate;            // 目标平台 -> 是否需要predicate
    std::map<std::string, bool> needs_split;                   // 目标平台 -> 是否需要拆分
    std::string special_handling;                              // 特殊处理标记
};

struct ConversionResult {
    bool successful = false;
    std::string error_message;
    std::vector<std::string> warnings;
    std::vector<std::string> info_messages;

    std::shared_ptr<AODGraph> aod_graph;
    std::map<const clang::Stmt*, int> stmt_to_node_map;
    std::map<const clang::FunctionDecl*, int> func_to_region_map;
    std::map<std::string, int> var_to_phi_map;

    // ç»Ÿè®¡ä¿¡æ¯
    int original_stmt_count = 0;
    int converted_node_count = 0;
    int data_flow_edges = 0;
    int control_flow_edges = 0;
    int interprocedural_calls = 0;
    int phi_nodes_created = 0;
};

// è·¨å‡½æ•°æ•°æ®æµåˆ†æžç»“æžœ
struct InterproceduralDataFlow {
    std::map<const clang::CallExpr*, std::map<std::string, std::string>> argument_flows;
    std::map<const clang::CallExpr*, std::string> return_value_flows;
    std::map<const clang::FunctionDecl*, std::set<std::string>> side_effects;
    std::map<const clang::CallExpr*, std::vector<std::string>> affected_variables;
    std::set<std::string> global_variables;
    std::map<std::string, std::set<const clang::CallExpr*>> var_to_callers;
};

// å¢žå¼ºçš„CPGåˆ°AODè½¬æ¢å™¨
class EnhancedCPGToAODConverter {
private:
    clang::ASTContext& ast_context;
    clang::SourceManager& source_manager;
    IntegratedCPGAnalyzer* analyzer;

    // è½¬æ¢çŠ¶æ€
    int next_node_id = 0;
    int next_region_id = 0;
    std::map<const clang::Stmt*, std::shared_ptr<AODNode>> stmt_to_node;
    std::map<const clang::FunctionDecl*, std::shared_ptr<AODNode>> func_entry_nodes;
    std::map<const clang::FunctionDecl*, std::shared_ptr<AODNode>> func_exit_nodes;
    std::map<std::string, std::shared_ptr<AODNode>> phi_nodes;

    // è·¨å‡½æ•°åˆ†æž
    InterproceduralDataFlow interprocedural_flow;
    std::set<const clang::CallExpr*> processed_calls;
    std::map<const clang::FunctionDecl*, std::set<const clang::CallExpr*>> function_callers;

public:
    explicit EnhancedCPGToAODConverter(clang::ASTContext& ctx, IntegratedCPGAnalyzer& a);
    explicit EnhancedCPGToAODConverter(clang::ASTContext& ctx);
    ~EnhancedCPGToAODConverter() = default;

    // ä¸»è¦è½¬æ¢æŽ¥å£
    ConversionResult convertFunctionWithCPG(const clang::FunctionDecl* func);
    ConversionResult convertTranslationUnit();
    ConversionResult convertCallGraph(const clang::FunctionDecl* root);

    // ä¸“é—¨çš„è½¬æ¢æ–¹æ³•
    ConversionResult convertStringProcessingFunction(const clang::FunctionDecl* func);
    ConversionResult convertBitwiseOperationsFunction(const clang::FunctionDecl* func);
    ConversionResult convertUTF8ValidationFunction(const clang::FunctionDecl* func);

    // è·¨å‡½æ•°è½¬æ¢
    ConversionResult convertWithInterproceduralAnalysis(const clang::FunctionDecl* func);
    InterproceduralDataFlow analyzeInterproceduralDataFlow(const clang::FunctionDecl* root);

    // æ¨¡å¼è¯†åˆ«å’Œè½¬æ¢
    std::vector<SIMDPatternMatch> identifySIMDPatternsDuringConversion(const clang::FunctionDecl* func);
    std::shared_ptr<AODNode> convertToSIMDNode(const clang::Stmt* stmt, const SIMDPatternMatch& pattern);
    void optimizeDuringConversion(std::shared_ptr<AODGraph> graph);

    // æ•°æ®æµè½¬æ¢
    void convertDataDependencies(const cpg::DataDependency& dep, std::shared_ptr<AODGraph> graph);
    void convertControlDependencies(const cpg::ControlDependency& dep, std::shared_ptr<AODGraph> graph);
    void createPhiNodes(const std::string& var, const std::set<const clang::Stmt*>& defs,
                       const std::set<const clang::Stmt*>& uses, std::shared_ptr<AODGraph> graph);

    // ============================================
    // 新增：算子级别转换接口（支持跨架构SIMD转换）
    // ============================================

    // 从CPG提取计算图
    std::vector<ComputeGraphNode> extractComputeGraphFromCPG(
        const clang::FunctionDecl* func,
        cpg::CPGContext& cpg_context);

    // 识别SIMD算子（算子级别的模式匹配）
    std::vector<SIMDOperator> identifySIMDOperators(
        const clang::FunctionDecl* func,
        const std::vector<ComputeGraphNode>& compute_graph);

    // 算子级别的完整转换流程
    ConversionResult convertWithOperators(
        const clang::FunctionDecl* func,
        const std::string& source_arch,    // 如 "AVX2"
        const std::string& target_arch);   // 如 "SVE"

    // 将算子映射回AST子树（用于兼容现有的AST pattern系统）
    std::vector<const clang::Stmt*> mapOperatorToASTSubtree(
        const SIMDOperator& op);

    // 生成目标架构代码
    std::string generateTargetCode(
        const std::vector<SIMDOperator>& operators,
        const std::string& target_arch);

    // 获取转换规则库
    std::vector<ComputeGraphPattern> getConversionRules(
        const std::string& source_arch,
        const std::string& target_arch);


    // æŽ§åˆ¶æµè½¬æ¢
    std::shared_ptr<AODNode> convertICFGNode(cpg::ICFGNode* icfg_node);
    std::shared_ptr<AODNode> convertPDGNode(cpg::PDGNode* pdg_node);
    void connectControlFlow(std::shared_ptr<AODGraph> graph, cpg::ICFGNode* source, cpg::ICFGNode* target);

    // è°ƒç”¨å›¾è½¬æ¢
    void convertCallGraph(std::shared_ptr<AODGraph> global_graph, const clang::FunctionDecl* root);
    std::shared_ptr<AODNode> convertCallSite(const clang::CallExpr* call, cpg::ICFGNode* call_node);
    std::shared_ptr<AODNode> convertFunctionEntry(const clang::FunctionDecl* func);
    std::shared_ptr<AODNode> convertFunctionExit(const clang::FunctionDecl* func);
    void convertParameterFlow(const clang::CallExpr* call, std::shared_ptr<AODGraph> graph);
    void convertReturnValueFlow(const clang::CallExpr* call, std::shared_ptr<AODGraph> graph);

    // å†…å­˜æµè½¬æ¢
    void convertMemoryFlows(const cpg::PDGNode* pdg_node, std::shared_ptr<AODGraph> graph);
    void detectAliasDependencies(const clang::FunctionDecl* func, std::shared_ptr<AODGraph> graph);
    void convertMemoryAccessPatterns(const clang::FunctionDecl* func, std::shared_ptr<AODGraph> graph);

    // SIMDç‰¹å®šè½¬æ¢
    std::shared_ptr<AODNode> convertAVXPattern(const clang::Stmt* stmt, const std::string& operation);
    std::shared_ptr<AODNode> convertNEONPattern(const clang::Stmt* stmt, const std::string& operation);
    std::shared_ptr<AODNode> convertSVEattern(const clang::Stmt* stmt, const std::string& operation);
    std::shared_ptr<AODNode> convertAVX512Pattern(const clang::Stmt* stmt, const std::string& operation);

    // ä¼˜åŒ–è½¬æ¢
    void eliminateDeadNodes(std::shared_ptr<AODGraph> graph);
    void performConstantFolding(std::shared_ptr<AODGraph> graph);
    void performCommonSubexpressionElimination(std::shared_ptr<AODGraph> graph);
    void performLoopInvariantCodeMotion(std::shared_ptr<AODGraph> graph);

    // éªŒè¯å’Œè°ƒè¯•
    bool validateConversion(const ConversionResult& result) const;
    void checkDataFlowConsistency(const ConversionResult& result) const;
    void checkControlFlowConsistency(const ConversionResult& result) const;
    void checkSIMDConsistency(const ConversionResult& result) const;

    // æŠ¥å‘Šå’Œå¯è§†åŒ–
    std::string generateConversionReport(const ConversionResult& result) const;
    std::string generateDataFlowReport(const InterproceduralDataFlow& flow) const;
    void saveConversionToDOT(const ConversionResult& result, const std::string& filename);
    void saveDataFlowToDOT(const InterproceduralDataFlow& flow, const std::string& filename);

    // å·¥å…·æ–¹æ³•
    const InterproceduralDataFlow& getInterproceduralFlow() const { return interprocedural_flow; }
    std::map<const clang::Stmt*, std::shared_ptr<AODNode>> getNodeMapping() const { return stmt_to_node; }
    void resetState() {
        next_node_id = 0;
        next_region_id = 0;
        stmt_to_node.clear();
        func_entry_nodes.clear();
        func_exit_nodes.clear();
        phi_nodes.clear();
        interprocedural_flow = {};
        processed_calls.clear();
        function_callers.clear();
    }

private:
    // å†…éƒ¨è½¬æ¢è¾…åŠ©æ–¹æ³•
    std::shared_ptr<AODNode> createAODNodeForStmt(const clang::Stmt* stmt);
    std::shared_ptr<AODNode> createAODNodeForDecl(const clang::Decl* decl);
    std::shared_ptr<AODNode> createAODNodeForExpr(const clang::Expr* expr);

    // è¡¨è¾¾å¼è½¬æ¢
    std::shared_ptr<AODNode> convertBinaryOperator(const clang::BinaryOperator* op);
    std::shared_ptr<AODNode> convertUnaryOperator(const clang::UnaryOperator* op);
    std::shared_ptr<AODNode> convertCallExpr(const clang::CallExpr* call);
    std::shared_ptr<AODNode> convertArraySubscript(const clang::ArraySubscriptExpr* arr);
    std::shared_ptr<AODNode> convertMemberExpr(const clang::MemberExpr* member);
    std::shared_ptr<AODNode> convertDeclRefExpr(const clang::DeclRefExpr* ref);
    std::shared_ptr<AODNode> convertCastExpr(const clang::CastExpr* cast);

    // å£°æ˜Žè½¬æ¢
    std::shared_ptr<AODNode> convertVarDecl(const clang::VarDecl* var);
    std::shared_ptr<AODNode> convertFunctionDecl(const clang::FunctionDecl* func);
    std::shared_ptr<AODNode> convertParmVarDecl(const clang::ParmVarDecl* param);

    // è¯­å¥è½¬æ¢
    std::shared_ptr<AODNode> convertForStmt(const clang::ForStmt* for_stmt);
    std::shared_ptr<AODNode> convertWhileStmt(const clang::WhileStmt* while_stmt);
    std::shared_ptr<AODNode> convertDoStmt(const clang::DoStmt* do_stmt);
    std::shared_ptr<AODNode> convertIfStmt(const clang::IfStmt* if_stmt);
    std::shared_ptr<AODNode> convertSwitchStmt(const clang::SwitchStmt* switch_stmt);
    std::shared_ptr<AODNode> convertReturnStmt(const clang::ReturnStmt* return_stmt);
    std::shared_ptr<AODNode> convertBreakStmt(const clang::BreakStmt* break_stmt);
    std::shared_ptr<AODNode> convertContinueStmt(const clang::ContinueStmt* continue_stmt);
    std::shared_ptr<AODNode> convertCompoundStmt(const clang::CompoundStmt* comp_stmt);

    // å¤æ‚è¯­å¥è½¬æ¢
    std::vector<std::shared_ptr<AODNode>> convertStatementList(const clang::Stmt* stmt);
    void connectSequentialNodes(const std::vector<std::shared_ptr<AODNode>>& nodes, std::shared_ptr<AODGraph> graph);
    void connectConditionalBranches(const clang::IfStmt* if_stmt, std::shared_ptr<AODNode> condition_node,
                                   std::shared_ptr<AODNode> then_node, std::shared_ptr<AODNode> else_node,
                                   std::shared_ptr<AODGraph> graph);
    void connectLoopNodes(const clang::Stmt* loop_stmt, std::shared_ptr<AODNode> init_node,
                         std::shared_ptr<AODNode> condition_node, std::shared_ptr<AODNode> body_node,
                         std::shared_ptr<AODGraph> graph);

    // ç‰¹æ®Šå¤„ç†æ–¹æ³•
    void handleDataFlowDisruption(const clang::Stmt* stmt, std::shared_ptr<AODGraph> graph);
    void handleControlFlowComplexity(const clang::Stmt* stmt, std::shared_ptr<AODGraph> graph);
    void handleInterproceduralCalls(const clang::CallExpr* call, std::shared_ptr<AODGraph> graph);
    void handleSideEffects(const clang::Stmt* stmt, std::shared_ptr<AODGraph> graph);

    // è·¨å‡½æ•°æ•°æ®æµåˆ†æž
    void analyzeCallArgumentFlow(const clang::CallExpr* call);
    void analyzeCallReturnFlow(const clang::CallExpr* call);
    void analyzeFunctionSideEffects(const clang::FunctionDecl* func);
    void propagateGlobalEffects();
    void detectParameterAliasing(const clang::CallExpr* call);

    // æ¨¡å¼è¯†åˆ«è¾…åŠ©æ–¹æ³•
    bool isSIMDIntrinsicCall(const clang::CallExpr* call) const;
    std::string identifySIMDOperation(const clang::CallExpr* call) const;
    std::string identifySIMDType(const clang::CallExpr* call) const;
    int getVectorWidth(const clang::CallExpr* call) const;

    // ä¼˜åŒ–è¾…åŠ©æ–¹æ³•
    bool isDeadCode(const std::shared_ptr<AODNode>& node) const;
    bool isConstantExpression(const clang::Expr* expr) const;
    std::set<std::string> getLoopInvariantVariables(const clang::Stmt* loop_stmt) const;
    std::vector<std::shared_ptr<AODNode>> findCommonSubexpressions(std::shared_ptr<AODGraph> graph) const;

    // éªŒè¯è¾…åŠ©æ–¹æ³•
    bool hasCircularDataFlow(const std::shared_ptr<AODGraph>& graph) const;
    bool hasOrphanedNodes(const std::shared_ptr<AODGraph>& graph) const;
    bool hasInconsistentControlFlow(const std::shared_ptr<AODGraph>& graph) const;
    bool hasMissingDataDependencies(const std::shared_ptr<AODGraph>& graph) const;

    // ============================================
    // 新增：算子级别转换的辅助方法
    // ============================================

    // 算子识别辅助方法
    bool matchComputePattern(
        const std::vector<ComputeGraphNode>& subgraph,
        const ComputeGraphPattern& pattern);

    ComputeGraphNode createComputeNodeFromStmt(
        const clang::Stmt* stmt,
        int node_id);

    // 从Call表达式创建计算图节点
    ComputeGraphNode createComputeNodeFromCallExpr(
        const clang::CallExpr* call,
        int node_id);

    // ========== 新增：支持标量操作 ==========
    ComputeGraphNode createComputeNodeFromBinaryOp(
        const clang::BinaryOperator* bin_op,
        int node_id);

    ComputeGraphNode createComputeNodeFromUnaryOp(
        const clang::UnaryOperator* unary_op,
        int node_id);

    ComputeGraphNode createComputeNodeFromArrayAccess(
        const clang::ArraySubscriptExpr* array_sub,
        int node_id);

    // 算子转换辅助方法
    std::string generateOperatorCode(
        const SIMDOperator& op,
        const std::string& target_arch);

    std::map<std::string, std::string> createVariableMapping(
        const SIMDOperator& op);

    // 创建预定义的转换规则
    std::vector<ComputeGraphPattern> createAVX2ToSVERules();
    std::vector<ComputeGraphPattern> createSSEToNEONRules();

    // 模式匹配辅助
    bool isSubgraphMatch(
        const std::vector<ComputeGraphNode>& graph,
        size_t start_idx,
        const ComputeGraphPattern& pattern);

    // 提取连续的SIMD操作序列
    std::vector<std::vector<const clang::Stmt*>> extractSIMDSequences(
        const clang::FunctionDecl* func);

    // ============================================
    // 新增: 动态代码生成相关函数 (不写死操作数)
    // ============================================

    // 操作数信息结构
    struct OperandInfo {
        std::string name;        // 变量名
        std::string value;       // 常量值(如果是常量)
        bool is_constant;

        OperandInfo() : is_constant(false) {}
    };



    // 动态生成算子代码 (基于实际操作数)
    std::string generateOperatorCodeDynamic(
        const SIMDOperator& op,
        const std::string& target_arch,
        std::map<std::string, std::string>& operand_map,
        std::set<std::string>& declared_vars,
        int& temp_var_counter);

    // 从 AST 提取操作数信息
    std::vector<OperandInfo> extractOperandsFromAST(const SIMDOperator& op);

    // 从表达式提取操作数
    OperandInfo extractOperandFromExpr(const clang::Expr* expr);

    // 生成目标架构的变量名
    std::string generateTargetVarName(
        const std::string& source_name,
        const std::string& target_arch);

    // 查找操作数映射
    std::string lookupOperand(
        const std::string& source_name,
        const std::map<std::string, std::string>& operand_map);

    // ============================================
    // 新增: 规则驱动转换引擎的辅助函数
    // ============================================

    // 查找转换规则
    TransformRule* findTransformRule(
        const std::string& source_intrinsic,
        const std::string& target_arch);

    // 初始化规则数据库
    void initializeRuleDatabase(
        std::map<std::string, std::map<std::string, TransformRule>>& db);

    // 获取规则的代码模板
    std::string getCodeTemplate(
        const TransformRule* rule,
        const std::string& target_arch);

    // 获取规则的返回类型
    std::string getRuleReturnType(
        const TransformRule* rule,
        const std::string& target_arch);

    // 获取规则的 predicate 名称
    std::string getRulePredicate(
        const TransformRule* rule,
        const std::string& target_arch);

    // 检查是否需要拆分(NEON)
    bool needsSplitting(const TransformRule* rule);

    // 模板替换引擎
    std::string applyTemplateSubstitutions(
        const std::string& template_str,
        const std::map<std::string, std::string>& substitutions);

    // 生成输出变量名
    std::string generateOutputVarName(
        const ComputeGraphPattern& pattern,
        int counter);

    // 应用 NEON 拆分策略
    std::string applySplitStrategy(
        const std::string& code,
        const TransformRule* rule,
        const std::vector<OperandInfo>& operands,
        std::map<std::string, std::string>& operand_map,
        std::set<std::string>& declared_vars,
        int& temp_var_counter);
};

// ä¸“é—¨ç”¨äºŽå¤„ç†å¤æ‚æ¡ˆä¾‹çš„è½¬æ¢å™¨
class ComplexCaseConverter {
private:
    EnhancedCPGToAODConverter& base_converter;

public:
    explicit ComplexCaseConverter(EnhancedCPGToAODConverter& c) : base_converter(c) {}

    // æ¡ˆä¾‹1è½¬æ¢ - å­—ç¬¦ä¸²å¤„ç†
    ConversionResult convertStringCase(const clang::FunctionDecl* func);
    void identifyStringPatterns(const clang::FunctionDecl* func, std::vector<SIMDPatternMatch>& patterns);
    std::shared_ptr<AODNode> convertStringSIMDOperation(const clang::Stmt* stmt, const std::string& op);

    // æ¡ˆä¾‹2è½¬æ¢ - ä½æ“ä½œ
    ConversionResult convertBitwiseCase(const clang::FunctionDecl* func);
    void identifyBitwisePatterns(const clang::FunctionDecl* func, std::vector<SIMDPatternMatch>& patterns);
    std::shared_ptr<AODNode> convertBitwiseSIMDOperation(const clang::Stmt* stmt, const std::string& op);

    // æ¡ˆä¾‹3è½¬æ¢ - UTF-8éªŒè¯
    ConversionResult convertUTF8Case(const clang::FunctionDecl* func);
    void identifyUTF8Patterns(const clang::FunctionDecl* func, std::vector<SIMDPatternMatch>& patterns);
    std::shared_ptr<AODNode> convertUTF8SIMDOperation(const clang::Stmt* stmt, const std::string& op);

    // é€šç”¨å¤æ‚å¤„ç†
    void handleMultiFunctionScenarios();
    void handleDataFlowDisruptions();
    void handleControlFlowComplexities();
    void generateCaseSpecificOptimizations(int case_id);
};

} // namespace aodsolve