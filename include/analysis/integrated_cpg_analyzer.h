#pragma once

#include "aod/enhanced_aod_graph.h"
#include "analysis/enhanced_ast_analyzer.h"
#include "analysis/CPGAnnotation.h"

#include <memory>
#include <map>
#include <set>
#include <vector>
#include <string>
#include <functional>

namespace aodsolve {

// ============================================
// CPG到AOD的转换结果
// ============================================

struct CPGToAODConversion {
    std::shared_ptr<AODGraph> aod_graph;
    std::map<const clang::Stmt*, int> stmt_to_node_id;
    std::map<const clang::FunctionDecl*, int> func_to_region_id;
    std::vector<std::string> conversion_log;
    std::vector<std::string> warnings;
    std::vector<std::string> errors;
    
    // 分析结果
    bool successful = false;
    int node_count = 0;
    int edge_count = 0;
    int control_flow_preserved = true;
    int data_flow_preserved = true;
    int interprocedural_calls = 0;
};

// 增强的CPG分析器，集成AOD框架
class IntegratedCPGAnalyzer {
private:
    clang::ASTContext& ast_context;
    clang::SourceManager& source_manager;
    cpg::CPGContext cpg_context;
    EnhancedASTAnalyzer aod_analyzer;
    
    // 转换缓存
    std::map<const clang::FunctionDecl*, CPGToAODConversion> conversion_cache;
    std::map<const clang::CallExpr*, std::shared_ptr<AODGraph>> call_site_conversions;
    
    // 分析结果
    std::map<const clang::FunctionDecl*, std::shared_ptr<AODGraph>> function_graphs;
    std::shared_ptr<AODGraph> global_graph;
    std::map<std::string, std::shared_ptr<AODGraph>> module_graphs;

public:
    explicit IntegratedCPGAnalyzer(clang::ASTContext& ctx);
    ~IntegratedCPGAnalyzer() = default;

    // 主要分析接口
    CPGToAODConversion analyzeFunctionWithCPG(const clang::FunctionDecl* func);
    CPGToAODConversion analyzeTranslationUnitWithCPG();
    std::vector<CPGToAODConversion> analyzeCallGraph(const clang::FunctionDecl* root);
    
    // CPG特定分析
    std::vector<cpg::DataDependency> getDataDependencies(const clang::Stmt* stmt) const;
    std::vector<cpg::ControlDependency> getControlDependencies(const clang::Stmt* stmt) const;
    std::set<std::string> getVariablesAtStatement(const clang::Stmt* stmt) const;
    std::set<std::string> getDefinitions(const clang::Stmt* stmt, const std::string& var) const;
    std::set<std::string> getUses(const clang::Stmt* stmt, const std::string& var) const;
    
    // 数据流分析
    bool hasDataFlowPath(const clang::Stmt* source, const clang::Stmt* sink, const std::string& var = "") const;
    bool hasControlFlowPath(const clang::Stmt* source, const clang::Stmt* sink) const;
    std::vector<std::vector<clang::Stmt*>> findAllPaths(clang::Stmt* source, clang::Stmt* sink, int max_depth = 100) const;
    
    // 跨函数分析
    std::vector<std::string> traceVariableAcrossFunctions(const std::string& var, const clang::CallExpr* call);
    std::map<std::string, std::string> analyzeParameterFlow(const clang::CallExpr* call);
    std::vector<std::string> identifySideEffects(const clang::FunctionDecl* func);
    bool isPureFunction(const clang::FunctionDecl* func);
    
    // SIMD分析集成
    std::vector<SIMDPatternMatch> findSIMDPatternsInCPG(const clang::FunctionDecl* func);
    std::vector<std::string> identifyVectorizableRegions(const clang::FunctionDecl* func);
    std::vector<std::string> analyzeDataHazardsInSIMD(const clang::FunctionDecl* func);
    
    // 循环分析
    std::vector<int> findLoopsWithCPG(const clang::FunctionDecl* func);
    std::vector<std::string> analyzeLoopDependencies(const clang::FunctionDecl* func, int loop_id);
    std::vector<std::string> findLoopCarriedDependencies(int loop_id);
    bool canVectorizeLoop(int loop_id);
    
    // 优化分析
    std::vector<std::string> findDeadCode(const clang::FunctionDecl* func);
    std::vector<std::string> findCommonSubexpressions(const clang::FunctionDecl* func);
    std::vector<std::string> findLoopInvariantCode(int loop_id);
    std::vector<std::string> generateOptimizationSuggestions(const clang::FunctionDecl* func);
    
    // 性能分析
    int computeCyclomaticComplexity(const clang::FunctionDecl* func) const;
    double estimateMemoryFootprint(const clang::FunctionDecl* func) const;
    std::string generatePerformanceReport(const clang::FunctionDecl* func);
    
    // 可视化
    std::string generateCPGVisualization(const clang::FunctionDecl* func);
    std::string generateIntegratedVisualization(const clang::FunctionDecl* func);
    void saveVisualizationToFile(const clang::FunctionDecl* func, const std::string& filename);
    
    // 工具方法
    const cpg::CPGContext& getCPGContext() const { return cpg_context; }
    const EnhancedASTAnalyzer& getAODAnalyzer() const { return aod_analyzer; }
    std::shared_ptr<AODGraph> getFunctionGraph(const clang::FunctionDecl* func) const;
    std::shared_ptr<AODGraph> getGlobalGraph() const { return global_graph; }
    
    // 缓存管理
    void clearConversionCache();
    void invalidateFunctionCache(const clang::FunctionDecl* func);
    
    // 高级分析
    std::vector<std::string> analyzeExceptionPaths(const clang::FunctionDecl* func);
    std::vector<std::string> analyzeConcurrencyOpportunities(const clang::FunctionDecl* func);
    std::vector<std::string> findCriticalPath(const clang::FunctionDecl* func);
    
    // 报告生成
    std::string generateComprehensiveReport(const clang::FunctionDecl* func);
    std::string generateCallGraphReport();
    void generateOptimizationPlan(const clang::FunctionDecl* func, std::ostream& out);
    
private:
    // 内部转换方法
    cpg::ICFGNode* getICFGNode(const clang::Stmt* stmt) const;
    cpg::PDGNode* getPDGNode(const clang::Stmt* stmt) const;
    std::shared_ptr<AODNode> convertICFGNodeToAOD(cpg::ICFGNode* icfg_node);
    std::shared_ptr<AODNode> convertPDGNodeToAOD(cpg::PDGNode* pdg_node);
    void connectNodesWithCPGEdges(const cpg::ICFGNode* source, const cpg::ICFGNode* target, 
                                 std::shared_ptr<AODGraph> graph);
    void connectNodesWithDataFlow(const cpg::DataDependency& dep, std::shared_ptr<AODGraph> graph);
    void connectNodesWithControlFlow(const cpg::ControlDependency& dep, std::shared_ptr<AODGraph> graph);
    
    // 模式匹配
    bool isSIMDLoadPattern(const cpg::PDGNode* node) const;
    bool isSIMDStorePattern(const cpg::PDGNode* node) const;
    bool isSIMDArithmeticPattern(const cpg::PDGNode* node) const;
    std::string identifySIMDType(const cpg::PDGNode* node) const;
    
    // 循环分析
    std::vector<int> findLoopHeadersInCPG(const clang::FunctionDecl* func) const;
    std::set<int> getBlocksInLoop(int loop_header, const clang::FunctionDecl* func) const;
    std::vector<std::string> analyzeDataDependencesInLoop(int loop_id) const;
    
    // 性能估算
    int estimateNodeCost(const cpg::PDGNode* node) const;
    int estimateEdgeCost(const cpg::DataDependency& dep) const;
    int estimateControlFlowCost(const cpg::ControlDependency& dep) const;
    
    // 验证方法
    bool validateConversion(const CPGToAODConversion& conversion) const;
    void checkDataFlowConsistency(const CPGToAODConversion& conversion) const;
    void checkControlFlowConsistency(const CPGToAODConversion& conversion) const;
};

// 专门用于复杂案例分析的类
class ComplexCaseAnalyzer {
private:
    IntegratedCPGAnalyzer& analyzer;
    
public:
    explicit ComplexCaseAnalyzer(IntegratedCPGAnalyzer& a) : analyzer(a) {}
    
    // 案例1分析 - 字符串处理
    CPGToAODConversion analyzeStringProcessingCase(const clang::FunctionDecl* func);
    std::vector<SIMDPatternMatch> findStringSIMDPatterns(const clang::FunctionDecl* func);
    std::string optimizeStringProcessing(const clang::FunctionDecl* func);
    
    // 案例2分析 - 位操作
    CPGToAODConversion analyzeBitwiseOperationsCase(const clang::FunctionDecl* func);
    std::vector<SIMDPatternMatch> findBitwiseSIMDPatterns(const clang::FunctionDecl* func);
    std::string optimizeBitwiseOperations(const clang::FunctionDecl* func);
    
    // 案例3分析 - UTF-8验证
    CPGToAODConversion analyzeUTF8ValidationCase(const clang::FunctionDecl* func);
    std::vector<SIMDPatternMatch> findUTF8SIMDPatterns(const clang::FunctionDecl* func);
    std::string optimizeUTF8Validation(const clang::FunctionDecl* func);
    
    // 通用复杂分析
    std::vector<std::string> findMultiFunctionPatterns();
    std::vector<std::string> identifyDataFlowDisruptions();
    std::vector<std::string> analyzeControlFlowComplexity();
    
    // 优化建议
    std::vector<std::string> generateCaseSpecificOptimizations(int case_id);
    std::string generateOptimizationForDataDisruptions();
    std::string generateOptimizationForComplexControlFlow();
};

// 性能分析器
class PerformanceAnalyzer {
private:
    IntegratedCPGAnalyzer& analyzer;
    
public:
    explicit PerformanceAnalyzer(IntegratedCPGAnalyzer& a) : analyzer(a) {}
    
    // 性能建模
    struct PerformanceModel {
        double execution_time = 0.0;
        double memory_bandwidth = 0.0;
        double cache_misses = 0.0;
        double branch_mispredictions = 0.0;
        int vector_operations = 0;
        int scalar_operations = 0;
        std::map<std::string, double> function_timings;
    };
    
    PerformanceModel buildPerformanceModel(const clang::FunctionDecl* func);
    double estimateExecutionTime(const PerformanceModel& model);
    double estimateMemoryBandwidth(const PerformanceModel& model);
    double estimateCachePerformance(const PerformanceModel& model);
    
    // SIMD性能分析
    double estimateSIMDSpeedup(const clang::FunctionDecl* func, const std::string& target_arch);
    std::string identifyPerformanceBottlenecks(const clang::FunctionDecl* func);
    std::vector<std::string> generatePerformanceRecommendations(const clang::FunctionDecl* func);
    
    // 架构特定分析
    PerformanceModel analyzeForAVX(const clang::FunctionDecl* func);
    PerformanceModel analyzeForNEON(const clang::FunctionDecl* func);
    PerformanceModel analyzeForSVE(const clang::FunctionDecl* func);
    
    // 报告
    std::string generatePerformanceComparison(const clang::FunctionDecl* func, 
                                            const std::vector<std::string>& architectures);
    void savePerformanceReport(const clang::FunctionDecl* func, const std::string& filename);
};

} // namespace aodsolve