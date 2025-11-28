#pragma once

#include <string>
#include <vector>
#include <memory>
#include <map>
#include <set>
#include <functional>
#include <optional>
#include <atomic>

#include "aod/enhanced_aod_graph.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/AST/Stmt.h"
#include "clang/AST/Decl.h"
#include "clang/Analysis/CFG.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Frontend/CompilerInstance.h"

namespace aodsolve {

// ============================================
// ASTåˆ†æžç»“æžœ
// ============================================

struct ASTAnalysisResult {
    std::string function_name;
    bool is_vectorizable = false;
    std::vector<std::string> vectorizable_patterns;
    std::map<std::string, std::vector<int>> variable_lifetimes;
    std::vector<std::string> control_dependencies;
    std::vector<std::string> data_dependencies;
    int complexity_score = 0;
    std::string simd_instruction_set;
    std::vector<std::string> optimization_suggestions;
    double estimated_speedup = 1.0;

    // è·¨å‡½æ•°åˆ†æžç»“æžœ
    std::vector<std::string> called_functions;
    std::map<std::string, std::string> parameter_mapping;
    std::vector<std::string> side_effects;
    bool is_pure_function = true;
};

// SIMDæ¨¡å¼åŒ¹é…ç»“æžœ
struct SIMDPatternMatch {
    std::string pattern_type;
    std::vector<std::string> matched_nodes;
    std::string source_instruction_set;
    std::string target_instruction_set;
    double confidence_score = 0.0;
    std::string replacement_code;
    int performance_benefit = 0; // ä¼°è®¡çš„åŠ é€Ÿæ¯”
    std::vector<std::string> dependencies;
};

// æ•°æ®æµåˆ†æžç»“æžœ
struct DataFlowAnalysis {
    std::map<std::string, std::set<std::string>> reaching_definitions;
    std::map<std::string, std::set<std::string>> live_variables;
    std::map<std::string, std::set<std::string>> available_expressions;
    std::map<std::string, std::set<std::string>> very_busy_expressions;
    std::map<std::string, std::set<std::string>> antidependencies;
    std::map<std::string, std::set<std::string>> output_dependencies;
};

// æŽ§åˆ¶æµåˆ†æžç»“æžœ
struct ControlFlowAnalysis {
    std::vector<int> loop_headers;
    std::vector<std::vector<int>> loops;
    std::set<std::string> loop_invariants;
    std::vector<std::string> break_statements;
    std::vector<std::string> continue_statements;
    std::vector<std::string> return_statements;
    bool has_exception_handling = false;
    int cyclomatic_complexity = 0;
};

// å¢žå¼ºçš„ASTåˆ†æžå™¨
class EnhancedASTAnalyzer {
private:
    // åˆ†æžç»“æžœç¼“å­˜
    std::map<const clang::FunctionDecl*, ASTAnalysisResult> function_analysis_cache;
    std::map<const clang::Stmt*, DataFlowAnalysis> dataflow_cache;
    std::map<const clang::FunctionDecl*, ControlFlowAnalysis> controlflow_cache;

    // SIMDæ¨¡å¼åŒ¹é…å™¨
    std::map<std::string, std::function<bool(const clang::Stmt*)>> simd_pattern_matchers;
    std::vector<SIMDPatternMatch> last_pattern_matches;

public:
    clang::ASTContext& ast_context;
    clang::SourceManager& source_manager;
    std::atomic<int> analysis_counter{0};
    explicit EnhancedASTAnalyzer(clang::ASTContext& ctx);
    ~EnhancedASTAnalyzer() = default;

    // ä¸»è¦åˆ†æžæŽ¥å£
    ASTAnalysisResult analyzeFunction(const clang::FunctionDecl* func);
    ASTAnalysisResult analyzeCallSite(const clang::CallExpr* call);
    std::vector<ASTAnalysisResult> analyzeTranslationUnit();

    // SIMDåˆ†æž
    bool canVectorize(const clang::FunctionDecl* func);
    std::vector<SIMDPatternMatch> findSIMDPatterns(const clang::FunctionDecl* func);
    std::vector<SIMDPatternMatch> findSIMDPatternsInStmt(const clang::Stmt* stmt);

    // æ•°æ®æµåˆ†æž
    DataFlowAnalysis analyzeDataFlow(const clang::FunctionDecl* func);
    DataFlowAnalysis analyzeDataFlow(const clang::Stmt* stmt);
    std::vector<std::string> getVariablesAtStatement(const clang::Stmt* stmt);
    std::set<std::string> getReachingDefinitions(const clang::Stmt* stmt, const std::string& var);

    // æŽ§åˆ¶æµåˆ†æž
    ControlFlowAnalysis analyzeControlFlow(const clang::FunctionDecl* func);
    std::vector<int> findLoops(const clang::FunctionDecl* func);
    bool isLoopInvariant(const clang::Stmt* stmt, int loop_id);
    std::vector<std::string> identifyLoopCarriedDependencies(int loop_id);

    // è·¨å‡½æ•°åˆ†æž
    std::vector<ASTAnalysisResult> analyzeCallGraph(const clang::FunctionDecl* root);
    std::map<std::string, std::string> analyzeParameterFlow(const clang::CallExpr* call);
    std::vector<std::string> traceVariableAcrossFunctions(const std::string& var, const clang::CallExpr* call);

    // æž¶æž„ç‰¹å®šåˆ†æž
    std::string identifyInstructionSet(const clang::FunctionDecl* func);
    std::vector<std::string> detectAVXPatterns(const clang::FunctionDecl* func);
    std::vector<std::string> detectNEONPatterns(const clang::FunctionDecl* func);
    std::vector<std::string> detectSVEPatterns(const clang::FunctionDecl* func);

    // ä¼˜åŒ–å»ºè®®
    std::vector<std::string> generateOptimizationSuggestions(const clang::FunctionDecl* func);
    std::vector<std::string> suggestLoopOptimizations(int loop_id);
    std::vector<std::string> suggestMemoryOptimizations(const clang::FunctionDecl* func);

    // é”™è¯¯å’Œè­¦å‘Š
    std::vector<std::string> findVectorizationBarriers(const clang::FunctionDecl* func);
    std::vector<std::string> findDataHazards(const clang::FunctionDecl* func);
    std::vector<std::string> findControlHazards(const clang::FunctionDecl* func);

    // æ€§èƒ½åˆ†æž
    int estimateComplexity(const clang::FunctionDecl* func);
    double estimateSpeedupPotential(const clang::FunctionDecl* func);
    std::string generatePerformanceReport(const clang::FunctionDecl* func);

    // å¯è§†åŒ–å’ŒæŠ¥å‘Š
    std::string generateASTVisualization(const clang::FunctionDecl* func);
    std::string generateControlFlowGraph(const clang::FunctionDecl* func);
    std::string generateDataFlowGraph(const clang::FunctionDecl* func);

    // ç¼“å­˜ç®¡ç†
    void clearCache();
    void invalidateCache(const clang::FunctionDecl* func);
    void setCacheSize(size_t max_size);

    // é«˜çº§åˆ†æžåŠŸèƒ½
    std::vector<std::string> findParallelizationOpportunities(const clang::FunctionDecl* func);
    std::vector<std::string> findReductionPatterns(const clang::FunctionDecl* func);
    std::vector<std::string> findMemoryAccessPatterns(const clang::FunctionDecl* func);

    // å¼‚å¸¸å¤„ç†åˆ†æž
    bool hasExceptionHandling(const clang::FunctionDecl* func);
    std::vector<std::string> analyzeExceptionPaths(const clang::FunctionDecl* func);
    bool isExceptionSafe(const clang::FunctionDecl* func);

    // å†…è”åˆ†æž
    bool shouldInline(const clang::CallExpr* call);
    double estimateInlineBenefit(const clang::CallExpr* call);
    std::vector<std::string> getInlineCandidates(const clang::FunctionDecl* func);

    // æ­»ä»£ç æ¶ˆé™¤
    std::vector<std::string> findDeadCode(const clang::FunctionDecl* func);
    std::vector<std::string> findDeadVariables(const clang::FunctionDecl* func);
    std::vector<std::string> findDeadFunctions();

    // å¸¸é‡ä¼ æ’­
    std::map<std::string, std::string> findConstants(const clang::FunctionDecl* func);
    bool isConstantExpression(const clang::Expr* expr);

    // å¾ªçŽ¯åˆ†æž
    bool isPerfectLoopNest(const std::vector<int>& loops);
    std::vector<int> analyzeLoopBounds(const clang::Stmt* loop_stmt);
    std::string analyzeLoopTripCount(const clang::Stmt* loop_stmt);

    // å†…å­˜åˆ†æž
    std::vector<std::string> analyzeMemoryAliases(const clang::FunctionDecl* func);
    std::vector<std::string> findMemoryLeaks(const clang::FunctionDecl* func);
    bool hasStackOverflowRisk(const clang::FunctionDecl* func);

    // å·¥å…·æ–¹æ³•
    std::string getStatementSource(const clang::Stmt* stmt) const;
    std::string getDeclSource(const clang::Decl* decl) const;
    bool isInSourceCode(const clang::SourceLocation& loc) const;
    std::string getSourceFile(const clang::Stmt* stmt) const;
    int getSourceLine(const clang::Stmt* stmt) const;
    int getSourceColumn(const clang::Stmt* stmt) const;

private:
    // å†…éƒ¨åˆ†æžå®žçŽ°
    void initializePatternMatchers();
    bool matchAVXLoadStore(const clang::Stmt* stmt) const;
    bool matchAVXArithmetic(const clang::Stmt* stmt) const;
    bool matchAVXCompare(const clang::Stmt* stmt) const;
    bool matchAVXBlend(const clang::Stmt* stmt) const;
    bool matchAVXShuffle(const clang::Stmt* stmt) const;

    bool matchNEONLoadStore(const clang::Stmt* stmt) const;
    bool matchNEONArithmetic(const clang::Stmt* stmt) const;
    bool matchSVELoadStore(const clang::Stmt* stmt) const;
    bool matchSVEArithmetic(const clang::Stmt* stmt) const;

    // CFGç›¸å…³
    std::unique_ptr<clang::CFG> buildCFG(const clang::FunctionDecl* func) const;
    void analyzeCFGBlocks(const clang::CFG* cfg);
    void analyzeBasicBlock(const clang::CFGBlock* block);

    // å˜é‡è¿½è¸ª
    std::set<std::string> getVariablesInExpression(const clang::Expr* expr) const;
    std::set<std::string> getVariablesInStatement(const clang::Stmt* stmt) const;
    void collectVariableUses(const clang::Stmt* stmt, std::set<std::string>& uses) const;
    void collectVariableDefs(const clang::Stmt* stmt, std::set<std::string>& defs) const;

    // è°ƒç”¨å›¾åˆ†æž
    void buildCallGraph(std::map<const clang::FunctionDecl*, std::set<const clang::CallExpr*>>& call_graph) const;
    std::set<const clang::FunctionDecl*> getCallees(const clang::CallExpr* call) const;
    std::set<const clang::CallExpr*> getCallers(const clang::FunctionDecl* func) const;

    // æ€§èƒ½è¯„ä¼°
    int estimateInstructionCount(const clang::FunctionDecl* func) const;
    int estimateMemoryAccessCount(const clang::FunctionDecl* func) const;
    int estimateBranchCount(const clang::FunctionDecl* func) const;
    double estimateCacheMissPenalty(const clang::FunctionDecl* func) const;

    // è¾…åŠ©æ–¹æ³•
    bool isSIMDIntrinsic(const clang::CallExpr* call) const;
    std::string extractSIMDType(const clang::CallExpr* call) const;
    int getVectorWidth(const clang::CallExpr* call) const;
    std::string getSIMDOperation(const clang::CallExpr* call) const;
};

// ============================================
// 主文件过滤访问器 - 只访问主文件中的节点
// ============================================

// 模板基类:自动过滤头文件内容,只遍历主文件中的AST节点
template<typename Derived>
class MainFileOnlyASTVisitor : public clang::RecursiveASTVisitor<Derived> {
protected:
    clang::SourceManager& source_manager_;

public:
    explicit MainFileOnlyASTVisitor(clang::SourceManager& sm)
        : source_manager_(sm) {}

    // 不访问隐式生成的代码
    bool shouldVisitImplicitCode() const { return false; }

    // 重写 TraverseDecl: 只遍历主文件中的声明
    bool TraverseDecl(clang::Decl* decl) {
        if (!decl) return true;

        // 跳过非主文件的声明(头文件内容)
        if (!source_manager_.isInMainFile(decl->getLocation())) {
            return true;
        }

        return clang::RecursiveASTVisitor<Derived>::TraverseDecl(decl);
    }

    // 重写 TraverseStmt: 只遍历主文件中的语句
    bool TraverseStmt(clang::Stmt* stmt) {
        if (!stmt) return true;

        // 跳过非主文件的语句
        if (!source_manager_.isInMainFile(stmt->getBeginLoc())) {
            return true;
        }

        return clang::RecursiveASTVisitor<Derived>::TraverseStmt(stmt);
    }

    // 检查节点是否在主文件中
    bool isInMainFile(const clang::Decl* decl) const {
        return decl && source_manager_.isInMainFile(decl->getLocation());
    }

    bool isInMainFile(const clang::Stmt* stmt) const {
        return stmt && source_manager_.isInMainFile(stmt->getBeginLoc());
    }
};

// ASTè®¿é—®è€…åŸºç±» - 继承MainFileOnlyASTVisitor以自动过滤头文件内容
class ASTAnalysisVisitor : public MainFileOnlyASTVisitor<ASTAnalysisVisitor> {
protected:
    EnhancedASTAnalyzer& analyzer;
    const clang::FunctionDecl* current_function;
    std::map<std::string, std::set<std::string>>& variable_info;

public:
    ASTAnalysisVisitor(EnhancedASTAnalyzer& a, const clang::FunctionDecl* func,
                      std::map<std::string, std::set<std::string>>& var_info)
        : MainFileOnlyASTVisitor(a.ast_context.getSourceManager()),
          analyzer(a), current_function(func), variable_info(var_info) {}

    // é‡è½½è™šå‡½æ•°ä»¥å®žçŽ°è‡ªå®šä¹‰åˆ†æž
    bool VisitFunctionDecl(clang::FunctionDecl* decl);
    bool VisitCallExpr(clang::CallExpr* expr);
    bool VisitBinaryOperator(clang::BinaryOperator* op);
    bool VisitUnaryOperator(clang::UnaryOperator* op);
    bool VisitForStmt(clang::ForStmt* for_stmt);
    bool VisitWhileStmt(clang::WhileStmt* while_stmt);
    bool VisitDoStmt(clang::DoStmt* do_stmt);
    bool VisitIfStmt(clang::IfStmt* if_stmt);
    bool VisitSwitchStmt(clang::SwitchStmt* switch_stmt);
    bool VisitReturnStmt(clang::ReturnStmt* return_stmt);
    bool VisitDeclRefExpr(clang::DeclRefExpr* expr);
    bool VisitArraySubscriptExpr(clang::ArraySubscriptExpr* expr);
    bool VisitMemberExpr(clang::MemberExpr* expr);

private:
    void analyzeAssignment(clang::BinaryOperator* op);
    void analyzeFunctionCall(clang::CallExpr* call);
    void analyzeLoop(clang::Stmt* loop_stmt);
    void analyzeControlFlow(clang::Stmt* stmt);
    void collectVariableUse(const clang::Expr* expr);
};

// å‘é‡åŒ–åˆ†æžä¸“é—¨åŒ–ç±»
class VectorizationAnalyzer {
private:
    EnhancedASTAnalyzer& base_analyzer;
    std::map<const clang::ForStmt*, std::string> loop_analyzers;

public:
    explicit VectorizationAnalyzer(EnhancedASTAnalyzer& a) : base_analyzer(a) {}

    // å¾ªçŽ¯å‘é‡åŒ–
    bool canVectorizeLoop(const clang::ForStmt* for_stmt);
    std::vector<std::string> identifyLoopCarriedDependencies(const clang::ForStmt* for_stmt);
    std::string analyzeLoopBounds(const clang::ForStmt* for_stmt);
    int estimateLoopTripCount(const clang::ForStmt* for_stmt);

    // å†…å­˜è®¿é—®åˆ†æž
    std::vector<std::string> analyzeMemoryAccessPattern(const clang::ForStmt* for_stmt);
    bool hasRegularMemoryAccess(const clang::ForStmt* for_stmt);
    int estimateMemoryAlignment(const clang::ForStmt* for_stmt);

    // æ•°æ®ä¾èµ–åˆ†æž
    std::vector<std::string> findReadAfterWriteDependencies(const clang::ForStmt* for_stmt);
    std::vector<std::string> findWriteAfterReadDependencies(const clang::ForStmt* for_stmt);
    std::vector<std::string> findWriteAfterWriteDependencies(const clang::ForStmt* for_stmt);

    // å‘é‡åŒ–å»ºè®®
    std::vector<std::string> generateVectorizationSuggestions(const clang::ForStmt* for_stmt);
    std::vector<std::string> generateLoopTransformations(const clang::ForStmt* for_stmt);
    std::string generatePragmaHints(const clang::ForStmt* for_stmt);
};

} // namespace aodsolve
