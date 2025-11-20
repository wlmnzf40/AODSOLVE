// CPGAnnotation.h
// 13.56 KB •381 lines
// •
// Formatting may be inconsistent from source
// CPGAnnotation_v2.h - 改进版本：支持ICFG、正确的PDG、可视化和扩展能力
#ifndef CPG_ANNOTATION_V2_H
#define CPG_ANNOTATION_V2_H

#include "clang/AST/ASTContext.h"
#include "clang/AST/Stmt.h"
#include "clang/AST/Decl.h"
#include "clang/Analysis/CFG.h"

#include <map>
#include <set>
#include <vector>
#include <memory>
#include <string>
#include <functional>

namespace cpg {

// ============================================
// 前置声明
// ============================================
class CPGContext;
class ICFGNode;
class PDGNode;

// ============================================
// ICFG节点类型
// ============================================
enum class ICFGNodeKind {
    Entry,           // 函数入口
    Exit,            // 函数出口
    Statement,       // 普通语句
    CallSite,        // 调用点
    ReturnSite,      // 返回点
    FormalIn,        // 形参入口
    FormalOut,       // 形参出口
    ActualIn,        // 实参入口
    ActualOut        // 实参出口
};

// ============================================
// ICFG边类型
// ============================================
enum class ICFGEdgeKind {
    Intraprocedural,  // 过程内边
    Call,             // 调用边
    Return,           // 返回边
    ParamIn,          // 参数传入边
    ParamOut,         // 参数传出边
    True,             // true分支
    False,            // false分支
    Unconditional     // 无条件边
};

// ============================================
// ICFG节点
// ============================================
class ICFGNode {
public:
    ICFGNodeKind kind;
    const clang::Stmt* stmt = nullptr;
    const clang::FunctionDecl* func = nullptr;
    const clang::CFGBlock* cfgBlock = nullptr;

    // 调用相关信息
    const clang::CallExpr* callExpr = nullptr;
    const clang::FunctionDecl* callee = nullptr;
    int paramIndex = -1;  // 对于参数节点

    std::vector<std::pair<ICFGNode*, ICFGEdgeKind>> successors;
    std::vector<std::pair<ICFGNode*, ICFGEdgeKind>> predecessors;

    explicit ICFGNode(ICFGNodeKind k) : kind(k) {}

    std::string getLabel() const;
    void dump(const clang::SourceManager* SM = nullptr) const;
};

// ============================================
// 数据依赖信息（改进版）
// ============================================
struct DataDependency {
    const clang::Stmt* sourceStmt;    // 定义语句
    const clang::Stmt* sinkStmt;      // 使用语句
    std::string varName;               // 变量名

    enum class DepKind {
        Flow,          // 流依赖 (RAW)
        Anti,          // 反依赖 (WAR)
        Output         // 输出依赖 (WAW)
    } kind;

    DataDependency(const clang::Stmt* src, const clang::Stmt* sink,
                   const std::string& var, DepKind k)
        : sourceStmt(src), sinkStmt(sink), varName(var), kind(k) {}
};

// ============================================
// 控制依赖信息（改进版）
// ============================================
struct ControlDependency {
    const clang::Stmt* controlStmt;    // 控制语句（条件）
    const clang::Stmt* dependentStmt;  // 被控制语句
    bool branchValue;                   // true/false 分支

    ControlDependency(const clang::Stmt* ctrl, const clang::Stmt* dep, bool val)
        : controlStmt(ctrl), dependentStmt(dep), branchValue(val) {}
};

// ============================================
// 程序依赖图节点
// ============================================
class PDGNode {
public:
    const clang::Stmt* stmt;
    const clang::FunctionDecl* func;

    // 数据依赖
    std::vector<DataDependency> dataDeps;

    // 控制依赖
    std::vector<ControlDependency> controlDeps;

    explicit PDGNode(const clang::Stmt* s, const clang::FunctionDecl* f = nullptr)
        : stmt(s), func(f) {}

    void addDataDep(const DataDependency& dep) { dataDeps.push_back(dep); }
    void addControlDep(const ControlDependency& dep) { controlDeps.push_back(dep); }

    void dump(const clang::SourceManager* SM = nullptr) const;
};

// ============================================
// 上下文信息（预留用于上下文敏感分析）
// ============================================
class CallContext {
public:
    std::vector<const clang::CallExpr*> callStack;  // 调用栈

    bool operator<(const CallContext& other) const {
        return callStack < other.callStack;
    }

    bool operator==(const CallContext& other) const {
        return callStack == other.callStack;
    }

    std::string toString() const;
};

// ============================================
// 路径条件（预留用于路径敏感分析）
// ============================================
class PathCondition {
public:
    std::vector<std::pair<const clang::Stmt*, bool>> conditions;  // 路径上的条件及其值

    void addCondition(const clang::Stmt* cond, bool value) {
        conditions.push_back({cond, value});
    }

    bool isFeasible() const;  // 预留：检查路径可行性
    std::string toString() const;
};

// ============================================
// Reaching Definitions 分析结果
// ============================================
struct ReachingDefsInfo {
    // 每个程序点的reaching definitions
    std::map<const clang::Stmt*, std::map<std::string, std::set<const clang::Stmt*>>> reachingDefs;

    // 每个语句定义的变量
    std::map<const clang::Stmt*, std::set<std::string>> definitions;

    // 每个语句使用的变量
    std::map<const clang::Stmt*, std::set<std::string>> uses;
};

// ============================================
// CPG上下文（改进版）
// ============================================
class CPGContext {
private:
    clang::ASTContext& astContext;

    // ICFG相关
    std::map<const clang::FunctionDecl*, std::vector<std::unique_ptr<ICFGNode>>> icfgNodes;
    std::map<const clang::Stmt*, ICFGNode*> stmtToICFGNode;
    std::map<const clang::FunctionDecl*, ICFGNode*> funcEntries;
    std::map<const clang::FunctionDecl*, ICFGNode*> funcExits;

    // PDG相关
    std::map<const clang::Stmt*, std::unique_ptr<PDGNode>> pdgNodes;

    // Reaching Definitions分析
    std::map<const clang::FunctionDecl*, ReachingDefsInfo> reachingDefsMap;

    // CFG缓存
    std::map<const clang::FunctionDecl*, std::unique_ptr<clang::CFG>> cfgCache;

    // 调用图
    std::map<const clang::FunctionDecl*, std::set<const clang::CallExpr*>> callSites;
    std::map<const clang::CallExpr*, const clang::FunctionDecl*> callTargets;

    // 预留：上下文敏感分析
    std::map<CallContext, std::unique_ptr<PDGNode>> contextSensitivePDG;

public:
    explicit CPGContext(clang::ASTContext& ctx);

    // ============================================
    // ICFG接口
    // ============================================
    ICFGNode* getICFGNode(const clang::Stmt* stmt) const;
    ICFGNode* getFunctionEntry(const clang::FunctionDecl* func) const;
    ICFGNode* getFunctionExit(const clang::FunctionDecl* func) const;

    std::vector<ICFGNode*> getSuccessors(ICFGNode* node) const;
    std::vector<ICFGNode*> getPredecessors(ICFGNode* node) const;

    std::vector<std::pair<ICFGNode*, ICFGEdgeKind>>
        getSuccessorsWithEdgeKind(ICFGNode* node) const;

    // ============================================
    // PDG接口
    // ============================================
    PDGNode* getPDGNode(const clang::Stmt* stmt) const;

    std::vector<DataDependency> getDataDependencies(const clang::Stmt* stmt) const;
    std::vector<ControlDependency> getControlDependencies(const clang::Stmt* stmt) const;

    // 获取定义某变量的所有语句
    std::set<const clang::Stmt*> getDefinitions(const clang::Stmt* useStmt,
                                                  const std::string& varName) const;

    // 获取使用某定义的所有语句
    std::set<const clang::Stmt*> getUses(const clang::Stmt* defStmt,
                                          const std::string& varName) const;

    // ============================================
    // 路径查询
    // ============================================
    bool hasDataFlowPath(const clang::Stmt* source, const clang::Stmt* sink,
                         const std::string& varName = "") const;

    bool hasControlFlowPath(const clang::Stmt* source, const clang::Stmt* sink) const;

    std::vector<std::vector<ICFGNode*>>
        findAllPaths(ICFGNode* source, ICFGNode* sink, int maxDepth = 100) const;

    // ============================================
    // 辅助功能
    // ============================================
    const clang::FunctionDecl* getContainingFunction(const clang::Stmt* stmt) const;
    const clang::CFG* getCFG(const clang::FunctionDecl* func) const;

    // ============================================
    // 可视化接口
    // ============================================
    void dumpICFG(const clang::FunctionDecl* func) const;
    void dumpPDG(const clang::FunctionDecl* func) const;
    void dumpCPG(const clang::FunctionDecl* func) const;

    void visualizeICFG(const clang::FunctionDecl* func, const std::string& outputPath = ".") const;
    void visualizePDG(const clang::FunctionDecl* func, const std::string& outputPath = ".") const;
    void visualizeCPG(const clang::FunctionDecl* func, const std::string& outputPath = ".") const;

    // 单个节点的dump
    void dumpNode(ICFGNode* node) const;
    void dumpNode(PDGNode* node) const;

    // ============================================
    // 统计信息
    // ============================================
    void printStatistics() const;

    // ============================================
    // 构建接口
    // ============================================
    void buildCPG(const clang::FunctionDecl* func);
    void buildICFGForTranslationUnit();  // 构建全局ICFG

    // ============================================
    // 预留：上下文敏感和路径敏感接口
    // ============================================

    // 获取特定上下文的PDG节点
    PDGNode* getPDGNodeInContext(const clang::Stmt* stmt,
                                  const CallContext& context) const;

    // 路径敏感的数据流分析
    std::vector<DataDependency>
        getDataDependenciesOnPath(const clang::Stmt* stmt,
                                  const PathCondition& path) const;

    // 上下文敏感的调用图遍历
    using CallGraphVisitor = std::function<void(const clang::FunctionDecl*, const CallContext&)>;
    void traverseCallGraphContextSensitive(const clang::FunctionDecl* entry,
                                           CallGraphVisitor visitor,
                                           int maxDepth = 10) const;


    // 在 "路径查询" 部分之前添加这个新的部分
    // ============================================
    // 新增：改进的数据流分析接口
    // ============================================

    // 从表达式中提取所有使用的变量名
    std::set<std::string> extractVariables(const clang::Expr* expr) const;

    // 追踪变量的定义链（向后追踪）- 过程内版本
    std::vector<const clang::Stmt*> traceVariableDefinitions(
        const clang::Expr* expr,
        int maxDepth = 10) const;

    // 新增：跨函数追踪变量的定义链
    std::vector<const clang::Stmt*> traceVariableDefinitionsInterprocedural(
        const clang::Expr* expr,
        int maxDepth = 10) const;

    // 获取包含某个表达式的顶层语句
    const clang::Stmt* getContainingStmt(const clang::Expr* expr) const;

    // 新增：获取调用点传入的实参表达式
    const clang::Expr* getArgumentAtCallSite(
        const clang::CallExpr* callExpr,
        unsigned paramIndex) const;

    // 新增：获取形参在被调函数中的使用
    std::vector<const clang::Stmt*> getParameterUsages(
        const clang::ParmVarDecl* param) const;

private:
    // ============================================
    // 内部构建方法
    // ============================================

    // ICFG构建
    void buildICFG(const clang::FunctionDecl* func);
    void buildCallGraph();
    void linkCallSites();

    ICFGNode* createICFGNode(ICFGNodeKind kind, const clang::FunctionDecl* func);
    void addICFGEdge(ICFGNode* from, ICFGNode* to, ICFGEdgeKind kind);

    // PDG构建
    void buildPDG(const clang::FunctionDecl* func);
    void computeReachingDefinitions(const clang::FunctionDecl* func);
    void computeDataDependencies(const clang::FunctionDecl* func);
    void computeControlDependencies(const clang::FunctionDecl* func);
    void computePostDominators(const clang::FunctionDecl* func,
                               std::map<const clang::CFGBlock*,
                                       std::set<const clang::CFGBlock*>>& postDom);

    // 可视化辅助
    std::string getStmtSource(const clang::Stmt* stmt) const;
    std::string escapeForDot(const std::string& str) const;
    void exportICFGDotFile(const clang::FunctionDecl* func, const std::string& filename) const;
    void exportPDGDotFile(const clang::FunctionDecl* func, const std::string& filename) const;
    void exportCPGDotFile(const clang::FunctionDecl* func, const std::string& filename) const;

    // 辅助函数
    std::set<std::string> getUsedVars(const clang::Stmt* stmt) const;
    std::set<std::string> getDefinedVars(const clang::Stmt* stmt) const;

    friend class CPGBuilder;
};

// ============================================
// CPG构建器
// ============================================
class CPGBuilder {
public:
    static void buildForTranslationUnit(clang::ASTContext& astCtx, CPGContext& cpgCtx);
    static void buildForFunction(const clang::FunctionDecl* func, CPGContext& cpgCtx);
};

} // namespace cpg

// ============================================
// 计算图模式定义（用于跨架构SIMD转换）
// ============================================

namespace aodsolve {

// 前向声明
struct SIMDPatternMatch;

// 计算图节点类型
enum class ComputeNodeType {
    LOAD,           // 内存加载
    STORE,          // 内存存储
    CONSTANT,       // 常量设置
    COMPARE,        // 比较操作
    LOGICAL,        // 逻辑操作（与、或、非）
    ARITHMETIC,     // 算术操作（加、减、乘）
    COMPOSITE       // 复合操作（多个基础操作组合）
};

// 计算图节点（表示一个SIMD操作）
struct ComputeGraphNode {
    ComputeNodeType type;
    std::string operation;                   // 具体操作名（如 "cmpgt", "and", "add"）
    std::vector<int> input_node_ids;         // 输入节点ID
    int node_id;                             // 当前节点ID
    std::string data_type;                   // 数据类型（如 "uint8", "int8", "float32"）
    int vector_width;                        // 向量宽度（如256, 512）

    // 关联的CPG和AST节点（用于映射回AST）
    const clang::Stmt* ast_stmt = nullptr;
    cpg::ICFGNode* icfg_node = nullptr;
    cpg::PDGNode* pdg_node = nullptr;

    // 变量名（如果有）
    std::string result_var;                  // 结果变量名
    std::vector<std::string> input_vars;     // 输入变量名

    // 属性
    std::map<std::string, std::string> attributes;

    ComputeGraphNode() : type(ComputeNodeType::COMPOSITE), node_id(-1), vector_width(0) {}
};

// 计算图模式（表示一个算子或操作序列）
struct ComputeGraphPattern {
    std::string name;                        // 模式名称（如 "range_check"）
    std::string description;                 // 描述
    std::vector<ComputeGraphNode> nodes;     // 节点序列

    // 节点间的数据流连接（from_node_id -> to_node_id）
    std::map<int, std::vector<int>> data_flow_edges;

    // 节点间的控制流连接
    std::map<int, std::vector<int>> control_flow_edges;

    // AST子树根节点列表（用于映射回AST）
    std::vector<const clang::Stmt*> ast_subtree_roots;

    // 关联的CPG子图
    std::vector<cpg::ICFGNode*> icfg_subgraph;
    std::vector<cpg::PDGNode*> pdg_subgraph;

    // 源架构和目标架构
    std::string source_arch;                 // 如 "AVX2", "SSE4.2"
    std::string target_arch;                 // 如 "SVE", "NEON"

    // 指令映射
    std::vector<std::string> source_instructions;  // 源指令序列
    std::vector<std::string> target_instructions;  // 目标指令序列

    // 优化信息
    bool is_optimizable = false;
    std::string optimization_type;           // 如 "merge_operations", "instruction_fusion"
    int instruction_reduction = 0;           // 指令数减少量

    ComputeGraphPattern() {}
};

// SIMD算子（高层次的操作单元，可以是一个或多个计算图模式）
struct SIMDOperator {
    std::string name;                        // 算子名称（如 "uppercase_to_lowercase"）
    std::string semantic_description;        // 语义描述

    // 底层计算图模式
    ComputeGraphPattern compute_pattern;

    // 算子的性能特征
    int estimated_cycles_source;             // 源架构估计周期数
    int estimated_cycles_target;             // 目标架构估计周期数
    int memory_accesses;                     // 内存访问次数
    double estimated_speedup;                // 估计加速比

    // 关联的SIMD模式匹配（兼容现有的AST pattern系统）
    SIMDPatternMatch* simd_pattern = nullptr;

    // 代码生成信息
    std::string generated_source_code;       // 生成的源架构代码
    std::string generated_target_code;       // 生成的目标架构代码

    SIMDOperator() : estimated_cycles_source(0), estimated_cycles_target(0),
                     memory_accesses(0), estimated_speedup(1.0) {}
};

// 计算图模式匹配器（用于识别代码中的算子模式）
class ComputeGraphPatternMatcher {
public:
    // 从CPG提取计算图
    static std::vector<ComputeGraphNode> extractComputeGraph(
        const clang::FunctionDecl* func,
        cpg::CPGContext& cpg_context);

    // 在计算图中识别模式
    static std::vector<SIMDOperator> matchPatterns(
        const std::vector<ComputeGraphNode>& compute_graph,
        const std::vector<ComputeGraphPattern>& pattern_library);

    // 将计算图模式映射回AST子树
    static std::vector<const clang::Stmt*> mapToASTSubtree(
        const ComputeGraphPattern& pattern,
        const clang::ASTContext& ast_context);
};

} // namespace aodsolve

#endif // CPG_ANNOTATION_V2_H