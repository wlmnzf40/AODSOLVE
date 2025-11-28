#pragma once

#include "analysis/integrated_cpg_analyzer.h"
#include "aod/enhanced_aod_node.h"
#include "aod/enhanced_aod_graph.h"
#include "aod/optimization_rule_system.h"
#include <memory>
#include <map>
#include <set>
#include <vector>
#include <string>

namespace aodsolve {

struct InterproceduralDataFlow {
    std::map<const clang::CallExpr*, std::map<std::string, std::string>> argument_flows;
    std::map<const clang::CallExpr*, std::string> return_value_flows;
    std::map<const clang::FunctionDecl*, std::set<std::string>> side_effects;
    std::map<const clang::CallExpr*, std::vector<std::string>> affected_variables;
};

struct ConversionResult {
    bool successful = false;
    std::string error_message;
    std::vector<std::string> warnings;
    std::vector<std::string> info_messages;
    std::shared_ptr<AODGraph> aod_graph;
    std::map<const clang::Stmt*, int> stmt_to_node_map;
    std::map<const clang::Stmt*, std::shared_ptr<AODNode>> stmt_to_node_ptr_map; // 关键
    int converted_node_count = 0;
};

class EnhancedCPGToAODConverter {
private:
    clang::SourceManager& source_manager;
    IntegratedCPGAnalyzer* analyzer;
    std::map<const clang::Stmt*, std::shared_ptr<AODNode>> stmt_to_node_map;

public:
    explicit EnhancedCPGToAODConverter(clang::ASTContext& ctx, IntegratedCPGAnalyzer& a);

    ConversionResult convertWithOperators(
        const clang::FunctionDecl* func,
        const std::string& source_arch,
        const std::string& target_arch);

    void buildFullAODGraph(const clang::FunctionDecl* func, AODGraph& graph);
    const IntegratedCPGAnalyzer& getAnalyzer() const { return *analyzer; }

private:
    void traverseAndBuild(const clang::Stmt* stmt, AODGraph& graph, bool is_top_level = true);
    void traverseExpressionTree(const clang::Stmt* expr, AODGraph& graph);
    std::shared_ptr<AODNode> createAODNodeFromStmt(const clang::Stmt* stmt, bool is_stmt);
    std::shared_ptr<AODNode> createSIMDNode(const clang::Stmt* stmt);
    bool isSIMDIntrinsic(const clang::Stmt* stmt);
    bool isVectorizableScalarOp(const clang::Stmt* stmt);
    void connectDataFlow(const clang::FunctionDecl* func, AODGraph& graph);
    AODNodeType mapStmtToNodeType(const clang::Stmt* stmt);
    std::shared_ptr<AODNode> getOrCreateNode(const clang::Stmt* stmt); // 如果没用可以删，这里保留兼容
};

} // namespace aodsolve
