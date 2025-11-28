#pragma once

#include "enhanced_aod_node.h"
#include <vector>
#include <map>
#include <set>
#include <string>
#include <memory>
#include <iostream>

namespace aodsolve {

enum class AODEdgeType {
    Data,       // 数据依赖
    Control,    // 控制依赖
    Memory,     // 内存依赖
    Vector      // 向量化依赖
};

struct AODEdgeProperties {
    std::string variable_name; // 传递的变量名或参数索引 (如 "arg_0", "init")
    int weight = 1;
};

class AODEdge {
private:
    std::shared_ptr<AODNode> source;
    std::shared_ptr<AODNode> target;
    AODEdgeType type;
    AODEdgeProperties properties;

public:
    AODEdge(std::shared_ptr<AODNode> src, std::shared_ptr<AODNode> tgt, AODEdgeType edge_type);

    std::shared_ptr<AODNode> getSource() const { return source; }
    std::shared_ptr<AODNode> getTarget() const { return target; }
    AODEdgeType getType() const { return type; }

    void setVariableName(const std::string& name) { properties.variable_name = name; }
    const AODEdgeProperties& getProperties() const { return properties; }

    std::string toString() const;
    std::string getDOTLabel() const;
};

class AODGraph {
public:
    struct GraphStatistics {
        int node_count = 0;
        int edge_count = 0;
        int simd_nodes = 0;
        int control_nodes = 0;
        int data_nodes = 0;
        int call_nodes = 0;
        int complexity_score = 0;
    };

private:
    std::string name;
    std::vector<std::shared_ptr<AODNode>> nodes;
    std::vector<std::shared_ptr<AODEdge>> edges;

    // 索引
    std::map<int, std::shared_ptr<AODNode>> node_map;
    std::map<std::string, std::shared_ptr<AODNode>> nodes_by_name;

    // 分析缓存
    std::map<std::string, std::set<std::shared_ptr<AODNode>>> variable_defs_map;
    std::map<std::string, std::set<std::shared_ptr<AODNode>>> variable_uses_map;

public:
    explicit AODGraph(const std::string& graph_name);
    ~AODGraph() = default;

    // 基本操作
    void addNode(std::shared_ptr<AODNode> node);
    bool removeNode(int node_id);

    std::shared_ptr<AODNode> getNode(int node_id) const;
    std::shared_ptr<AODNode> getNodeByName(const std::string& name) const;
    const std::vector<std::shared_ptr<AODNode>>& getNodes() const { return nodes; }

    void addEdge(std::shared_ptr<AODNode> source, std::shared_ptr<AODNode> target, AODEdgeType type, const std::string& variable);
    void addEdge(std::shared_ptr<AODNode> source, std::shared_ptr<AODNode> target, AODEdgeType type);
    bool removeEdge(int source_id, int target_id);

    std::vector<std::shared_ptr<AODEdge>> getEdgesFrom(int node_id) const;
    std::vector<std::shared_ptr<AODEdge>> getEdgesTo(int node_id) const;
    std::vector<std::shared_ptr<AODEdge>> getIncomingEdges(int node_id) const { return getEdgesTo(node_id); }

    // 分析功能 (保留接口以满足链接)
    void topologicalSort() const;
    std::vector<int> getPath(int start_id, int end_id) const;
    bool hasPath(int start_id, int end_id) const;

    void computeVariableDefinitions();
    void computeVariableUses();
    std::set<std::string> getVariablesAtNode(int node_id) const;
    std::set<std::shared_ptr<AODNode>> getDefinitionsOf(const std::string& variable) const;
    std::set<std::shared_ptr<AODNode>> getUsesOf(const std::string& variable) const;

    void computeDominators();
    std::vector<int> getImmediateDominators() const;
    std::set<int> getDominators(int node_id) const;
    bool isDominatedBy(int dominated, int dominator) const;
    bool isCyclic() const;

    std::vector<int> getEntryNodes() const;
    std::vector<int> getExitNodes() const;

    void eliminateDeadCode();
    void constantPropagation();
    void commonSubexpressionElimination();

    bool isValid() const;
    std::vector<std::string> getValidationErrors() const;
    void validateCycles() const;
    void validateNoOrphanedNodes() const;

    // ==========================================
    // 可视化接口 (新增)
    // ==========================================
    std::string toDOT() const;
    void saveToDotFile(const std::string& filename) const;

    // 统计
    GraphStatistics getStatistics() const;
    void printStatistics() const;

    int getNodeCount() const { return nodes.size(); }
};

} // namespace aodsolve
