#include "aod/enhanced_aod_graph.h"
#include <algorithm>
#include <stdexcept>
#include <queue>
#include <stack>
#include <sstream>
#include <iostream>
#include <fstream>
#include <iomanip>

namespace aodsolve {

// Edge Implementation
AODEdge::AODEdge(std::shared_ptr<AODNode> src, std::shared_ptr<AODNode> tgt, AODEdgeType edge_type)
    : source(src), target(tgt), type(edge_type) {
    if (!src || !tgt) throw std::invalid_argument("Source or target node cannot be null");
}

std::string AODEdge::toString() const { return "Edge"; }
std::string AODEdge::getDOTLabel() const { return ""; }

// Graph Implementation
AODGraph::AODGraph(const std::string& graph_name) : name(graph_name) {}

void AODGraph::addNode(std::shared_ptr<AODNode> node) {
    if (!node) return;
    nodes.push_back(node);
    node_map[node->getId()] = node;
    // 如果有重名，可能会覆盖，但在当前逻辑中 ID 是唯一的
    nodes_by_name[node->getName()] = node;
}

bool AODGraph::removeNode(int /*node_id*/) { return false; }

std::shared_ptr<AODNode> AODGraph::getNode(int node_id) const {
    auto it = node_map.find(node_id);
    return (it != node_map.end()) ? it->second : nullptr;
}

std::shared_ptr<AODNode> AODGraph::getNodeByName(const std::string& name) const {
    auto it = nodes_by_name.find(name);
    return (it != nodes_by_name.end()) ? it->second : nullptr;
}

void AODGraph::addEdge(std::shared_ptr<AODNode> source, std::shared_ptr<AODNode> target, AODEdgeType type, const std::string& variable) {
    if (!source || !target) return;
    auto edge = std::make_shared<AODEdge>(source, target, type);
    edge->setVariableName(variable);
    edges.push_back(edge);
}

void AODGraph::addEdge(std::shared_ptr<AODNode> source, std::shared_ptr<AODNode> target, AODEdgeType type) {
    addEdge(source, target, type, "");
}

bool AODGraph::removeEdge(int /*source_id*/, int /*target_id*/) { return false; }

std::vector<std::shared_ptr<AODEdge>> AODGraph::getEdgesFrom(int node_id) const {
    std::vector<std::shared_ptr<AODEdge>> result;
    for (const auto& edge : edges) {
        if (edge->getSource()->getId() == node_id) result.push_back(edge);
    }
    return result;
}

std::vector<std::shared_ptr<AODEdge>> AODGraph::getEdgesTo(int node_id) const {
    std::vector<std::shared_ptr<AODEdge>> result;
    for (const auto& edge : edges) {
        if (edge->getTarget()->getId() == node_id) result.push_back(edge);
    }
    return result;
}

// ==========================================
// 可视化核心实现
// ==========================================

// 辅助函数：转义 DOT 标签中的特殊字符
static std::string escapeLabel(const std::string& label) {
    std::string escaped;
    for (char c : label) {
        if (c == '"') escaped += "\\\"";
        else if (c == '\\') escaped += "\\\\";
        else if (c == '\n') escaped += "\\n";
        else escaped += c;
    }
    return escaped;
}

std::string AODGraph::toDOT() const {
    std::stringstream ss;

    // 1. 图头定义
    ss << "digraph \"" << name << "\" {\n";
    ss << "  rankdir=TB;\n"; // 布局方向：从上到下
    ss << "  node [fontname=\"Helvetica\", fontsize=10, shape=box, style=filled];\n";
    ss << "  edge [fontname=\"Helvetica\", fontsize=9];\n\n";

    // 2. 遍历节点生成定义
    for (const auto& node : nodes) {
        std::string color = "#FFFFFF"; // 默认白色
        std::string shape = "box";
        std::string fontColor = "black";
        std::string label;

        // 根据节点类型设置样式
        switch (node->getType()) {
            case AODNodeType::SIMD_Intrinsic:
                color = "#E6F3FF"; // 浅蓝：SIMD 算子
                shape = "component";
                if (node->getProperty("op_name") == "define") {
                    color = "#E6FFFA"; // 浅绿：变量定义
                    shape = "note";
                    label = node->getProperty("var_type") + " " + node->getProperty("var_name") + "\n(Define)";
                } else {
                    label = node->getProperty("op_name");
                }
                break;

            case AODNodeType::GenericStmt:
                color = "#FFF0F5"; // 浅红：普通标量语句
                if (node->getName().find("DeclStmt") != std::string::npos) {
                     label = "Decl: " + node->getProperty("var_name");
                } else {
                     label = node->getProperty("op_name").empty() ? node->getName() : node->getProperty("op_name");
                }
                // 如果被标记为向量化候选
                if (node->getProperty("vectorize") == "true") {
                    color = "#FFFACD"; // 柠檬黄：待向量化标量
                    label += "\n[AutoVec]";
                    shape = "cds";
                }
                break;

            case AODNodeType::Control:
                color = "#FFE4C4"; // 杏仁色：控制流
                shape = "diamond";
                label = node->getName();
                // 标记循环向量化
                if (node->getProperty("vectorize") == "true") {
                    label += "\n[VecLoop]";
                    color = "#FFD700"; // 金色
                }
                break;

            case AODNodeType::BlockEnd:
                color = "#D3D3D3"; // 灰色：块结束
                shape = "point";
                label = "";
                break;

            default:
                color = "#F5F5F5";
                label = node->getName();
                break;
        }

        // 构造节点属性
        ss << "  node_" << node->getId() << " ["
           << "label=\"" << escapeLabel(label) << "\", "
           << "fillcolor=\"" << color << "\", "
           << "fontcolor=\"" << fontColor << "\", "
           << "shape=\"" << shape << "\""
           << "];\n";
    }

    ss << "\n";

    // 3. 遍历边生成连接
    for (const auto& edge : edges) {
        auto src = edge->getSource();
        auto dst = edge->getTarget();

        std::string edgeLabel = edge->getProperties().variable_name;
        std::string edgeStyle = "solid";
        std::string edgeColor = "#666666";
        std::string arrowHead = "normal";

        // 特殊边样式
        if (edgeLabel == "init") {
            edgeColor = "blue";
            edgeStyle = "dashed";
            arrowHead = "dot";
        } else if (edgeLabel.find("arg_") == 0) {
            edgeColor = "#333333";
            // 简化标签，只显示参数索引
            edgeLabel = edgeLabel.substr(4);
        }

        ss << "  node_" << src->getId() << " -> node_" << dst->getId()
           << " [label=\"" << escapeLabel(edgeLabel) << "\", "
           << "color=\"" << edgeColor << "\", "
           << "style=\"" << edgeStyle << "\", "
           << "arrowhead=\"" << arrowHead << "\""
           << "];\n";
    }

    ss << "}\n";
    return ss.str();
}

void AODGraph::saveToDotFile(const std::string& filename) const {
    std::ofstream outFile(filename);
    if (outFile.is_open()) {
        outFile << toDOT();
        outFile.close();
    } else {
        std::cerr << "Error: Unable to save DOT file to " << filename << std::endl;
    }
}

// ==========================================
// 空实现 (满足链接需求)
// ==========================================

void AODGraph::topologicalSort() const {}
std::vector<int> AODGraph::getPath(int, int) const { return {}; }
bool AODGraph::hasPath(int, int) const { return false; }

void AODGraph::computeVariableDefinitions() {
    variable_defs_map.clear();
    for (const auto& node : nodes) {
        auto defs = node->getDefinedVariables();
        for (const auto& var : defs) variable_defs_map[var].insert(node);
    }
}

void AODGraph::computeVariableUses() {
    variable_uses_map.clear();
    for (const auto& node : nodes) {
        auto uses = node->getUsedVariables();
        for (const auto& var : uses) variable_uses_map[var].insert(node);
    }
}

std::set<std::string> AODGraph::getVariablesAtNode(int) const { return {}; }
std::set<std::shared_ptr<AODNode>> AODGraph::getDefinitionsOf(const std::string&) const { return {}; }
std::set<std::shared_ptr<AODNode>> AODGraph::getUsesOf(const std::string&) const { return {}; }

void AODGraph::computeDominators() {}
std::vector<int> AODGraph::getImmediateDominators() const { return {}; }
std::set<int> AODGraph::getDominators(int) const { return {}; }
bool AODGraph::isDominatedBy(int, int) const { return false; }
bool AODGraph::isCyclic() const { return false; }

std::vector<int> AODGraph::getEntryNodes() const { return {}; }
std::vector<int> AODGraph::getExitNodes() const { return {}; }

void AODGraph::eliminateDeadCode() {}
void AODGraph::constantPropagation() {}
void AODGraph::commonSubexpressionElimination() {}

bool AODGraph::isValid() const { return true; }
std::vector<std::string> AODGraph::getValidationErrors() const { return {}; }
void AODGraph::validateCycles() const {}
void AODGraph::validateNoOrphanedNodes() const {}

AODGraph::GraphStatistics AODGraph::getStatistics() const {
    GraphStatistics stats;
    stats.node_count = nodes.size();
    stats.edge_count = edges.size();
    for (const auto& node : nodes) {
        if (node->isSIMDNode()) stats.simd_nodes++;
        else if (node->isControlNode()) stats.control_nodes++;
        else if (node->isDataNode()) stats.data_nodes++;
        else if (node->isCallNode()) stats.call_nodes++;
        stats.complexity_score += node->getComplexity();
    }
    return stats;
}

void AODGraph::printStatistics() const {
    auto stats = getStatistics();
    std::cout << "Graph Stats: " << stats.node_count << " nodes, " << stats.edge_count << " edges." << std::endl;
}

} // namespace aodsolve
