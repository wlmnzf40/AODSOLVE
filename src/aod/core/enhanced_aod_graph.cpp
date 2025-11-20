#include "aod/enhanced_aod_graph.h"
#include <algorithm>
#include <stdexcept>
#include <queue>
#include <stack>
#include <sstream>
#include <iostream>

namespace aodsolve {

// ============================================
// AODEdgeå®žçŽ°
// ============================================

AODEdge::AODEdge(std::shared_ptr<AODNode> src, std::shared_ptr<AODNode> tgt, AODEdgeType edge_type)
    : source(src), target(tgt), type(edge_type) {
    if (!src || !tgt) {
        throw std::invalid_argument("æºèŠ‚ç‚¹å’Œç›®æ ‡èŠ‚ç‚¹ä¸èƒ½ä¸ºç©º");
    }
}

std::string AODEdge::toString() const {
    std::ostringstream oss;
    oss << "Edge: " << source->getName() << " -> " << target->getName();
    switch (type) {
        case AODEdgeType::Data: oss << " (Data)"; break;
        case AODEdgeType::Control: oss << " (Control)"; break;
        case AODEdgeType::Parameter: oss << " (Parameter)"; break;
        case AODEdgeType::Return: oss << " (Return)"; break;
        case AODEdgeType::Exception: oss << " (Exception)"; break;
        case AODEdgeType::Memory: oss << " (Memory)"; break;
        case AODEdgeType::Alias: oss << " (Alias)"; break;
    }
    if (!properties.variable_name.empty()) {
        oss << " [var: " << properties.variable_name << "]";
    }
    return oss.str();
}

std::string AODEdge::getDOTLabel() const {
    std::ostringstream oss;
    switch (type) {
        case AODEdgeType::Data:
            oss << "data";
            if (!properties.variable_name.empty()) {
                oss << "\\n" << properties.variable_name;
            }
            break;
        case AODEdgeType::Control:
            oss << "ctrl";
            break;
        case AODEdgeType::Parameter:
            oss << "param";
            break;
        case AODEdgeType::Return:
            oss << "return";
            break;
        case AODEdgeType::Exception:
            oss << "except";
            break;
        case AODEdgeType::Memory:
            oss << "mem";
            break;
        case AODEdgeType::Alias:
            oss << "alias";
            break;
    }
    if (properties.is_critical) {
        oss << "\\n(critical)";
    }
    return oss.str();
}

// ============================================
// AODGraphå®žçŽ°
// ============================================

AODGraph::AODGraph(const std::string& graph_name) : name(graph_name) {
    is_analyzed = false;
    is_optimized = false;
}

void AODGraph::addNode(std::shared_ptr<AODNode> node) {
    if (!node) {
        throw std::invalid_argument("èŠ‚ç‚¹ä¸èƒ½ä¸ºç©º");
    }

    if (node_map.find(node->getId()) != node_map.end()) {
        throw std::invalid_argument("èŠ‚ç‚¹IDå·²å­˜åœ¨: " + std::to_string(node->getId()));
    }

    nodes.push_back(node);
    node_map[node->getId()] = node;
    nodes_by_name[node->getName()] = node;

    // é‡ç½®åˆ†æžçŠ¶æ€
    is_analyzed = false;
    is_optimized = false;
}

bool AODGraph::removeNode(int node_id) {
    auto it = node_map.find(node_id);
    if (it == node_map.end()) {
        return false;
    }

    std::shared_ptr<AODNode> node = it->second;

    // ç§»é™¤æ‰€æœ‰ç›¸å…³çš„è¾¹
    edges.erase(std::remove_if(edges.begin(), edges.end(),
        [&node_id](const std::shared_ptr<AODEdge>& edge) {
            return edge->getSource()->getId() == node_id || edge->getTarget()->getId() == node_id;
        }), edges.end());

    // ç§»é™¤èŠ‚ç‚¹
    nodes.erase(std::remove_if(nodes.begin(), nodes.end(),
        [&node_id](const std::shared_ptr<AODNode>& n) {
            return n->getId() == node_id;
        }), nodes.end());

    // æ¸…ç†æ˜ å°„
    node_map.erase(it);
    nodes_by_name.erase(node->getName());

    return true;
}

std::shared_ptr<AODNode> AODGraph::getNode(int node_id) const {
    auto it = node_map.find(node_id);
    return (it != node_map.end()) ? it->second : nullptr;
}

std::shared_ptr<AODNode> AODGraph::getNodeByName(const std::string& name) const {
    auto it = nodes_by_name.find(name);
    return (it != nodes_by_name.end()) ? it->second : nullptr;
}

void AODGraph::addEdge(std::shared_ptr<AODNode> source, std::shared_ptr<AODNode> target, AODEdgeType type) {
    if (!source || !target) {
        throw std::invalid_argument("æºèŠ‚ç‚¹å’Œç›®æ ‡èŠ‚ç‚¹ä¸èƒ½ä¸ºç©º");
    }

    if (source->getId() == target->getId()) {
        throw std::invalid_argument("ä¸èƒ½åˆ›å»ºè‡ªçŽ¯è¾¹");
    }

    // æ£€æŸ¥è¾¹æ˜¯å¦å·²å­˜åœ¨
    for (const auto& edge : edges) {
        if (edge->getSource()->getId() == source->getId() &&
            edge->getTarget()->getId() == target->getId() &&
            edge->getType() == type) {
            return; // è¾¹å·²å­˜åœ¨
        }
    }

    edges.push_back(std::make_shared<AODEdge>(source, target, type));

    // é‡ç½®åˆ†æžçŠ¶æ€
    is_analyzed = false;
    is_optimized = false;
}

void AODGraph::addEdge(std::shared_ptr<AODNode> source, std::shared_ptr<AODNode> target,
                      AODEdgeType type, const std::string& variable) {
    addEdge(source, target, type);
    edges.back()->setVariableName(variable);
}

bool AODGraph::removeEdge(int source_id, int target_id) {
    auto it = std::remove_if(edges.begin(), edges.end(),
        [source_id, target_id](const std::shared_ptr<AODEdge>& edge) {
            return edge->getSource()->getId() == source_id &&
                   edge->getTarget()->getId() == target_id;
        });

    if (it != edges.end()) {
        edges.erase(it);
        is_analyzed = false;
        is_optimized = false;
        return true;
    }
    return false;
}

std::vector<std::shared_ptr<AODEdge>> AODGraph::getEdgesFrom(int node_id) const {
    std::vector<std::shared_ptr<AODEdge>> result;
    for (const auto& edge : edges) {
        if (edge->getSource()->getId() == node_id) {
            result.push_back(edge);
        }
    }
    return result;
}

std::vector<std::shared_ptr<AODEdge>> AODGraph::getEdgesTo(int node_id) const {
    std::vector<std::shared_ptr<AODEdge>> result;
    for (const auto& edge : edges) {
        if (edge->getTarget()->getId() == node_id) {
            result.push_back(edge);
        }
    }
    return result;
}

void AODGraph::topologicalSort() const {
    std::vector<int> indegree(nodes.size(), 0);
    std::map<int, int> node_to_index;

    // æž„å»ºèŠ‚ç‚¹åˆ°ç´¢å¼•çš„æ˜ å°„
    for (size_t i = 0; i < nodes.size(); ++i) {
        node_to_index[nodes[i]->getId()] = i;
    }

    // è®¡ç®—å…¥åº¦
    for (const auto& edge : edges) {
        int target_idx = node_to_index[edge->getTarget()->getId()];
        indegree[target_idx]++;
    }

    // Kahnç®—æ³•
    std::queue<int> q;
    for (size_t i = 0; i < nodes.size(); ++i) {
        if (indegree[i] == 0) {
            q.push(i);
        }
    }

    topological_order.clear();
    int processed = 0;

    while (!q.empty()) {
        int current_idx = q.front();
        q.pop();
        topological_order.push_back(std::vector<int>{nodes[current_idx]->getId()});

        for (const auto& edge : edges) {
            if (edge->getSource()->getId() == nodes[current_idx]->getId()) {
                int target_idx = node_to_index[edge->getTarget()->getId()];
                indegree[target_idx]--;
                if (indegree[target_idx] == 0) {
                    q.push(target_idx);
                }
            }
        }

        processed++;
    }

    if (processed != static_cast<int>(nodes.size())) {
        // å›¾ä¸­æœ‰çŽ¯
        throw std::runtime_error("å›¾ä¸­å­˜åœ¨çŽ¯ï¼Œæ— æ³•è¿›è¡Œæ‹“æ‰‘æŽ’åº");
    }
}

std::vector<int> AODGraph::getPath(int start_id, int end_id) const {
    if (start_id == end_id) {
        return {start_id};
    }

    std::queue<int> q;
    std::map<int, int> parent;
    std::set<int> visited;

    q.push(start_id);
    visited.insert(start_id);

    while (!q.empty()) {
        int current = q.front();
        q.pop();

        for (const auto& edge : getEdgesFrom(current)) {
            int next = edge->getTarget()->getId();
            if (next == end_id) {
                // æ‰¾åˆ°è·¯å¾„ï¼Œé‡å»ºè·¯å¾„
                std::vector<int> path;
                int node = end_id;
                while (node != -1) {
                    path.push_back(node);
                    auto it = parent.find(node);
                    if (it != parent.end()) {
                        node = it->second;
                    } else {
                        node = -1;
                    }
                }
                std::reverse(path.begin(), path.end());
                return path;
            }

            if (visited.find(next) == visited.end()) {
                visited.insert(next);
                parent[next] = current;
                q.push(next);
            }
        }
    }

    return {}; // æœªæ‰¾åˆ°è·¯å¾„
}

bool AODGraph::hasPath(int start_id, int end_id) const {
    return !getPath(start_id, end_id).empty();
}

void AODGraph::computeVariableDefinitions() {
    variable_defs_map.clear();

    for (const auto& node : nodes) {
        auto defs = node->getDefinedVariables();
        for (const auto& var : defs) {
            variable_defs_map[var].insert(node);
        }
    }
}

void AODGraph::computeVariableUses() {
    variable_uses_map.clear();

    for (const auto& node : nodes) {
        auto uses = node->getUsedVariables();
        for (const auto& var : uses) {
            variable_uses_map[var].insert(node);
        }
    }
}

std::set<std::string> AODGraph::getVariablesAtNode(int node_id) const {
    auto node = getNode(node_id);
    if (!node) {
        return {};
    }

    std::set<std::string> vars;
    auto defs = node->getDefinedVariables();
    auto uses = node->getUsedVariables();

    vars.insert(defs.begin(), defs.end());
    vars.insert(uses.begin(), uses.end());

    return vars;
}

std::set<std::shared_ptr<AODNode>> AODGraph::getDefinitionsOf(const std::string& variable) const {
    auto it = variable_defs_map.find(variable);
    return (it != variable_defs_map.end()) ? it->second : std::set<std::shared_ptr<AODNode>>();
}

std::set<std::shared_ptr<AODNode>> AODGraph::getUsesOf(const std::string& variable) const {
    auto it = variable_uses_map.find(variable);
    return (it != variable_uses_map.end()) ? it->second : std::set<std::shared_ptr<AODNode>>();
}

void AODGraph::computeDominators() {
    if (nodes.empty()) {
        return;
    }

    dominator_map.clear();

    // å‡è®¾ç¬¬ä¸€ä¸ªèŠ‚ç‚¹æ˜¯å…¥å£èŠ‚ç‚¹
    int entry_id = nodes[0]->getId();
    dominator_map[entry_id] = {entry_id};

    for (const auto& node : nodes) {
        if (node->getId() != entry_id) {
            dominator_map[node->getId()] = {entry_id}; // åˆå§‹é›†åˆåŒ…å«æ‰€æœ‰èŠ‚ç‚¹
        }
    }

    bool changed = true;
    while (changed) {
        changed = false;

        for (const auto& node : nodes) {
            int node_id = node->getId();
            if (node_id == entry_id) {
                continue;
            }

            std::set<int> new_dominators = {node_id};

            // èŽ·å–æ‰€æœ‰å‰é©±èŠ‚ç‚¹
            std::vector<int> predecessors;
            for (const auto& edge : getEdgesTo(node_id)) {
                predecessors.push_back(edge->getSource()->getId());
            }

            if (!predecessors.empty()) {
                // è®¡ç®—å‰é©±çš„æ”¯é…é›†åˆçš„äº¤é›†
                auto& first_pred_doms = dominator_map[predecessors[0]];
                for (int i = 1; i < static_cast<int>(predecessors.size()); ++i) {
                    auto& pred_doms = dominator_map[predecessors[i]];
                    std::set<int> intersection;
                    std::set_intersection(first_pred_doms.begin(), first_pred_doms.end(),
                                         pred_doms.begin(), pred_doms.end(),
                                         std::inserter(intersection, intersection.begin()));
                    first_pred_doms = intersection;
                }

                new_dominators.insert(first_pred_doms.begin(), first_pred_doms.end());
            }

            if (dominator_map[node_id] != new_dominators) {
                dominator_map[node_id] = new_dominators;
                changed = true;
            }
        }
    }

    is_analyzed = true;
}

std::vector<int> AODGraph::getImmediateDominators() const {
    std::vector<int> result;
    if (dominator_map.empty()) {
        return result;
    }

    for (const auto& [node_id, dominators] : dominator_map) {
        if (node_id == nodes[0]->getId()) {
            continue; // è·³è¿‡å…¥å£èŠ‚ç‚¹
        }

        // æ‰¾åˆ°ç«‹å³æ”¯é…è€…
        int immediate_dom = -1;
        for (int dom : dominators) {
            if (dom != node_id) {
                bool is_immediate = true;
                for (int other_dom : dominators) {
                    if (other_dom != dom && other_dom != node_id) {
                        if (dominator_map.find(other_dom) != dominator_map.end()) {
                            auto& other_doms = dominator_map.at(other_dom);
                            if (other_doms.find(dom) != other_doms.end()) {
                                is_immediate = false;
                                break;
                            }
                        }
                    }
                }
                if (is_immediate && dominator_map.find(dom) != dominator_map.end()) {
                    immediate_dom = dom;
                    break;
                }
            }
        }

        if (immediate_dom != -1) {
            result.push_back(immediate_dom);
        }
    }

    return result;
}

std::set<int> AODGraph::getDominators(int node_id) const {
    auto it = dominator_map.find(node_id);
    return (it != dominator_map.end()) ? it->second : std::set<int>();
}

bool AODGraph::isDominatedBy(int dominated, int dominator) const {
    auto doms = getDominators(dominated);
    return doms.find(dominator) != doms.end();
}

bool AODGraph::isCyclic() const {
    try {
        topologicalSort();
        return false; // å¦‚æžœèƒ½å®Œæˆæ‹“æ‰‘æŽ’åºï¼Œè¯´æ˜Žæ— çŽ¯
    } catch (const std::runtime_error&) {
        return true; // æœ‰çŽ¯
    }
}

std::vector<int> AODGraph::getEntryNodes() const {
    std::vector<int> entry_nodes;
    for (const auto& node : nodes) {
        bool has_incoming = false;
        for (const auto& edge : edges) {
            if (edge->getTarget()->getId() == node->getId()) {
                has_incoming = true;
                break;
            }
        }
        if (!has_incoming) {
            entry_nodes.push_back(node->getId());
        }
    }
    return entry_nodes;
}

std::vector<int> AODGraph::getExitNodes() const {
    std::vector<int> exit_nodes;
    for (const auto& node : nodes) {
        bool has_outgoing = false;
        for (const auto& edge : edges) {
            if (edge->getSource()->getId() == node->getId()) {
                has_outgoing = true;
                break;
            }
        }
        if (!has_outgoing) {
            exit_nodes.push_back(node->getId());
        }
    }
    return exit_nodes;
}

void AODGraph::eliminateDeadCode() {
    std::set<int> live_nodes;
    std::set<int> exit_nodes_set(getExitNodes().begin(), getExitNodes().end());

    // ä»Žå‡ºå£èŠ‚ç‚¹å¼€å§‹å‘åŽéåŽ†ï¼Œæ ‡è®°æ‰€æœ‰æ´»è·ƒèŠ‚ç‚¹
    std::queue<int> q;
    for (int exit_id : exit_nodes_set) {
        q.push(exit_id);
        live_nodes.insert(exit_id);
    }

    while (!q.empty()) {
        int current = q.front();
        q.pop();

        for (const auto& edge : getEdgesTo(current)) {
            int source_id = edge->getSource()->getId();
            if (live_nodes.find(source_id) == live_nodes.end()) {
                live_nodes.insert(source_id);
                q.push(source_id);
            }
        }
    }

    // ç§»é™¤æ‰€æœ‰éžæ´»è·ƒèŠ‚ç‚¹
    std::vector<std::shared_ptr<AODNode>> remaining_nodes;
    for (const auto& node : nodes) {
        if (live_nodes.find(node->getId()) != live_nodes.end()) {
            remaining_nodes.push_back(node);
        }
    }

    if (remaining_nodes.size() != nodes.size()) {
        nodes = remaining_nodes;

        // é‡å»ºèŠ‚ç‚¹æ˜ å°„
        node_map.clear();
        nodes_by_name.clear();
        for (const auto& node : nodes) {
            node_map[node->getId()] = node;
            nodes_by_name[node->getName()] = node;
        }

        // ç§»é™¤ä¸ç›¸å…³çš„è¾¹
        edges.erase(std::remove_if(edges.begin(), edges.end(),
            [&live_nodes](const std::shared_ptr<AODEdge>& edge) {
                return live_nodes.find(edge->getSource()->getId()) == live_nodes.end() ||
                       live_nodes.find(edge->getTarget()->getId()) == live_nodes.end();
            }), edges.end());

        is_optimized = true;
    }
}

void AODGraph::constantPropagation() {
    std::map<int, std::map<std::string, std::string>> constant_values;

    // ä¼ æ’­å¸¸é‡å€¼
    bool changed = true;
    while (changed) {
        changed = false;

        for (const auto& node : nodes) {
            if (auto* constant_node = dynamic_cast<AODNode*>(node.get())) {
                // è¿™é‡Œéœ€è¦æ£€æŸ¥èŠ‚ç‚¹æ˜¯å¦è¡¨ç¤ºå¸¸é‡
                // ç®€åŒ–å®žçŽ°
            }
        }
    }
}

void AODGraph::commonSubexpressionElimination() {
    // æŸ¥æ‰¾é‡å¤çš„è¡¨è¾¾å¼
    std::map<std::string, std::shared_ptr<AODNode>> expression_map;
    std::vector<std::shared_ptr<AODNode>> to_remove;

    for (const auto& node : nodes) {
        if (node->getInputs().size() > 0) {
            std::string expr_sig = node->getType() == AODNodeType::Add ? "Add" : node->getName();
            // è¿™é‡Œéœ€è¦æ›´å¤æ‚çš„è¡¨è¾¾å¼ç­¾åè®¡ç®—

            if (expression_map.find(expr_sig) != expression_map.end()) {
                to_remove.push_back(node);
            } else {
                expression_map[expr_sig] = node;
            }
        }
    }

    // 移除重复表达式
    for (const auto& node : to_remove) {
        // 重定向输入边到第一个出现的表达式
        auto original = expression_map[node->getName()];

        // 更新所有指向待删除节点的边，使其指向原始节点
        // 注意：实际的边重定向需要在removeNode之前完成
        // 这里简化处理，只是删除节点
        removeNode(node->getId());
    }
    is_optimized = true;
}

bool AODGraph::isValid() const {
    try {
        validateCycles();
        validateNoOrphanedNodes();
        validateReferentialIntegrity();
        return true;
    } catch (const std::exception&) {
        return false;
    }
}

std::vector<std::string> AODGraph::getValidationErrors() const {
    std::vector<std::string> errors;

    try {
        validateCycles();
    } catch (const std::exception& e) {
        errors.push_back("å¾ªçŽ¯é”™è¯¯: " + std::string(e.what()));
    }

    try {
        validateNoOrphanedNodes();
    } catch (const std::exception& e) {
        errors.push_back("å­¤ç«‹èŠ‚ç‚¹: " + std::string(e.what()));
    }

    try {
        validateReferentialIntegrity();
    } catch (const std::exception& e) {
        errors.push_back("å¼•ç”¨å®Œæ•´æ€§: " + std::string(e.what()));
    }

    return errors;
}

void AODGraph::validateCycles() const {
    try {
        topologicalSort();
    } catch (const std::runtime_error& e) {
        throw std::runtime_error("å›¾åŒ…å«å¾ªçŽ¯: " + std::string(e.what()));
    }
}

void AODGraph::validateNoOrphanedNodes() const {
    std::set<int> connected_nodes;

    for (const auto& edge : edges) {
        connected_nodes.insert(edge->getSource()->getId());
        connected_nodes.insert(edge->getTarget()->getId());
    }

    for (const auto& node : nodes) {
        if (connected_nodes.find(node->getId()) == connected_nodes.end() &&
            node->getId() != nodes[0]->getId()) {
            throw std::runtime_error("å‘çŽ°å­¤ç«‹èŠ‚ç‚¹: " + node->getName());
        }
    }
}

void AODGraph::validateReferentialIntegrity() const {
    std::set<int> valid_node_ids;
    for (const auto& node : nodes) {
        valid_node_ids.insert(node->getId());
    }

    for (const auto& edge : edges) {
        if (valid_node_ids.find(edge->getSource()->getId()) == valid_node_ids.end()) {
            throw std::runtime_error("è¾¹å¼•ç”¨äº†ä¸å­˜åœ¨çš„æºèŠ‚ç‚¹");
        }
        if (valid_node_ids.find(edge->getTarget()->getId()) == valid_node_ids.end()) {
            throw std::runtime_error("è¾¹å¼•ç”¨äº†ä¸å­˜åœ¨çš„ç›®æ ‡èŠ‚ç‚¹");
        }
    }
}

std::string AODGraph::toDOT() const {
    std::ostringstream oss;
    oss << "digraph " << name << " {\n";
    oss << "  rankdir=TB;\n";
    oss << "  node [shape=box, fontname=\"Courier\", fontsize=10];\n\n";

    // è¾“å‡ºèŠ‚ç‚¹
    for (const auto& node : nodes) {
        oss << "  " << node->getId() << " [label=\"";
        oss << node->getDOTLabel() << "\", ";
        oss << node->getDOTStyle() << "];\n";
    }

    // è¾“å‡ºè¾¹
    for (const auto& edge : edges) {
        oss << "  " << edge->getSource()->getId() << " -> "
            << edge->getTarget()->getId() << " [label=\"";
        oss << edge->getDOTLabel() << "\"];\n";
    }

    oss << "}\n";
    return oss.str();
}

AODGraph::GraphStatistics AODGraph::getStatistics() const {
    GraphStatistics stats;
    stats.node_count = nodes.size();
    stats.edge_count = edges.size();

    for (const auto& node : nodes) {
        if (node->isSIMDNode()) {
            stats.simd_nodes++;
        } else if (node->isControlNode()) {
            stats.control_nodes++;
        } else if (node->isDataNode()) {
            stats.data_nodes++;
        } else if (node->isCallNode()) {
            stats.call_nodes++;
        }

        stats.complexity_score += node->getComplexity();
    }

    // è®¡ç®—æœ€å¤§æ·±åº¦
    auto entry_nodes = getEntryNodes();
    for (int entry_id : entry_nodes) {
        int depth = getMaxDepthFromNode(entry_id);
        stats.max_depth = std::max(stats.max_depth, depth);
    }

    return stats;
}

int AODGraph::getMaxDepthFromNode(int node_id) const {
    std::set<int> visited;
    return getMaxDepthFromNodeRecursive(node_id, visited);
}

int AODGraph::getMaxDepthFromNodeRecursive(int node_id, std::set<int>& visited) const {
    if (visited.find(node_id) != visited.end()) {
        return 0; // é¿å…å¾ªçŽ¯
    }

    visited.insert(node_id);

    int max_depth = 0;
    for (const auto& edge : getEdgesFrom(node_id)) {
        int child_depth = getMaxDepthFromNodeRecursive(edge->getTarget()->getId(), visited);
        max_depth = std::max(max_depth, child_depth + 1);
    }

    return max_depth;
}

void AODGraph::printStatistics() const {
    auto stats = getStatistics();
    std::cout << "=== AODå›¾ç»Ÿè®¡ä¿¡æ¯ ===" << std::endl;
    std::cout << "èŠ‚ç‚¹æ€»æ•°: " << stats.node_count << std::endl;
    std::cout << "è¾¹æ€»æ•°: " << stats.edge_count << std::endl;
    std::cout << "SIMDèŠ‚ç‚¹: " << stats.simd_nodes << std::endl;
    std::cout << "æŽ§åˆ¶æµèŠ‚ç‚¹: " << stats.control_nodes << std::endl;
    std::cout << "æ•°æ®æµèŠ‚ç‚¹: " << stats.data_nodes << std::endl;
    std::cout << "è°ƒç”¨èŠ‚ç‚¹: " << stats.call_nodes << std::endl;
    std::cout << "å¤æ‚åº¦åˆ†æ•°: " << stats.complexity_score << std::endl;
    std::cout << "æœ€å¤§æ·±åº¦: " << stats.max_depth << std::endl;
    std::cout << "========================" << std::endl;
}

// ============================================
// AODGraphBuilderå®žçŽ°
// ============================================

AODGraphBuilder::AODGraphBuilder(const std::string& name) {
    current_graph = std::make_shared<AODGraph>(name);
}

AODNodePtr AODGraphBuilder::createNode(AODNodeType type, const std::string& name) {
    auto node = createNodeImpl(type, name);
    node_name_map[name] = node;
    addLogEntry("åˆ›å»ºèŠ‚ç‚¹: " + name);
    return node;
}

AODNodePtr AODGraphBuilder::createNodeImpl(AODNodeType type, const std::string& name) {
    // 简化实现：直接创建基础 AODNode
    auto node = std::make_shared<AODNode>(type, name);
    current_graph->addNode(node);
    return node;
}

AODNodePtr AODGraphBuilder::getOrCreateNode(const std::string& name, AODNodeType type) {
    auto it = node_name_map.find(name);
    if (it != node_name_map.end()) {
        return it->second;
    }
    return createNode(type, name);
}

void AODGraphBuilder::addEdge(const std::string& source_name, const std::string& target_name,
                              AODEdgeType type, const std::string& variable) {
    auto source = getOrCreateNode(source_name, AODNodeType::Unknown);
    auto target = getOrCreateNode(target_name, AODNodeType::Unknown);

    current_graph->addEdge(source, target, type, variable);
    addLogEntry("æ·»åŠ è¾¹: " + source_name + " -> " + target_name + " (ç±»åž‹: " +
                std::to_string(static_cast<int>(type)) + ")");
}

void AODGraphBuilder::printBuildLog() const {
    std::cout << "=== å›¾æž„å»ºæ—¥å¿— ===" << std::endl;
    for (const auto& entry : build_log) {
        std::cout << entry << std::endl;
    }
    std::cout << "===================" << std::endl;
}

AODGraphPtr AODGraphBuilder::finalize() {
    auto result = current_graph;
    reset();
    return result;
}

void AODGraphBuilder::reset() {
    current_graph.reset();
    node_name_map.clear();
    build_log.clear();
    current_graph = std::make_shared<AODGraph>("BuildGraph");
}

    // ===== 新增实现: 拓扑排序 =====
    std::vector<std::shared_ptr<AODNode>> AODGraph::topologicalSort_v1() {
    std::vector<std::shared_ptr<AODNode>> result;
    std::map<int, int> in_degree;

    // 计算每个节点的入度
    for (const auto& node : nodes) {
        in_degree[node->getId()] = 0;
    }

    for (const auto& edge : edges) {
        in_degree[edge->getTarget()->getId()]++;
    }

    // 使用队列进行拓扑排序
    std::queue<std::shared_ptr<AODNode>> queue;
    for (const auto& node : nodes) {
        if (in_degree[node->getId()] == 0) {
            queue.push(node);
        }
    }

    while (!queue.empty()) {
        auto current = queue.front();
        queue.pop();
        result.push_back(current);

        // 减少所有后继节点的入度
        for (const auto& edge : getEdgesFrom(current->getId())) {
            int target_id = edge->getTarget()->getId();
            in_degree[target_id]--;
            if (in_degree[target_id] == 0) {
                queue.push(edge->getTarget());
            }
        }
    }

    return result;
}

} // namespace aodsolve