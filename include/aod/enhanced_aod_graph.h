#pragma once

#include "enhanced_aod_node.h"
#include <map>
#include <set>
#include <vector>
#include <memory>
#include <string>
#include <functional>
#include <optional>
#include <unordered_map>

namespace aodsolve {

// ============================================
// AODå›¾ä¸­çš„è¾¹ç±»åž‹
// ============================================

enum class AODEdgeType {
    Data,      // æ•°æ®æµè¾¹
    Control,   // æŽ§åˆ¶æµè¾¹
    Parameter, // å‚æ•°è¾¹
    Return,    // è¿”å›žè¾¹
    Exception, // å¼‚å¸¸è¾¹
    Memory,    // å†…å­˜ä¾èµ–è¾¹
    Alias      // åˆ«åè¾¹
};

// è¾¹å±žæ€§
struct AODEdgeProperties {
    std::string variable_name;
    std::string edge_type_str;
    std::map<std::string, std::string> attributes;
    int weight = 1;
    bool is_critical = false;
    std::string source_location;
    std::string target_location;
};

// è¾¹ç±»
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
    AODEdgeProperties& getProperties() { return properties; }
    const AODEdgeProperties& getProperties() const { return properties; }

    void setVariableName(const std::string& var) { properties.variable_name = var; }
    void setWeight(int w) { properties.weight = w; }
    void setCritical(bool critical) { properties.is_critical = critical; }
    void addAttribute(const std::string& key, const std::string& value) {
        properties.attributes[key] = value;
    }

    std::string toString() const;
    std::string getDOTLabel() const;
};

// å¢žå¼ºçš„AODå›¾ç±»
class AODGraph {
private:
    std::string name;
    std::vector<std::shared_ptr<AODNode>> nodes;
    std::vector<std::shared_ptr<AODEdge>> edges;
    std::map<int, std::shared_ptr<AODNode>> node_map;
    std::map<std::string, std::shared_ptr<AODNode>> nodes_by_name;

    // åˆ†æžç»“æžœç¼“å­˜
    std::map<int, std::set<int>> dominator_map; // ä¿®æ”¹ä¸ºä½¿ç”¨èŠ‚ç‚¹ID
    std::map<std::string, std::set<std::shared_ptr<AODNode>>> variable_defs_map;
    std::map<std::string, std::set<std::shared_ptr<AODNode>>> variable_uses_map;
    mutable std::vector<std::vector<int>> topological_order;

    // åˆ†æžæ ‡å¿—
    bool is_analyzed = false;
    bool is_optimized = false;

public:
    explicit AODGraph(const std::string& graph_name = "AODGraph");
    ~AODGraph() = default;

    // èŠ‚ç‚¹ç®¡ç†
    void addNode(std::shared_ptr<AODNode> node);
    bool removeNode(int node_id);
    std::shared_ptr<AODNode> getNode(int node_id) const;
    std::shared_ptr<AODNode> getNodeByName(const std::string& name) const;
    const std::vector<std::shared_ptr<AODNode>>& getNodes() const { return nodes; }
    int getNodeCount() const { return nodes.size(); }

    // è¾¹ç®¡ç†
    void addEdge(std::shared_ptr<AODNode> source, std::shared_ptr<AODNode> target, AODEdgeType type);
    void addEdge(std::shared_ptr<AODNode> source, std::shared_ptr<AODNode> target,
                 AODEdgeType type, const std::string& variable);
    bool removeEdge(int source_id, int target_id);
    std::vector<std::shared_ptr<AODEdge>> getEdges() const { return edges; }
    std::vector<std::shared_ptr<AODEdge>> getEdgesFrom(int node_id) const;
    std::vector<std::shared_ptr<AODEdge>> getEdgesTo(int node_id) const;
    std::vector<std::shared_ptr<AODEdge>> getIncomingEdges(int node_id) const { return getEdgesTo(node_id); }

    // æ‹“æ‰‘æ“ä½œ
    // void topologicalSort() const;
    void topologicalSort() const;
    const std::vector<std::vector<int>>& getTopologicalOrder() const { return topological_order; }
    std::vector<std::shared_ptr<AODNode>> topologicalSort_v1();

    std::vector<int> getPath(int start_id, int end_id) const;
    bool hasPath(int start_id, int end_id) const;

    // æ•°æ®æµåˆ†æž
    void computeVariableDefinitions();
    void computeVariableUses();
    void computeReachingDefinitions();
    std::set<std::string> getVariablesAtNode(int node_id) const;
    std::set<std::shared_ptr<AODNode>> getDefinitionsOf(const std::string& variable) const;
    std::set<std::shared_ptr<AODNode>> getUsesOf(const std::string& variable) const;

    // æŽ§åˆ¶æµåˆ†æž
    void computeDominators();
    void computePostDominators();
    std::vector<int> getImmediateDominators() const;
    std::set<int> getDominators(int node_id) const;
    std::set<int> getPostDominators(int node_id) const;
    bool isDominatedBy(int dominated, int dominator) const;
    bool postDominates(int dominated, int dominator) const;

    // SIMDåˆ†æž
    void identifySIMDRegions();
    std::vector<std::vector<int>> getSIMDRegions() const;
    std::vector<int> getSIMDNodes() const;
    bool canVectorize() const;
    std::vector<std::shared_ptr<AODNode>> getVectorizableNodes() const;

    // æŽ§åˆ¶æµåˆ†æž
    std::vector<int> getEntryNodes() const;
    std::vector<int> getExitNodes() const;
    std::vector<int> getCriticalPath() const;
    bool isCyclic() const;
    std::vector<int> getLoopHeaders() const;

    // ä¼˜åŒ–æ“ä½œ
    void eliminateDeadCode();
    void constantPropagation();
    void commonSubexpressionElimination();
    void loopInvariantCodeMotion();
    void strengthReduction();
    void removePhiNodes();
    void compressGraph();

    // éªŒè¯å’Œè°ƒè¯•
    bool isValid() const;
    std::vector<std::string> getValidationErrors() const;
    void validateCycles() const;
    void validateNoOrphanedNodes() const;

    // å¯è§†åŒ–
    std::string toDOT() const;
    std::string toGraphML() const;
    void saveToFile(const std::string& filename) const;

    // ç»Ÿè®¡ä¿¡æ¯
    struct GraphStatistics {
        int node_count = 0;
        int edge_count = 0;
        int simd_nodes = 0;
        int control_nodes = 0;
        int data_nodes = 0;
        int call_nodes = 0;
        int complexity_score = 0;
        int critical_path_length = 0;
        int loop_count = 0;
        int max_depth = 0;
    };

    GraphStatistics getStatistics() const;
    void printStatistics() const;

    // åˆå¹¶å’Œæ¯”è¾ƒ
    void merge(const AODGraph& other);
    void intersect(const AODGraph& other);
    bool isIsomorphicTo(const AODGraph& other) const;

    // åºåˆ—åŒ–
    std::string serialize() const;
    static AODGraph deserialize(const std::string& serialized);

    // å·¥åŽ‚æ–¹æ³•
    static std::shared_ptr<AODGraph> createEmptyGraph(const std::string& name);
    static std::shared_ptr<AODGraph> createLinearGraph(int node_count);
    static std::shared_ptr<AODGraph> createControlFlowGraph(int branch_count);
    static std::shared_ptr<AODGraph> createDataFlowGraph(int variable_count);

    // è®¿é—®è€…æ¨¡å¼
    template<typename Visitor>
    void acceptVisitor(Visitor& visitor) {
        for (auto& node : nodes) {
            visitor.visit(node);
        }
    }

    // è¿‡æ»¤å’Œæœç´¢
    std::vector<std::shared_ptr<AODNode>> filterNodes(std::function<bool(const std::shared_ptr<AODNode>&)> predicate) const;
    std::shared_ptr<AODNode> findNode(std::function<bool(const std::shared_ptr<AODNode>&)> predicate) const;
    std::vector<std::shared_ptr<AODEdge>> filterEdges(std::function<bool(const std::shared_ptr<AODEdge>&)> predicate) const;

    // é‡ç½®åˆ†æžçŠ¶æ€
    void resetAnalysis() {
        is_analyzed = false;
        is_optimized = false;
        dominator_map.clear();
        variable_defs_map.clear();
        variable_uses_map.clear();
        topological_order.clear();
    }

    // å›¾åç§°ç®¡ç†
    void setName(const std::string& name) { this->name = name; }
    const std::string& getName() const { return name; }

private:
    // å†…éƒ¨è¾…åŠ©æ–¹æ³•
    void ensureAnalyzed();
    void computeTopologicalOrderDFS(int node_id, std::vector<bool>& visited, std::vector<int>& order) const;
    void computeDominatorsDFS(int node_id, std::set<int>& visited, std::map<int, std::set<int>>& doms) const;
    std::set<int> computeDominatorsOfNode(int node_id, const std::set<int>& initial_dom) const;
    std::set<int> intersectDominatorSets(const std::set<int>& dom1, const std::set<int>& dom2) const;
    int getMaxDepthFromNode(int node_id) const;
    int getMaxDepthFromNodeRecursive(int node_id, std::set<int>& visited) const;

    // éªŒè¯è¾…åŠ©æ–¹æ³•
    void validateNoDuplicateNodes() const;
    void validateNoDuplicateEdges() const;
    void validateReferentialIntegrity() const;
};

// æ™ºèƒ½æŒ‡é’ˆç±»åž‹å®šä¹‰
using AODGraphPtr = std::shared_ptr<AODGraph>;
using AODNodePtr = std::shared_ptr<AODNode>;
using AODEdgePtr = std::shared_ptr<AODEdge>;

// å›¾æž„å»ºå™¨ç±»
class AODGraphBuilder {
private:
    AODGraphPtr current_graph;
    std::map<std::string, AODNodePtr> node_name_map;
    std::vector<std::string> build_log;

public:
    explicit AODGraphBuilder(const std::string& name = "BuildGraph");

    AODGraphPtr getGraph() const { return current_graph; }
    const std::vector<std::string>& getBuildLog() const { return build_log; }

    // èŠ‚ç‚¹æž„å»º
    AODNodePtr createNode(AODNodeType type, const std::string& name = "");
    AODNodePtr getOrCreateNode(const std::string& name, AODNodeType type);

    // 内部辅助方法
private:
    AODNodePtr createNodeImpl(AODNodeType type, const std::string& name);

    // è¾¹æž„å»º
    void addEdge(const std::string& source_name, const std::string& target_name,
                 AODEdgeType type, const std::string& variable = "");

    // å›¾æ“ä½œ
    void mergeWith(const AODGraphBuilder& other);
    void cloneNode(const std::string& source_name, const std::string& target_name);
    void removeNode(const std::string& name);

    // æž„å»ºæ—¥å¿—
    void addLogEntry(const std::string& entry) { build_log.push_back(entry); }
    void printBuildLog() const;

    // æœ€ç»ˆåŒ–
    AODGraphPtr finalize();
    void reset();
};

// å‘é‡åŒ–å’ŒSIMDä¼˜åŒ–çš„å›¾æ“ä½œå™¨
class AODGraphSIMDOptimizer {
private:
    AODGraphPtr graph;

public:
    explicit AODGraphSIMDOptimizer(AODGraphPtr g) : graph(g) {}

    // SIMDæ¨¡å¼è¯†åˆ«
    void identifyLoadsAndStores();
    void identifyArithmeticPatterns();
    void identifyComparisonPatterns();
    void identifyBlendOperations();

    // å‘é‡åŒ–å˜æ¢
    void createSIMDNodes();
    void optimizeMemoryAccess();
    void alignLoops();
    void unrollLoops(int factor = 2);

    // æž¶æž„ç‰¹å®šä¼˜åŒ–
    void optimizeForAVX();
    void optimizeForNEON();
    void optimizeForSVE();
    void optimizeForAVX512();

    // æ€§èƒ½åˆ†æž
    void estimatePerformance();
    void generateOptimizationReport();
};

// å›¾åˆ†æžå™¨
class AODGraphAnalyzer {
private:
    AODGraphPtr graph;

public:
    explicit AODGraphAnalyzer(AODGraphPtr g) : graph(g) {}

    // å¤æ‚æ€§åˆ†æž
    int computeCyclomaticComplexity() const;
    int computeHalsteadComplexity() const;
    int computeLinesOfCode() const;

    // æ•°æ®æµåˆ†æž
    void performReachingDefinitionsAnalysis();
    void performAvailableExpressionsAnalysis();
    void performLiveVariablesAnalysis();
    void performVeryBusyExpressionsAnalysis();

    // æŽ§åˆ¶æµåˆ†æž
    void identifyLoops();
    void identifyLoopInvariants();
    void identifyNestedLoops();
    void analyzeLoopCarriedDependencies();

    // SIMDåˆ†æž
    void analyzeSIMDPotential();
    void identifyMemoryAccessPatterns();
    void analyzeVectorizationBarriers();
    void estimateVectorizationBenefit();

    // ç”Ÿæˆåˆ†æžæŠ¥å‘Š
    std::string generateAnalysisReport() const;
    void saveAnalysisToFile(const std::string& filename) const;
};

} // namespace aodsolve