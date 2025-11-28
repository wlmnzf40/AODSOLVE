#pragma once
#include "aod/enhanced_aod_graph.h"
#include "aod/optimization_rule_system.h"
#include "conversion/enhanced_cpg_to_aod_converter.h"
#include "clang/AST/ASTContext.h"

#include <memory>
#include <map>
#include <vector>
#include <string>

namespace aodsolve {

struct CodeGenerationResult {
    bool successful = false;
    std::string generated_code;
    double estimated_speedup = 0.0;
    int simd_intrinsics = 0;
    std::vector<std::string> info_messages;
    std::string target_architecture;
};

class EnhancedCodeGenerator {
private:
    clang::ASTContext& ast_context;
    std::string target_architecture;
    RuleDatabase* rule_db = nullptr;

    std::map<const clang::Stmt*, std::shared_ptr<AODNode>> stmt_map;
    std::shared_ptr<AODGraph> current_graph;

public:
    explicit EnhancedCodeGenerator(clang::ASTContext& ctx);
    ~EnhancedCodeGenerator() = default;

    void setTargetArchitecture(const std::string& arch) { target_architecture = arch; }
    void setRuleDatabase(RuleDatabase* db) { rule_db = db; }

    CodeGenerationResult generate(const clang::FunctionDecl* func, const ConversionResult& conv_res);
    CodeGenerationResult generateCodeFromGraph(const std::shared_ptr<AODGraph>& graph);
    std::string generateLoopFromTemplate(const std::map<std::string, std::string>&, const std::string&) { return ""; }

private:
    // 修复参数默认值
    void traverseAST(const clang::Stmt* stmt, std::stringstream& code, bool force_scalar = false);

    std::string generateFromAOD(const std::shared_ptr<AODNode>& node, const std::shared_ptr<AODGraph>& graph);
    std::string tryApplyRules(const std::shared_ptr<AODNode>& node, const std::shared_ptr<AODGraph>& graph);
    std::string generateDefineNode(const std::shared_ptr<AODNode>& node, const std::shared_ptr<AODGraph>& graph);

    std::string generateFallbackCode(const clang::Stmt* stmt);
    std::string generateOutputVar(const std::shared_ptr<AODNode>& node);
    bool needsSemicolon(const clang::Stmt* stmt);

    const OptimizationRule* getRuleForNode(const std::shared_ptr<AODNode>& node);
    std::string getReturnTypeFromRule(const std::shared_ptr<AODNode>& node);
    std::string applyReplacements(std::string code, const TransformTemplate& tmpl);
};

} // namespace aodsolve
