#include "generation/enhanced_code_generator.h"
#include <sstream>
#include <iostream>
#include <clang/AST/Stmt.h>
#include <clang/AST/Expr.h>
#include <clang/AST/Decl.h>

namespace aodsolve {

EnhancedCodeGenerator::EnhancedCodeGenerator(clang::ASTContext& ctx)
    : ast_context(ctx), target_architecture("SVE") {}

bool EnhancedCodeGenerator::needsSemicolon(const clang::Stmt* stmt) {
    if (llvm::isa<clang::CompoundStmt>(stmt)) return false;
    if (llvm::isa<clang::IfStmt>(stmt)) return false;
    if (llvm::isa<clang::WhileStmt>(stmt)) return false;
    if (llvm::isa<clang::ForStmt>(stmt)) return false;
    if (llvm::isa<clang::DeclStmt>(stmt)) return true;
    return true;
}

CodeGenerationResult EnhancedCodeGenerator::generateCodeFromGraph(const std::shared_ptr<AODGraph>& graph) { return {}; }

CodeGenerationResult EnhancedCodeGenerator::generate(const clang::FunctionDecl* func, const ConversionResult& conv_res) {
    CodeGenerationResult result;
    stmt_map = conv_res.stmt_to_node_ptr_map;
    current_graph = conv_res.aod_graph;
    std::stringstream code;

    if (func->hasBody()) traverseAST(func->getBody(), code, false);

    result.generated_code = code.str();
    result.successful = true;
    stmt_map.clear();
    current_graph.reset();
    return result;
}

std::string getLoopVarName(const clang::Stmt* inc) {
    if (!inc) return "";
    if (auto* uo = llvm::dyn_cast<clang::UnaryOperator>(inc)) {
        if (auto* dre = llvm::dyn_cast<clang::DeclRefExpr>(uo->getSubExpr()->IgnoreParenCasts()))
            return dre->getDecl()->getNameAsString();
    }
    else if (auto* bo = llvm::dyn_cast<clang::BinaryOperator>(inc)) { // i += 1
        if (auto* dre = llvm::dyn_cast<clang::DeclRefExpr>(bo->getLHS()->IgnoreParenCasts()))
            return dre->getDecl()->getNameAsString();
    }
    else if (auto* cao = llvm::dyn_cast<clang::CompoundAssignOperator>(inc)) { // i += 1
         if (auto* dre = llvm::dyn_cast<clang::DeclRefExpr>(cao->getLHS()->IgnoreParenCasts()))
             return dre->getDecl()->getNameAsString();
    }
    return "";
}

int getVectorWidth(const clang::Stmt* stmt, const std::map<const clang::Stmt*, std::shared_ptr<AODNode>>& map) {
    if (!stmt) return 0;
    int width = 0;
    auto it = map.find(stmt);
    if (it != map.end()) {
        std::string w = it->second->getProperty("vector_width");
        if (!w.empty()) width = std::max(width, std::stoi(w));
    }
    for (const auto* child : stmt->children()) {
        width = std::max(width, getVectorWidth(child, map));
    }
    return width;
}

void EnhancedCodeGenerator::traverseAST(const clang::Stmt* stmt, std::stringstream& code, bool force_scalar) {
    if (!stmt) return;

    bool handled_by_aod = false;
    auto it = stmt_map.find(stmt);

    if (!force_scalar && it != stmt_map.end()) {
        auto node = it->second;
        if (node->getProperty("op_name") == "bf16_dot" ||
            node->getType() == AODNodeType::SIMD_Intrinsic ||
            node->getProperty("op_name") == "define") {
            handled_by_aod = true;
        }
        if (node->getProperty("vectorize") == "true" && llvm::isa<clang::BinaryOperator>(stmt)) {
            handled_by_aod = true;
        }
    }

    if (handled_by_aod) {
        std::string gen = generateFromAOD(it->second, current_graph);
        if (!gen.empty()) {
            code << "    " << gen;
            if (needsSemicolon(stmt) && gen.back() != ';' && gen.back() != '}') code << ";";
            code << "\n";
            return;
        }
    }

    if (auto* compound = llvm::dyn_cast<clang::CompoundStmt>(stmt)) {
        for (auto* child : compound->body()) traverseAST(child, code, force_scalar);
    }
    else if (auto* whileStmt = llvm::dyn_cast<clang::WhileStmt>(stmt)) {
        std::string cond = generateFallbackCode(whileStmt->getCond());
        code << "    while (" << cond << ") {\n";
        traverseAST(whileStmt->getBody(), code, force_scalar);
        code << "    }\n";
    }
    else if (auto* forStmt = llvm::dyn_cast<clang::ForStmt>(stmt)) {
        std::string init = generateFallbackCode(forStmt->getInit());
        std::string cond = generateFallbackCode(forStmt->getCond());
        std::string inc = generateFallbackCode(forStmt->getInc());
        std::string loop_var = getLoopVarName(forStmt->getInc());

        int vec_width = 0;
        if (!force_scalar && (target_architecture == "NEON" || target_architecture == "SVE")) {
            vec_width = getVectorWidth(forStmt->getBody(), stmt_map);
        }

        if (vec_width > 0 || (target_architecture == "SVE" && stmt_map.count(stmt) && stmt_map[stmt]->getProperty("vectorize") == "true")) {
            std::string step_str = (target_architecture == "SVE") ? "svcntw()" : "4";
            if (vec_width > 0 && target_architecture == "NEON") step_str = std::to_string(vec_width);

            std::string vec_cond = cond;
            size_t lt = vec_cond.find('<');
            // NEON 需要预留 Tail
            if (target_architecture == "NEON" && lt != std::string::npos) {
                std::string lhs = vec_cond.substr(0, lt);
                std::string rhs = vec_cond.substr(lt + 1);
                vec_cond = lhs + " + " + step_str + " <= " + rhs;
            }

            // [修复] 正确构造步长
            std::string vec_inc = inc;
            if (!loop_var.empty()) vec_inc = loop_var + " += " + step_str;

            code << "    // Vector Loop\n";
            code << "    for (" << init << "; " << vec_cond << "; " << vec_inc << ") {\n";

            if (target_architecture == "SVE") {
                std::string bound = "n";
                if (lt != std::string::npos) bound = cond.substr(lt + 1);
                code << "        pg = svwhilelt_b32(" << (loop_var.empty()?"i":loop_var) << ", " << bound << ");\n";
            }

            traverseAST(forStmt->getBody(), code, false);
            code << "    }\n";

            if (target_architecture != "SVE") {
                code << "    // Tail Loop\n";
                code << "    for (; " << cond << "; " << inc << ") {\n";
                traverseAST(forStmt->getBody(), code, true);
                code << "    }\n";
            }

        } else {
            code << "    for (" << init << "; " << cond << "; " << inc << ") {\n";
            traverseAST(forStmt->getBody(), code, force_scalar);
            code << "    }\n";
        }
    }
    else if (auto* ifStmt = llvm::dyn_cast<clang::IfStmt>(stmt)) {
        std::string cond = generateFallbackCode(ifStmt->getCond());
        code << "    if (" << cond << ") {\n";
        traverseAST(ifStmt->getThen(), code, force_scalar);
        code << "    }\n";
        if (ifStmt->getElse()) {
            code << "    else {\n";
            traverseAST(ifStmt->getElse(), code, force_scalar);
            code << "    }\n";
        }
    }
    else {
        code << "    " << generateFallbackCode(stmt) << ";\n";
    }
}

std::string EnhancedCodeGenerator::generateFromAOD(const std::shared_ptr<AODNode>& node, const std::shared_ptr<AODGraph>& graph) {
    if (!rule_db) return "";

    if (node->getProperty("op_name") == "define") {
        return generateDefineNode(node, graph);
    }

    if (node->getProperty("op_name") == "bf16_dot") {
        auto* rule = rule_db->findRuleForOp("bf16_dot");
        if (rule && rule->target_templates.count(target_architecture)) {
            auto tmpl = rule->target_templates.at(target_architecture);
            std::string code = tmpl.code_template;
            std::map<std::string, std::string> bindings;
            bindings["{{accum_var}}"] = node->getProperty("accum_var");
            bindings["{{input_0}}"] = node->getProperty("base_x");
            bindings["{{input_1}}"] = node->getProperty("base_y");
            for (auto& kv : bindings) {
                size_t pos;
                while ((pos = code.find(kv.first)) != std::string::npos) code.replace(pos, kv.first.length(), kv.second);
            }
            return code;
        }
    }

    std::string op_name = node->getProperty("op_name");
    const OptimizationRule* rule = getRuleForNode(node);

    if (!rule || rule->target_templates.find(target_architecture) == rule->target_templates.end()) {
        return "";
    }

    const auto& tmpl = rule->target_templates.at(target_architecture);
    std::string code = tmpl.code_template;
    std::map<std::string, std::string> bindings;

    auto edges = graph->getIncomingEdges(node->getId());

    const clang::Expr* expr_ptr = llvm::dyn_cast_or_null<clang::Expr>(node->getAstStmt());
    if (expr_ptr) {
        if (auto* call = llvm::dyn_cast<clang::CallExpr>(expr_ptr->IgnoreParenCasts())) {
            for (unsigned i = 0; i < call->getNumArgs(); ++i) {
                bindings["{{input_" + std::to_string(i) + "}}"] = generateFallbackCode(call->getArg(i));
            }
        } else if (auto* bo = llvm::dyn_cast<clang::BinaryOperator>(expr_ptr->IgnoreParenCasts())) {
            bindings["{{input_0}}"] = generateFallbackCode(bo->getLHS());
            bindings["{{input_1}}"] = generateFallbackCode(bo->getRHS());
        }
    }

    for (auto& edge : edges) {
        std::string var = edge->getProperties().variable_name;
        if (var.find("arg_") == 0) {
            int idx = std::stoi(var.substr(4));
            auto src = edge->getSource();
            std::string val;

            if (src->getProperty("op_name") == "define") {
                val = src->getProperty("var_name");
            } else {
                val = generateFromAOD(src, graph);
            }

            if (target_architecture == "SVE" && op_name.find("and") != std::string::npos && src->getProperty("op_name").find("cmp") != std::string::npos) {
                val = "svsel_s8(" + val + ", svdup_s8(0xFF), svdup_s8(0x00))";
            }
            size_t pos;
            while ((pos = val.find("(__m256i *)")) != std::string::npos) val.replace(pos, 11, "(int8_t *)");

            bindings["{{input_" + std::to_string(idx) + "}}"] = val;
        }
    }

    if (target_architecture == "SVE") bindings["{{predicate}}"] = "pg";

    for (const auto& [k, v] : bindings) {
        size_t pos;
        while ((pos = code.find(k)) != std::string::npos) code.replace(pos, k.length(), v);
    }
    return code;
}

std::string EnhancedCodeGenerator::generateDefineNode(const std::shared_ptr<AODNode>& node, const std::shared_ptr<AODGraph>& graph) {
    std::string var_name = node->getProperty("var_name");
    std::string rhs_code;

    auto edges = graph->getIncomingEdges(node->getId());
    std::shared_ptr<AODNode> init_src = nullptr;
    for (auto& edge : edges) {
        if (edge->getProperties().variable_name == "init") {
            init_src = edge->getSource();
            break;
        }
    }

    if (init_src) rhs_code = generateFromAOD(init_src, graph);
    else {
        if (auto* declStmt = llvm::dyn_cast<clang::DeclStmt>(node->getAstStmt())) {
            if (auto* var = llvm::dyn_cast<clang::VarDecl>(declStmt->getSingleDecl())) {
                if (var->getInit()) rhs_code = generateFallbackCode(var->getInit());
            }
        }
    }

    if (rhs_code.empty()) return "";

    std::string type = "auto";
    if (init_src) type = getReturnTypeFromRule(init_src);
    if (type == "auto") {
        if (target_architecture == "SVE") type = "svint8_t";
        else if (target_architecture == "NEON") type = "float32x4_t";
    }

    if ((rhs_code.find("sv") == std::string::npos && rhs_code.find("v") != 0) || var_name == "i" || var_name == "j") {
         if (auto* declStmt = llvm::dyn_cast<clang::DeclStmt>(node->getAstStmt())) {
            if (auto* var = llvm::dyn_cast<clang::VarDecl>(declStmt->getSingleDecl())) type = var->getType().getAsString();
        }
    }

    bool is_const = false;
    if (auto* declStmt = llvm::dyn_cast<clang::DeclStmt>(node->getAstStmt())) {
        if (auto* var = llvm::dyn_cast<clang::VarDecl>(declStmt->getSingleDecl())) {
            if (var->getType().isConstQualified()) is_const = true;
        }
    }
    if (is_const && type.find("const") == std::string::npos) type = "const " + type;

    return type + " " + var_name + " = " + rhs_code;
}

const OptimizationRule* EnhancedCodeGenerator::getRuleForNode(const std::shared_ptr<AODNode>& node) {
    if (!rule_db) return nullptr;
    return rule_db->findRuleForOp(node->getProperty("op_name"));
}

std::string EnhancedCodeGenerator::getReturnTypeFromRule(const std::shared_ptr<AODNode>& node) {
    auto* rule = getRuleForNode(node);
    if (rule && rule->target_templates.count(target_architecture)) {
        auto& tmpl = rule->target_templates.at(target_architecture);
        if (tmpl.performance_hints.count("return_type")) return tmpl.performance_hints.at("return_type");
    }
    return "auto";
}

std::string EnhancedCodeGenerator::applyReplacements(std::string code, const TransformTemplate& tmpl) {
    for (const auto& kv : tmpl.arg_replacements) {
        std::string from = kv.first;
        std::string to = kv.second;
        size_t pos = 0;
        while ((pos = code.find(from, pos)) != std::string::npos) {
            code.replace(pos, from.length(), to);
            pos += to.length();
        }
    }
    return code;
}

std::string EnhancedCodeGenerator::tryApplyRules(const std::shared_ptr<AODNode>& node, const std::shared_ptr<AODGraph>& graph) {
    return generateFromAOD(node, graph);
}

std::string EnhancedCodeGenerator::generateFallbackCode(const clang::Stmt* stmt) {
    if (!stmt) return "";
    std::string code;
    llvm::raw_string_ostream os(code);
    stmt->printPretty(os, nullptr, ast_context.getPrintingPolicy());
    std::string s = os.str();
    while (!s.empty() && (s.back() == ';' || s.back() == '\n' || s.back() == ' ')) s.pop_back();
    return s;
}

std::string EnhancedCodeGenerator::generateOutputVar(const std::shared_ptr<AODNode>& node) {
    return "vec_" + std::to_string(node->getId());
}

} // namespace aodsolve
