#pragma once

#include "aod/optimization_rule_system.h"
#include "tools/aodsolve_main_analyzer.h"
#include "conversion/enhanced_cpg_to_aod_converter.h"
#include "generation/enhanced_code_generator.h"

#include <clang/AST/RecursiveASTVisitor.h>
#include <clang/AST/Stmt.h>
#include <clang/AST/Expr.h>
#include <clang/AST/Decl.h>

#include <map>
#include <string>
#include <vector>
#include <sstream>
#include <memory>

namespace aodsolve {

/**
 * 规则驱动的代码生成器 - 真正从规则模板生成代码
 * 
 * 这个类负责:
 * 1. 从AST提取绑定
 * 2. 查找匹配的规则
 * 3. 应用规则模板生成代码
 */
class RuleDrivenCodeGenerator {
public:
    RuleDrivenCodeGenerator(
        RuleDatabase* rule_db,
        clang::ASTContext* ast_ctx,
        clang::SourceManager* src_mgr)
        : rule_db_(rule_db)
        , ast_context_(ast_ctx)
        , source_manager_(src_mgr)
    {}
    
    /**
     * 为循环生成向量化代码
     */
    std::string generateVectorizedLoop(
        const clang::ForStmt* loop,
        const std::string& target_arch);
    
    /**
     * 为包含函数调用的循环生成代码
     */
    std::string generateLoopWithInlinedCalls(
        const clang::ForStmt* loop,
        const std::vector<const clang::CallExpr*>& calls,
        const std::string& target_arch);
    
    /**
     * 生成内联的向量化函数体
     */
    std::string generateInlinedVectorFunction(
        const clang::FunctionDecl* func,
        const std::map<std::string, std::string>& arg_bindings,
        const std::string& target_arch);
    
    /**
     * 从SIMD指令生成转换后的代码
     */
    std::string generateConvertedInstruction(
        const clang::CallExpr* simd_call,
        const std::string& target_arch);

    std::string generateGenericInlinedFunction(
    const clang::FunctionDecl* func,
    const std::map<std::string, std::string>& arg_bindings,
    const std::string& target_arch);
    
private:
    RuleDatabase* rule_db_;
    clang::ASTContext* ast_context_;
    clang::SourceManager* source_manager_;
    
    // === 绑定提取 ===
    
    /**
     * 从循环提取绑定
     */
    std::map<std::string, std::string> extractLoopBindings(
        const clang::ForStmt* loop);
    
    /**
     * 从函数提取绑定
     */
    std::map<std::string, std::string> extractFunctionBindings(
        const clang::FunctionDecl* func);
    
    /**
     * 从SIMD调用提取绑定
     */
    std::map<std::string, std::string> extractSIMDCallBindings(
        const clang::CallExpr* call);
    
    /**
     * 从表达式提取绑定
     */
    std::map<std::string, std::string> extractExpressionBindings(
        const clang::Expr* expr);
    
    // === 模式识别 ===
    
    /**
     * 识别循环模式
     */
    std::string identifyLoopPattern(const clang::ForStmt* loop);
    
    /**
     * 识别函数模式
     */
    std::string identifyFunctionPattern(const clang::FunctionDecl* func);
    
    /**
     * 识别条件模式
     */
    std::string identifyConditionalPattern(const clang::IfStmt* if_stmt);
    
    // === 模板应用 ===
    
    /**
     * 应用规则模板
     */
    std::string applyRuleTemplate(
        const TransformTemplate& tmpl,
        const std::map<std::string, std::string>& bindings);
    
    /**
     * 替换模板占位符
     */
    std::string replacePlaceholders(
        const std::string& template_str,
        const std::map<std::string, std::string>& bindings);
    
    // === 辅助函数 ===
    
    /**
     * 推断元素类型
     */
    std::string inferElementType(const clang::QualType& type);
    
    /**
     * 推断NEON类型
     */
    std::string inferNEONType(const std::string& element_type);
    
    /**
     * 推断操作名称
     */
    std::string inferOperationName(const clang::BinaryOperator* op);
    
    /**
     * 提取数组名称
     */
    std::string extractArrayName(const clang::Expr* expr);
    
    /**
     * 分析数组访问模式
     */
    std::string analyzeArrayAccessPattern(
        const clang::ArraySubscriptExpr* access,
        const std::string& loop_var);
};

// ============================================================================
// 实现: 循环向量化代码生成
// ============================================================================

std::string RuleDrivenCodeGenerator::generateVectorizedLoop(
    const clang::ForStmt* loop,
    const std::string& target_arch) {
    
    // 1. 识别循环模式
    std::string pattern = identifyLoopPattern(loop);
    
    // 2. 查找匹配的规则
    auto rules = rule_db_->queryRules("loop_vectorization");
    OptimizationRule* matched_rule = nullptr;
    
    for (auto* rule : rules) {
        if (rule->source_pattern.pattern_id == pattern) {
            matched_rule = rule;
            break;
        }
    }
    
    if (!matched_rule) {
        return "// No matching rule found for loop pattern: " + pattern;
    }
    
    // 3. 提取绑定
    auto bindings = extractLoopBindings(loop);
    
    // 4. 获取目标架构的模板
    auto tmpl_it = matched_rule->target_templates.find(target_arch);
    if (tmpl_it == matched_rule->target_templates.end()) {
        return "// No template for target architecture: " + target_arch;
    }
    
    // 5. 应用模板生成代码
    return applyRuleTemplate(tmpl_it->second, bindings);
}

// ============================================================================
// 实现: 提取循环绑定
// ============================================================================

std::map<std::string, std::string> RuleDrivenCodeGenerator::extractLoopBindings(
    const clang::ForStmt* loop) {
    
    std::map<std::string, std::string> bindings;
    
    // 提取循环变量
    if (auto* init = loop->getInit()) {
        if (auto* decl_stmt = clang::dyn_cast<clang::DeclStmt>(init)) {
            if (decl_stmt->isSingleDecl()) {
                if (auto* var_decl = clang::dyn_cast<clang::VarDecl>(decl_stmt->getSingleDecl())) {
                    bindings["{{loop_var}}"] = var_decl->getNameAsString();
                    bindings["{{index_type}}"] = var_decl->getType().getAsString();
                    
                    // 提取起始值
                    if (auto* init_expr = var_decl->getInit()) {
                        if (auto* int_lit = clang::dyn_cast<clang::IntegerLiteral>(init_expr)) {
                            bindings["{{start_value}}"] = std::to_string(
                                int_lit->getValue().getLimitedValue());
                        }
                    }
                }
            }
        }
    }
    
    // 提取结束条件
    if (auto* cond = loop->getCond()) {
        if (auto* bin_op = clang::dyn_cast<clang::BinaryOperator>(cond)) {
            if (auto* rhs = clang::dyn_cast<clang::DeclRefExpr>(bin_op->getRHS())) {
                bindings["{{end_value}}"] = rhs->getDecl()->getNameAsString();
            }
        }
    }
    
    // 分析循环体,提取数组访问
    if (auto* body = loop->getBody()) {
        class ArrayAccessExtractor : public clang::RecursiveASTVisitor<ArrayAccessExtractor> {
        public:
            std::map<std::string, std::string>& bindings;
            const std::string& loop_var;
            int input_count = 0;
            int output_count = 0;
            
            ArrayAccessExtractor(std::map<std::string, std::string>& b, const std::string& lv)
                : bindings(b), loop_var(lv) {}
            
            bool VisitArraySubscriptExpr(clang::ArraySubscriptExpr* access) {
                auto* base = access->getBase()->IgnoreImpCasts();
                
                if (auto* decl_ref = clang::dyn_cast<clang::DeclRefExpr>(base)) {
                    std::string array_name = decl_ref->getDecl()->getNameAsString();
                    
                    // 检查是否使用循环变量作为索引
                    if (auto* idx = clang::dyn_cast<clang::DeclRefExpr>(
                            access->getIdx()->IgnoreImpCasts())) {
                        if (idx->getDecl()->getNameAsString() == loop_var) {
                            // 这是顺序访问
                            std::string key = "{{input_" + std::to_string(input_count++) + "}}";
                            bindings[key] = array_name;
                        }
                    }
                }
                return true;
            }
            
            bool VisitBinaryOperator(clang::BinaryOperator* op) {
                if (op->isAssignmentOp()) {
                    // 找到赋值的左侧(输出)
                    if (auto* array_access = clang::dyn_cast<clang::ArraySubscriptExpr>(
                            op->getLHS()->IgnoreImpCasts())) {
                        if (auto* base = clang::dyn_cast<clang::DeclRefExpr>(
                                array_access->getBase()->IgnoreImpCasts())) {
                            bindings["{{output}}"] = base->getDecl()->getNameAsString();
                        }
                    }
                }
                return true;
            }
        };
        
        ArrayAccessExtractor extractor(bindings, bindings["{{loop_var}}"]);
        extractor.TraverseStmt(const_cast<clang::Stmt*>(body));
    }
    
    // 推断元素类型
    // 这里需要从数组访问的类型推断
    bindings["{{element_type}}"] = "f32";  // 默认,应该从AST推断
    bindings["{{width}}"] = "b32";
    
    return bindings;
}

// ============================================================================
// 实现: 识别循环模式
// ============================================================================

std::string RuleDrivenCodeGenerator::identifyLoopPattern(
    const clang::ForStmt* loop) {
    
    // 检查是否是简单的顺序循环
    bool has_sequential_access = true;
    bool has_condition = false;
    bool has_reduction = false;
    bool has_function_call = false;
    
    if (auto* body = loop->getBody()) {
        class PatternDetector : public clang::RecursiveASTVisitor<PatternDetector> {
        public:
            bool& has_condition;
            bool& has_reduction;
            bool& has_function_call;
            
            PatternDetector(bool& c, bool& r, bool& f)
                : has_condition(c), has_reduction(r), has_function_call(f) {}
            
            bool VisitIfStmt(clang::IfStmt* if_stmt) {
                has_condition = true;
                return true;
            }
            
            bool VisitCompoundAssignOperator(clang::CompoundAssignOperator* op) {
                has_reduction = true;
                return true;
            }
            
            bool VisitCallExpr(clang::CallExpr* call) {
                has_function_call = true;
                return true;
            }
        };
        
        PatternDetector detector(has_condition, has_reduction, has_function_call);
        detector.TraverseStmt(const_cast<clang::Stmt*>(body));
    }
    
    // 确定模式
    if (has_function_call) {
        return "loop_with_call";
    } else if (has_reduction) {
        return "reduction_loop";
    } else if (has_condition) {
        return "conditional_loop";
    } else {
        return "simple_sequential_loop";
    }
}

// ============================================================================
// 实现: 应用规则模板
// ============================================================================

std::string RuleDrivenCodeGenerator::applyRuleTemplate(
    const TransformTemplate& tmpl,
    const std::map<std::string, std::string>& bindings) {
    
    std::string code = tmpl.code_template;
    
    // 替换所有占位符
    code = replacePlaceholders(code, bindings);
    
    return code;
}

std::string RuleDrivenCodeGenerator::replacePlaceholders(
    const std::string& template_str,
    const std::map<std::string, std::string>& bindings) {
    
    std::string result = template_str;
    
    // 按长度排序,先替换长的占位符
    std::vector<std::pair<std::string, std::string>> sorted_bindings(
        bindings.begin(), bindings.end());
    
    std::sort(sorted_bindings.begin(), sorted_bindings.end(),
        [](const auto& a, const auto& b) { return a.first.length() > b.first.length(); });
    
    for (const auto& [placeholder, value] : sorted_bindings) {
        size_t pos = 0;
        while ((pos = result.find(placeholder, pos)) != std::string::npos) {
            result.replace(pos, placeholder.length(), value);
            pos += value.length();
        }
    }
    
    return result;
}

// ============================================================================
// 实现: 包含函数调用的循环
// ============================================================================

std::string RuleDrivenCodeGenerator::generateLoopWithInlinedCalls(
    const clang::ForStmt* loop,
    const std::vector<const clang::CallExpr*>& calls,
    const std::string& target_arch) {
    
    // 1. 查找loop_with_function_call规则
    auto rules = rule_db_->queryRules("loop_vectorization");
    OptimizationRule* loop_rule = nullptr;
    
    for (auto* rule : rules) {
        if (rule->rule_id == "loop_with_function_call_vectorization") {
            loop_rule = rule;
            break;
        }
    }
    
    if (!loop_rule) {
        return "// No rule for loop with function calls";
    }
    
    // 2. 提取循环绑定
    auto loop_bindings = extractLoopBindings(loop);
    
    // 3. 为每个函数调用生成内联代码
    std::stringstream inlined_code;
    
    for (auto* call : calls) {
        const clang::FunctionDecl* callee = call->getDirectCallee();
        if (!callee) continue;
        
        // 提取参数绑定
        std::map<std::string, std::string> arg_bindings;
        for (unsigned i = 0; i < call->getNumArgs(); ++i) {
            auto* arg = call->getArg(i);
            if (auto* decl_ref = clang::dyn_cast<clang::DeclRefExpr>(arg->IgnoreImpCasts())) {
                std::string param_name = callee->getParamDecl(i)->getNameAsString();
                std::string arg_name = decl_ref->getDecl()->getNameAsString();
                arg_bindings[param_name] = arg_name;
            }
        }
        
        // 生成内联的向量化代码
        std::string inlined = generateInlinedVectorFunction(callee, arg_bindings, target_arch);
        inlined_code << inlined << "\n";
    }
    
    // 4. 将内联代码插入循环模板
    loop_bindings["{{inlined_vector_code}}"] = inlined_code.str();
    
    // 5. 应用模板
    auto tmpl_it = loop_rule->target_templates.find(target_arch);
    if (tmpl_it == loop_rule->target_templates.end()) {
        return "// No template for target: " + target_arch;
    }
    
    return applyRuleTemplate(tmpl_it->second, loop_bindings);
}

// ============================================================================
// 实现: 生成内联的向量化函数
// ============================================================================

std::string RuleDrivenCodeGenerator::generateInlinedVectorFunction(
    const clang::FunctionDecl* func,
    const std::map<std::string, std::string>& arg_bindings,
    const std::string& target_arch) {
    
    // 1. 识别函数模式
    std::string pattern = identifyFunctionPattern(func);
    
    // 2. 查找匹配的内联规则
    auto rules = rule_db_->queryRules("function_inline");
    OptimizationRule* matched_rule = nullptr;
    
    for (auto* rule : rules) {
        if (rule->source_pattern.pattern_id == pattern) {
            matched_rule = rule;
            break;
        }
    }
    
    if (!matched_rule) {
        // 没有预定义规则,分析函数体直接转换
        return generateGenericInlinedFunction(func, arg_bindings, target_arch);
    }
    
    // 3. 提取函数绑定
    auto func_bindings = extractFunctionBindings(func);
    
    // 4. 合并参数绑定
    for (const auto& [param, arg] : arg_bindings) {
        func_bindings["{{" + param + "}}"] = arg + "_vec";
    }
    
    // 5. 应用模板
    auto tmpl_it = matched_rule->target_templates.find(target_arch);
    if (tmpl_it == matched_rule->target_templates.end()) {
        return "// No inline template for: " + target_arch;
    }
    
    return applyRuleTemplate(tmpl_it->second, func_bindings);
}

// ============================================================================
// 实现: 通用函数内联(当没有预定义规则时)
// ============================================================================

std::string RuleDrivenCodeGenerator::generateGenericInlinedFunction(
    const clang::FunctionDecl* func,
    const std::map<std::string, std::string>& arg_bindings,
    const std::string& target_arch) {
    
    std::stringstream code;
    
    // 遍历函数体,转换每个语句
    if (func->hasBody()) {
        auto* body = func->getBody();
        
        class BodyConverter : public clang::RecursiveASTVisitor<BodyConverter> {
        public:
            std::stringstream& code;
            const std::map<std::string, std::string>& arg_bindings;
            const std::string& target_arch;
            
            BodyConverter(std::stringstream& c, 
                         const std::map<std::string, std::string>& ab,
                         const std::string& arch)
                : code(c), arg_bindings(ab), target_arch(arch) {}
            
            bool VisitBinaryOperator(clang::BinaryOperator* op) {
                // 转换二元操作
                std::string lhs = getExprString(op->getLHS());
                std::string rhs = getExprString(op->getRHS());
                std::string operation = getOperationName(op->getOpcode());
                
                if (target_arch == "SVE") {
                    code << "svfloat32_t result_vec = sv" << operation 
                         << "_f32_z(pg, " << lhs << ", " << rhs << ");\n";
                }
                
                return true;
            }
            
        private:
            std::string getExprString(const clang::Expr* expr) {
                // 简化版本
                if (auto* decl_ref = clang::dyn_cast<clang::DeclRefExpr>(expr->IgnoreImpCasts())) {
                    std::string name = decl_ref->getDecl()->getNameAsString();
                    auto it = arg_bindings.find(name);
                    if (it != arg_bindings.end()) {
                        return it->second;
                    }
                    return name + "_vec";
                }
                return "unknown";
            }
            
            std::string getOperationName(clang::BinaryOperatorKind opcode) {
                switch (opcode) {
                    case clang::BO_Add: return "add";
                    case clang::BO_Sub: return "sub";
                    case clang::BO_Mul: return "mul";
                    default: return "unknown";
                }
            }
        };
        
        BodyConverter converter(code, arg_bindings, target_arch);
        converter.TraverseStmt(const_cast<clang::Stmt*>(body));
    }
    
    return code.str();
}

// ============================================================================
// 实现: 识别函数模式
// ============================================================================

std::string RuleDrivenCodeGenerator::identifyFunctionPattern(
    const clang::FunctionDecl* func) {
    
    std::string func_name = func->getNameAsString();
    
    // 检查函数名模式
    if (func_name.find("min") != std::string::npos ||
        func_name.find("max") != std::string::npos) {
        return "minmax_call";
    }
    
    if (func_name.find("clamp") != std::string::npos ||
        func_name.find("clip") != std::string::npos) {
        return "clamp_call";
    }
    
    if (func_name.find("abs") != std::string::npos) {
        return "abs_call";
    }
    
    // 如果没有匹配,检查函数体
    if (func->hasBody()) {
        auto* body = func->getBody();
        
        // 检查是否只有简单的算术运算
        class SimpleArithmeticChecker : public clang::RecursiveASTVisitor<SimpleArithmeticChecker> {
        public:
            bool is_simple = true;
            
            bool VisitCallExpr(clang::CallExpr*) {
                is_simple = false;
                return false;
            }
            
            bool VisitIfStmt(clang::IfStmt*) {
                is_simple = false;
                return false;
            }
        };
        
        SimpleArithmeticChecker checker;
        checker.TraverseStmt(const_cast<clang::Stmt*>(body));
        
        if (checker.is_simple) {
            return "arithmetic_function";
        }
    }
    
    return "unknown_function";
}

} // namespace aodsolve