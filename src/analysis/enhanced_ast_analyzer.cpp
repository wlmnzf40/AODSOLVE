#include "analysis/enhanced_ast_analyzer.h"

namespace aodsolve {

    // 简化实现
    EnhancedASTAnalyzer::EnhancedASTAnalyzer(clang::ASTContext& ctx) : ast_context(ctx), source_manager(ctx.getSourceManager()) {
        // 初始化
    }

    ASTAnalysisResult EnhancedASTAnalyzer::analyzeFunction(const clang::FunctionDecl* func) {
        ASTAnalysisResult result;

        // 检查函数是否在主文件中
        if (!source_manager.isInMainFile(func->getLocation())) {
            result.function_name = func->getNameAsString() + " (skipped - in header)";
            result.is_vectorizable = false;
            return result;
        }

        result.function_name = func->getNameAsString();
        result.is_vectorizable = true;
        result.simd_instruction_set = "AVX2";
        result.estimated_speedup = 2.0;
        return result;
    }

    std::vector<ASTAnalysisResult> EnhancedASTAnalyzer::analyzeTranslationUnit() {
        std::vector<ASTAnalysisResult> results;

        auto* tu = ast_context.getTranslationUnitDecl();

        // 只分析主文件中的函数
        for (auto* decl : tu->decls()) {
            // 【关键】跳过头文件中的声明
            if (!source_manager.isInMainFile(decl->getLocation())) {
                continue;
            }

            if (auto* func = clang::dyn_cast<clang::FunctionDecl>(decl)) {
                if (func->hasBody() && func->isThisDeclarationADefinition()) {
                    results.push_back(analyzeFunction(func));
                }
            }
        }

        return results;
    }

} // namespace aodsolve