#pragma once

#include "generation/enhanced_code_generator.h"
#include "conversion/enhanced_cpg_to_aod_converter.h" // 包含定义
#include "analysis/integrated_cpg_analyzer.h"
#include "aod/enhanced_aod_graph.h"

#include <memory>
#include <map>
#include <set>
#include <vector>
#include <string>
#include <iostream>
#include <fstream>
#include <chrono>

namespace aodsolve {

// 完整分析结果
struct ComprehensiveAnalysisResult {
    bool successful = false;
    std::string analysis_report;
    std::vector<std::string> warnings;
    std::vector<std::string> errors;

    std::map<const clang::FunctionDecl*, ASTAnalysisResult> function_analyses;
    std::map<const clang::FunctionDecl*, ConversionResult> conversion_results;
    std::map<const clang::FunctionDecl*, CodeGenerationResult> code_results;

    InterproceduralDataFlow interprocedural_flow;

    double total_speedup_estimate = 0.0;
    std::string best_architecture;
    std::vector<std::string> recommended_optimizations;
    std::map<std::string, double> architecture_performance;

    int functions_analyzed = 0;
    int total_nodes = 0;
    int total_edges = 0;
    int simd_opportunities = 0;
};

class AODSolveMainAnalyzer {
private:
    clang::ASTContext& ast_context;
    clang::SourceManager& source_manager;

    std::unique_ptr<IntegratedCPGAnalyzer> cpg_analyzer;
    std::unique_ptr<EnhancedCPGToAODConverter> converter;
    std::unique_ptr<EnhancedCodeGenerator> code_generator;

    std::string target_architecture;
    int optimization_level;
    bool enable_interprocedural_analysis;
    bool generate_visualizations;
    bool generate_reports;
    bool save_intermediate_results;

public:
    explicit AODSolveMainAnalyzer(clang::ASTContext& ctx);
    ~AODSolveMainAnalyzer() = default;

    ComprehensiveAnalysisResult analyzeTranslationUnit();
    ComprehensiveAnalysisResult analyzeFunction(const clang::FunctionDecl* func);
    ComprehensiveAnalysisResult analyzeFile(const std::string& filename);

    void setTargetArchitecture(const std::string& arch) { target_architecture = arch; }
    void setOptimizationLevel(int level) { optimization_level = level; }
    void enableInterproceduralAnalysis(bool enable) { enable_interprocedural_analysis = enable; }

    std::string generateComprehensiveReport(const ComprehensiveAnalysisResult& result);
    std::string generatePerformanceReport(const ComprehensiveAnalysisResult& result);

private:
    void initializeComponents();
};

} // namespace aodsolve
