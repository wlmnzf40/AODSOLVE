#pragma once

#include "generation/enhanced_code_generator.h"
#include "conversion/enhanced_cpg_to_aod_converter.h"
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

// ============================================
// 完整分析结果
// ============================================

struct ComprehensiveAnalysisResult {
    bool successful = false;
    std::string analysis_report;
    std::vector<std::string> warnings;
    std::vector<std::string> errors;

    // 分析结果
    std::map<const clang::FunctionDecl*, ASTAnalysisResult> function_analyses;
    std::map<const clang::FunctionDecl*, ConversionResult> conversion_results;
    std::map<const clang::FunctionDecl*, CodeGenerationResult> code_results;
    InterproceduralDataFlow interprocedural_flow;

    // 性能评估
    double total_speedup_estimate = 0.0;
    std::string best_architecture;
    std::vector<std::string> recommended_optimizations;
    std::map<std::string, double> architecture_performance;

    // 统计信息
    int functions_analyzed = 0;
    int total_nodes = 0;
    int total_edges = 0;
    int simd_opportunities = 0;
    int optimized_loops = 0;
    int dead_code_found = 0;

    // 时间统计
    std::chrono::milliseconds analysis_time{0};
    std::chrono::milliseconds conversion_time{0};
    std::chrono::milliseconds generation_time{0};
};

// 主要的AODSOLVE分析引擎
class AODSolveMainAnalyzer {
private:
    clang::ASTContext& ast_context;
    clang::SourceManager& source_manager;

    // 组件
    std::unique_ptr<IntegratedCPGAnalyzer> cpg_analyzer;
    std::unique_ptr<EnhancedCPGToAODConverter> converter;
    std::unique_ptr<EnhancedCodeGenerator> code_generator;

    // 配置
    std::string target_architecture;
    int optimization_level;
    bool enable_interprocedural_analysis;
    bool generate_visualizations;
    bool generate_reports;
    bool save_intermediate_results;

    // 结果缓存
    std::map<const clang::FunctionDecl*, ComprehensiveAnalysisResult> analysis_cache;
    std::map<std::string, std::string> intermediate_files;

public:
    explicit AODSolveMainAnalyzer(clang::ASTContext& ctx);
    ~AODSolveMainAnalyzer() = default;

    // 主要分析接口
    ComprehensiveAnalysisResult analyzeTranslationUnit();
    ComprehensiveAnalysisResult analyzeFunction(const clang::FunctionDecl* func);
    ComprehensiveAnalysisResult analyzeFile(const std::string& filename);

    // 复杂案例分析
    ComprehensiveAnalysisResult analyzeStringProcessingCase(const std::string& source_code);
    ComprehensiveAnalysisResult analyzeBitwiseOperationsCase(const std::string& source_code);
    ComprehensiveAnalysisResult analyzeUTF8ValidationCase(const std::string& source_code);

    // 批量分析
    std::vector<ComprehensiveAnalysisResult> analyzeMultipleFunctions(const std::vector<const clang::FunctionDecl*>& functions);
    std::map<std::string, ComprehensiveAnalysisResult> analyzeMultipleFiles(const std::vector<std::string>& filenames);

    // 架构优化分析
    ComprehensiveAnalysisResult analyzeForAVX();
    ComprehensiveAnalysisResult analyzeForAVX2();
    ComprehensiveAnalysisResult analyzeForAVX512();
    ComprehensiveAnalysisResult analyzeForNEON();
    ComprehensiveAnalysisResult analyzeForSVE();
    ComprehensiveAnalysisResult analyzeForSVE2();
    ComprehensiveAnalysisResult findBestArchitecture();

    // 交互式分析
    void startInteractiveAnalysis();
    void analyzeSpecificFunction(const std::string& function_name);
    void showFunctionAnalysis(const std::string& function_name);
    void showOptimizationSuggestions(const std::string& function_name);
    void showPerformanceComparison();

    // 配置管理
    void setTargetArchitecture(const std::string& arch) { target_architecture = arch; }
    void setOptimizationLevel(int level) { optimization_level = level; }
    void enableInterproceduralAnalysis(bool enable) { enable_interprocedural_analysis = enable; }
    void enableVisualizations(bool enable) { generate_visualizations = enable; }
    void enableReportGeneration(bool enable) { generate_reports = enable; }
    void saveIntermediateResults(bool save) { save_intermediate_results = save; }

    // 报告生成
    std::string generateComprehensiveReport(const ComprehensiveAnalysisResult& result);
    std::string generatePerformanceReport(const ComprehensiveAnalysisResult& result);
    std::string generateOptimizationReport(const ComprehensiveAnalysisResult& result);
    std::string generateCallGraphReport();
    std::string generateSIMDAnalysisReport();

    // 可视化生成
    void generateFunctionVisualization(const clang::FunctionDecl* func, const std::string& output_dir);
    void generateCallGraphVisualization(const std::string& output_dir);
    void generatePerformanceVisualization(const ComprehensiveAnalysisResult& result, const std::string& output_dir);
    void generateOptimizationVisualization(const ComprehensiveAnalysisResult& result, const std::string& output_dir);

    // 文件输出
    void saveAnalysisToFile(const ComprehensiveAnalysisResult& result, const std::string& filename);
    void saveCodeToFile(const CodeGenerationResult& result, const std::string& filename);
    void saveVisualizationToFile(const std::string& dot_content, const std::string& filename);
    void exportResultsToJSON(const ComprehensiveAnalysisResult& result, const std::string& filename);
    void exportResultsToXML(const ComprehensiveAnalysisResult& result, const std::string& filename);

    // 比较分析
    ComprehensiveAnalysisResult compareArchitectures(const std::vector<std::string>& architectures);
    ComprehensiveAnalysisResult compareOptimizationLevels(const std::vector<int>& levels);
    void compareOptimizationStrategies(const std::string& function_name);

    // 调试支持
    void enableDebugMode();
    void setVerboseOutput(bool verbose);
    void enableTracing();
    void saveDebugInfo(const std::string& filename);

    // 工具方法
    const IntegratedCPGAnalyzer& getCPGAnalyzer() const { return *cpg_analyzer; }
    const EnhancedCPGToAODConverter& getConverter() const { return *converter; }
    const EnhancedCodeGenerator& getCodeGenerator() const { return *code_generator; }
    void clearCache() { analysis_cache.clear(); intermediate_files.clear(); }

    // 高级分析
    std::vector<std::string> findAllVectorizationOpportunities();
    std::vector<std::string> findAllParallelizationOpportunities();
    std::vector<std::string> findAllMemoryOptimizations();
    std::vector<std::string> findAllLoopOptimizations();

    // 性能预测
    struct PerformancePrediction {
        double execution_time_scalar = 0.0;
        double execution_time_vectorized = 0.0;
        double speedup_ratio = 0.0;
        std::string limiting_factor;
        std::map<std::string, double> architecture_timings;
        std::vector<std::string> optimization_impact;
    };

    PerformancePrediction predictPerformance(const clang::FunctionDecl* func);
    std::string generatePerformancePredictionReport(const PerformancePrediction& prediction);

    // 质量保证
    bool validateAnalysisResult(const ComprehensiveAnalysisResult& result);
    std::vector<std::string> findAnalysisQualityIssues(const ComprehensiveAnalysisResult& result);
    void improveAnalysisQuality(ComprehensiveAnalysisResult& result);

    // 集成测试
    void runUnitTests();
    void runIntegrationTests();
    void runPerformanceTests();
    bool validateAgainstBenchmarks(const ComprehensiveAnalysisResult& result);

private:
    // 内部实现方法
    void initializeComponents();
    ComprehensiveAnalysisResult performSingleFunctionAnalysis(const clang::FunctionDecl* func);
    void updateProgress(const std::string& message, int progress, int total);

    // 缓存管理
    ComprehensiveAnalysisResult getCachedAnalysis(const clang::FunctionDecl* func);
    void cacheAnalysis(const clang::FunctionDecl* func, const ComprehensiveAnalysisResult& result);
    bool isCacheValid(const clang::FunctionDecl* func) const;

    // 结果处理
    void combineResults(ComprehensiveAnalysisResult& combined, const ComprehensiveAnalysisResult& individual);
    void computeStatistics(ComprehensiveAnalysisResult& result);
    void generateRecommendations(ComprehensiveAnalysisResult& result);

    // 报告生成辅助
    std::string formatAnalysisResult(const clang::FunctionDecl* func, const ASTAnalysisResult& result);
    std::string formatConversionResult(const clang::FunctionDecl* func, const ConversionResult& result);
    std::string formatCodeResult(const clang::FunctionDecl* func, const CodeGenerationResult& result);
    std::string formatPerformanceAnalysis(const ComprehensiveAnalysisResult& result);

    // 错误处理
    void handleAnalysisError(const std::string& error, ComprehensiveAnalysisResult& result);
    void logWarning(const std::string& warning, ComprehensiveAnalysisResult& result);
    void logInfo(const std::string& info, ComprehensiveAnalysisResult& result);

    // 性能监控
    void startTiming();
    std::chrono::milliseconds endTiming();
    void logPerformanceMetrics(const ComprehensiveAnalysisResult& result);

    // 工具方法
    std::string getFunctionSignature(const clang::FunctionDecl* func) const;
    std::string getSourceFileName(const clang::FunctionDecl* func) const;
    int getSourceLineNumber(const clang::FunctionDecl* func) const;
    std::string getFunctionLocation(const clang::FunctionDecl* func) const;

    // 验证方法
    bool validateFunction(const clang::FunctionDecl* func);
    bool validateSourceCode(const std::string& source);
    bool validateArchitecture(const std::string& arch);
    bool validateOptimizationLevel(int level);
};

// 命令行工具支持
class AODSolveCommandLineTool {
private:
    std::unique_ptr<AODSolveMainAnalyzer> analyzer;
    std::string input_file;
    std::string output_dir;
    std::string target_architecture;
    int optimization_level;
    bool verbose;
    bool generate_reports;
    bool generate_visualizations;

public:
    AODSolveCommandLineTool();
    ~AODSolveCommandLineTool() = default;

    int run(int argc, char* argv[]);
    void printUsage();
    void printVersion();

    // 解析命令行参数
    bool parseCommandLine(int argc, char* argv[]);
    void validateArguments();

    // 执行分析
    int analyzeFile();
    int analyzeFunction();
    int analyzeBestArchitecture();
    int compareArchitectures();
    int generateReport();
    int generateVisualization();

    // 批量处理
    int analyzeDirectory();
    int processMultipleFiles(const std::vector<std::string>& files);

    // 帮助和调试
    void printHelp();
    void printExamples();
    void printArchitectureInfo();
    void runDiagnostic();

private:
    void setupAnalyzer();
    void printProgress(const std::string& message, int progress);
    std::string formatFileSize(size_t size);
    std::string formatDuration(std::chrono::milliseconds duration);
};

// 批处理工具
class AODSolveBatchProcessor {
private:
    std::vector<std::string> input_files;
    std::string output_dir;
    AODSolveMainAnalyzer* analyzer;

public:
    explicit AODSolveBatchProcessor(AODSolveMainAnalyzer* a) : analyzer(a) {}

    void addInputFile(const std::string& file) { input_files.push_back(file); }
    void addInputFiles(const std::vector<std::string>& files);
    void setOutputDirectory(const std::string& dir) { output_dir = dir; }

    // 批量分析
    void analyzeAllFiles();
    void analyzeForBestArchitecture();
    void generateAllReports();
    void generateAllVisualizations();

    // 结果处理
    void saveBatchResults(const std::map<std::string, ComprehensiveAnalysisResult>& results);
    void generateBatchSummary(const std::map<std::string, ComprehensiveAnalysisResult>& results);
    void compareBatchResults(const std::map<std::string, ComprehensiveAnalysisResult>& results);

    // 并行处理
    void analyzeFilesInParallel(int num_threads = 4);
    void analyzeFilesWithProgressBar();

    // 报告生成
    std::string generateBatchReport(const std::map<std::string, ComprehensiveAnalysisResult>& results);
    void saveBatchReport(const std::string& report, const std::string& filename);
};

} // namespace aodsolve