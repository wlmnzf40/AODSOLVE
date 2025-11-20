#include "tools/aodsolve_main_analyzer.h"
#include "conversion/enhanced_cpg_to_aod_converter.h"
#include "generation/enhanced_code_generator.h"
#include "analysis/CPGAnnotation.h"
#include "analysis/loop_vectorization_analyzer.h"

#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <memory>
#include <chrono>
#include <clang/Tooling/CommonOptionsParser.h>
#include <clang/Tooling/Tooling.h>
#include <llvm/Support/CommandLine.h>
#include <iomanip>

using namespace aodsolve;

// 演示程序主类
class AODSolveDemo {
private:
    std::string source_code;
    std::string file_name;
    bool use_custom_code;

public:
    AODSolveDemo() : use_custom_code(false) {}

    // 运行演示
    void runStringProcessingDemo();
    void runBitwiseOperationsDemo();
    void runUTF8ValidationDemo();
    void runComplexControlFlowDemo();
    void runInterproceduralAnalysisDemo();
    void runFullComparisonDemo();

    // 新增: 案例4和案例5
    void runScalarLoopVectorizationDemo();
    void runCrossFunctionVectorizationDemo();

    // 设置自定义代码
    void setSourceCode(const std::string& code, const std::string& filename = "custom.cpp") {
        source_code = code;
        file_name = filename;
        use_custom_code = true;
    }

    // 显示分析结果
    void displayAnalysisResults(const ComprehensiveAnalysisResult& result);
    void displayPerformanceComparison(const std::vector<ComprehensiveAnalysisResult>& results);
    void displayOptimizationSuggestions(const std::string& function_name);

    // 辅助方法：显示生成的向量化代码
    void displayGeneratedVectorCode(const std::string& arch, const std::string& code);

private:
    // 辅助方法
    std::string loadFileContent(const std::string& filename);
    void saveToFile(const std::string& content, const std::string& filename);
    void runClangAnalysis(const std::string& code, const std::string& filename);
    void generateVisualization(const ComprehensiveAnalysisResult& result, const std::string& name);

    // 复杂案例特定的演示
    void demonstrateDataFlowDisruption();
    void demonstrateControlFlowComplexity();
    void demonstrateMultiFunctionAnalysis();
    void demonstrateSIMDOptimization();
    void demonstrateCrossFunctionDataFlow();
};

// 案例1: 字符串处理演示
void AODSolveDemo::runStringProcessingDemo() {
    std::cout << "\n=== 字符串处理案例分析演示 ===" << std::endl;

    // 使用用户提供的案例1代码
    std::string case1_code = R"(
#include <immintrin.h>
#include <stdint.h>
#include <stddef.h>

void lower_case_avx2(uint8_t* dst, const uint8_t* src, size_t len) {
#if defined(__AVX2__)
    const __m256i _A = _mm256_set1_epi8('A' - 1);
    const __m256i Z_ = _mm256_set1_epi8('Z' + 1);
    const __m256i delta = _mm256_set1_epi8('a' - 'A');
    uint8_t* q = dst;

    while (len >= 32) {
        __m256i op = _mm256_loadu_si256((__m256i*)src);
        __m256i gt = _mm256_cmpgt_epi8(op, _A);
        __m256i lt = _mm256_cmpgt_epi8(Z_, op);
        __m256i mingle = _mm256_and_si256(gt, lt);
        __m256i add = _mm256_and_si256(mingle, delta);
        __m256i lower = _mm256_add_epi8(op, add);
        _mm256_storeu_si256((__m256i *)q, lower);
        src += 32;
        q += 32;
        len -= 32;
    }
#endif
}
)";

    runClangAnalysis(case1_code, "string_processing.cpp");
    std::cout << "字符串处理案例分析完成" << std::endl;
}

// 案例2: 位操作演示
void AODSolveDemo::runBitwiseOperationsDemo() {
    std::cout << "\n=== 位操作案例分析演示 ===" << std::endl;

    std::string case2_code = R"(
static always_inline uint64_t get_nonspace_bits(const uint8_t* s) {
#if defined(__AVX2__)
    __m256i space_tab = _mm256_setr_epi8(
        '\x20', 0, 0, 0, 0, 0, 0, 0,
         0, '\x09', '\x0A', 0, 0, '\x0D', 0, 0,
        '\x20', 0, 0, 0, 0, 0, 0, 0,
         0, '\x09', '\x0A', 0, 0, '\x0D', 0, 0
    );

    __m256i lo = _mm256_loadu_si256((__m256i*)s);
    __m256i hi = _mm256_loadu_si256((__m256i*)(s + 32));
    __m256i shuf_lo = _mm256_shuffle_epi8(space_tab, lo);
    __m256i shuf_hi = _mm256_shuffle_epi8(space_tab, hi);
    uint32_t mask_lo = (uint32_t)_mm256_movemask_epi8(_mm256_cmpeq_epi8(lo, shuf_lo));
    uint32_t mask_hi = (uint32_t)_mm256_movemask_epi8(_mm256_cmpeq_epi8(hi, shuf_hi));
    return ~((uint64_t)mask_lo | ((uint64_t)(mask_hi) << 32));
#else
    return 0;
#endif
}
)";

    runClangAnalysis(case2_code, "bitwise_operations.cpp");
    std::cout << "位操作案例分析完成" << std::endl;
}

// 案例3: UTF-8验证演示
void AODSolveDemo::runUTF8ValidationDemo() {
    std::cout << "\n=== UTF-8验证案例分析演示 ===" << std::endl;

    std::string case3_code = R"(
static always_inline __m256i simd256_shr(const __m256i input, const int shift) {
    __m256i shifted = _mm256_srli_epi16(input, shift);
    __m256i mask = _mm256_set1_epi8(0xFFu >> shift);
    return _mm256_and_si256(shifted, mask);
}

static always_inline __m256i must_be_2_3_continuation(const __m256i prev2, const __m256i prev3) {
    __m256i is_third_byte  = _mm256_subs_epu8(prev2, _mm256_set1_epi8(0b11100000u-1));
    __m256i is_fourth_byte = _mm256_subs_epu8(prev3, _mm256_set1_epi8(0b11110000u-1));
    __m256i or = _mm256_or_si256(is_third_byte, is_fourth_byte);
    return _mm256_cmpgt_epi8(or, _mm256_set1_epi8(0));
}

static always_inline long validate_utf8_avx2(const uint8_t* data, size_t len) {
    if (len == 0) return 0;

    __m256i input = _mm256_loadu_si256((__m256i*)data);
    __m256i input2 = _mm256_loadu_si256((__m256i*)(data + 32));

    return 0;
}
)";

    runClangAnalysis(case3_code, "utf8_validation.cpp");
    std::cout << "UTF-8验证案例分析完成" << std::endl;
}

// 案例4: 标量循环向量化演示 - 去除硬编码版本
void AODSolveDemo::runScalarLoopVectorizationDemo() {
    std::cout << "\n=== 案例4: 标量循环向量化演示 ===" << std::endl;

    std::string case4_code = R"(
#include <stddef.h>

// 标量版本 - 需要向量化
void Test(float volatile* xNorms, int i, float volatile* yNorms,
          float volatile* ipLine, size_t ny) {
    for (size_t j = 0; j < ny; j++) {
        float ip = *ipLine;
        float dis = xNorms[i] + yNorms[j] - 2 * ip;
        if (dis < 0) {
            dis = 0;
        }
        *ipLine = dis;
        ipLine++;
    }
}
)";

    std::cout << "\n原始标量代码:" << std::endl;
    std::cout << case4_code << std::endl;

    try {
        // 构建AST
        std::string temp_file = "/tmp/case4_scalar_loop.cpp";
        std::vector<std::string> args = {
            "-xc++",
            "--target=x86_64-pc-linux-gnu",
            "-std=c++17",
            "-I/mnt/d/WSL/llvm-project-llvmorg-17.0.4/clang/lib/Headers"
        };

        auto owner = clang::tooling::buildASTFromCodeWithArgs(case4_code, args, temp_file);
        auto& ast_context = owner->getASTContext();

        // 创建CPG分析器和循环向量化分析器
        IntegratedCPGAnalyzer cpg_analyzer(ast_context);
        const cpg::CPGContext& cpg_ctx_const = cpg_analyzer.getCPGContext();
        cpg::CPGContext& cpg_ctx = const_cast<cpg::CPGContext&>(cpg_ctx_const);
        LoopVectorizationAnalyzer loop_analyzer(ast_context, &cpg_ctx);

        // 查找循环
        const clang::ForStmt* target_loop = nullptr;
        for (auto* decl : ast_context.getTranslationUnitDecl()->decls()) {
            if (auto* func = clang::dyn_cast<clang::FunctionDecl>(decl)) {
                if (func->getNameAsString() == "Test" && func->hasBody()) {
                    // 遍历函数体找循环
                    class LoopFinder : public clang::RecursiveASTVisitor<LoopFinder> {
                    public:
                        const clang::ForStmt* found_loop = nullptr;
                        bool VisitForStmt(clang::ForStmt* loop) {
                            if (!found_loop) found_loop = loop;
                            return true;
                        }
                    };
                    LoopFinder finder;
                    finder.TraverseStmt(func->getBody());
                    target_loop = finder.found_loop;
                    break;
                }
            }
        }

        if (target_loop) {
            std::cout << "\n✓ 找到循环,开始分析..." << std::endl;

            // 分析循环向量化模式
            auto pattern = loop_analyzer.analyzeLoopVectorizability(target_loop);

            // 显示分析结果(基于实际分析,不硬编码)
            std::cout << "\n" << std::string(70, '=') << std::endl;
            std::cout << "案例4 向量化分析结果" << std::endl;
            std::cout << std::string(70, '=') << std::endl;

            std::cout << "\n✓ 检测到的循环模式:" << std::endl;
            std::cout << "  循环变量: " << pattern.iterator_name << std::endl;
            std::cout << "  起始值: " << pattern.start_value << std::endl;
            std::cout << "  结束条件: " << pattern.iterator_name << " < " << pattern.end_variable << std::endl;
            std::cout << "  步长: " << pattern.step << std::endl;

            std::cout << "\n✓ 数组访问分析:" << std::endl;
            for (const auto& access : pattern.array_accesses) {
                std::cout << "  [" << (access.is_read ? "读" : "写") << "] "
                          << access.array_name << "[" << access.index_expr << "]";
                if (access.is_sequential) {
                    std::cout << " - 顺序访问 ✓";
                }
                std::cout << std::endl;
            }

            std::cout << "\n✓ 操作识别:" << std::endl;
            for (size_t i = 0; i < pattern.operations.size(); ++i) {
                const auto& op = pattern.operations[i];
                std::cout << "  " << (i+1) << ". " << op.op_type << ": ";
                for (size_t j = 0; j < op.operands.size(); ++j) {
                    std::cout << op.operands[j];
                    if (j < op.operands.size() - 1) std::cout << ", ";
                }
                std::cout << std::endl;
            }

            std::cout << "\n✓ 向量化可行性: " << (pattern.is_vectorizable ? "是" : "否") << std::endl;
            if (pattern.has_loop_dependencies) {
                std::cout << "  警告: 存在循环携带依赖" << std::endl;
            } else {
                std::cout << "  无循环携带依赖 ✓" << std::endl;
            }

            // 如果可向量化,使用规则引擎生成代码
            if (pattern.is_vectorizable) {
                std::cout << "\n✓ 生成向量化代码..." << std::endl;

                // 创建bindings(从pattern提取,不硬编码)
                std::map<std::string, std::string> bindings;
                bindings["{{loop_var}}"] = pattern.iterator_name;
                bindings["{{start_value}}"] = std::to_string(pattern.start_value);
                bindings["{{end_value}}"] = pattern.end_variable;

                std::cout << "\n  提取的绑定:" << std::endl;
                for (const auto& [k, v] : bindings) {
                    std::cout << "    " << k << " = " << v << std::endl;
                }

                // 生成NEON代码
                EnhancedCodeGenerator generator("NEON", 2);
                std::string neon_code = generator.generateLoopFromTemplate(bindings, "NEON");
                displayGeneratedVectorCode("NEON (基于规则生成)", neon_code);

                // 生成SVE代码
                generator.setTargetArchitecture("SVE");
                std::string sve_code = generator.generateLoopFromTemplate(bindings, "SVE");
                displayGeneratedVectorCode("SVE (基于规则生成)", sve_code);
            }
        } else {
            std::cout << "  ✗ 未找到循环" << std::endl;
        }

    } catch (const std::exception& e) {
        std::cout << "  ✗ 分析出错: " << e.what() << std::endl;
    }

    std::cout << "\n✓ 案例4分析完成!" << std::endl;
}
// 案例5: 跨函数向量化演示 - 去除硬编码版本
void AODSolveDemo::runCrossFunctionVectorizationDemo() {
    std::cout << "\n=== 案例5: 跨函数向量化演示 ===" << std::endl;

    std::string case5_code = R"(
#include <stddef.h>

// 被调用的辅助函数
float cal_call(float volatile* xNorms, int i, int j,
               float volatile* yNorms, float ip) {
    return xNorms[i] + yNorms[j] - 2 * ip;
}

// 主循环函数 - 包含函数调用
void Test_call(float volatile* xNorms, int i, float volatile* yNorms,
               float volatile* ipLine, size_t ny) {
    for (size_t j = 0; j < ny; j++) {
        float ip = *ipLine;
        float dis = cal_call(xNorms, i, j, yNorms, ip);
        if (dis < 0) {
            dis = 0;
        }
        *ipLine = dis;
        ipLine++;
    }
}
)";

    std::cout << "\n原始标量代码 (带函数调用):" << std::endl;
    std::cout << case5_code << std::endl;

    try {
        std::string temp_file = "/tmp/case5_cross_function.cpp";
        std::vector<std::string> args = {
            "-xc++",
            "--target=x86_64-pc-linux-gnu",
            "-std=c++17"
        };

        auto owner = clang::tooling::buildASTFromCodeWithArgs(case5_code, args, temp_file);
        auto& ast_context = owner->getASTContext();

        // 创建CPG分析器和内联分析器
        IntegratedCPGAnalyzer cpg_analyzer(ast_context);
        const cpg::CPGContext& cpg_ctx_const = cpg_analyzer.getCPGContext();
        cpg::CPGContext& cpg_ctx = const_cast<cpg::CPGContext&>(cpg_ctx_const);
        LoopVectorizationAnalyzer loop_analyzer(ast_context, &cpg_ctx);
        FunctionInlineAnalyzer inline_analyzer(ast_context, &cpg_ctx);

        // 查找两个函数
        const clang::FunctionDecl* cal_call_func = nullptr;
        const clang::FunctionDecl* test_func = nullptr;
        const clang::ForStmt* target_loop = nullptr;

        for (auto* decl : ast_context.getTranslationUnitDecl()->decls()) {
            if (auto* func = clang::dyn_cast<clang::FunctionDecl>(decl)) {
                std::string func_name = func->getNameAsString();
                if (func_name == "cal_call") {
                    cal_call_func = func;
                } else if (func_name == "Test_call") {
                    test_func = func;
                    if (func->hasBody()) {
                        // 查找循环
                        class LoopFinder : public clang::RecursiveASTVisitor<LoopFinder> {
                        public:
                            const clang::ForStmt* found_loop = nullptr;
                            bool VisitForStmt(clang::ForStmt* loop) {
                                if (!found_loop) found_loop = loop;
                                return true;
                            }
                        };
                        LoopFinder finder;
                        finder.TraverseStmt(func->getBody());
                        target_loop = finder.found_loop;
                    }
                }
            }
        }

        if (!cal_call_func || !test_func || !target_loop) {
            std::cout << "  ✗ 未找到必要的函数或循环" << std::endl;
            return;
        }

        std::cout << "\n✓ 找到cal_call和Test_call函数" << std::endl;

        // 分析cal_call是否可内联
        auto inline_candidate = inline_analyzer.analyzeFunctionInlineability(cal_call_func);

        // 显示分析结果(基于实际分析)
        std::cout << "\n" << std::string(70, '=') << std::endl;
        std::cout << "案例5 跨函数向量化分析结果" << std::endl;
        std::cout << std::string(70, '=') << std::endl;

        std::cout << "\n✓ 函数调用检测:" << std::endl;
        std::cout << "  发现函数: " << inline_candidate.function_name << "()" << std::endl;

        std::cout << "\n✓ " << inline_candidate.function_name << "() 函数分析:" << std::endl;
        std::cout << "  是否小函数: " << (inline_candidate.is_small_function ? "是" : "否") << std::endl;
        std::cout << "  是否纯函数: " << (inline_candidate.is_pure ? "是" : "否") << std::endl;
        std::cout << "  有SIMD等价: " << (inline_candidate.has_simd_equivalent ? "是" : "否") << std::endl;
        std::cout << "  有控制流: " << (inline_candidate.has_control_flow ? "是" : "否") << std::endl;
        std::cout << "  可内联: " << (inline_candidate.can_be_inlined ? "是" : "否") << std::endl;

        if (!inline_candidate.simd_pattern.empty()) {
            std::cout << "\n✓ SIMD模式识别: " << inline_candidate.simd_pattern << std::endl;
        } else {
            std::cout << "\n✓ SIMD模式识别: 未匹配预定义模式" << std::endl;
            std::cout << "  (但可以直接内联并向量化)" << std::endl;
        }

        std::cout << "\n✓ 读取的变量:" << std::endl;
        for (const auto& var : inline_candidate.read_variables) {
            std::cout << "  - " << var << std::endl;
        }

        if (!inline_candidate.modified_variables.empty()) {
            std::cout << "\n✓ 修改的变量:" << std::endl;
            for (const auto& var : inline_candidate.modified_variables) {
                std::cout << "  - " << var << std::endl;
            }
        }

        // 如果可以内联,分析循环并生成向量化代码
        if (inline_candidate.can_be_inlined) {
            std::cout << "\n✓ 函数可以内联,分析循环..." << std::endl;

            auto pattern = loop_analyzer.analyzeLoopVectorizability(target_loop);

            std::cout << "\n✓ 循环分析结果:" << std::endl;
            std::cout << "  循环变量: " << pattern.iterator_name << std::endl;
            std::cout << "  可向量化: " << (pattern.is_vectorizable ? "是" : "否") << std::endl;

            if (pattern.is_vectorizable) {
                std::cout << "\n💡 向量化策略:" << std::endl;
                std::cout << "  1. 内联 " << inline_candidate.function_name << "() 函数体到循环" << std::endl;
                std::cout << "  2. 向量化内联后的算术操作" << std::endl;
                std::cout << "  3. 优化条件分支" << std::endl;

                std::cout << "\n✓ 生成向量化代码..." << std::endl;

                std::map<std::string, std::string> bindings;
                bindings["{{loop_var}}"] = pattern.iterator_name;
                bindings["{{start_value}}"] = std::to_string(pattern.start_value);
                bindings["{{end_value}}"] = pattern.end_variable;

                std::cout << "\n  提取的绑定:" << std::endl;
                for (const auto& [k, v] : bindings) {
                    std::cout << "    " << k << " = " << v << std::endl;
                }

                EnhancedCodeGenerator generator("SVE", 2);
                std::string sve_code = generator.generateLoopFromTemplate(bindings, "SVE");
                displayGeneratedVectorCode("SVE (跨函数向量化，内联后)", sve_code);

                generator.setTargetArchitecture("NEON");
                std::string neon_code = generator.generateLoopFromTemplate(bindings, "NEON");
                displayGeneratedVectorCode("NEON (跨函数向量化，内联后)", neon_code);

                std::cout << "\n✓ 关键洞察:" << std::endl;
                std::cout << "  案例4 vs 案例5 的唯一区别:" << std::endl;
                std::cout << "  案例4: 算术直接在循环内" << std::endl;
                std::cout << "  案例5: 算术在 " << inline_candidate.function_name << "() 函数内" << std::endl;
                std::cout << "  → 通过函数内联，两者等价!" << std::endl;
                std::cout << "  → 生成的向量化代码几乎相同!" << std::endl;
            }
        } else {
            std::cout << "\n✗ 函数无法内联,原因:" << std::endl;
            if (!inline_candidate.is_small_function) {
                std::cout << "  - 函数体过大" << std::endl;
            }
            if (!inline_candidate.is_pure) {
                std::cout << "  - 函数有副作用" << std::endl;
            }
            if (inline_candidate.has_control_flow) {
                std::cout << "  - 函数有复杂控制流" << std::endl;
            }
        }

    } catch (const std::exception& e) {
        std::cout << "  ✗ 分析出错: " << e.what() << std::endl;
    }

    std::cout << "\n✓ 案例5分析完成!" << std::endl;
}


// 辅助方法：显示生成的向量化代码
// TODO: 这里目前是hardcoded的输出，未来应该改为规则驱动的动态生成
// 参考设计文档: /mnt/user-data/outputs/COMPLETE_GUIDE.md
void AODSolveDemo::displayGeneratedVectorCode(const std::string& arch,
                                               const std::string& code) {
    std::cout << "\n" << std::string(70, '-') << std::endl;
    std::cout << "生成的 " << arch << " 向量化代码:" << std::endl;
    std::cout << std::string(70, '-') << std::endl;
    std::cout << code << std::endl;
    std::cout << std::string(70, '-') << std::endl;
}

// 复杂控制流演示
void AODSolveDemo::runComplexControlFlowDemo() {
    std::cout << "\n=== 复杂控制流分析演示 ===" << std::endl;

    std::string complex_code = R"(
void complex_control_flow(int* data, int size) {
    int a = 2;
    int b = 3;

    solve(b);

    if (a > 0) {
        for (int i = 0; i < size; i++) {
            if (i % 2 == 0) {
                data[i] = a * i;
            } else {
                data[i] = b * i;
            }
        }
    } else {
        for (int i = 0; i < size; i++) {
            for (int j = 0; j < size; j++) {
                data[i * size + j] = a + b + i + j;
            }
        }
    }

    switch (a) {
        case 1:
            process_data(data, size);
            break;
        case 2:
            process_data_alt(data, size);
            break;
        default:
            error_handling();
            break;
    }
}
)";

    runClangAnalysis(complex_code, "complex_control_flow.cpp");
    std::cout << "复杂控制流案例分析完成" << std::endl;
}

// 跨函数分析演示
void AODSolveDemo::runInterproceduralAnalysisDemo() {
    std::cout << "\n=== 跨函数分析演示 ===" << std::endl;

    std::string interproc_code = R"(
void solve(int& value) {
    value = value * 2;
    global_counter++;
}

int process_value(int x) {
    solve(x);
    return x + global_counter;
}

void main_function() {
    int a = 2;
    int b = 3;
    int result = process_value(b);
    int c = a + b;
    use_result(c, result);
}
)";

    runClangAnalysis(interproc_code, "interprocedural.cpp");
    std::cout << "跨函数分析演示完成" << std::endl;
}

// 完整对比演示
void AODSolveDemo::runFullComparisonDemo() {
    std::cout << "\n=== 完整架构对比演示 ===" << std::endl;

    std::vector<std::string> architectures = {"AVX2", "AVX512", "NEON", "SVE"};
    std::vector<ComprehensiveAnalysisResult> results;

    for (const auto& arch : architectures) {
        std::cout << "分析 " << arch << " 架构..." << std::endl;
    }

    displayPerformanceComparison(results);
}

// 核心分析执行方法
void AODSolveDemo::runClangAnalysis(const std::string& code, const std::string& filename) {
    try {
        std::string temp_file = "/tmp/" + filename;
        saveToFile(code, temp_file);

        std::vector<std::string> args = {
            "-xc++",
            "--target=x86_64-pc-linux-gnu",
            "-mavx2",
            "-D__AVX2__",
            "-std=c++17",
            "-I/usr/include",
            "-I/usr/local/include"
        };

        auto owner = clang::tooling::buildASTFromCodeWithArgs(code, args, temp_file);
        auto& ast_context = owner->getASTContext();
        auto& source_manager = ast_context.getSourceManager();

        std::cout << "\n=== 开始分析 ===" << std::endl;
        std::cout << "文件: " << filename << std::endl;

        // 【关键】创建CPGContext并构建CPG
        cpg::CPGContext cpg_context(ast_context);

        auto* tu = ast_context.getTranslationUnitDecl();
        std::vector<const clang::FunctionDecl*> functions;

        std::cout << "\n[步骤1] 提取函数定义..." << std::endl;

        for (auto* decl : tu->decls()) {
            if (!source_manager.isInMainFile(decl->getLocation())) {
                continue;
            }

            if (auto* func = clang::dyn_cast<clang::FunctionDecl>(decl)) {
                if (func->hasBody() && func->isThisDeclarationADefinition()) {
                    std::cout << "  ✓ 发现函数: " << func->getNameAsString() << std::endl;
                    functions.push_back(func);
                }
            }
        }

        if (functions.empty()) {
            std::cout << "  ⚠️ 未找到函数定义" << std::endl;
            return;
        }

        std::cout << "  共找到 " << functions.size() << " 个函数" << std::endl;

        // 【关键】使用CPGBuilder构建CPG
        std::cout << "\n[步骤2] 构建Code Property Graph..." << std::endl;
        for (auto* func : functions) {
            cpg::CPGBuilder::buildForFunction(func, cpg_context);
        }
        std::cout << "  ✓ CPG构建完成" << std::endl;

        // 打印CPG统计
        cpg_context.printStatistics();

        // 使用AODSolveMainAnalyzer进行分析
        std::cout << "\n[步骤3] 进行SIMD向量化分析..." << std::endl;
        AODSolveMainAnalyzer analyzer(ast_context);
        analyzer.setTargetArchitecture("AVX2");
        analyzer.setOptimizationLevel(2);
        analyzer.enableInterproceduralAnalysis(true);

        auto result = analyzer.analyzeFunction(functions[0]);
        displayAnalysisResults(result);

        if (result.successful) {
            std::cout << "\n✓ 分析成功完成" << std::endl;

            // 生成分析报告
            std::string report = analyzer.generateComprehensiveReport(result);
            std::string report_file = filename.substr(0, filename.find('.')) + "_report.txt";
            saveToFile(report, report_file);
            std::cout << "  分析报告已保存到: " << report_file << std::endl;
        }

    } catch (const std::exception& e) {
        std::cerr << "\n❌ 分析过程中出现错误: " << e.what() << std::endl;
    }
}

// 显示分析结果
void AODSolveDemo::displayAnalysisResults(const ComprehensiveAnalysisResult& result) {
    std::cout << "\n" << std::string(70, '=') << std::endl;
    std::cout << "分析结果" << std::endl;
    std::cout << std::string(70, '=') << std::endl;

    if (!result.successful) {
        std::cout << "❌ 分析失败!" << std::endl;
        for (const auto& error : result.errors) {
            std::cout << "错误: " << error << std::endl;
        }
        return;
    }

    std::cout << "✓ 分析成功!" << std::endl;
    std::cout << "\n基本统计:" << std::endl;
    std::cout << "  函数数量: " << result.functions_analyzed << std::endl;
    std::cout << "  总节点数: " << result.total_nodes << std::endl;
    std::cout << "  总边数: " << result.total_edges << std::endl;
    std::cout << "  SIMD机会: " << result.simd_opportunities << std::endl;
    std::cout << "  估计加速比: " << result.total_speedup_estimate << "x" << std::endl;
    std::cout << "  推荐架构: " << result.best_architecture << std::endl;

    if (!result.warnings.empty()) {
        std::cout << "\n⚠ 警告:" << std::endl;
        for (const auto& warning : result.warnings) {
            std::cout << "  - " << warning << std::endl;
        }
    }

    if (!result.recommended_optimizations.empty()) {
        std::cout << "\n💡 推荐优化:" << std::endl;
        for (const auto& opt : result.recommended_optimizations) {
            std::cout << "  - " << opt << std::endl;
        }
    }

    std::cout << std::string(70, '=') << std::endl;
}

// 性能对比显示
void AODSolveDemo::displayPerformanceComparison(const std::vector<ComprehensiveAnalysisResult>& results) {
    std::cout << "\n=== 架构性能对比 ===" << std::endl;
    std::cout << std::setw(15) << "架构" << std::setw(15) << "加速比" << std::endl;
    std::cout << "==============================" << std::endl;

    for (const auto& result : results) {
        std::cout << std::setw(15) << result.best_architecture
                  << std::setw(15) << result.total_speedup_estimate << std::endl;
    }
}

// 保存文件工具方法
void AODSolveDemo::saveToFile(const std::string& content, const std::string& filename) {
    std::ofstream file(filename);
    if (file.is_open()) {
        file << content;
        file.close();
    }
}

// 演示主函数
int main(int argc, char* argv[]) {
    std::cout << "=== AODSOLVE 向量化优化引擎演示 ===" << std::endl;
    std::cout << "支持复杂的控制流和数据流分析" << std::endl;
    std::cout << "以及跨函数的变量影响分析" << std::endl;
    std::cout << "新增: 标量循环向量化 (案例4) 和跨函数向量化 (案例5)" << std::endl;

    AODSolveDemo demo;

    if (argc > 1) {
        std::string command = argv[1];
        if (command == "string") {
            demo.runStringProcessingDemo();
        } else if (command == "bitwise") {
            demo.runBitwiseOperationsDemo();
        } else if (command == "utf8") {
            demo.runUTF8ValidationDemo();
        } else if (command == "control") {
            demo.runComplexControlFlowDemo();
        } else if (command == "interproc") {
            demo.runInterproceduralAnalysisDemo();
        } else if (command == "compare") {
            demo.runFullComparisonDemo();
        } else if (command == "case4" || command == "loop") {
            demo.runScalarLoopVectorizationDemo();
        } else if (command == "case5" || command == "crossfunc") {
            demo.runCrossFunctionVectorizationDemo();
        } else if (command == "all") {
            demo.runStringProcessingDemo();
            demo.runBitwiseOperationsDemo();
            demo.runUTF8ValidationDemo();
            demo.runComplexControlFlowDemo();
            demo.runInterproceduralAnalysisDemo();
            demo.runScalarLoopVectorizationDemo();
            demo.runCrossFunctionVectorizationDemo();
            demo.runFullComparisonDemo();
        } else {
            std::cout << "未知命令: " << command << std::endl;
            std::cout << "可用命令:" << std::endl;
            std::cout << "  string    - 案例1: 字符串处理 (SIMD→SIMD)" << std::endl;
            std::cout << "  bitwise   - 案例2: 位操作 (SIMD→SIMD)" << std::endl;
            std::cout << "  utf8      - 案例3: UTF-8验证 (SIMD→SIMD)" << std::endl;
            std::cout << "  case4     - 案例4: 标量循环向量化" << std::endl;
            std::cout << "  case5     - 案例5: 跨函数向量化" << std::endl;
            std::cout << "  control   - 复杂控制流分析" << std::endl;
            std::cout << "  interproc - 跨函数分析" << std::endl;
            std::cout << "  compare   - 完整架构对比" << std::endl;
            std::cout << "  all       - 运行所有演示" << std::endl;
        }
    } else {
        // 默认运行字符串处理演示
        std::cout << "\n运行默认演示(字符串处理)..." << std::endl;
        std::cout << "使用 './demo case4' 或 './demo case5' 查看新案例" << std::endl;
        demo.runStringProcessingDemo();
    }

    std::cout << "\n演示完成!" << std::endl;
    return 0;
}