#include "analysis/loop_vectorization_analyzer.h"
#include <clang/AST/RecursiveASTVisitor.h>
#include <sstream>

namespace aodsolve {
    // ============================================================================
    // LoopVectorizationAnalyzer 实现
    // ============================================================================

    std::vector<LoopVectorizationPattern> LoopVectorizationAnalyzer::analyzeFunction(
        const clang::FunctionDecl *func) {
        std::vector<LoopVectorizationPattern> patterns;

        if (!func || !func->hasBody()) {
            return patterns;
        }

        // 遍历函数体查找所有for循环
        class LoopFinder : public clang::RecursiveASTVisitor<LoopFinder> {
        public:
            std::vector<const clang::ForStmt *> loops;

            bool VisitForStmt(const clang::ForStmt *loop) {
                loops.push_back(loop);
                return true;
            }
        };

        LoopFinder finder;
        finder.TraverseStmt(const_cast<clang::Stmt *>(func->getBody()));

        // 分析每个循环
        for (auto *loop: finder.loops) {
            LoopVectorizationPattern pattern = analyzeLoop(loop);
            if (pattern.is_vectorizable) {
                patterns.push_back(pattern);
            }
        }

        return patterns;
    }

    LoopVectorizationPattern LoopVectorizationAnalyzer::analyzeLoop(const clang::ForStmt *loop) {
        LoopVectorizationPattern pattern;
        pattern.loop = loop;
        pattern.is_vectorizable = false;

        // 步骤1: 提取循环控制信息
        if (!extractLoopControl(loop, pattern)) {
            return pattern;
        }

        // 步骤2: 分析循环体中的数组访问
        if (loop->getBody()) {
            pattern.array_accesses = analyzeArrayAccesses(loop->getBody(), pattern.iterator_name);
        }

        // 步骤3: 分析循环体中的操作
        if (loop->getBody()) {
            pattern.operations = analyzeOperations(loop->getBody());
        }

        // 步骤4: 检测归约模式
        pattern.is_reduction = detectReductionPattern(loop, pattern);

        // 步骤5: 依赖性分析
        pattern.has_loop_dependencies = hasLoopCarriedDependencies(pattern);

        // 步骤6: 判断是否可向量化
        pattern.is_vectorizable = isVectorizable(pattern);

        return pattern;
    }

    LoopVectorizationPattern LoopVectorizationAnalyzer::analyzeLoopVectorizability(
        const clang::ForStmt* loop) {
        return analyzeLoop(loop);
    }

    bool LoopVectorizationAnalyzer::extractLoopControl(const clang::ForStmt *loop,
                                                       LoopVectorizationPattern &pattern) {
        // 提取初始化: for (int i = 0; ...)
        if (auto *init = loop->getInit()) {
            if (auto *decl_stmt = clang::dyn_cast<clang::DeclStmt>(init)) {
                if (decl_stmt->isSingleDecl()) {
                    if (auto *var_decl = clang::dyn_cast<clang::VarDecl>(decl_stmt->getSingleDecl())) {
                        pattern.iterator_name = var_decl->getNameAsString();

                        if (var_decl->hasInit()) {
                            clang::Expr::EvalResult result;
                            if (var_decl->getInit()->EvaluateAsInt(result, ast_context)) {
                                pattern.start_value = result.Val.getInt().getLimitedValue();
                            }
                        }
                    }
                }
            }
        }

        // 提取条件: ... i < n; ...
        if (auto *cond = loop->getCond()) {
            if (auto *bin_op = clang::dyn_cast<clang::BinaryOperator>(cond)) {
                if (bin_op->getOpcode() == clang::BO_LT) {
                    if (auto *rhs = clang::dyn_cast<clang::DeclRefExpr>(bin_op->getRHS()->IgnoreImpCasts())) {
                        pattern.end_variable = rhs->getDecl()->getNameAsString();
                    }
                }
            }
        }

        // 提取增量: ... i++
        if (auto *inc = loop->getInc()) {
            if (auto *unary = clang::dyn_cast<clang::UnaryOperator>(inc)) {
                if (unary->isIncrementOp()) {
                    pattern.step = 1;
                }
            }
        }

        return !pattern.iterator_name.empty() && !pattern.end_variable.empty();
    }

    std::vector<ArrayAccess> LoopVectorizationAnalyzer::analyzeArrayAccesses(
        const clang::Stmt *body, const std::string &iterator) {
        std::vector<ArrayAccess> accesses;

        class ArrayAccessFinder : public clang::RecursiveASTVisitor<ArrayAccessFinder> {
        public:
            std::vector<ArrayAccess> accesses;
            std::string iterator;
            clang::ASTContext &ctx;

            ArrayAccessFinder(const std::string &it, clang::ASTContext &c)
                : iterator(it), ctx(c) {
            }

            bool VisitArraySubscriptExpr(const clang::ArraySubscriptExpr *expr) {
                ArrayAccess access;
                access.ast_expr = expr;

                // 获取数组名
                if (auto *base = clang::dyn_cast<clang::DeclRefExpr>(expr->getBase()->IgnoreImpCasts())) {
                    access.array_name = base->getDecl()->getNameAsString();
                }

                // 获取索引表达式
                if (auto *idx = expr->getIdx()) {
                    access.index_expr = getExprAsString(idx);
                    access.is_sequential = (access.index_expr == iterator);
                }

                // 判断读/写
                // 简化版本：需要检查父节点
                access.is_read = true;

                accesses.push_back(access);
                return true;
            }

        private:
            std::string getExprAsString(const clang::Expr *expr) {
                if (auto *decl_ref = clang::dyn_cast<clang::DeclRefExpr>(expr->IgnoreImpCasts())) {
                    return decl_ref->getDecl()->getNameAsString();
                }
                return "complex_expr";
            }
        };

        ArrayAccessFinder finder(iterator, ast_context);
        finder.TraverseStmt(const_cast<clang::Stmt *>(body));

        return finder.accesses;
    }

    std::vector<ScalarOperation> LoopVectorizationAnalyzer::analyzeOperations(
        const clang::Stmt *body) {
        std::vector<ScalarOperation> operations;

        class OperationFinder : public clang::RecursiveASTVisitor<OperationFinder> {
        public:
            std::vector<ScalarOperation> operations;

            bool VisitBinaryOperator(const clang::BinaryOperator *op) {
                ScalarOperation scalar_op;
                scalar_op.ast_expr = op;

                switch (op->getOpcode()) {
                    case clang::BO_Add: scalar_op.op_type = "add";
                        break;
                    case clang::BO_Sub: scalar_op.op_type = "sub";
                        break;
                    case clang::BO_Mul: scalar_op.op_type = "mul";
                        break;
                    case clang::BO_Div: scalar_op.op_type = "div";
                        break;
                    case clang::BO_AddAssign: scalar_op.op_type = "add_assign";
                        break;
                    default: scalar_op.op_type = "unknown";
                        break;
                }

                operations.push_back(scalar_op);
                return true;
            }
        };

        OperationFinder finder;
        finder.TraverseStmt(const_cast<clang::Stmt *>(body));

        return finder.operations;
    }

    bool LoopVectorizationAnalyzer::detectReductionPattern(const clang::ForStmt *loop,
                                                           LoopVectorizationPattern &pattern) {
        // 检测归约模式: sum += array[i]
        // 简化版本：查找形如 var += expr 的模式

        class ReductionFinder : public clang::RecursiveASTVisitor<ReductionFinder> {
        public:
            bool found_reduction = false;
            std::string reduction_var;
            std::string reduction_op;

            bool VisitBinaryOperator(const clang::BinaryOperator *op) {
                if (op->getOpcode() == clang::BO_AddAssign) {
                    if (auto *lhs = clang::dyn_cast<clang::DeclRefExpr>(op->getLHS()->IgnoreImpCasts())) {
                        reduction_var = lhs->getDecl()->getNameAsString();
                        reduction_op = "sum";
                        found_reduction = true;
                    }
                }
                return true;
            }
        };

        if (!loop->getBody()) {
            return false;
        }

        ReductionFinder finder;
        finder.TraverseStmt(const_cast<clang::Stmt *>(loop->getBody()));

        if (finder.found_reduction) {
            pattern.reduction_var = finder.reduction_var;
            pattern.reduction_op = finder.reduction_op;
        }

        return finder.found_reduction;
    }

    bool LoopVectorizationAnalyzer::hasLoopCarriedDependencies(
        const LoopVectorizationPattern &pattern) {
        // 检查是否有循环携带依赖
        // 简化版本：检查是否有 a[i] = f(a[i-1]) 这样的模式

        for (const auto &access: pattern.array_accesses) {
            if (!access.is_sequential) {
                // 非顺序访问可能有依赖
                return true;
            }
        }

        // 归约操作本身不阻止向量化（需要特殊处理）
        if (pattern.is_reduction) {
            return false;
        }

        return false;
    }

    bool LoopVectorizationAnalyzer::isVectorizable(const LoopVectorizationPattern &pattern) {
        // 向量化条件:
        // 1. 循环步长为1
        // 2. 所有数组访问都是顺序的或是归约
        // 3. 没有循环携带依赖(除非是归约)

        if (pattern.step != 1) {
            return false;
        }

        if (pattern.has_loop_dependencies && !pattern.is_reduction) {
            return false;
        }

        // 检查所有数组访问
        bool has_sequential_access = false;
        for (const auto &access: pattern.array_accesses) {
            if (access.is_sequential) {
                has_sequential_access = true;
            }
        }

        if (!has_sequential_access && !pattern.is_reduction) {
            return false;
        }

        return true;
    }

    std::string LoopVectorizationAnalyzer::generateVectorizedCode(
        const LoopVectorizationPattern &pattern,
        const std::string &target_arch) {
        std::stringstream code;
        VectorizedCodeGenerator generator;

        code << "// 向量化循环: " << pattern.iterator_name << " = " << pattern.start_value
                << " to " << pattern.end_variable << "\n";
        code << "// 目标架构: " << target_arch << "\n\n";

        // 生成初始化代码
        code << generator.generateInitialization(pattern, target_arch) << "\n\n";

        // 生成主循环
        code << generator.generateMainLoop(pattern, target_arch) << "\n\n";

        // 生成尾部处理
        code << generator.generateTailLoop(pattern, target_arch) << "\n\n";

        // 如果是归约，生成归约代码
        if (pattern.is_reduction) {
            code << generator.generateReduction(pattern, target_arch) << "\n";
        }

        return code.str();
    }

    // ============================================================================
    // VectorizedCodeGenerator 实现
    // ============================================================================

    std::string VectorizedCodeGenerator::generateInitialization(
        const LoopVectorizationPattern &pattern,
        const std::string &target_arch) {
        std::stringstream code;

        if (target_arch == "SVE") {
            code << "svbool_t pg = svptrue_b32();\n";
            code << "uint64_t vl = svcntw();\n";
            code << "size_t i = 0;\n";

            if (pattern.is_reduction) {
                code << "svint32_t sum_vec = svdup_n_s32(0);\n";
            }
        } else if (target_arch == "AVX2") {
            code << "size_t i = 0;\n";
            code << "const size_t vl = 8;  // AVX2 处理8个int32\n";

            if (pattern.is_reduction) {
                code << "__m256i sum_vec = _mm256_setzero_si256();\n";
            }
        }

        return code.str();
    }

    std::string VectorizedCodeGenerator::generateMainLoop(
        const LoopVectorizationPattern &pattern,
        const std::string &target_arch) {
        std::stringstream code;

        if (target_arch == "SVE") {
            code << "// 主向量化循环\n";
            code << "while (i + vl <= " << pattern.end_variable << ") {\n";

            // 生成加载指令
            for (const auto &access: pattern.array_accesses) {
                if (access.is_read && access.is_sequential) {
                    code << "    svint32_t " << access.array_name << "_vec = svld1_s32(pg, "
                            << access.array_name << " + i);\n";
                }
            }

            // 生成操作指令
            for (const auto &op: pattern.operations) {
                if (op.op_type == "add") {
                    code << "    svint32_t result_vec = svadd_s32_z(pg, vec_a, vec_b);\n";
                } else if (op.op_type == "add_assign" && pattern.is_reduction) {
                    code << "    sum_vec = svadd_s32_m(pg, sum_vec, "
                            << pattern.array_accesses[0].array_name << "_vec);\n";
                }
            }

            // 生成存储指令
            for (const auto &access: pattern.array_accesses) {
                if (!access.is_read && access.is_sequential) {
                    code << "    svst1_s32(pg, " << access.array_name << " + i, result_vec);\n";
                }
            }

            code << "    i += vl;\n";
            code << "}\n";
        } else if (target_arch == "AVX2") {
            code << "// 主向量化循环\n";
            code << "for (; i + vl <= " << pattern.end_variable << "; i += vl) {\n";

            // AVX2 类似逻辑
            for (const auto &access: pattern.array_accesses) {
                if (access.is_read && access.is_sequential) {
                    code << "    __m256i " << access.array_name << "_vec = _mm256_loadu_si256"
                            << "((__m256i*)(" << access.array_name << " + i));\n";
                }
            }

            if (pattern.is_reduction) {
                code << "    sum_vec = _mm256_add_epi32(sum_vec, "
                        << pattern.array_accesses[0].array_name << "_vec);\n";
            }

            code << "}\n";
        }

        return code.str();
    }

    std::string VectorizedCodeGenerator::generateTailLoop(
        const LoopVectorizationPattern &pattern,
        const std::string &target_arch) {
        std::stringstream code;

        if (target_arch == "SVE") {
            code << "// 尾部处理\n";
            code << "if (i < " << pattern.end_variable << ") {\n";
            code << "    svbool_t pg_tail = svwhilelt_b32(i, " << pattern.end_variable << ");\n";
            code << "    // 使用 pg_tail 处理剩余元素\n";
            code << "}\n";
        } else if (target_arch == "AVX2") {
            code << "// 标量尾部处理\n";
            code << "for (; i < " << pattern.end_variable << "; i++) {\n";
            code << "    // 标量处理\n";
            code << "}\n";
        }

        return code.str();
    }

    std::string VectorizedCodeGenerator::generateReduction(
        const LoopVectorizationPattern &pattern,
        const std::string &target_arch) {
        std::stringstream code;

        if (target_arch == "SVE") {
            code << "// 水平归约\n";
            code << pattern.reduction_var << " = svaddv_s32(svptrue_b32(), sum_vec);\n";
        } else if (target_arch == "AVX2") {
            code << "// 水平归约\n";
            code << "int temp[8];\n";
            code << "_mm256_storeu_si256((__m256i*)temp, sum_vec);\n";
            code << pattern.reduction_var << " = 0;\n";
            code << "for (int j = 0; j < 8; j++) " << pattern.reduction_var << " += temp[j];\n";
        }

        return code.str();
    }
} // namespace aodsolve