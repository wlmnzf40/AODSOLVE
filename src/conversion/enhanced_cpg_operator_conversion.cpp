#include "conversion/enhanced_cpg_to_aod_converter.h"
#include <sstream>
#include <algorithm>

namespace aodsolve {

// ============================================
// 从CPG提取计算图
// ============================================

std::vector<ComputeGraphNode> EnhancedCPGToAODConverter::extractComputeGraphFromCPG(
    const clang::FunctionDecl* func,
    cpg::CPGContext& cpg_context) {
    
    std::vector<ComputeGraphNode> compute_graph;
    
    if (!func || !func->hasBody()) {
        return compute_graph;
    }
    
    int node_id = 0;
    
    // 遍历函数体中的所有语句
    std::function<void(const clang::Stmt*)> visitStmt = [&](const clang::Stmt* stmt) {
        if (!stmt) return;
        
        // 只处理主文件中的语句
        if (!source_manager.isInMainFile(stmt->getBeginLoc())) {
            return;
        }
        
        // 检查是否是SIMD intrinsic调用
        if (auto* call = clang::dyn_cast<clang::CallExpr>(stmt)) {
            if (isSIMDIntrinsicCall(call)) {
                ComputeGraphNode node = createComputeNodeFromCallExpr(call, node_id++);
                compute_graph.push_back(node);
            }
        }
        
        // ========== 新增：支持标量操作 ==========
        // 识别二元操作符 (加减乘除等)
        if (auto* bin_op = clang::dyn_cast<clang::BinaryOperator>(stmt)) {
            ComputeGraphNode node = createComputeNodeFromBinaryOp(bin_op, node_id++);
            compute_graph.push_back(node);
        }

        // 识别一元操作符
        if (auto* unary_op = clang::dyn_cast<clang::UnaryOperator>(stmt)) {
            ComputeGraphNode node = createComputeNodeFromUnaryOp(unary_op, node_id++);
            compute_graph.push_back(node);
        }

        // 识别数组访问
        if (auto* array_sub = clang::dyn_cast<clang::ArraySubscriptExpr>(stmt)) {
            ComputeGraphNode node = createComputeNodeFromArrayAccess(array_sub, node_id++);
            compute_graph.push_back(node);
        }

        // 递归访问子节点
        for (auto* child : stmt->children()) {
            visitStmt(child);
        }
    };

    visitStmt(func->getBody());

    return compute_graph;
}

// ============================================
// 从CallExpr创建计算图节点
// ============================================

ComputeGraphNode EnhancedCPGToAODConverter::createComputeNodeFromCallExpr(
    const clang::CallExpr* call,
    int node_id) {

    ComputeGraphNode node;
    node.node_id = node_id;
    node.ast_stmt = call;

    // 获取函数名
    const clang::FunctionDecl* callee = call->getDirectCallee();
    if (!callee) {
        node.operation = "unknown";
        return node;
    }

    std::string func_name = callee->getNameAsString();
    node.operation = func_name;

    // 识别操作类型
    if (func_name.find("_load") != std::string::npos ||
        func_name.find("loadu") != std::string::npos) {
        node.type = ComputeNodeType::LOAD;
        node.attributes["alignment"] = "unaligned";
    }
    else if (func_name.find("_store") != std::string::npos ||
             func_name.find("storeu") != std::string::npos) {
        node.type = ComputeNodeType::STORE;
        node.attributes["alignment"] = "unaligned";
    }
    else if (func_name.find("_set1") != std::string::npos ||
             func_name.find("_set") != std::string::npos) {
        node.type = ComputeNodeType::CONSTANT;
    }
    else if (func_name.find("_cmp") != std::string::npos) {
        node.type = ComputeNodeType::COMPARE;
        if (func_name.find("gt") != std::string::npos) {
            node.attributes["compare_type"] = "greater_than";
        } else if (func_name.find("lt") != std::string::npos) {
            node.attributes["compare_type"] = "less_than";
        } else if (func_name.find("eq") != std::string::npos) {
            node.attributes["compare_type"] = "equal";
        }
    }
    else if (func_name.find("_and") != std::string::npos ||
             func_name.find("_or") != std::string::npos ||
             func_name.find("_xor") != std::string::npos) {
        node.type = ComputeNodeType::LOGICAL;
    }
    else if (func_name.find("_add") != std::string::npos ||
             func_name.find("_sub") != std::string::npos ||
             func_name.find("_mul") != std::string::npos) {
        node.type = ComputeNodeType::ARITHMETIC;
    }

    // 识别数据类型和向量宽度
    if (func_name.find("_mm256") != std::string::npos) {
        node.vector_width = 256;
    } else if (func_name.find("_mm512") != std::string::npos) {
        node.vector_width = 512;
    } else if (func_name.find("_mm") != std::string::npos) {
        node.vector_width = 128;
    }

    if (func_name.find("epi8") != std::string::npos || func_name.find("_s8") != std::string::npos) {
        node.data_type = "int8";
    } else if (func_name.find("epu8") != std::string::npos || func_name.find("_u8") != std::string::npos) {
        node.data_type = "uint8";
    } else if (func_name.find("ps") != std::string::npos) {
        node.data_type = "float32";
    }

    return node;
}

// ============================================
// 新增：从标量操作创建计算图节点
// ============================================

ComputeGraphNode EnhancedCPGToAODConverter::createComputeNodeFromBinaryOp(
    const clang::BinaryOperator* bin_op,
    int node_id) {

    ComputeGraphNode node;
    node.node_id = node_id;
    node.ast_stmt = bin_op;
    node.type = ComputeNodeType::ARITHMETIC;

    // 识别操作类型
    switch (bin_op->getOpcode()) {
        case clang::BO_Add:
            node.operation = "add";
            break;
        case clang::BO_Sub:
            node.operation = "sub";
            break;
        case clang::BO_Mul:
            node.operation = "mul";
            break;
        case clang::BO_Div:
            node.operation = "div";
            break;
        case clang::BO_LT:
        case clang::BO_GT:
        case clang::BO_LE:
        case clang::BO_GE:
        case clang::BO_EQ:
        case clang::BO_NE:
            node.type = ComputeNodeType::COMPARE;
            node.operation = "compare";
            break;
        case clang::BO_Assign:
            node.type = ComputeNodeType::STORE;
            node.operation = "assign";
            break;
        default:
            node.operation = "unknown";
            break;
    }

    // 推断数据类型
    clang::QualType type = bin_op->getType();
    if (type->isFloatingType()) {
        if (type->isSpecificBuiltinType(clang::BuiltinType::Float)) {
            node.data_type = "float32";
        } else if (type->isSpecificBuiltinType(clang::BuiltinType::Double)) {
            node.data_type = "float64";
        }
    } else if (type->isIntegerType()) {
        node.data_type = "int32";  // 简化
    }

    return node;
}

ComputeGraphNode EnhancedCPGToAODConverter::createComputeNodeFromUnaryOp(
    const clang::UnaryOperator* unary_op,
    int node_id) {

    ComputeGraphNode node;
    node.node_id = node_id;
    node.ast_stmt = unary_op;
    node.type = ComputeNodeType::ARITHMETIC;

    switch (unary_op->getOpcode()) {
        case clang::UO_Minus:
            node.operation = "neg";
            break;
        case clang::UO_Not:
            node.operation = "not";
            break;
        default:
            node.operation = "unknown";
            break;
    }

    return node;
}

ComputeGraphNode EnhancedCPGToAODConverter::createComputeNodeFromArrayAccess(
    const clang::ArraySubscriptExpr* array_sub,
    int node_id) {

    ComputeGraphNode node;
    node.node_id = node_id;
    node.ast_stmt = array_sub;
    node.type = ComputeNodeType::LOAD;
    node.operation = "array_load";

    // 提取数组名
    if (auto* base = clang::dyn_cast<clang::DeclRefExpr>(
            array_sub->getBase()->IgnoreImpCasts())) {
        node.attributes["array_name"] = base->getDecl()->getNameAsString();
    }

    // 推断数据类型
    clang::QualType type = array_sub->getType();
    if (type->isFloatingType()) {
        node.data_type = "float32";
    } else if (type->isIntegerType()) {
        node.data_type = "int32";
    }
    
    return node;
}

// ============================================
// 识别SIMD算子
// ============================================

std::vector<SIMDOperator> EnhancedCPGToAODConverter::identifySIMDOperators(
    const clang::FunctionDecl* func,
    const std::vector<ComputeGraphNode>& compute_graph) {
    
    std::vector<SIMDOperator> operators;
    
    // 获取转换规则
    auto patterns = createAVX2ToSVERules();
    
    // 在计算图中匹配模式
    for (size_t i = 0; i < compute_graph.size(); ) {
        bool matched = false;
        
        // 尝试匹配每个模式
        for (const auto& pattern : patterns) {
            if (isSubgraphMatch(compute_graph, i, pattern)) {
                // 创建算子
                SIMDOperator op;
                op.name = pattern.name;
                op.semantic_description = pattern.description;
                op.compute_pattern = pattern;
                
                // 提取算子对应的AST子树
                for (size_t j = 0; j < pattern.nodes.size() && i + j < compute_graph.size(); ++j) {
                    if (compute_graph[i + j].ast_stmt) {
                        op.compute_pattern.ast_subtree_roots.push_back(
                            compute_graph[i + j].ast_stmt);
                    }
                }
                
                operators.push_back(op);
                i += pattern.nodes.size();
                matched = true;
                break;
            }
        }
        
        if (!matched) {
            i++;
        }
    }
    
    return operators;
}

// ============================================
// 创建AVX2到SVE的转换规则
// ============================================

std::vector<ComputeGraphPattern> EnhancedCPGToAODConverter::createAVX2ToSVERules() {
    std::vector<ComputeGraphPattern> rules;
    
    // 规则1: 加载操作
    {
        ComputeGraphPattern pattern;
        pattern.name = "load_operation";
        pattern.description = "Memory load operation";
        pattern.source_arch = "AVX2";
        pattern.target_arch = "SVE";
        
        ComputeGraphNode node;
        node.type = ComputeNodeType::LOAD;
        node.operation = "_mm256_loadu_si256";
        pattern.nodes.push_back(node);
        
        pattern.source_instructions = {"_mm256_loadu_si256"};
        pattern.target_instructions = {"svld1_u8"};
        
        rules.push_back(pattern);
    }
    
    // 规则2: 存储操作
    {
        ComputeGraphPattern pattern;
        pattern.name = "store_operation";
        pattern.description = "Memory store operation";
        pattern.source_arch = "AVX2";
        pattern.target_arch = "SVE";
        
        ComputeGraphNode node;
        node.type = ComputeNodeType::STORE;
        node.operation = "_mm256_storeu_si256";
        pattern.nodes.push_back(node);
        
        pattern.source_instructions = {"_mm256_storeu_si256"};
        pattern.target_instructions = {"svst1_u8"};
        
        rules.push_back(pattern);
    }
    
    // 规则3: 常量设置
    {
        ComputeGraphPattern pattern;
        pattern.name = "set_constant";
        pattern.description = "Set constant vector";
        pattern.source_arch = "AVX2";
        pattern.target_arch = "SVE";
        
        ComputeGraphNode node;
        node.type = ComputeNodeType::CONSTANT;
        node.operation = "_mm256_set1_epi8";
        pattern.nodes.push_back(node);
        
        pattern.source_instructions = {"_mm256_set1_epi8"};
        pattern.target_instructions = {"svdup_n_u8"};
        
        rules.push_back(pattern);
    }
    
    // 规则4: 范围检查复合模式 (算子级别)
    {
        ComputeGraphPattern pattern;
        pattern.name = "range_check";
        pattern.description = "Check if value is in range [lower, upper]";
        pattern.source_arch = "AVX2";
        pattern.target_arch = "SVE";
        
        // 节点1: 大于比较
        ComputeGraphNode node1;
        node1.type = ComputeNodeType::COMPARE;
        node1.operation = "_mm256_cmpgt_epi8";
        node1.attributes["compare_type"] = "greater_than";
        pattern.nodes.push_back(node1);
        
        // 节点2: 小于比较
        ComputeGraphNode node2;
        node2.type = ComputeNodeType::COMPARE;
        node2.operation = "_mm256_cmpgt_epi8";
        node2.attributes["compare_type"] = "greater_than";
        pattern.nodes.push_back(node2);
        
        // 节点3: 逻辑与
        ComputeGraphNode node3;
        node3.type = ComputeNodeType::LOGICAL;
        node3.operation = "_mm256_and_si256";
        pattern.nodes.push_back(node3);
        
        pattern.source_instructions = {
            "_mm256_cmpgt_epi8",
            "_mm256_cmpgt_epi8",
            "_mm256_and_si256"
        };
        
        pattern.target_instructions = {
            "svcmpge_s8",
            "svcmple_s8",
            "svand_b_z"
        };
        
        pattern.is_optimizable = false;
        
        rules.push_back(pattern);
    }
    
    // 规则5: 条件加法复合模式 (算子级别 - 优化!)
    {
        ComputeGraphPattern pattern;
        pattern.name = "conditional_add";
        pattern.description = "Conditionally add constant based on mask";
        pattern.source_arch = "AVX2";
        pattern.target_arch = "SVE";
        
        // 节点1: 应用掩码
        ComputeGraphNode node1;
        node1.type = ComputeNodeType::LOGICAL;
        node1.operation = "_mm256_and_si256";
        pattern.nodes.push_back(node1);
        
        // 节点2: 加法
        ComputeGraphNode node2;
        node2.type = ComputeNodeType::ARITHMETIC;
        node2.operation = "_mm256_add_epi8";
        pattern.nodes.push_back(node2);
        
        pattern.source_instructions = {
            "_mm256_and_si256",
            "_mm256_add_epi8"
        };
        
        pattern.target_instructions = {
            "svadd_u8_m"  // 合并为一条指令!
        };
        
        pattern.is_optimizable = true;
        pattern.optimization_type = "merge_operations";
        pattern.instruction_reduction = 1;  // 减少1条指令
        
        rules.push_back(pattern);
    }
    
    return rules;
}

// ============================================
// 模式匹配辅助函数
// ============================================

bool EnhancedCPGToAODConverter::isSubgraphMatch(
    const std::vector<ComputeGraphNode>& graph,
    size_t start_idx,
    const ComputeGraphPattern& pattern) {
    
    if (start_idx + pattern.nodes.size() > graph.size()) {
        return false;
    }
    
    // 检查每个节点是否匹配
    for (size_t i = 0; i < pattern.nodes.size(); ++i) {
        const auto& pattern_node = pattern.nodes[i];
        const auto& graph_node = graph[start_idx + i];
        
        // 检查类型
        if (pattern_node.type != ComputeNodeType::COMPOSITE &&
            graph_node.type != pattern_node.type) {
            return false;
        }
        
        // 检查操作名（如果指定了）
        if (!pattern_node.operation.empty() &&
            graph_node.operation.find(pattern_node.operation) == std::string::npos) {
            return false;
        }
        
        // 检查数据类型（如果指定了）
        if (!pattern_node.data_type.empty() &&
            graph_node.data_type != pattern_node.data_type) {
            return false;
        }
    }
    
    return true;
}

// ============================================
// 算子级别的完整转换流程
// ============================================

ConversionResult EnhancedCPGToAODConverter::convertWithOperators(
    const clang::FunctionDecl* func,
    const std::string& source_arch,
    const std::string& target_arch) {
    
    ConversionResult result;
    
    try {
        // 步骤1: 构建CPG (使用analyzer成员)
        if (!analyzer) {
            result.error_message = "CPG analyzer not initialized";
            return result;
        }
        
        // 步骤2: 提取计算图
        // cpg::CPGContext& cpg_context = analyzer->getCPGContext();
        cpg::CPGContext& cpg_context =
    const_cast<cpg::CPGContext&>(analyzer->getCPGContext());
        auto compute_graph = extractComputeGraphFromCPG(func, cpg_context);
        
        result.info_messages.push_back(
            "Extracted " + std::to_string(compute_graph.size()) + " compute nodes");
        
        // 步骤3: 识别算子
        auto operators = identifySIMDOperators(func, compute_graph);
        
        result.info_messages.push_back(
            "Identified " + std::to_string(operators.size()) + " SIMD operators");
        
        // 步骤4: 生成目标代码
        std::string generated_code = generateTargetCode(operators, target_arch);
        
        // 将生成的代码保存到结果中
        result.info_messages.push_back("Generated target code for " + target_arch);
        result.info_messages.push_back("Code:\n" + generated_code);
        
        result.successful = true;
        
    } catch (const std::exception& e) {
        result.successful = false;
        result.error_message = std::string("Exception during conversion: ") + e.what();
    }
    
    return result;
}

// ============================================
// 生成目标架构代码 (动态版本 - 基于规则和操作数映射)
// ============================================

std::string EnhancedCPGToAODConverter::generateTargetCode(
    const std::vector<SIMDOperator>& operators,
    const std::string& target_arch) {

    std::stringstream code;

    // 操作数名称追踪器 (动态跟踪变量名，不写死)
    std::map<std::string, std::string> operand_map;  // AVX变量名 -> 目标架构变量名
    std::set<std::string> declared_vars;             // 已声明的变量
    int temp_var_counter = 0;

    // 生成头文件
    if (target_arch == "SVE") {
        code << "#include <arm_sve.h>\n";
    } else if (target_arch == "NEON") {
        code << "#include <arm_neon.h>\n";
    }

    code << "#include <stdint.h>\n";
    code << "#include <stddef.h>\n\n";

    // 从第一个算子的AST中提取函数签名信息
    std::string func_name = "converted_function";
    std::string func_params = "uint8_t* dst, const uint8_t* src, size_t len";

    if (!operators.empty() && !operators[0].compute_pattern.ast_subtree_roots.empty()) {
        // 尝试获取父函数信息 (简化版本)
        func_name = "lower_case_" + target_arch;
    }

    code << "void " << func_name << "(" << func_params << ") {\n";

    if (target_arch == "SVE") {
        code << "#if defined(__ARM_FEATURE_SVE)\n";
        code << "    const size_t vec_len = svcntb();\n";
        code << "    svbool_t pg = svptrue_b8();\n\n";

        // 第一遍: 生成常量初始化 (从AST中提取实际值)
        code << "    // Constants extracted from source\n";
        for (const auto& op : operators) {
            if (op.compute_pattern.name == "set_constant") {
                std::string var_code = generateOperatorCodeDynamic(
                    op, target_arch, operand_map, declared_vars, temp_var_counter);
                if (!var_code.empty()) {
                    code << "    " << var_code << "\n";
                }
            }
        }

        code << "\n    uint8_t* q = dst;\n\n";
        code << "    // Main vector processing loop\n";
        code << "    while (len >= vec_len) {\n";

        // 第二遍: 生成计算操作
        for (const auto& op : operators) {
            if (op.compute_pattern.name != "set_constant") {
                std::string op_code = generateOperatorCodeDynamic(
                    op, target_arch, operand_map, declared_vars, temp_var_counter);
                if (!op_code.empty()) {
                    code << "        " << op_code << "\n";
                }
            }
        }

        code << "\n        src += vec_len;\n";
        code << "        q += vec_len;\n";
        code << "        len -= vec_len;\n";
        code << "    }\n\n";

        // 尾处理
        code << "    // Process remaining elements\n";
        code << "    if (len > 0) {\n";
        code << "        pg = svwhilelt_b8(0ULL, len);\n\n";

        // 重置计数器，重新生成代码
        declared_vars.clear();
        temp_var_counter = 0;

        for (const auto& op : operators) {
            if (op.compute_pattern.name != "set_constant") {
                std::string op_code = generateOperatorCodeDynamic(
                    op, target_arch, operand_map, declared_vars, temp_var_counter);
                if (!op_code.empty()) {
                    code << "        " << op_code << "\n";
                }
            }
        }

        code << "    }\n";
        code << "#endif\n";
    }
    else if (target_arch == "NEON") {
        code << "#if defined(__ARM_NEON)\n";
        code << "    // NEON implementation\n";
        code << "    // (256-bit operations split into 2x 128-bit)\n\n";

        // 生成NEON代码 (需要拆分256-bit到128-bit)
        for (const auto& op : operators) {
            std::string op_code = generateOperatorCodeDynamic(
                op, target_arch, operand_map, declared_vars, temp_var_counter);
            if (!op_code.empty()) {
                code << "    " << op_code << "\n";
            }
        }

        code << "#endif\n";
    }

    code << "}\n";

    return code.str();
}

// ============================================
// 生成单个算子的代码 (动态版本 - 提取实际操作数)
// ============================================

std::string EnhancedCPGToAODConverter::generateOperatorCode(
    const SIMDOperator& op,
    const std::string& target_arch) {

    // 旧版本保留为兼容接口，内部调用新版本
    std::map<std::string, std::string> dummy_map;
    std::set<std::string> dummy_set;
    int dummy_counter = 0;
    return generateOperatorCodeDynamic(op, target_arch, dummy_map, dummy_set, dummy_counter);
}

// ============================================
// 规则驱动的通用代码生成引擎
// ============================================

std::string EnhancedCPGToAODConverter::generateOperatorCodeDynamic(
    const SIMDOperator& op,
    const std::string& target_arch,
    std::map<std::string, std::string>& operand_map,
    std::set<std::string>& declared_vars,
    int& temp_var_counter) {

    std::stringstream code;
    const auto& pattern = op.compute_pattern;

    // 步骤1: 从AST中提取操作数信息
    std::vector<OperandInfo> operands = extractOperandsFromAST(op);

    // 步骤2: 查找对应的转换规则
    TransformRule* rule = findTransformRule(pattern.source_instructions[0], target_arch);

    if (!rule) {
        code << "// WARNING: No rule found for " << pattern.source_instructions[0]
             << " -> " << target_arch << "\n";
        return code.str();
    }

    // 步骤3: 使用规则的 code_template 生成代码
    std::string template_str = getCodeTemplate(rule, target_arch);

    // 步骤4: 准备替换映射
    std::map<std::string, std::string> substitutions;

    // 4.1 输出变量
    std::string output_var;
    if (pattern.name.find("store") != std::string::npos) {
        output_var = "";  // store没有返回值
    } else {
        output_var = generateOutputVarName(pattern, temp_var_counter++);
        substitutions["{{output}}"] = output_var;
    }

    // 4.2 输入操作数
    for (size_t i = 0; i < operands.size(); ++i) {
        std::string placeholder = "{{input_" + std::to_string(i) + "}}";
        std::string actual_value;

        if (operands[i].is_constant) {
            actual_value = operands[i].value;
        } else {
            actual_value = lookupOperand(operands[i].name, operand_map);
        }

        substitutions[placeholder] = actual_value;
    }

    // 4.3 特殊占位符
    substitutions["{{ret_type}}"] = getRuleReturnType(rule, target_arch);
    substitutions["{{predicate}}"] = getRulePredicate(rule, target_arch);

    // 4.4 处理特殊情况 (如 _mm256_setr_epi8 的32个参数)
    if (pattern.source_instructions[0] == "_mm256_setr_epi8") {
        std::string all_values;
        for (size_t i = 0; i < operands.size(); ++i) {
            if (i > 0) all_values += ", ";
            all_values += operands[i].is_constant ? operands[i].value : operands[i].name;
        }
        substitutions["{{input_all}}"] = all_values;
    }

    // 步骤5: 执行模板替换
    std::string generated_code = applyTemplateSubstitutions(template_str, substitutions);

    // 步骤6: 更新操作数映射表
    if (!output_var.empty()) {
        // 记录新生成的变量
        if (!operands.empty()) {
            operand_map[operands[0].name + "_result"] = output_var;
        }
        operand_map["_last_result"] = output_var;
        declared_vars.insert(output_var);
    }

    // 步骤7: 处理NEON的拆分策略
    if (target_arch == "NEON" && needsSplitting(rule)) {
        generated_code = applySplitStrategy(generated_code, rule, operands,
                                           operand_map, declared_vars, temp_var_counter);
    }

    code << generated_code;
    return code.str();
}

// ============================================
// 查找转换规则 (从规则库中)
// ============================================

TransformRule* EnhancedCPGToAODConverter::findTransformRule(
    const std::string& source_intrinsic,
    const std::string& target_arch) {

    // 在实际实现中，这应该从JSON规则文件加载
    // 这里简化为静态规则表
    static std::map<std::string, std::map<std::string, TransformRule>> rule_db;

    // 初始化规则库 (第一次调用时)
    if (rule_db.empty()) {
        initializeRuleDatabase(rule_db);
    }

    auto it = rule_db.find(source_intrinsic);
    if (it != rule_db.end()) {
        auto it2 = it->second.find(target_arch);
        if (it2 != it->second.end()) {
            return &it2->second;
        }
    }

    return nullptr;
}

// ============================================
// 初始化规则数据库
// ============================================

void EnhancedCPGToAODConverter::initializeRuleDatabase(
    std::map<std::string, std::map<std::string, TransformRule>>& db) {

    // 规则: _mm256_set1_epi8
    {
        TransformRule rule;
        rule.source_intrinsic = "_mm256_set1_epi8";
        rule.target_templates["SVE"] = "{{ret_type}} {{output}} = svdup_n_s8({{input_0}});";
        rule.target_templates["NEON"] = "int8x16_t {{output}}_lo = vdupq_n_s8({{input_0}});\nint8x16_t {{output}}_hi = vdupq_n_s8({{input_0}});";
        rule.return_types["SVE"] = "svint8_t";
        rule.return_types["NEON"] = "int8x16_t";
        rule.requires_predicate["SVE"] = false;
        rule.needs_split["NEON"] = true;
        db["_mm256_set1_epi8"]["SVE"] = rule;
        db["_mm256_set1_epi8"]["NEON"] = rule;
    }

    // 规则: _mm256_loadu_si256
    {
        TransformRule rule;
        rule.source_intrinsic = "_mm256_loadu_si256";
        rule.target_templates["SVE"] = "{{ret_type}} {{output}} = svld1_s8({{predicate}}, (const int8_t*){{input_0}});";
        rule.target_templates["NEON"] = "int8x16_t {{output}}_lo = vld1q_s8((const int8_t*){{input_0}});\nint8x16_t {{output}}_hi = vld1q_s8((const int8_t*)({{input_0}} + 16));";
        rule.return_types["SVE"] = "svint8_t";
        rule.return_types["NEON"] = "int8x16_t";
        rule.requires_predicate["SVE"] = true;
        rule.needs_split["NEON"] = true;
        db["_mm256_loadu_si256"]["SVE"] = rule;
        db["_mm256_loadu_si256"]["NEON"] = rule;
    }

    // 规则: _mm256_storeu_si256
    {
        TransformRule rule;
        rule.source_intrinsic = "_mm256_storeu_si256";
        rule.target_templates["SVE"] = "svst1_s8({{predicate}}, (int8_t*){{input_0}}, {{input_1}});";
        rule.target_templates["NEON"] = "vst1q_s8((int8_t*){{input_0}}, {{input_1}}_lo);\nvst1q_s8((int8_t*)({{input_0}} + 16), {{input_1}}_hi);";
        rule.return_types["SVE"] = "void";
        rule.return_types["NEON"] = "void";
        rule.requires_predicate["SVE"] = true;
        rule.needs_split["NEON"] = true;
        db["_mm256_storeu_si256"]["SVE"] = rule;
        db["_mm256_storeu_si256"]["NEON"] = rule;
    }

    // 规则: _mm256_cmpgt_epi8
    {
        TransformRule rule;
        rule.source_intrinsic = "_mm256_cmpgt_epi8";
        rule.target_templates["SVE"] = "svbool_t {{output}} = svcmpgt_s8({{predicate}}, {{input_0}}, {{input_1}});";
        rule.target_templates["NEON"] = "uint8x16_t {{output}}_lo = vcgtq_s8({{input_0}}_lo, {{input_1}}_lo);\nuint8x16_t {{output}}_hi = vcgtq_s8({{input_0}}_hi, {{input_1}}_hi);";
        rule.return_types["SVE"] = "svbool_t";
        rule.return_types["NEON"] = "uint8x16_t";
        rule.requires_predicate["SVE"] = true;
        rule.needs_split["NEON"] = true;
        db["_mm256_cmpgt_epi8"]["SVE"] = rule;
        db["_mm256_cmpgt_epi8"]["NEON"] = rule;
    }

    // 规则: _mm256_cmpeq_epi8
    {
        TransformRule rule;
        rule.source_intrinsic = "_mm256_cmpeq_epi8";
        rule.target_templates["SVE"] = "svbool_t {{output}} = svcmpeq_s8({{predicate}}, {{input_0}}, {{input_1}});";
        rule.return_types["SVE"] = "svbool_t";
        rule.requires_predicate["SVE"] = true;
        db["_mm256_cmpeq_epi8"]["SVE"] = rule;
    }

    // 规则: _mm256_and_si256
    {
        TransformRule rule;
        rule.source_intrinsic = "_mm256_and_si256";
        rule.target_templates["SVE"] = "{{ret_type}} {{output}} = svand_s8_z({{predicate}}, {{input_0}}, {{input_1}});";
        rule.return_types["SVE"] = "svint8_t";
        rule.requires_predicate["SVE"] = true;
        db["_mm256_and_si256"]["SVE"] = rule;
    }

    // 规则: _mm256_add_epi8
    {
        TransformRule rule;
        rule.source_intrinsic = "_mm256_add_epi8";
        rule.target_templates["SVE"] = "{{ret_type}} {{output}} = svadd_s8_z({{predicate}}, {{input_0}}, {{input_1}});";
        rule.return_types["SVE"] = "svint8_t";
        rule.requires_predicate["SVE"] = true;
        db["_mm256_add_epi8"]["SVE"] = rule;
    }

    // 规则: _mm256_setr_epi8 (特殊处理 - 32个参数)
    {
        TransformRule rule;
        rule.source_intrinsic = "_mm256_setr_epi8";
        rule.target_templates["SVE"] = "// Construct lookup table\nint8_t table_data_{{temp_id}}[32] = {{{input_all}}};\n{{ret_type}} {{output}} = svld1_s8(svptrue_b8(), table_data_{{temp_id}});";
        rule.return_types["SVE"] = "svint8_t";
        rule.requires_predicate["SVE"] = false;
        rule.special_handling = "variadic_32_args";
        db["_mm256_setr_epi8"]["SVE"] = rule;
    }

    // 规则: _mm256_shuffle_epi8
    {
        TransformRule rule;
        rule.source_intrinsic = "_mm256_shuffle_epi8";
        rule.target_templates["SVE"] = "{{ret_type}} {{output}} = svreinterpret_s8_u8(svtbl_u8(svreinterpret_u8_s8({{input_0}}), svreinterpret_u8_s8({{input_1}})));";
        rule.return_types["SVE"] = "svint8_t";
        rule.requires_predicate["SVE"] = false;
        db["_mm256_shuffle_epi8"]["SVE"] = rule;
    }

    // 规则: _mm256_movemask_epi8 (复杂 - 需要手动实现)
    {
        TransformRule rule;
        rule.source_intrinsic = "_mm256_movemask_epi8";
        rule.target_templates["SVE"] = "// Extract sign bits (no direct SVE equivalent)\nsvbool_t sign_pred_{{temp_id}} = svcmplt_s8(svptrue_b8(), {{input_0}}, svdup_n_s8(0));\nuint32_t {{output}} = extract_movemask_sve(sign_pred_{{temp_id}});";
        rule.return_types["SVE"] = "uint32_t";
        rule.requires_predicate["SVE"] = false;
        rule.special_handling = "movemask_manual";
        db["_mm256_movemask_epi8"]["SVE"] = rule;
    }

    // 规则: _mm256_srli_epi16
    {
        TransformRule rule;
        rule.source_intrinsic = "_mm256_srli_epi16";
        rule.target_templates["SVE"] = "{{ret_type}} {{output}} = svreinterpret_s16_u16(svlsr_n_u16(svreinterpret_u16_s16({{input_0}}), {{input_1}}));";
        rule.return_types["SVE"] = "svint16_t";
        rule.requires_predicate["SVE"] = false;
        db["_mm256_srli_epi16"]["SVE"] = rule;
    }

    // 规则: _mm256_subs_epu8
    {
        TransformRule rule;
        rule.source_intrinsic = "_mm256_subs_epu8";
        rule.target_templates["SVE"] = "{{ret_type}} {{output}} = svqsub_u8({{input_0}}, {{input_1}});";
        rule.return_types["SVE"] = "svuint8_t";
        rule.requires_predicate["SVE"] = false;
        db["_mm256_subs_epu8"]["SVE"] = rule;
    }

    // 规则: _mm256_or_si256
    {
        TransformRule rule;
        rule.source_intrinsic = "_mm256_or_si256";
        rule.target_templates["SVE"] = "{{ret_type}} {{output}} = svorr_s8_z({{predicate}}, {{input_0}}, {{input_1}});";
        rule.return_types["SVE"] = "svint8_t";
        rule.requires_predicate["SVE"] = true;
        db["_mm256_or_si256"]["SVE"] = rule;
    }
}

// ============================================
// 辅助函数: 获取规则的代码模板
// ============================================

std::string EnhancedCPGToAODConverter::getCodeTemplate(
    const TransformRule* rule,
    const std::string& target_arch) {

    auto it = rule->target_templates.find(target_arch);
    if (it != rule->target_templates.end()) {
        return it->second;
    }
    return "";
}

std::string EnhancedCPGToAODConverter::getRuleReturnType(
    const TransformRule* rule,
    const std::string& target_arch) {

    auto it = rule->return_types.find(target_arch);
    if (it != rule->return_types.end()) {
        return it->second;
    }
    return "auto";
}

std::string EnhancedCPGToAODConverter::getRulePredicate(
    const TransformRule* rule,
    const std::string& target_arch) {

    if (rule->requires_predicate.count(target_arch) &&
        rule->requires_predicate.at(target_arch)) {
        return "pg";
    }
    return "";
}

bool EnhancedCPGToAODConverter::needsSplitting(const TransformRule* rule) {
    return rule->needs_split.count("NEON") && rule->needs_split.at("NEON");
}

// ============================================
// 模板替换引擎
// ============================================

std::string EnhancedCPGToAODConverter::applyTemplateSubstitutions(
    const std::string& template_str,
    const std::map<std::string, std::string>& substitutions) {

    std::string result = template_str;

    // 按长度排序(长的先替换,避免嵌套问题)
    std::vector<std::pair<std::string, std::string>> sorted_subs(
        substitutions.begin(), substitutions.end());

    std::sort(sorted_subs.begin(), sorted_subs.end(),
        [](const auto& a, const auto& b) { return a.first.size() > b.first.size(); });

    for (const auto& [placeholder, value] : sorted_subs) {
        size_t pos = 0;
        while ((pos = result.find(placeholder, pos)) != std::string::npos) {
            result.replace(pos, placeholder.size(), value);
            pos += value.size();
        }
    }

    return result;
}

// ============================================
// 生成输出变量名
// ============================================

std::string EnhancedCPGToAODConverter::generateOutputVarName(
    const ComputeGraphPattern& pattern,
    int counter) {

    std::string prefix;
    if (pattern.name.find("load") != std::string::npos) {
        prefix = "loaded";
    } else if (pattern.name.find("cmp") != std::string::npos) {
        prefix = "cmp_result";
    } else if (pattern.name.find("add") != std::string::npos) {
        prefix = "sum";
    } else if (pattern.name.find("and") != std::string::npos) {
        prefix = "and_result";
    } else if (pattern.name.find("shuffle") != std::string::npos) {
        prefix = "shuffled";
    } else {
        prefix = "result";
    }

    return prefix + "_" + std::to_string(counter);
}

// ============================================
// 应用 NEON 拆分策略 (将 256-bit 操作拆分为 2x 128-bit)
// ============================================

std::string EnhancedCPGToAODConverter::applySplitStrategy(
    const std::string& code,
    const TransformRule* rule,
    const std::vector<OperandInfo>& operands,
    std::map<std::string, std::string>& operand_map,
    std::set<std::string>& declared_vars,
    int& temp_var_counter) {

    // NEON 拆分策略已经在规则的 target_templates["NEON"] 中定义
    // 这里不需要额外处理，直接返回代码
    return code;
}

// ============================================
// 从AST中提取操作数信息 (关键函数!)
// ============================================

std::vector<EnhancedCPGToAODConverter::OperandInfo>
EnhancedCPGToAODConverter::extractOperandsFromAST(const SIMDOperator& op) {

    std::vector<OperandInfo> operands;

    const auto& pattern = op.compute_pattern;

    // 遍历AST子树提取操作数
    for (const auto* stmt : pattern.ast_subtree_roots) {
        if (auto* call = clang::dyn_cast<clang::CallExpr>(stmt)) {
            // 提取函数调用的参数
            for (unsigned i = 0; i < call->getNumArgs(); ++i) {
                const clang::Expr* arg = call->getArg(i);
                OperandInfo info = extractOperandFromExpr(arg);
                if (!info.name.empty()) {
                    operands.push_back(info);
                }
            }
        }
    }

    return operands;
}

// ============================================
// 从表达式提取操作数信息
// ============================================

EnhancedCPGToAODConverter::OperandInfo
EnhancedCPGToAODConverter::extractOperandFromExpr(const clang::Expr* expr) {

    OperandInfo info;
    info.is_constant = false;

    if (!expr) return info;

    // 去除隐式类型转换
    expr = expr->IgnoreImpCasts();

    // 检查是否是变量引用
    if (auto* decl_ref = clang::dyn_cast<clang::DeclRefExpr>(expr)) {
        info.name = decl_ref->getDecl()->getNameAsString();
        return info;
    }

    // 检查是否是字符常量
    if (auto* char_lit = clang::dyn_cast<clang::CharacterLiteral>(expr)) {
        info.is_constant = true;
        info.value = "'" + std::string(1, (char)char_lit->getValue()) + "'";
        info.name = "_const_" + std::to_string(char_lit->getValue());
        return info;
    }

    // 检查是否是整数常量
    if (auto* int_lit = clang::dyn_cast<clang::IntegerLiteral>(expr)) {
        info.is_constant = true;
        info.value = std::to_string(int_lit->getValue().getLimitedValue());
        info.name = "_const_" + info.value;
        return info;
    }

    // 检查是否是数组下标
    if (auto* array_sub = clang::dyn_cast<clang::ArraySubscriptExpr>(expr)) {
        // 提取基址
        if (auto* base_decl = clang::dyn_cast<clang::DeclRefExpr>(
                array_sub->getBase()->IgnoreImpCasts())) {
            info.name = base_decl->getDecl()->getNameAsString();
            // 可以进一步提取下标
            return info;
        }
    }

    // 检查是否是一元操作符(如取地址&, 解引用*)
    if (auto* unary = clang::dyn_cast<clang::UnaryOperator>(expr)) {
        return extractOperandFromExpr(unary->getSubExpr());
    }

    // 检查是否是二元操作符(如算术运算)
    if (auto* binary = clang::dyn_cast<clang::BinaryOperator>(expr)) {
        // 对于如 'a' - 'A' 这样的表达式，尝试求值
        clang::Expr::EvalResult eval_result;
        if (expr->EvaluateAsInt(eval_result, ast_context)) {
            info.is_constant = true;
            info.value = std::to_string(eval_result.Val.getInt().getLimitedValue());
            info.name = "_const_" + info.value;
            return info;
        }
    }

    return info;
}

// ============================================
// 辅助函数: 生成目标架构的变量名
// ============================================

std::string EnhancedCPGToAODConverter::generateTargetVarName(
    const std::string& source_name,
    const std::string& target_arch) {

    // 去除源架构前缀 (_mm256, __m256i等)
    std::string clean_name = source_name;

    // 移除常见的AVX2前缀
    if (clean_name.find("__m256") == 0) {
        clean_name = clean_name.substr(6);  // 去掉 "__m256"
    }

    if (clean_name.empty() || clean_name[0] == '_') {
        clean_name = "var" + clean_name;
    }

    return clean_name;
}

// ============================================
// 辅助函数: 查找操作数映射
// ============================================

std::string EnhancedCPGToAODConverter::lookupOperand(
    const std::string& source_name,
    const std::map<std::string, std::string>& operand_map) {

    auto it = operand_map.find(source_name);
    if (it != operand_map.end()) {
        return it->second;
    }

    // 未找到映射，返回原名(可能是函数参数等)
    return source_name;
}

// ============================================
// 将算子映射回AST子树
// ============================================

std::vector<const clang::Stmt*> EnhancedCPGToAODConverter::mapOperatorToASTSubtree(
    const SIMDOperator& op) {

    return op.compute_pattern.ast_subtree_roots;
}

// ============================================
// 获取转换规则
// ============================================

std::vector<ComputeGraphPattern> EnhancedCPGToAODConverter::getConversionRules(
    const std::string& source_arch,
    const std::string& target_arch) {

    if (source_arch == "AVX2" && target_arch == "SVE") {
        return createAVX2ToSVERules();
    }
    // 可以添加更多架构组合

    return {};
}

} // namespace aodsolve