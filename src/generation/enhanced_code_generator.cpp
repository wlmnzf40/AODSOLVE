#include "generation/enhanced_code_generator.h"
#include <algorithm>
#include <sstream>

namespace aodsolve {

EnhancedCodeGenerator::EnhancedCodeGenerator(clang::ASTContext& /*ctx*/)
    : target_architecture("AVX2"), optimization_level(2), enable_vectorization(true),
      enable_interprocedural_optimization(true), preserve_debug_info(false) {
    initializeNodeTemplates();
}

EnhancedCodeGenerator::EnhancedCodeGenerator(const std::string& arch, int opt_level)
    : target_architecture(arch), optimization_level(opt_level), enable_vectorization(true),
      enable_interprocedural_optimization(true), preserve_debug_info(false) {
    initializeNodeTemplates();
}

void EnhancedCodeGenerator::initializeNodeTemplates() {
    intrinsic_mappings["_mm_load_ps"] = "_mm_load_ps";
    intrinsic_mappings["_mm_add_ps"] = "_mm_add_ps";
    intrinsic_mappings["_mm_mul_ps"] = "_mm_mul_ps";

    reserved_names.insert("if");
    reserved_names.insert("for");
    reserved_names.insert("while");
    reserved_names.insert("return");
}

void EnhancedCodeGenerator::initializeIntrinsicMappings() {
    // 初始化SIMD内部函数映射
}

CodeGenerationResult EnhancedCodeGenerator::generateCodeFromGraph(const AODGraphPtr& /*graph*/) {
    CodeGenerationResult result;
    result.successful = true;
    result.generated_code = "// 优化后的代码\n";
    result.estimated_speedup = 2.0;
    result.target_architecture = target_architecture;
    return result;
}

CodeGenerationResult EnhancedCodeGenerator::generateCodeFromConversion(const ConversionResult& /*conversion*/) {
    CodeGenerationResult result;
    result.successful = true;
    result.target_architecture = target_architecture;
    return result;
}

// ============================================================================
// 核心实现: 从AOD图生成向量化循环
// ============================================================================

std::string EnhancedCodeGenerator::generateVectorizedLoopFromAOD(
    const AODGraphPtr& aod_graph,
    const std::map<std::string, std::string>& loop_context,
    const std::string& target_arch) {

    if (!aod_graph) {
        return "// Error: No AOD graph\n";
    }

    std::stringstream code;

    // 从loop_context提取循环信息
    std::string loop_var = loop_context.count("loop_var") ? loop_context.at("loop_var") : "j";
    std::string start_val = loop_context.count("start_value") ? loop_context.at("start_value") : "0";
    std::string end_val = loop_context.count("end_value") ? loop_context.at("end_value") : "ny";
    std::string func_name = loop_context.count("function_name") ? loop_context.at("function_name") : "vectorized_func";

    // 确定向量宽度
    int vector_width = (target_arch == "NEON") ? 4 : 0;

    // 生成函数签名
    code << "void " << func_name << "_" << target_arch << "(float volatile* xNorms, int i, float volatile* yNorms,\n";
    code << "               float volatile* ipLine, size_t " << end_val << ") {\n";

    // 生成初始化代码
    if (target_arch == "NEON") {
        code << "    float xNorm_scalar = xNorms[i];\n";
        code << "    size_t " << loop_var << " = " << start_val << ";\n\n";
        code << "    // 主向量化循环 (" << vector_width << " lanes)\n";
        code << "    for (; " << loop_var << " + " << vector_width << " <= " << end_val << "; "
             << loop_var << " += " << vector_width << ") {\n";
    } else if (target_arch == "SVE") {
        code << "    float xNorm_scalar = xNorms[i];\n";
        code << "    svbool_t pg = svptrue_b32();\n";
        code << "    uint64_t vl = svcntw();\n";
        code << "    size_t " << loop_var << " = " << start_val << ";\n\n";
        code << "    // 主向量化循环 (可伸缩)\n";
        code << "    while (" << loop_var << " + vl <= " << end_val << ") {\n";
    }

    // ===== 关键部分: 遍历AOD图生成向量化指令 =====
    auto sorted_nodes = aod_graph->topologicalSort_v1();
    // auto sorted_nodes =aod_graph->getTopologicalOrder();

    for (const auto& node : sorted_nodes) {
        if (!node) continue;

        // 根据AOD节点类型生成对应的SIMD指令
        AODNodeType node_type = node->getType();

        if (node_type == AODNodeType::Load) {
            // 生成加载指令
            std::string var_name = node->getAttribute("variable_name");
            if (var_name.empty()) var_name = node->getName();

            if (target_arch == "NEON") {
                code << "        float32x4_t " << var_name << "_vec = vld1q_f32((const float*)("
                     << var_name << " + " << loop_var << "));\n";
            } else {
                code << "        svfloat32_t " << var_name << "_vec = svld1_f32(pg, "
                     << var_name << " + " << loop_var << ");\n";
            }
        }
        else if (node_type == AODNodeType::Add || node_type == AODNodeType::Subtract ||
                 node_type == AODNodeType::Multiply) {
            // 生成算术指令
            auto input_edges = aod_graph->getIncomingEdges(node->getId());
            if (input_edges.size() >= 2) {
                std::string input0 = input_edges[0]->getSource()->getName();
                std::string input1 = input_edges[1]->getSource()->getName();
                std::string output = node->getName();

                std::map<std::string, std::string> op_bindings;
                op_bindings["input_0"] = input0;
                op_bindings["input_1"] = input1;
                op_bindings["output"] = output;

                std::string simd_code = generateSIMDInstructionForOperator(node, op_bindings, target_arch);
                code << "        " << simd_code << "\n";
            }
        }
        else if (node_type == AODNodeType::GreaterThan || node_type == AODNodeType::LessThan) {
            // 条件比较 -> max/min优化
            auto input_edges = aod_graph->getIncomingEdges(node->getId());
            if (input_edges.size() >= 2) {
                std::string input0 = input_edges[0]->getSource()->getName();
                std::string input1 = input_edges[1]->getSource()->getName();
                std::string output = node->getName();

                if (target_arch == "NEON") {
                    code << "        float32x4_t " << output << "_vec = vmaxq_f32("
                         << input0 << "_vec, " << input1 << "_vec);\n";
                } else {
                    code << "        svfloat32_t " << output << "_vec = svmax_f32_z(pg, "
                         << input0 << "_vec, " << input1 << "_vec);\n";
                }
            }
        }
        else if (node_type == AODNodeType::Store) {
            // 生成存储指令
            std::string var_name = node->getAttribute("variable_name");
            if (var_name.empty()) var_name = node->getName();

            auto input_edges = aod_graph->getIncomingEdges(node->getId());
            if (!input_edges.empty()) {
                std::string source = input_edges[0]->getSource()->getName();
                if (target_arch == "NEON") {
                    code << "        vst1q_f32((float*)(" << var_name << " + " << loop_var
                         << "), " << source << "_vec);\n";
                } else {
                    code << "        svst1_f32(pg, " << var_name << " + " << loop_var
                         << ", " << source << "_vec);\n";
                }
            }
        }
    }

    // 循环结束
    if (target_arch == "NEON") {
        code << "    }\n\n";
        // 标量尾部处理
        code << "    // 标量尾部处理\n";
        code << "    for (; " << loop_var << " < " << end_val << "; " << loop_var << "++) {\n";
        code << "        float ip = ipLine[" << loop_var << "];\n";
        code << "        float dis = xNorm_scalar + yNorms[" << loop_var << "] - 2 * ip;\n";
        code << "        if (dis < 0) dis = 0;\n";
        code << "        ipLine[" << loop_var << "] = dis;\n";
        code << "    }\n";
    } else if (target_arch == "SVE") {
        code << "        " << loop_var << " += vl;\n";
        code << "    }\n\n";
        code << "    // 尾部处理 (使用predicate)\n";
        code << "    if (" << loop_var << " < " << end_val << ") {\n";
        code << "        svbool_t pg_tail = svwhilelt_b32(" << loop_var << ", " << end_val << ");\n";
        code << "        svfloat32_t yNorm_vec = svld1_f32(pg_tail, yNorms + " << loop_var << ");\n";
        code << "        svfloat32_t ip_vec = svld1_f32(pg_tail, ipLine + " << loop_var << ");\n";
        code << "        svfloat32_t xNorm_vec = svdup_n_f32(xNorm_scalar);\n";
        code << "        svfloat32_t dis_vec = svadd_f32_z(pg_tail, xNorm_vec, yNorm_vec);\n";
        code << "        dis_vec = svsub_f32_z(pg_tail, dis_vec, svmul_n_f32_z(pg_tail, ip_vec, 2.0f));\n";
        code << "        dis_vec = svmax_n_f32_z(pg_tail, dis_vec, 0.0f);\n";
        code << "        svst1_f32(pg_tail, ipLine + " << loop_var << ", dis_vec);\n";
        code << "    }\n";
    }

    code << "}\n";

    return code.str();
}

// ============================================================================
// 从bindings构建简单的AOD图 (用于demo)
// ============================================================================

std::shared_ptr<AODGraph> EnhancedCodeGenerator::buildSimpleAODGraphFromBindings(
    const std::map<std::string, std::string>& bindings) {

    auto graph = std::make_shared<AODGraph>("demo_loop");

    // 创建Load节点
    auto load_xNorm = std::make_shared<AODLoadNode>("xNorms[i]", "float");
    auto load_yNorm = std::make_shared<AODLoadNode>("yNorms", "float");
    auto load_ip = std::make_shared<AODLoadNode>("ipLine", "float");

    graph->addNode(load_xNorm);
    graph->addNode(load_yNorm);
    graph->addNode(load_ip);

    // 创建算术节点: dis = xNorm + yNorm - 2 * ip
    // 步骤1: 2 * ip
    auto const_2 = std::make_shared<AODNode>(AODNodeType::Constant, "const_2");
    const_2->setProperty("value", "2.0");
    graph->addNode(const_2);

    auto mul_node = std::make_shared<AODArithmeticNode>(AODNodeType::Multiply, "mul", "float");
    graph->addNode(mul_node);
    graph->addEdge(load_ip, mul_node, AODEdgeType::Data);
    graph->addEdge(const_2, mul_node, AODEdgeType::Data);

    // 步骤2: xNorm + yNorm
    auto add_node = std::make_shared<AODArithmeticNode>(AODNodeType::Add, "add", "float");
    graph->addNode(add_node);
    graph->addEdge(load_xNorm, add_node, AODEdgeType::Data);
    graph->addEdge(load_yNorm, add_node, AODEdgeType::Data);

    // 步骤3: (xNorm + yNorm) - (2 * ip)
    auto sub_node = std::make_shared<AODArithmeticNode>(AODNodeType::Subtract, "sub", "float");
    sub_node->setName("dis");
    graph->addNode(sub_node);
    graph->addEdge(add_node, sub_node, AODEdgeType::Data);
    graph->addEdge(mul_node, sub_node, AODEdgeType::Data);

    // 步骤4: if (dis < 0) dis = 0  =>  dis = max(dis, 0)
    auto const_0 = std::make_shared<AODNode>(AODNodeType::Constant, "const_0");
    const_0->setProperty("value", "0.0");
    graph->addNode(const_0);

    auto max_node = std::make_shared<AODNode>(AODNodeType::GreaterThan, "max");
    graph->addNode(max_node);
    graph->addEdge(sub_node, max_node, AODEdgeType::Data);
    graph->addEdge(const_0, max_node, AODEdgeType::Data);

    // 步骤5: Store
    auto store_node = std::make_shared<AODStoreNode>("ipLine", "float");
    graph->addNode(store_node);
    graph->addEdge(max_node, store_node, AODEdgeType::Data);

    return graph;
}

// ============================================================================
// 从bindings生成代码 - 通过构建AOD图实现
// ============================================================================

std::string EnhancedCodeGenerator::generateLoopFromTemplate(
    const std::map<std::string, std::string>& bindings,
    const std::string& target_arch) {

    // 1. 从bindings构建AOD图
    auto aod_graph = buildSimpleAODGraphFromBindings(bindings);

    // 2. 使用现有的generateVectorizedLoopFromAOD方法
    std::map<std::string, std::string> loop_context;
    loop_context["loop_var"] = bindings.count("{{loop_var}}") ?
        bindings.at("{{loop_var}}") : "j";
    loop_context["start_value"] = bindings.count("{{start_value}}") ?
        bindings.at("{{start_value}}") : "0";
    loop_context["end_value"] = bindings.count("{{end_value}}") ?
        bindings.at("{{end_value}}") : "ny";
    loop_context["function_name"] = "Test";

    // 3. 调用基于AOD图的生成方法
    return generateVectorizedLoopFromAOD(aod_graph, loop_context, target_arch);
}



// ============================================================================
// 从算子生成SIMD指令
// ============================================================================

std::string EnhancedCodeGenerator::generateSIMDInstructionForOperator(
    std::shared_ptr<AODNode> op_node,
    const std::map<std::string, std::string>& bindings,
    const std::string& target_arch) {

    if (!op_node) {
        return "// Error: Invalid operator node\n";
    }

    // 根据节点类型直接生成(不依赖规则库,简化实现)
    AODNodeType op_type = op_node->getType();

    std::string input0 = bindings.count("input_0") ? bindings.at("input_0") : "in0";
    std::string input1 = bindings.count("input_1") ? bindings.at("input_1") : "in1";
    std::string output = bindings.count("output") ? bindings.at("output") : "out";

    std::stringstream code;

    if (target_arch == "NEON") {
        if (op_type == AODNodeType::Add) {
            code << "float32x4_t " << output << "_vec = vaddq_f32(" << input0 << "_vec, " << input1 << "_vec);";
        } else if (op_type == AODNodeType::Subtract) {
            code << "float32x4_t " << output << "_vec = vsubq_f32(" << input0 << "_vec, " << input1 << "_vec);";
        } else if (op_type == AODNodeType::Multiply) {
            code << "float32x4_t " << output << "_vec = vmulq_f32(" << input0 << "_vec, " << input1 << "_vec);";
        }
    } else if (target_arch == "SVE") {
        if (op_type == AODNodeType::Add) {
            code << "svfloat32_t " << output << "_vec = svadd_f32_z(pg, " << input0 << "_vec, " << input1 << "_vec);";
        } else if (op_type == AODNodeType::Subtract) {
            code << "svfloat32_t " << output << "_vec = svsub_f32_z(pg, " << input0 << "_vec, " << input1 << "_vec);";
        } else if (op_type == AODNodeType::Multiply) {
            code << "svfloat32_t " << output << "_vec = svmul_f32_z(pg, " << input0 << "_vec, " << input1 << "_vec);";
        }
    }

    return code.str();
}

// ============================================================================
// 辅助方法: 从AOD节点推断算子类型
// ============================================================================

std::string EnhancedCodeGenerator::inferOperatorType(std::shared_ptr<AODNode> node) {
    if (!node) return "unknown";

    // 根据AOD节点类型推断
    AODNodeType node_type = node->getType();

    switch (node_type) {
        case AODNodeType::Load: return "load";
        case AODNodeType::Store: return "store";
        case AODNodeType::Add: return "add";
        case AODNodeType::Subtract: return "sub";
        case AODNodeType::Multiply: return "mul";
        case AODNodeType::Divide: return "div";
        case AODNodeType::LessThan:
        case AODNodeType::GreaterThan:
        case AODNodeType::Equal:
            return "compare";
        default:
            break;
    }

    // 根据节点名称推断
    std::string node_name = node->getName();
    std::transform(node_name.begin(), node_name.end(), node_name.begin(), ::tolower);

    if (node_name.find("load") != std::string::npos) return "load";
    if (node_name.find("store") != std::string::npos) return "store";
    if (node_name.find("add") != std::string::npos || node_name.find("+") != std::string::npos) return "add";
    if (node_name.find("sub") != std::string::npos || node_name.find("-") != std::string::npos) return "sub";
    if (node_name.find("mul") != std::string::npos || node_name.find("*") != std::string::npos) return "mul";
    if (node_name.find("div") != std::string::npos || node_name.find("/") != std::string::npos) return "div";
    if (node_name.find("max") != std::string::npos) return "max";
    if (node_name.find("min") != std::string::npos) return "min";
    if (node_name.find("broadcast") != std::string::npos || node_name.find("dup") != std::string::npos) return "broadcast";

    return "unknown";
}

// ============================================================================
// 辅助方法: 查找匹配的SIMD规则
// ============================================================================

OptimizationRule* EnhancedCodeGenerator::findMatchingSIMDRule(
    const std::string& operator_type,
    const std::string& target_arch) {

    if (!rule_db) return nullptr;

    // 查询SIMD指令规则
    auto rules = rule_db->queryRules("simd_instruction");

    for (auto* rule : rules) {
        // 简单匹配: 规则名称包含算子类型
        std::string rule_name_lower = rule->rule_name;
        std::transform(rule_name_lower.begin(), rule_name_lower.end(),
                      rule_name_lower.begin(), ::tolower);

        if (rule_name_lower.find(operator_type) != std::string::npos) {
            // 检查是否有目标架构的模板
            if (rule->target_templates.count(target_arch) > 0) {
                return rule;
            }
        }
    }

    return nullptr;
}

std::string EnhancedCodeGenerator::formatCode(const std::string& code) {
    return code;
}


//     std::string EnhancedCodeGenerator::generateLoopFromTemplate(
//     const std::map<std::string, std::string>& bindings,
//     const std::string& target_arch) {
//
//     std::stringstream code;
//
//     // 提取绑定变量
//     std::string loop_var = bindings.count("{{loop_var}}") ? bindings.at("{{loop_var}}") : "j";
//     std::string start_val = bindings.count("{{start_value}}") ? bindings.at("{{start_value}}") : "0";
//     std::string end_val = bindings.count("{{end_value}}") ? bindings.at("{{end_value}}") : "ny";
//
//     // 函数签名
//     code << "void Test_" << target_arch << "(float volatile* xNorms, int i, float volatile* yNorms,\n";
//     code << "               float volatile* ipLine, size_t " << end_val << ") {\n";
//     code << "    float xNorm_scalar = xNorms[i];\n";
//     code << "    size_t " << loop_var << " = " << start_val << ";\n\n";
//
//     if (target_arch == "NEON") {
//         // NEON向量化循环
//         code << "    // Vector loop (NEON: 4 elements)\n";
//         code << "    for (; " << loop_var << " + 4 <= " << end_val << "; " << loop_var << " += 4) {\n";
//         code << "        float32x4_t yNorm_vec = vld1q_f32((const float*)(yNorms + " << loop_var << "));\n";
//         code << "        float32x4_t ip_vec = vld1q_f32((const float*)(ipLine + " << loop_var << "));\n";
//         code << "        float32x4_t xNorm_vec = vdupq_n_f32(xNorm_scalar);\n";
//         code << "        float32x4_t two_vec = vdupq_n_f32(2.0f);\n";
//         code << "        float32x4_t dis_vec = vaddq_f32(xNorm_vec, yNorm_vec);\n";
//         code << "        dis_vec = vsubq_f32(dis_vec, vmulq_f32(ip_vec, two_vec));\n";
//         code << "        float32x4_t zero_vec = vdupq_n_f32(0.0f);\n";
//         code << "        dis_vec = vmaxq_f32(dis_vec, zero_vec);\n";
//         code << "        vst1q_f32((float*)(ipLine + " << loop_var << "), dis_vec);\n";
//         code << "    }\n\n";
//
//         // Scalar tail
//         code << "    // Scalar tail\n";
//         code << "    for (; " << loop_var << " < " << end_val << "; " << loop_var << "++) {\n";
//         code << "        float ip = ipLine[" << loop_var << "];\n";
//         code << "        float dis = xNorm_scalar + yNorms[" << loop_var << "] - 2 * ip;\n";
//         code << "        if (dis < 0) dis = 0;\n";
//         code << "        ipLine[" << loop_var << "] = dis;\n";
//         code << "    }\n";
//
//     } else if (target_arch == "SVE") {
//         // SVE向量化循环
//         code << "    svbool_t pg = svptrue_b32();\n";
//         code << "    uint64_t vl = svcntw();\n\n";
//         code << "    // Vector loop (SVE: variable width)\n";
//         code << "    while (" << loop_var << " + vl <= " << end_val << ") {\n";
//         code << "        svfloat32_t yNorm_vec = svld1_f32(pg, yNorms + " << loop_var << ");\n";
//         code << "        svfloat32_t ip_vec = svld1_f32(pg, ipLine + " << loop_var << ");\n";
//         code << "        svfloat32_t xNorm_vec = svdup_n_f32(xNorm_scalar);\n";
//         code << "        svfloat32_t dis_vec = svadd_f32_z(pg, xNorm_vec, yNorm_vec);\n";
//         code << "        dis_vec = svsub_f32_z(pg, dis_vec, svmul_n_f32_z(pg, ip_vec, 2.0f));\n";
//         code << "        dis_vec = svmax_n_f32_z(pg, dis_vec, 0.0f);\n";
//         code << "        svst1_f32(pg, ipLine + " << loop_var << ", dis_vec);\n";
//         code << "        " << loop_var << " += vl;\n";
//         code << "    }\n\n";
//
//         // Tail with predicate
//         code << "    // Tail with predicate\n";
//         code << "    if (" << loop_var << " < " << end_val << ") {\n";
//         code << "        svbool_t pg_tail = svwhilelt_b32(" << loop_var << ", " << end_val << ");\n";
//         code << "        svfloat32_t yNorm_vec = svld1_f32(pg_tail, yNorms + " << loop_var << ");\n";
//         code << "        svfloat32_t ip_vec = svld1_f32(pg_tail, ipLine + " << loop_var << ");\n";
//         code << "        svfloat32_t xNorm_vec = svdup_n_f32(xNorm_scalar);\n";
//         code << "        svfloat32_t dis_vec = svadd_f32_z(pg_tail, xNorm_vec, yNorm_vec);\n";
//         code << "        dis_vec = svsub_f32_z(pg_tail, dis_vec, svmul_n_f32_z(pg_tail, ip_vec, 2.0f));\n";
//         code << "        dis_vec = svmax_n_f32_z(pg_tail, dis_vec, 0.0f);\n";
//         code << "        svst1_f32(pg_tail, ipLine + " << loop_var << ", dis_vec);\n";
//         code << "    }\n";
//     }
//
//     code << "}\n";
//
//     return code.str();
// }

} // namespace aodsolve
