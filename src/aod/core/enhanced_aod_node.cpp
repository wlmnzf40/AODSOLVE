#include "aod/enhanced_aod_node.h"
#include <sstream>
#include <algorithm>
#include <stdexcept>

namespace aodsolve {

// AODNode 实现
AODNode::AODNode(AODNodeType t, const std::string& name)
    : type(t), properties{} {
    id = next_id++;
    properties.name = name.empty() ? nodeTypeToString(t) + "_" + std::to_string(id) : name;
}

void AODNode::addInput(std::shared_ptr<AODNode> input) {
    if (input && std::find(inputs.begin(), inputs.end(), input) == inputs.end()) {
        inputs.push_back(input);
        input->addOutput(shared_from_this());
    }
}

void AODNode::addOutput(std::shared_ptr<AODNode> output) {
    if (output && std::find(outputs.begin(), outputs.end(), output) == outputs.end()) {
        outputs.push_back(output);
    }
}

void AODNode::setProperty(const std::string& key, const std::string& value) {
    properties.attributes[key] = value;
}

std::string AODNode::getProperty(const std::string& key, const std::string& default_value) const {
    auto it = properties.attributes.find(key);
    return (it != properties.attributes.end()) ? it->second : default_value;
}

// 虚函数默认实现
std::vector<std::string> AODNode::getUsedVariables() const { return {}; }
std::vector<std::string> AODNode::getDefinedVariables() const { return {}; }
bool AODNode::isSideEffectFree() const { return !properties.has_side_effects; }
bool AODNode::isSafeToReorder() const { return isSideEffectFree() && !isControlNode(); }
int AODNode::getComplexity() const { return properties.complexity; }
void AODNode::optimize() { properties.is_computed = true; }
bool AODNode::canCompress() const { return getInputs().size() == 1 && getOutputs().size() == 1; }

bool AODNode::isControlNode() const {
    switch (type) {
        case AODNodeType::Control:
        case AODNodeType::If:
        case AODNodeType::Loop:
        case AODNodeType::Switch:
        case AODNodeType::Break:
        case AODNodeType::Continue:
        case AODNodeType::Return:
            return true;
        default: return false;
    }
}

bool AODNode::isDataNode() const {
    return !isControlNode() && type != AODNodeType::Entry && type != AODNodeType::Exit && type != AODNodeType::BlockEnd;
}

bool AODNode::isSIMDNode() const {
    switch (type) {
        case AODNodeType::SIMD_Load:
        case AODNodeType::SIMD_Store:
        case AODNodeType::SIMD_Arithmetic:
        case AODNodeType::SIMD_Compare:
        case AODNodeType::SIMD_Blend:
        case AODNodeType::SIMD_Shuffle:
        case AODNodeType::SIMD_Permute:
        case AODNodeType::SIMD_Intrinsic:
            return true;
        default: return false;
    }
}

bool AODNode::isCallNode() const { return type == AODNodeType::Call; }

bool AODNode::isValid() const { return !properties.name.empty(); }

std::vector<std::string> AODNode::getValidationErrors() const {
    if (properties.name.empty()) return {"Empty name"};
    return {};
}

std::shared_ptr<AODNode> AODNode::clone() const {
    auto node = std::make_shared<AODNode>(type, properties.name);
    node->properties = properties;
    node->original_ast_stmt = original_ast_stmt;
    return node;
}

std::string AODNode::toString() const {
    return "Node[" + std::to_string(id) + "]: " + properties.name;
}

std::string AODNode::getDOTLabel() const { return properties.name; }
std::string AODNode::getDOTStyle() const {
    if (type == AODNodeType::BlockEnd) return "shape=point";
    if (isControlNode()) return "shape=diamond";
    if (isSIMDNode()) return "style=filled,fillcolor=lightblue";
    return "";
}

void AODNode::validateInputs() {}

// ============================================
// 子类实现
// ============================================

AODLoadNode::AODLoadNode(const std::string& var, const std::string& type, bool deref)
    : AODNode(AODNodeType::Load, "Load_" + var), var_name(var), var_type(type), is_dereference(deref) {}
std::vector<std::string> AODLoadNode::getUsedVariables() const { return {var_name}; }
std::string AODLoadNode::toString() const { return "Load " + var_name; }
std::string AODLoadNode::getDOTLabel() const { return "Load\\n" + var_name; }

AODStoreNode::AODStoreNode(const std::string& var, const std::string& type, bool deref)
    : AODNode(AODNodeType::Store, "Store_" + var), var_name(var), var_type(type), is_dereference(deref) {}
std::vector<std::string> AODStoreNode::getDefinedVariables() const { return {var_name}; }
std::vector<std::string> AODStoreNode::getUsedVariables() const { return {}; }
bool AODStoreNode::isSideEffectFree() const { return false; }
std::string AODStoreNode::toString() const { return "Store " + var_name; }
std::string AODStoreNode::getDOTLabel() const { return "Store\\n" + var_name; }

AODArithmeticNode::AODArithmeticNode(AODNodeType op, const std::string& op_name, const std::string& type)
    : AODNode(op, op_name), operation(op_name), result_type(type) {}
std::vector<std::string> AODArithmeticNode::getUsedVariables() const { return operands; }
std::string AODArithmeticNode::toString() const { return operation; }
std::string AODArithmeticNode::getDOTLabel() const { return operation; }

AODSIMDNode::AODSIMDNode(AODNodeType type, const std::string& simd_type, const std::string& op_name)
    : AODNode(type, op_name), simd_type(simd_type), operation_name(op_name) {}
std::vector<std::string> AODSIMDNode::getUsedVariables() const { return vector_operands; }
std::string AODSIMDNode::toString() const { return operation_name; }
std::string AODSIMDNode::getDOTLabel() const { return operation_name; }

AODCallNode::AODCallNode(const std::string& func_name, const std::string& ret_type)
    : AODNode(AODNodeType::Call, "Call_" + func_name), function_name(func_name), return_type(ret_type) {}
bool AODCallNode::isSideEffectFree() const { return false; }
std::vector<std::string> AODCallNode::getUsedVariables() const { return arguments; }
std::vector<std::string> AODCallNode::getDefinedVariables() const { return {}; }
std::string AODCallNode::toString() const { return "Call " + function_name; }
std::string AODCallNode::getDOTLabel() const { return "Call\\n" + function_name; }

AODControlNode::AODControlNode(const std::string& ctrl_type, const std::string& cond, bool uncond)
    : AODNode(AODNodeType::Control, ctrl_type), control_type(ctrl_type), condition(cond), is_unconditional(uncond) {}
std::vector<std::string> AODControlNode::getUsedVariables() const { return {}; }
std::string AODControlNode::toString() const { return control_type; }
std::string AODControlNode::getDOTLabel() const { return control_type + "\\n" + condition; }

AODPhiNode::AODPhiNode(const std::string& result_var)
    : AODNode(AODNodeType::Phi, "Phi_" + result_var), result_variable(result_var) {}
std::vector<std::string> AODPhiNode::getUsedVariables() const { return {}; }
std::vector<std::string> AODPhiNode::getDefinedVariables() const { return {result_variable}; }
std::string AODPhiNode::toString() const { return "Phi " + result_variable; }
std::string AODPhiNode::getDOTLabel() const { return "Phi\\n" + result_variable; }

// 工具函数
std::string nodeTypeToString(AODNodeType type) {
    switch(type) {
        case AODNodeType::Entry: return "Entry";
        case AODNodeType::Exit: return "Exit";
        default: return "Node";
    }
}

std::shared_ptr<AODNode> createNode(AODNodeType type, const std::string& name) {
    return std::make_shared<AODNode>(type, name);
}
std::shared_ptr<AODNode> createLoadNode(const std::string& var, const std::string& type) {
    return std::make_shared<AODLoadNode>(var, type);
}
std::shared_ptr<AODNode> createStoreNode(const std::string& var, const std::string& value) {
    return std::make_shared<AODStoreNode>(var, value);
}
std::shared_ptr<AODNode> createArithmeticNode(AODNodeType op, const std::string& op_name, const std::string& type) {
    return std::make_shared<AODArithmeticNode>(op, op_name, type);
}
std::shared_ptr<AODNode> createSIMDNode(AODNodeType type, const std::string& simd_type, const std::string& op_name) {
    return std::make_shared<AODSIMDNode>(type, simd_type, op_name);
}
std::shared_ptr<AODNode> createCallNode(const std::string& func_name, const std::string& ret_type) {
    return std::make_shared<AODCallNode>(func_name, ret_type);
}
std::shared_ptr<AODNode> createControlNode(const std::string& control_type, const std::string& cond, bool unconditional) {
    return std::make_shared<AODControlNode>(control_type, cond, unconditional);
}
std::shared_ptr<AODNode> createPhiNode(const std::string& result_var) {
    return std::make_shared<AODPhiNode>(result_var);
}

} // namespace aodsolve
