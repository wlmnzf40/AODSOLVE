#include "aod/enhanced_aod_node.h"
#include <sstream>
#include <algorithm>
#include <stdexcept>

namespace aodsolve {

// ============================================
// AODNodeå®žçŽ°
// ============================================

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

std::string AODNode::toString() const {
    std::ostringstream oss;
    oss << "AODNode[" << id << "](" << nodeTypeToString(type) << "): " << properties.name;
    if (!properties.attributes.empty()) {
        oss << " {";
        bool first = true;
        for (const auto& [key, value] : properties.attributes) {
            if (!first) oss << ", ";
            oss << key << "=" << value;
            first = false;
        }
        oss << "}";
    }
    return oss.str();
}

std::vector<std::string> AODNode::getUsedVariables() const {
    return std::vector<std::string>(); // é»˜è®¤å®žçŽ°
}

std::vector<std::string> AODNode::getDefinedVariables() const {
    return std::vector<std::string>(); // é»˜è®¤å®žçŽ°
}

bool AODNode::isSideEffectFree() const {
    return !properties.has_side_effects;
}

bool AODNode::isSafeToReorder() const {
    return isSideEffectFree() && !isControlNode();
}

int AODNode::getComplexity() const {
    return properties.complexity;
}

void AODNode::optimize() {
    properties.is_computed = true;
}

bool AODNode::canCompress() const {
    return getInputs().size() == 1 && getOutputs().size() == 1;
}

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
        default:
            return false;
    }
}

bool AODNode::isDataNode() const {
    return !isControlNode() && type != AODNodeType::Entry && type != AODNodeType::Exit;
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
            return true;
        default:
            return false;
    }
}

bool AODNode::isCallNode() const {
    return type == AODNodeType::Call;
}

bool AODNode::isValid() const {
    if (properties.name.empty()) return false;
    if (inputs.size() > 0 && !isControlNode() && !isCallNode() && getInputs().size() == 0) {
        return false; // æ•°æ®èŠ‚ç‚¹åº”è¯¥æœ‰è¾“å…¥
    }
    return true;
}

std::vector<std::string> AODNode::getValidationErrors() const {
    std::vector<std::string> errors;
    if (properties.name.empty()) {
        errors.push_back("èŠ‚ç‚¹åç§°ä¸ºç©º");
    }
    if (isControlNode() && inputs.size() == 0) {
        errors.push_back("æŽ§åˆ¶æµèŠ‚ç‚¹æ²¡æœ‰è¾“å…¥");
    }
    if (isDataNode() && inputs.size() == 0) {
        errors.push_back("æ•°æ®æµèŠ‚ç‚¹æ²¡æœ‰è¾“å…¥");
    }
    return errors;
}

std::shared_ptr<AODNode> AODNode::clone() const {
    auto cloned = std::make_shared<AODNode>(type, properties.name);
    cloned->properties = properties;
    cloned->analysis_context = analysis_context;
    return cloned;
}

std::string AODNode::getDOTLabel() const {
    return properties.name;
}

std::string AODNode::getDOTStyle() const {
    if (isControlNode()) return "color=red,style=filled,fillcolor=lightcoral";
    if (isSIMDNode()) return "color=blue,style=filled,fillcolor=lightblue";
    if (isCallNode()) return "color=green,style=filled,fillcolor=lightgreen";
    return "color=black,style=filled,fillcolor=white";
}

void AODNode::validateInputs() {
    // éªŒè¯è¾“å…¥çš„æœ‰æ•ˆæ€§
    for (const auto& input : inputs) {
        if (!input || !input->isValid()) {
            throw std::invalid_argument("è¾“å…¥èŠ‚ç‚¹æ— æ•ˆ");
        }
    }
}

// ============================================
// AODLoadNodeå®žçŽ°
// ============================================

AODLoadNode::AODLoadNode(const std::string& var, const std::string& type, bool deref)
    : AODNode(AODNodeType::Load, "Load_" + var), var_name(var), var_type(type),
      is_dereference(deref), alignment(1) {
    setProperty("variable", var);
    setProperty("type", type);
    setProperty("dereference", deref ? "true" : "false");
}

std::vector<std::string> AODLoadNode::getUsedVariables() const {
    return {var_name};
}

std::string AODLoadNode::toString() const {
    std::ostringstream oss;
    oss << "LoadNode[" << getId() << "]: " << var_name;
    if (!var_type.empty()) oss << " [" << var_type << "]";
    if (is_dereference) oss << " (deref)";
    return oss.str();
}

std::string AODLoadNode::getDOTLabel() const {
    std::ostringstream oss;
    oss << "Load: " << var_name;
    if (is_dereference) oss << "\\n(dereference)";
    return oss.str();
}

// ============================================
// AODStoreNodeå®žçŽ°
// ============================================

AODStoreNode::AODStoreNode(const std::string& var, const std::string& type, bool deref)
    : AODNode(AODNodeType::Store, "Store_" + var), var_name(var), var_type(type),
      is_dereference(deref), is_volatile(false) {
    setProperty("variable", var);
    setProperty("type", type);
    setProperty("dereference", deref ? "true" : "false");
}

std::vector<std::string> AODStoreNode::getDefinedVariables() const {
    return {var_name};
}

std::vector<std::string> AODStoreNode::getUsedVariables() const {
    std::vector<std::string> vars = {var_name};
    // è¿˜éœ€è¦åŒ…æ‹¬è¦å­˜å‚¨çš„å€¼ï¼ˆå¦‚æžœæœ‰è¾“å…¥ï¼‰
    for (const auto& input : getInputs()) {
        auto input_vars = input->getUsedVariables();
        vars.insert(vars.end(), input_vars.begin(), input_vars.end());
    }
    return vars;
}

bool AODStoreNode::isSideEffectFree() const {
    return !is_volatile;
}

std::string AODStoreNode::toString() const {
    std::ostringstream oss;
    oss << "StoreNode[" << getId() << "]: " << var_name;
    if (!var_type.empty()) oss << " [" << var_type << "]";
    if (is_dereference) oss << " (deref)";
    if (is_volatile) oss << " (volatile)";
    return oss.str();
}

std::string AODStoreNode::getDOTLabel() const {
    std::ostringstream oss;
    oss << "Store: " << var_name;
    if (is_volatile) oss << "\\n(volatile)";
    return oss.str();
}

// ============================================
// AODArithmeticNodeå®žçŽ°
// ============================================

AODArithmeticNode::AODArithmeticNode(AODNodeType op, const std::string& op_name, const std::string& type)
    : AODNode(op, op_name), operation(op_name), result_type(type),
      is_saturating(false), is_sat_safe(false) {
    setProperty("operation", op_name);
    setProperty("result_type", type);
}

std::vector<std::string> AODArithmeticNode::getUsedVariables() const {
    std::vector<std::string> vars;
    for (const auto& input : getInputs()) {
        auto input_vars = input->getUsedVariables();
        vars.insert(vars.end(), input_vars.begin(), input_vars.end());
    }
    vars.insert(vars.end(), operands.begin(), operands.end());
    return vars;
}

std::string AODArithmeticNode::toString() const {
    std::ostringstream oss;
    oss << "ArithmeticNode[" << getId() << "]: " << operation;
    if (!result_type.empty()) oss << " [" << result_type << "]";
    if (is_saturating) oss << " (saturating)";
    return oss.str();
}

std::string AODArithmeticNode::getDOTLabel() const {
    std::ostringstream oss;
    oss << operation;
    if (!result_type.empty()) oss << "\\n[" << result_type << "]";
    if (is_saturating) oss << "\\n(saturating)";
    return oss.str();
}

// ============================================
// AODSIMDNodeå®žçŽ°
// ============================================

AODSIMDNode::AODSIMDNode(AODNodeType type, const std::string& simd_type, const std::string& op_name)
    : AODNode(type, op_name.empty() ? "SIMD_" + nodeTypeToString(type) : op_name),
      simd_type(simd_type), vector_width(1), operation_name(op_name) {
    setProperty("simd_type", simd_type);
    setProperty("vector_width", std::to_string(vector_width));
}

std::vector<std::string> AODSIMDNode::getUsedVariables() const {
    std::vector<std::string> vars;
    for (const auto& input : getInputs()) {
        auto input_vars = input->getUsedVariables();
        vars.insert(vars.end(), input_vars.begin(), input_vars.end());
    }
    vars.insert(vars.end(), vector_operands.begin(), vector_operands.end());
    return vars;
}

std::string AODSIMDNode::toString() const {
    std::ostringstream oss;
    oss << "SIMDNode[" << getId() << "]: " << operation_name;
    oss << " <" << simd_type << "[" << vector_width << "]>";
    if (!instruction_set.empty()) oss << " [" << instruction_set << "]";
    return oss.str();
}

std::string AODSIMDNode::getDOTLabel() const {
    std::ostringstream oss;
    oss << operation_name;
    oss << "\\n" << simd_type << "[" << vector_width << "]";
    if (!instruction_set.empty()) {
        oss << "\\n(" << instruction_set << ")";
    }
    return oss.str();
}

// ============================================
// AODCallNodeå®žçŽ°
// ============================================

AODCallNode::AODCallNode(const std::string& func_name, const std::string& ret_type)
    : AODNode(AODNodeType::Call, "Call_" + func_name), function_name(func_name),
      return_type(ret_type), is_intrinsic(false), is_tail_call(false) {
    setProperty("function", func_name);
    setProperty("return_type", ret_type);
}

bool AODCallNode::isSideEffectFree() const {
    return is_intrinsic && !is_tail_call;
}

std::vector<std::string> AODCallNode::getUsedVariables() const {
    std::vector<std::string> vars = arguments;
    for (const auto& input : getInputs()) {
        auto input_vars = input->getUsedVariables();
        vars.insert(vars.end(), input_vars.begin(), input_vars.end());
    }
    return vars;
}

std::vector<std::string> AODCallNode::getDefinedVariables() const {
    std::vector<std::string> vars = parameters;
    if (!return_type.empty()) {
        vars.push_back("return_value");
    }
    return vars;
}

std::string AODCallNode::toString() const {
    std::ostringstream oss;
    oss << "CallNode[" << getId() << "]: " << function_name;
    if (!return_type.empty()) oss << " -> " << return_type;
    if (is_intrinsic) oss << " (intrinsic)";
    if (is_tail_call) oss << " (tail)";
    return oss.str();
}

std::string AODCallNode::getDOTLabel() const {
    std::ostringstream oss;
    oss << function_name;
    if (!return_type.empty()) oss << "\\n-> " << return_type;
    if (is_intrinsic) oss << "\\n(intrinsic)";
    return oss.str();
}

// ============================================
// AODControlNodeå®žçŽ°
// ============================================

AODControlNode::AODControlNode(const std::string& control_type, const std::string& cond, bool unconditional)
    : AODNode(AODNodeType::Control, "Control_" + control_type),
      control_type(control_type), condition(cond), is_unconditional(unconditional) {
    setProperty("control_type", control_type);
    setProperty("condition", cond);
    setProperty("unconditional", unconditional ? "true" : "false");
}

std::vector<std::string> AODControlNode::getUsedVariables() const {
    if (!condition.empty() && !is_unconditional) {
        return {condition};
    }
    return std::vector<std::string>();
}

std::string AODControlNode::toString() const {
    std::ostringstream oss;
    oss << "ControlNode[" << getId() << "]: " << control_type;
    if (!is_unconditional) {
        oss << " if " << condition;
    }
    return oss.str();
}

std::string AODControlNode::getDOTLabel() const {
    std::ostringstream oss;
    oss << control_type;
    if (!is_unconditional && !condition.empty()) {
        oss << "\\n" << condition;
    }
    return oss.str();
}

// ============================================
// AODPhiNodeå®žçŽ°
// ============================================

AODPhiNode::AODPhiNode(const std::string& result_var)
    : AODNode(AODNodeType::Phi, "Phi_" + result_var), result_variable(result_var) {
    setProperty("result", result_var);
}

std::vector<std::string> AODPhiNode::getDefinedVariables() const {
    return {result_variable};
}

std::vector<std::string> AODPhiNode::getUsedVariables() const {
    std::vector<std::string> vars;
    for (const auto& pair : incoming_values) {
        vars.push_back(pair.second);
    }
    return vars;
}

std::string AODPhiNode::toString() const {
    std::ostringstream oss;
    oss << "PhiNode[" << getId() << "]: " << result_variable << " = φ(";
    bool first = true;
    for (const auto& pair : incoming_values) {
        if (!first) oss << ", ";
        oss << pair.first << ":" << pair.second;
        first = false;
    }
    oss << ")";
    return oss.str();
}

std::string AODPhiNode::getDOTLabel() const {
    std::ostringstream oss;
    oss << result_variable << " = φ";
    if (!incoming_values.empty()) {
        oss << "\\n(";
        bool first = true;
        for (const auto& pair : incoming_values) {
            if (!first) oss << ", ";
            oss << pair.first << ":" << pair.second;
            first = false;
        }
        oss << ")";
    }
    return oss.str();
}

// ============================================
// å·¥åŽ‚å‡½æ•°å®žçŽ°
// ============================================

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


bool isArithmeticNodeType(AODNodeType type) {
    switch (type) {
        case AODNodeType::Add:
        case AODNodeType::Subtract:
        case AODNodeType::Multiply:
        case AODNodeType::Divide:
        case AODNodeType::Modulo:
        case AODNodeType::And:
        case AODNodeType::Or:
        case AODNodeType::Xor:
        case AODNodeType::Not:
        case AODNodeType::ShiftLeft:
        case AODNodeType::ShiftRight:
            return true;
        default:
            return false;
    }
}

bool isSIMDNodeType(AODNodeType type) {
    switch (type) {
        case AODNodeType::SIMD_Load:
        case AODNodeType::SIMD_Store:
        case AODNodeType::SIMD_Arithmetic:
        case AODNodeType::SIMD_Compare:
        case AODNodeType::SIMD_Blend:
        case AODNodeType::SIMD_Shuffle:
        case AODNodeType::SIMD_Permute:
            return true;
        default:
            return false;
    }
}

bool isControlNodeType(AODNodeType type) {
    switch (type) {
        case AODNodeType::Control:
        case AODNodeType::If:
        case AODNodeType::Loop:
        case AODNodeType::Switch:
        case AODNodeType::Break:
        case AODNodeType::Continue:
        case AODNodeType::Return:
            return true;
        default:
            return false;
    }
}

bool isCallNodeType(AODNodeType type) {
    return type == AODNodeType::Call;
}

int getNodePriority(AODNodeType type) {
    // æ›´é«˜çš„ä¼˜å…ˆçº§è¡¨ç¤ºæ›´é‡è¦ï¼Œåº”è¯¥æ›´æ—©å¤„ç†
    switch (type) {
        case AODNodeType::Entry:
        case AODNodeType::Exit:
            return 100;
        case AODNodeType::Call:
        case AODNodeType::Store:
            return 90;
        case AODNodeType::Load:
        case AODNodeType::SIMD_Load:
        case AODNodeType::SIMD_Store:
            return 80;
        case AODNodeType::SIMD_Arithmetic:
        case AODNodeType::SIMD_Compare:
        case AODNodeType::SIMD_Blend:
        case AODNodeType::SIMD_Shuffle:
        case AODNodeType::SIMD_Permute:
            return 70;
        case AODNodeType::Add:
        case AODNodeType::Subtract:
        case AODNodeType::Multiply:
        case AODNodeType::Divide:
            return 60;
        case AODNodeType::Control:
        case AODNodeType::If:
        case AODNodeType::Loop:
            return 50;
        default:
            return 10;
    }
}

} // namespace aodsolve