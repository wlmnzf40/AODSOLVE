#pragma once

#include <string>
#include <vector>
#include <memory>
#include <set>
#include <map>
#include <sstream>

namespace aodsolve {

enum class AODNodeType {
    Entry, Exit,
    Control, If, Loop, Switch, Break, Continue, Return,
    Load, Store,
    Add, Subtract, Multiply, Divide, Modulo,
    And, Or, Xor, Not,
    ShiftLeft, ShiftRight,
    Equal, NotEqual, LessThan, LessEqual, GreaterThan, GreaterEqual,
    SIMD_Load, SIMD_Store, SIMD_Arithmetic, SIMD_Compare,
    SIMD_Blend, SIMD_Shuffle, SIMD_Permute,
    Call, Param, ReturnValue,
    Phi, Merge,
    Constant, Global, Unknown
};

struct AODNodeProperties {
    std::string name;
    std::string type;
    std::map<std::string, std::string> attributes;
    std::set<std::string> dependencies;
    bool is_computed = false;
    bool has_side_effects = false;
    int complexity = 1;
    std::string location;
};

class AODNode : public std::enable_shared_from_this<AODNode> {
public:
    static inline int next_id = 0;

protected:
    int id;
    AODNodeType type;
    AODNodeProperties properties;
    std::vector<std::shared_ptr<AODNode>> inputs;
    std::vector<std::shared_ptr<AODNode>> outputs;
    std::set<std::string> analysis_context;

public:
    AODNode(AODNodeType t, const std::string& name = "");
    virtual ~AODNode() = default;

    int getId() const { return id; }
    AODNodeType getType() const { return type; }
    const std::string& getName() const { return properties.name; }
    void setName(const std::string& name) { properties.name = name; }

    void addInput(std::shared_ptr<AODNode> input);
    void addOutput(std::shared_ptr<AODNode> output);
    const std::vector<std::shared_ptr<AODNode>>& getInputs() const { return inputs; }
    const std::vector<std::shared_ptr<AODNode>>& getOutputs() const { return outputs; }

    void setProperty(const std::string& key, const std::string& value);
    std::string getProperty(const std::string& key, const std::string& default_value = "") const;
    void addAttribute(const std::string& key, const std::string& value) {
        properties.attributes[key] = value;
    }

    // ===== 新增: getAttribute方法 =====
    std::string getAttribute(const std::string& key) const {
        return getProperty(key, "");
    }

    const std::map<std::string, std::string>& getAttributes() const {
        return properties.attributes;
    }

    void setSideEffects(bool has_effects) { properties.has_side_effects = has_effects; }
    void setComplexity(int c) { properties.complexity = c; }

    void addAnalysisContext(const std::string& context) { analysis_context.insert(context); }
    const std::set<std::string>& getAnalysisContext() const { return analysis_context; }
    bool hasContext(const std::string& context) const { return analysis_context.count(context) > 0; }

    virtual std::vector<std::string> getUsedVariables() const;
    virtual std::vector<std::string> getDefinedVariables() const;
    virtual bool isSideEffectFree() const;
    virtual bool isSafeToReorder() const;
    virtual int getComplexity() const;
    virtual void optimize();
    virtual bool canCompress() const;
    virtual bool isControlNode() const;
    virtual bool isDataNode() const;
    virtual bool isSIMDNode() const;
    virtual bool isCallNode() const;
    virtual bool isValid() const;
    virtual std::vector<std::string> getValidationErrors() const;
    virtual std::shared_ptr<AODNode> clone() const;

    virtual std::string toString() const;
    virtual std::string getDOTLabel() const;
    virtual std::string getDOTStyle() const;

protected:
    virtual void validateInputs();
};

// ===== Load节点 =====
class AODLoadNode : public AODNode {
private:
    std::string var_name;
    std::string var_type;
    bool is_dereference = false;
    int alignment = 1;

public:
    AODLoadNode(const std::string& var, const std::string& type = "", bool deref = false);

    const std::string& getVariableName() const { return var_name; }
    const std::string& getVariableType() const { return var_type; }
    bool isDereference() const { return is_dereference; }
    int getAlignment() const { return alignment; }

    void setDereference(bool deref) { is_dereference = deref; }
    void setAlignment(int align) { alignment = align; }

    std::vector<std::string> getUsedVariables() const override;
    std::string toString() const override;
    std::string getDOTLabel() const override;
};

// ===== Store节点 =====
class AODStoreNode : public AODNode {
private:
    std::string var_name;
    std::string var_type;
    bool is_dereference = false;
    bool is_volatile = false;

public:
    AODStoreNode(const std::string& var, const std::string& type = "", bool deref = false);

    const std::string& getVariableName() const { return var_name; }
    const std::string& getVariableType() const { return var_type; }
    bool isDereference() const { return is_dereference; }
    bool isVolatile() const { return is_volatile; }

    void setVolatile(bool vol) { is_volatile = vol; }

    std::vector<std::string> getDefinedVariables() const override;
    std::vector<std::string> getUsedVariables() const override;
    bool isSideEffectFree() const override;
    std::string toString() const override;
    std::string getDOTLabel() const override;
};

// ===== Arithmetic节点 =====
class AODArithmeticNode : public AODNode {
private:
    std::string operation;
    std::string result_type;
    bool is_saturating = false;
    bool is_sat_safe = false;
    std::vector<std::string> operands;

public:
    AODArithmeticNode(AODNodeType op, const std::string& op_name, const std::string& type = "");

    const std::string& getOperation() const { return operation; }
    const std::string& getResultType() const { return result_type; }
    bool isSaturating() const { return is_saturating; }
    bool isSatSafe() const { return is_sat_safe; }

    void setSaturating(bool sat) { is_saturating = sat; }
    void setSatSafe(bool safe) { is_sat_safe = safe; }
    void addOperand(const std::string& operand) { operands.push_back(operand); }

    std::vector<std::string> getUsedVariables() const override;
    std::string toString() const override;
    std::string getDOTLabel() const override;
};

// ===== SIMD节点 =====
class AODSIMDNode : public AODNode {
private:
    std::string simd_type;
    int vector_width = 1;
    std::string instruction_set;
    std::string operation_name;
    std::vector<std::string> vector_operands;

public:
    AODSIMDNode(AODNodeType type, const std::string& simd_type, const std::string& op_name = "");

    const std::string& getSIMDType() const { return simd_type; }
    int getVectorWidth() const { return vector_width; }
    const std::string& getInstructionSet() const { return instruction_set; }
    const std::string& getOperationName() const { return operation_name; }

    void setSIMDType(const std::string& type) { simd_type = type; }
    void setVectorWidth(int width) { vector_width = width; }
    void setInstructionSet(const std::string& iset) { instruction_set = iset; }
    void setOperationName(const std::string& op) { operation_name = op; }
    void addVectorOperand(const std::string& operand) { vector_operands.push_back(operand); }

    std::vector<std::string> getUsedVariables() const override;
    std::string toString() const override;
    std::string getDOTLabel() const override;
};

// ===== Call节点 =====
class AODCallNode : public AODNode {
private:
    std::string function_name;
    std::string return_type;
    std::vector<std::string> arguments;
    std::vector<std::string> parameters;
    bool is_intrinsic = false;
    bool is_tail_call = false;

public:
    AODCallNode(const std::string& func_name, const std::string& ret_type = "");

    const std::string& getFunctionName() const { return function_name; }
    const std::string& getReturnType() const { return return_type; }
    const std::vector<std::string>& getArguments() const { return arguments; }
    bool isIntrinsic() const { return is_intrinsic; }
    bool isTailCall() const { return is_tail_call; }

    void setIntrinsic(bool intrinsic) { is_intrinsic = intrinsic; }
    void setTailCall(bool tail) { is_tail_call = tail; }
    void addArgument(const std::string& arg) { arguments.push_back(arg); }
    void addParameter(const std::string& param) { parameters.push_back(param); }

    bool isSideEffectFree() const override;
    std::vector<std::string> getUsedVariables() const override;
    std::vector<std::string> getDefinedVariables() const override;
    std::string toString() const override;
    std::string getDOTLabel() const override;
};

// ===== Control节点 =====
class AODControlNode : public AODNode {
private:
    std::string control_type;
    std::string condition;
    bool is_unconditional = false;
    // 移除 target_block - 这个成员在头文件中不存在

public:
    AODControlNode(const std::string& ctrl_type, const std::string& cond = "", bool uncond = false);

    const std::string& getControlType() const { return control_type; }
    const std::string& getCondition() const { return condition; }
    bool isUnconditional() const { return is_unconditional; }

    void setCondition(const std::string& cond) { condition = cond; }
    void setUnconditional(bool uncond) { is_unconditional = uncond; }

    std::vector<std::string> getUsedVariables() const override;
    std::string toString() const override;
    std::string getDOTLabel() const override;
};

// ===== Phi节点 =====
class AODPhiNode : public AODNode {
private:
    std::string result_variable;
    std::map<std::string, std::string> incoming_values;  // 使用incoming_values而非incoming_variables

public:
    explicit AODPhiNode(const std::string& result_var);

    const std::string& getResultVariable() const { return result_variable; }
    const std::map<std::string, std::string>& getIncomingValues() const { return incoming_values; }

    void addIncomingValue(const std::string& block, const std::string& value) {
        incoming_values[block] = value;
    }

    std::vector<std::string> getUsedVariables() const override;
    std::vector<std::string> getDefinedVariables() const override;
    std::string toString() const override;
    std::string getDOTLabel() const override;
};

// ===== 工具函数 =====
inline std::string nodeTypeToString(AODNodeType type) {
    switch (type) {
        case AODNodeType::Entry: return "Entry";
        case AODNodeType::Exit: return "Exit";
        case AODNodeType::Control: return "Control";
        case AODNodeType::If: return "If";
        case AODNodeType::Loop: return "Loop";
        case AODNodeType::Switch: return "Switch";
        case AODNodeType::Break: return "Break";
        case AODNodeType::Continue: return "Continue";
        case AODNodeType::Return: return "Return";
        case AODNodeType::Load: return "Load";
        case AODNodeType::Store: return "Store";
        case AODNodeType::Add: return "Add";
        case AODNodeType::Subtract: return "Subtract";
        case AODNodeType::Multiply: return "Multiply";
        case AODNodeType::Divide: return "Divide";
        case AODNodeType::Modulo: return "Modulo";
        case AODNodeType::And: return "And";
        case AODNodeType::Or: return "Or";
        case AODNodeType::Xor: return "Xor";
        case AODNodeType::Not: return "Not";
        case AODNodeType::ShiftLeft: return "ShiftLeft";
        case AODNodeType::ShiftRight: return "ShiftRight";
        case AODNodeType::Equal: return "Equal";
        case AODNodeType::NotEqual: return "NotEqual";
        case AODNodeType::LessThan: return "LessThan";
        case AODNodeType::LessEqual: return "LessEqual";
        case AODNodeType::GreaterThan: return "GreaterThan";
        case AODNodeType::GreaterEqual: return "GreaterEqual";
        case AODNodeType::SIMD_Load: return "SIMD_Load";
        case AODNodeType::SIMD_Store: return "SIMD_Store";
        case AODNodeType::SIMD_Arithmetic: return "SIMD_Arithmetic";
        case AODNodeType::SIMD_Compare: return "SIMD_Compare";
        case AODNodeType::SIMD_Blend: return "SIMD_Blend";
        case AODNodeType::SIMD_Shuffle: return "SIMD_Shuffle";
        case AODNodeType::SIMD_Permute: return "SIMD_Permute";
        case AODNodeType::Call: return "Call";
        case AODNodeType::Param: return "Param";
        case AODNodeType::ReturnValue: return "ReturnValue";
        case AODNodeType::Phi: return "Phi";
        case AODNodeType::Merge: return "Merge";
        case AODNodeType::Constant: return "Constant";
        case AODNodeType::Global: return "Global";
        case AODNodeType::Unknown: return "Unknown";
        default: return "Unknown";
    }
}

// ===== 工厂函数 =====
std::shared_ptr<AODNode> createNode(AODNodeType type, const std::string& name);
std::shared_ptr<AODNode> createLoadNode(const std::string& var, const std::string& type);
std::shared_ptr<AODNode> createStoreNode(const std::string& var, const std::string& value);
std::shared_ptr<AODNode> createArithmeticNode(AODNodeType op, const std::string& op_name, const std::string& type);
std::shared_ptr<AODNode> createSIMDNode(AODNodeType type, const std::string& simd_type, const std::string& op_name);
std::shared_ptr<AODNode> createCallNode(const std::string& func_name, const std::string& ret_type);
std::shared_ptr<AODNode> createControlNode(const std::string& control_type, const std::string& cond, bool unconditional);
std::shared_ptr<AODNode> createPhiNode(const std::string& result_var);

} // namespace aodsolve