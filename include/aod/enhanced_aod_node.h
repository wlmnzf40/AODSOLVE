#pragma once

#include <string>
#include <vector>
#include <memory>
#include <set>
#include <map>
#include <sstream>
#include <algorithm>

namespace clang { class Stmt; }

namespace aodsolve {

enum class AODNodeType {
    Entry, Exit,
    Control, If, Loop, Switch, Break, Continue, Return,
    BlockEnd,
    Load, Store,
    Add, Subtract, Multiply, Divide, Modulo,
    And, Or, Xor, Not,
    ShiftLeft, ShiftRight,
    Equal, NotEqual, LessThan, LessEqual, GreaterThan, GreaterEqual,
    SIMD_Load, SIMD_Store, SIMD_Arithmetic, SIMD_Compare,
    SIMD_Blend, SIMD_Shuffle, SIMD_Permute,
    SIMD_Intrinsic,
    GenericStmt,
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
    bool is_statement = false;
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
    const clang::Stmt* original_ast_stmt = nullptr;

public:
    AODNode(AODNodeType t, const std::string& name = "");
    virtual ~AODNode() = default;

    int getId() const { return id; }
    AODNodeType getType() const { return type; }
    const std::string& getName() const { return properties.name; }
    void setName(const std::string& name) { properties.name = name; }

    void setAstStmt(const clang::Stmt* stmt) { original_ast_stmt = stmt; }
    const clang::Stmt* getAstStmt() const { return original_ast_stmt; }

    void setIsStatement(bool is_stmt) { properties.is_statement = is_stmt; }
    bool isStatement() const { return properties.is_statement; }

    void addInput(std::shared_ptr<AODNode> input);
    void addOutput(std::shared_ptr<AODNode> output);
    const std::vector<std::shared_ptr<AODNode>>& getInputs() const { return inputs; }
    const std::vector<std::shared_ptr<AODNode>>& getOutputs() const { return outputs; }

    void setProperty(const std::string& key, const std::string& value);
    std::string getProperty(const std::string& key, const std::string& default_value = "") const;
    void addAttribute(const std::string& key, const std::string& value) {
        properties.attributes[key] = value;
    }
    std::string getAttribute(const std::string& key) const {
        return getProperty(key, "");
    }
    const std::map<std::string, std::string>& getAttributes() const {
        return properties.attributes;
    }

    void setSideEffects(bool has_effects) { properties.has_side_effects = has_effects; }
    void setComplexity(int c) { properties.complexity = c; }

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

// 子类定义
class AODLoadNode : public AODNode {
private:
    std::string var_name;
    std::string var_type;
    bool is_dereference = false;
    int alignment = 1;
public:
    AODLoadNode(const std::string& var, const std::string& type = "", bool deref = false);
    std::vector<std::string> getUsedVariables() const override;
    std::string toString() const override;
    std::string getDOTLabel() const override;
};

class AODStoreNode : public AODNode {
private:
    std::string var_name;
    std::string var_type;
    bool is_dereference = false;
    bool is_volatile = false;
public:
    AODStoreNode(const std::string& var, const std::string& type = "", bool deref = false);
    std::vector<std::string> getDefinedVariables() const override;
    std::vector<std::string> getUsedVariables() const override;
    bool isSideEffectFree() const override;
    std::string toString() const override;
    std::string getDOTLabel() const override;
};

class AODArithmeticNode : public AODNode {
private:
    std::string operation;
    std::string result_type;
    bool is_saturating = false;
    bool is_sat_safe = false;
    std::vector<std::string> operands;
public:
    AODArithmeticNode(AODNodeType op, const std::string& op_name, const std::string& type = "");
    std::vector<std::string> getUsedVariables() const override;
    std::string toString() const override;
    std::string getDOTLabel() const override;
};

class AODSIMDNode : public AODNode {
private:
    std::string simd_type;
    int vector_width = 1;
    std::string instruction_set;
    std::string operation_name;
    std::vector<std::string> vector_operands;
public:
    AODSIMDNode(AODNodeType type, const std::string& simd_type, const std::string& op_name = "");
    std::vector<std::string> getUsedVariables() const override;
    std::string toString() const override;
    std::string getDOTLabel() const override;
};

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
    bool isSideEffectFree() const override;
    std::vector<std::string> getUsedVariables() const override;
    std::vector<std::string> getDefinedVariables() const override;
    std::string toString() const override;
    std::string getDOTLabel() const override;
};

class AODControlNode : public AODNode {
private:
    std::string control_type;
    std::string condition;
    bool is_unconditional = false;
public:
    AODControlNode(const std::string& ctrl_type, const std::string& cond = "", bool uncond = false);
    std::vector<std::string> getUsedVariables() const override;
    std::string toString() const override;
    std::string getDOTLabel() const override;
};

class AODPhiNode : public AODNode {
private:
    std::string result_variable;
    std::map<std::string, std::string> incoming_values;
public:
    explicit AODPhiNode(const std::string& result_var);
    std::vector<std::string> getUsedVariables() const override;
    std::vector<std::string> getDefinedVariables() const override;
    std::string toString() const override;
    std::string getDOTLabel() const override;
};

// 工具函数
std::string nodeTypeToString(AODNodeType type);
std::shared_ptr<AODNode> createNode(AODNodeType type, const std::string& name);
std::shared_ptr<AODNode> createLoadNode(const std::string& var, const std::string& type);
std::shared_ptr<AODNode> createStoreNode(const std::string& var, const std::string& value);
std::shared_ptr<AODNode> createArithmeticNode(AODNodeType op, const std::string& op_name, const std::string& type);
std::shared_ptr<AODNode> createSIMDNode(AODNodeType type, const std::string& simd_type, const std::string& op_name);
std::shared_ptr<AODNode> createCallNode(const std::string& func_name, const std::string& ret_type);
std::shared_ptr<AODNode> createControlNode(const std::string& control_type, const std::string& cond, bool unconditional);
std::shared_ptr<AODNode> createPhiNode(const std::string& result_var);

} // namespace aodsolve
