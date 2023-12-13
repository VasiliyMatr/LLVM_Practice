#ifndef AST_HPP
#define AST_HPP

#include <cassert>
#include <cstddef>
#include <cstdint>
#include <string>

#define NODISCARD [[nodiscard]]

namespace lang::ast {

struct InterfaceNodeVisitor;

struct InterfaceNode {
    virtual ~InterfaceNode() = default;

    virtual void accept(InterfaceNodeVisitor &) const = 0;
};

struct InterfaceExpr : public InterfaceNode {};

class ChainNode : public InterfaceNode {
    const ChainNode *m_next = nullptr;
    size_t m_next_count = 0;

  public:
    NODISCARD const auto *getNext() const noexcept { return m_next; }
    NODISCARD auto getNextCount() const noexcept { return m_next_count; }

    void append(const ChainNode *next) noexcept {
        assert(m_next == nullptr);
        m_next = next;

        m_next_count = next == nullptr ? 0 : next->getNextCount() + 1;
    }
};

namespace node {

// Chain nodes
class FuncDef;
class FuncArg;

class ExprStatement;
class VarDef;
class IfStatement;
class WhileStatement;
class Return;
class CallArg;

// Expr nodes
class IntVal;
class FixedVal;
class VarVal;

class UnaryOp;
class BinaryOp;
class Assign;

class Call;

} // namespace node

struct InterfaceNodeVisitor {
    virtual ~InterfaceNodeVisitor() = default;

    virtual void visit(const node::FuncDef &) = 0;
    virtual void visit(const node::FuncArg &) = 0;

    virtual void visit(const node::CallArg &) = 0;
    virtual void visit(const node::ExprStatement &) = 0;
    virtual void visit(const node::VarDef &) = 0;
    virtual void visit(const node::IfStatement &) = 0;
    virtual void visit(const node::WhileStatement &) = 0;
    virtual void visit(const node::Return &) = 0;

    virtual void visit(const node::IntVal &) = 0;
    virtual void visit(const node::FixedVal &) = 0;
    virtual void visit(const node::VarVal &) = 0;

    virtual void visit(const node::UnaryOp &) = 0;
    virtual void visit(const node::BinaryOp &) = 0;
    virtual void visit(const node::Assign &) = 0;

    virtual void visit(const node::Call &) = 0;

    template <class Node> void visitP(const Node *node) {
        if (node != nullptr) {
            node->accept(*this);
        }
    }
};

enum class VarType { INT, FIXED };

enum class UnOpType { UN_PLUS, UN_MINUS };

enum class BinOpType {
    MUL,
    DIV,
    ADD,
    SUB,
    CMP_LESS,
    CMP_LESS_EQUAL,
    CMP_GREATER,
    CMP_GREATER_EQUAL,
    CMP_EQUAL,
    CMP_NOT_EQUAL,
};

namespace node {

class IntVal final : public InterfaceExpr {
    int32_t m_value = 0;

  public:
    explicit IntVal(int32_t value) : m_value(value) {}

    NODISCARD auto getValue() const noexcept { return m_value; }

    void accept(InterfaceNodeVisitor &v) const override { v.visit(*this); }
};

class FixedVal final : public InterfaceExpr {
  public:
    using FixedPoint = int32_t;

    static constexpr size_t FIXED_POINT_POS = 16;
    static constexpr FixedPoint FIXED_POINT_1 = FixedPoint{1}
                                                << FIXED_POINT_POS;

  private:
    int32_t m_value = 0;

  public:
    explicit FixedVal(float to_convert) {
        m_value = to_convert * FIXED_POINT_1;
    }

    NODISCARD auto getValue() const noexcept { return m_value; }
    NODISCARD auto getFloatValue() const noexcept {
        return static_cast<float>(m_value) / FIXED_POINT_1;
    }

    void accept(InterfaceNodeVisitor &v) const override { v.visit(*this); }
};

class VarVal final : public InterfaceExpr {
    std::string m_name{};

  public:
    VarVal(std::string name) : m_name(std::move(name)) {}

    NODISCARD const auto &getName() const noexcept { return m_name; }

    void accept(InterfaceNodeVisitor &v) const override { v.visit(*this); }
};

class UnaryOp final : public InterfaceExpr {
    UnOpType m_type;
    const InterfaceExpr *m_expr = nullptr;

  public:
    explicit UnaryOp(UnOpType type, const InterfaceExpr *expr)
        : m_type(type), m_expr(expr) {}

    NODISCARD auto getType() const noexcept { return m_type; }
    NODISCARD const auto *getExpr() const noexcept { return m_expr; }

    void accept(InterfaceNodeVisitor &v) const override { v.visit(*this); }
};

class BinaryOp final : public InterfaceExpr {
    BinOpType m_type;
    const InterfaceExpr *m_left = nullptr;
    const InterfaceExpr *m_right = nullptr;

  public:
    BinaryOp(BinOpType type, const InterfaceExpr *left,
             const InterfaceExpr *right)
        : m_type(type), m_left(left), m_right(right) {}

    NODISCARD auto getType() const noexcept { return m_type; }
    NODISCARD const auto *getLeft() const noexcept { return m_left; }
    NODISCARD const auto *getRight() const noexcept { return m_right; }

    void accept(InterfaceNodeVisitor &v) const override { v.visit(*this); }
};

class Assign final : public InterfaceExpr {
    std::string m_dest{};
    const InterfaceExpr *m_expr = nullptr;

  public:
    Assign(std::string dest, const InterfaceExpr *expr)
        : m_dest(std::move(dest)), m_expr(expr) {}

    NODISCARD const auto &getDest() const noexcept { return m_dest; }
    NODISCARD const auto *getExpr() const noexcept { return m_expr; }

    void accept(InterfaceNodeVisitor &v) const override { v.visit(*this); }
};

class Call final : public InterfaceExpr {
    std::string m_callee{};
    const CallArg *m_args = nullptr;

  public:
    Call(std::string callee, const CallArg *args)
        : m_callee(std::move(callee)), m_args(args) {}

    NODISCARD const auto &getCallee() const noexcept { return m_callee; }
    NODISCARD const auto *getArgs() const noexcept { return m_args; }

    void accept(InterfaceNodeVisitor &v) const override { v.visit(*this); }
};

class CallArg final : public ChainNode {
    const InterfaceExpr *m_expr = nullptr;

  public:
    CallArg(const InterfaceExpr *expr) : m_expr(expr) {}

    NODISCARD const auto *getExpr() const noexcept { return m_expr; }

    void accept(InterfaceNodeVisitor &v) const override { v.visit(*this); }
};

class ExprStatement final : public ChainNode {
    const InterfaceExpr *m_expr = nullptr;

  public:
    ExprStatement(const InterfaceExpr *expr) : m_expr(expr) {}

    NODISCARD const auto *getExpr() const noexcept { return m_expr; }

    void accept(InterfaceNodeVisitor &v) const override { v.visit(*this); }
};

class VarDef final : public ChainNode {
    VarType m_type;
    std::string m_name{};
    const InterfaceExpr *m_expr = nullptr;

  public:
    VarDef(VarType type, std::string name, const InterfaceExpr *expr)
        : m_type(type), m_name(std::move(name)), m_expr(expr) {}

    NODISCARD auto getType() const noexcept { return m_type; }
    NODISCARD const auto &getName() const noexcept { return m_name; }
    NODISCARD const auto *getExpr() const noexcept { return m_expr; }

    void accept(InterfaceNodeVisitor &v) const override { v.visit(*this); }
};

class IfStatement final : public ChainNode {
    const InterfaceNode *m_expr = nullptr;
    const ChainNode *m_statements = nullptr;

  public:
    IfStatement(const InterfaceNode *expr, const ChainNode *statements)
        : m_expr(expr), m_statements(statements) {}

    NODISCARD const auto *getExpr() const noexcept { return m_expr; }
    NODISCARD const auto *getStatements() const noexcept {
        return m_statements;
    }

    void accept(InterfaceNodeVisitor &v) const override { v.visit(*this); }
};

class WhileStatement final : public ChainNode {
    const InterfaceExpr *m_expr = nullptr;
    const ChainNode *m_statements = nullptr;

  public:
    WhileStatement(const InterfaceExpr *expr, const ChainNode *statements)
        : m_expr(expr), m_statements(statements) {}

    NODISCARD const auto *getExpr() const noexcept { return m_expr; }
    NODISCARD const auto *getStatements() const noexcept {
        return m_statements;
    }

    void accept(InterfaceNodeVisitor &v) const override { v.visit(*this); }
};

class Return final : public ChainNode {
    const InterfaceExpr *m_expr = nullptr;

  public:
    Return(const InterfaceExpr *expr) : m_expr(expr) {}

    NODISCARD const auto *getExpr() const noexcept { return m_expr; }

    void accept(InterfaceNodeVisitor &v) const override { v.visit(*this); }
};

class FuncArg final : public ChainNode {
    VarType m_type;
    std::string m_name{};

  public:
    FuncArg(VarType type, std::string name)
        : m_type(type), m_name(std::move(name)) {}

    NODISCARD auto getType() const noexcept { return m_type; }
    NODISCARD const auto &getName() const noexcept { return m_name; }

    NODISCARD const auto *getNext() const noexcept {
        return dynamic_cast<const FuncArg *>(ChainNode::getNext());
    }

    void accept(InterfaceNodeVisitor &v) const override { v.visit(*this); }
};

class FuncDef final : public ChainNode {
    VarType m_ret_type;
    std::string m_name{};
    const FuncArg *m_args = nullptr;
    const ChainNode *m_body = nullptr;

  public:
    FuncDef(VarType ret_type, std::string name, const FuncArg *args,
            const ChainNode *body)
        : m_ret_type(ret_type), m_name(std::move(name)), m_args(args),
          m_body(body) {}

    NODISCARD auto getRetType() const noexcept { return m_ret_type; }
    NODISCARD const auto &getName() const noexcept { return m_name; }
    NODISCARD const auto *getArgs() const noexcept { return m_args; }
    NODISCARD const auto *getBody() const noexcept { return m_body; }

    void accept(InterfaceNodeVisitor &v) const override { v.visit(*this); }
};

} // namespace node

} // namespace lang::ast

#endif // AST_HPP
