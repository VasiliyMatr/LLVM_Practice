#ifndef AST_HPP
#define AST_HPP

#include <cassert>
#include <cstddef>
#include <cstdint>
#include <string>

#include <lang/common.hpp>

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

// Global func def
class FuncDef;
// Global or local variable def
class VarDef;

// Statements
// local VarDef is a statement
class ExprStmt;
class If;
class While;
class Return;

// Args
class FuncArg;
class CallArg;

// Values
class IntVal;
class FixedVal;
class VarVal;

// Expr operations
class UnOp;
class BinOp;
class Assign;
class Call;

} // namespace node

struct InterfaceNodeVisitor {
    virtual ~InterfaceNodeVisitor() = default;

    virtual void visit(const node::FuncDef &) = 0;
    virtual void visit(const node::FuncArg &) = 0;

    virtual void visit(const node::CallArg &) = 0;
    virtual void visit(const node::ExprStmt &) = 0;
    virtual void visit(const node::VarDef &) = 0;
    virtual void visit(const node::If &) = 0;
    virtual void visit(const node::While &) = 0;
    virtual void visit(const node::Return &) = 0;

    virtual void visit(const node::IntVal &) = 0;
    virtual void visit(const node::FixedVal &) = 0;
    virtual void visit(const node::VarVal &) = 0;

    virtual void visit(const node::UnOp &) = 0;
    virtual void visit(const node::BinOp &) = 0;
    virtual void visit(const node::Assign &) = 0;

    virtual void visit(const node::Call &) = 0;

    template <class Node> void visitP(const Node *node) {
        if (node != nullptr) {
            node->accept(*this);
        }
    }
};

namespace node {

// Integral literal value
class IntVal final : public InterfaceExpr {
    int32_t m_value = 0;

  public:
    explicit IntVal(int32_t value) : m_value(value) {}

    NODISCARD auto getValue() const noexcept { return m_value; }

    void accept(InterfaceNodeVisitor &v) const override { v.visit(*this); }
};

// Fixed point literal value
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

// Variable value
class VarVal final : public InterfaceExpr {
    std::string m_name{};

  public:
    explicit VarVal(std::string name) : m_name(std::move(name)) {}

    NODISCARD const auto &getName() const noexcept { return m_name; }

    void accept(InterfaceNodeVisitor &v) const override { v.visit(*this); }
};

// Unary operation
class UnOp final : public InterfaceExpr {
    UnOpKind m_kind;
    const InterfaceExpr *m_expr = nullptr;

  public:
    explicit UnOp(UnOpKind kind, const InterfaceExpr *expr)
        : m_kind(kind), m_expr(expr) {}

    NODISCARD auto getKind() const noexcept { return m_kind; }
    NODISCARD const auto *getExpr() const noexcept { return m_expr; }

    void accept(InterfaceNodeVisitor &v) const override { v.visit(*this); }
};

// Binary operation
class BinOp final : public InterfaceExpr {
    BinOpKind m_kind;
    const InterfaceExpr *m_left = nullptr;
    const InterfaceExpr *m_right = nullptr;

  public:
    BinOp(BinOpKind kind, const InterfaceExpr *left,
             const InterfaceExpr *right)
        : m_kind(kind), m_left(left), m_right(right) {}

    NODISCARD auto getKind() const noexcept { return m_kind; }
    NODISCARD const auto *getLeft() const noexcept { return m_left; }
    NODISCARD const auto *getRight() const noexcept { return m_right; }

    void accept(InterfaceNodeVisitor &v) const override { v.visit(*this); }
};

// Assignment operation
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

// Function call
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

// Function call argument
class CallArg final : public ChainNode {
    const InterfaceExpr *m_expr = nullptr;

  public:
    CallArg(const InterfaceExpr *expr) : m_expr(expr) {}

    NODISCARD const auto *getExpr() const noexcept { return m_expr; }

    void accept(InterfaceNodeVisitor &v) const override { v.visit(*this); }
};

// Expression statement
class ExprStmt final : public ChainNode {
    const InterfaceExpr *m_expr = nullptr;

  public:
    ExprStmt(const InterfaceExpr *expr) : m_expr(expr) {}

    NODISCARD const auto *getExpr() const noexcept { return m_expr; }

    void accept(InterfaceNodeVisitor &v) const override { v.visit(*this); }
};

// Variable definition
class VarDef final : public ChainNode {
    ValType m_type;
    std::string m_name{};
    const InterfaceExpr *m_expr = nullptr;

  public:
    VarDef(ValType type, std::string name, const InterfaceExpr *expr)
        : m_type(type), m_name(std::move(name)), m_expr(expr) {}

    NODISCARD auto getType() const noexcept { return m_type; }
    NODISCARD const auto &getName() const noexcept { return m_name; }
    NODISCARD const auto *getExpr() const noexcept { return m_expr; }

    void accept(InterfaceNodeVisitor &v) const override { v.visit(*this); }
};

// If statement
class If final : public ChainNode {
    const InterfaceNode *m_expr = nullptr;
    const ChainNode *m_statements = nullptr;

  public:
    If(const InterfaceNode *expr, const ChainNode *statements)
        : m_expr(expr), m_statements(statements) {}

    NODISCARD const auto *getExpr() const noexcept { return m_expr; }
    NODISCARD const auto *getStatements() const noexcept {
        return m_statements;
    }

    void accept(InterfaceNodeVisitor &v) const override { v.visit(*this); }
};

// While statement
class While final : public ChainNode {
    const InterfaceExpr *m_expr = nullptr;
    const ChainNode *m_statements = nullptr;

  public:
    While(const InterfaceExpr *expr, const ChainNode *statements)
        : m_expr(expr), m_statements(statements) {}

    NODISCARD const auto *getExpr() const noexcept { return m_expr; }
    NODISCARD const auto *getStatements() const noexcept {
        return m_statements;
    }

    void accept(InterfaceNodeVisitor &v) const override { v.visit(*this); }
};

// Return statement
class Return final : public ChainNode {
    const InterfaceExpr *m_expr = nullptr;

  public:
    Return(const InterfaceExpr *expr) : m_expr(expr) {}

    NODISCARD const auto *getExpr() const noexcept { return m_expr; }

    void accept(InterfaceNodeVisitor &v) const override { v.visit(*this); }
};

class FuncArg final : public ChainNode {
    ValType m_type;
    std::string m_name{};

  public:
    FuncArg(ValType type, std::string name)
        : m_type(type), m_name(std::move(name)) {}

    NODISCARD auto getType() const noexcept { return m_type; }
    NODISCARD const auto &getName() const noexcept { return m_name; }

    NODISCARD const auto *getNext() const noexcept {
        return dynamic_cast<const FuncArg *>(ChainNode::getNext());
    }

    void accept(InterfaceNodeVisitor &v) const override { v.visit(*this); }
};

class FuncDef final : public ChainNode {
    ValType m_ret_type;
    std::string m_name{};
    const FuncArg *m_args = nullptr;
    const ChainNode *m_body = nullptr;

  public:
    FuncDef(ValType ret_type, std::string name, const FuncArg *args,
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
