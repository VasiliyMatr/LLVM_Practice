#ifndef AST_HPP
#define AST_HPP

#include <cassert>
#include <cstddef>
#include <cstdint>
#include <string>

#define NODISCARD [[nodiscard]]

namespace lang {

struct InterfaceASTNodeVisitor;

struct InterfaceASTNode {
    virtual ~InterfaceASTNode() = default;

    virtual void accept(InterfaceASTNodeVisitor &) const = 0;
};

struct InterfaceStatement : public InterfaceASTNode {
    virtual void append(const InterfaceStatement *other) noexcept = 0;
    NODISCARD virtual const InterfaceStatement *next() const noexcept = 0;
};

namespace ASTNode {

class ExprStatement;
class IfStatement;
class VarDef;

class Id;

class IntVal;
class FixedVal;

class UnaryOp;
class BinaryOp;
class Assign;

} // namespace ASTNode

struct InterfaceASTNodeVisitor {
    virtual ~InterfaceASTNodeVisitor() = default;

    virtual void visit(const ASTNode::ExprStatement &) = 0;
    virtual void visit(const ASTNode::IfStatement &) = 0;
    virtual void visit(const ASTNode::VarDef &) = 0;

    virtual void visit(const ASTNode::IntVal &) = 0;
    virtual void visit(const ASTNode::FixedVal &) = 0;
    virtual void visit(const ASTNode::Id &) = 0;
    virtual void visit(const ASTNode::UnaryOp &) = 0;
    virtual void visit(const ASTNode::BinaryOp &) = 0;
    virtual void visit(const ASTNode::Assign &) = 0;
};

namespace ASTNode {

class IntVal final : public InterfaceASTNode {
    int32_t m_value = 0;

  public:
    explicit IntVal(int32_t value) : m_value(value) {}

    NODISCARD auto getValue() const noexcept { return m_value; }

    void accept(InterfaceASTNodeVisitor &v) const override { v.visit(*this); }
};

class FixedVal final : public InterfaceASTNode {
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

    void accept(InterfaceASTNodeVisitor &v) const override { v.visit(*this); }
};

class Id final : public InterfaceASTNode {
    std::string m_id{};

  public:
    explicit Id(std::string id) : m_id(std::move(id)) {}

    NODISCARD const auto &getId() const noexcept { return m_id; }

    void accept(InterfaceASTNodeVisitor &v) const override { v.visit(*this); }
};

enum class UnOpType { UN_PLUS, UN_MINUS };

class UnaryOp final : public InterfaceASTNode {
    UnOpType m_type;
    const InterfaceASTNode *m_expr = nullptr;

  public:
    explicit UnaryOp(UnOpType type, const InterfaceASTNode *expr)
        : m_type(type), m_expr(expr) {}

    NODISCARD auto getType() const noexcept { return m_type; }
    NODISCARD const auto *getExpr() const noexcept { return m_expr; }

    void accept(InterfaceASTNodeVisitor &v) const override { v.visit(*this); }
};

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

class BinaryOp final : public InterfaceASTNode {
    BinOpType m_type;
    const InterfaceASTNode *m_left = nullptr;
    const InterfaceASTNode *m_right = nullptr;

  public:
    BinaryOp(BinOpType type, const InterfaceASTNode *left,
             const InterfaceASTNode *right)
        : m_type(type), m_left(left), m_right(right) {}

    NODISCARD auto getType() const noexcept { return m_type; }
    NODISCARD const auto *getLeft() const noexcept { return m_left; }
    NODISCARD const auto *getRight() const noexcept { return m_right; }

    void accept(InterfaceASTNodeVisitor &v) const override { v.visit(*this); }
};

class Assign final : public InterfaceASTNode {
    const Id *m_lval = nullptr;
    const InterfaceASTNode *m_expr = nullptr;

  public:
    Assign(const Id *id, const InterfaceASTNode *expr)
        : m_lval(id), m_expr(expr) {}

    NODISCARD const auto *getLval() const noexcept { return m_lval; }
    NODISCARD const auto *getExpr() const noexcept { return m_expr; }

    void accept(InterfaceASTNodeVisitor &v) const override { v.visit(*this); }
};

class ExprStatement : public InterfaceStatement {
    const InterfaceASTNode *m_expr = nullptr;
    const InterfaceStatement *m_next = nullptr;

  public:
    ExprStatement(const InterfaceASTNode *expr) : m_expr(expr) {}

    NODISCARD const auto *getExpr() const noexcept { return m_expr; }

    NODISCARD const InterfaceStatement *next() const noexcept override {
        return m_next;
    }

    void append(const InterfaceStatement *next) noexcept override {
        assert(m_next == nullptr);
        m_next = next;
    }

    void accept(InterfaceASTNodeVisitor &v) const override { v.visit(*this); }
};

enum class VarType { INT, FIXED };

class VarDef : public InterfaceStatement {
    VarType m_type;
    const Id *m_id = nullptr;
    const InterfaceASTNode *m_expr = nullptr;
    const InterfaceStatement *m_next = nullptr;

  public:
    VarDef(VarType type, const Id *id, const InterfaceASTNode *expr)
        : m_type(type), m_id(id), m_expr(expr) {}

    NODISCARD auto getType() const noexcept { return m_type; }
    NODISCARD const auto *getId() const noexcept { return m_id; }
    NODISCARD const auto *getExpr() const noexcept { return m_expr; }

    NODISCARD const InterfaceStatement *next() const noexcept override {
        return m_next;
    }

    void append(const InterfaceStatement *next) noexcept override {
        assert(m_next == nullptr);
        m_next = next;
    }

    void accept(InterfaceASTNodeVisitor &v) const override { v.visit(*this); }
};

class IfStatement : public InterfaceStatement {
    const InterfaceASTNode *m_expr = nullptr;
    const InterfaceStatement *m_statements = nullptr;
    const InterfaceStatement *m_next = nullptr;

  public:
    IfStatement(const InterfaceASTNode *expr,
                const InterfaceStatement *statements)
        : m_expr(expr), m_statements(statements) {}

    NODISCARD const auto *getExpr() const noexcept { return m_expr; }
    NODISCARD const auto *getStatements() const noexcept {
        return m_statements;
    }

    NODISCARD const InterfaceStatement *next() const noexcept override {
        return m_next;
    }

    void append(const InterfaceStatement *next) noexcept override {
        assert(m_next == nullptr);
        m_next = next;
    }

    void accept(InterfaceASTNodeVisitor &v) const override { v.visit(*this); }
};

} // namespace ASTNode

} // namespace lang

#endif // AST_HPP
