#ifndef DOT_DUMP_HPP
#define DOT_DUMP_HPP

#include <cassert>
#include <fstream>
#include <sstream>

#include <lang/ast.hpp>

namespace lang {

class ASTDotDumper final : public ast::InterfaceNodeVisitor {
    using INode = ast::InterfaceNode;

    std::ostream &m_out;

    std::string get_node_name(const INode *node) const {
        std::stringstream out;
        out << "n" << node;
        return out.str();
    }

    void dumpNode(const INode *node, const char *label) {
        m_out << get_node_name(node) << "[label = \"" << label << "\"]"
              << std::endl;
    }

    void dumpEdge(const INode *from, const INode *to, const char *label = "") {
        if (to == nullptr) {
            return;
        }

        m_out << get_node_name(from) << " -> " << get_node_name(to)
              << "[label = \"" << label << "\"]" << std::endl;
    }

    static const char *unOpKindToStr(UnOpKind kind) noexcept {
        switch (kind) {
        case UnOpKind::MINUS:
            return "-";
        case UnOpKind::PLUS:
            return "+";
        }

        UNREACHABLE();
    }

    static const char *binOpKindToStr(BinOpKind kind) noexcept {
        switch (kind) {
        case BinOpKind::ADD:
            return "+";
        case BinOpKind::SUB:
            return "-";
        case BinOpKind::MUL:
            return "*";
        case BinOpKind::DIV:
            return "/";
        case BinOpKind::CMP_LESS:
            return "<";
        case BinOpKind::CMP_LESS_EQUAL:
            return "<=";
        case BinOpKind::CMP_GREATER:
            return ">";
        case BinOpKind::CMP_GREATER_EQUAL:
            return ">=";
        case BinOpKind::CMP_EQUAL:
            return "==";
        case BinOpKind::CMP_NOT_EQUAL:
            return "!=";
        }

        UNREACHABLE();
    }

    static const char *valTypeToStr(ValType type) noexcept {
        switch (type) {
        case ValType::INT:
            return "int";
        case ValType::FIXED:
            return "fixed";
        }

        UNREACHABLE();
    }

  public:
    ASTDotDumper(std::ostream &out) : m_out(out) {}

    void dump_ast(const INode *ast_root) {
        m_out << "digraph {" << std::endl;

        ast_root->accept(*this);

        m_out << "}" << std::endl;
    }

    void visit(const ast::node::IntVal &node) override {
        std::ostringstream label;

        label << "IntVal = " << node.getValue();

        auto label_str = label.str();
        dumpNode(&node, label_str.c_str());
    }

    void visit(const ast::node::FixedVal &node) override {
        std::ostringstream label;

        label << "FixedVal = " << node.getFloatValue();

        auto label_str = label.str();
        dumpNode(&node, label_str.c_str());
    }

    void visit(const ast::node::VarVal &node) override {
        std::ostringstream label;

        label << "VarVal: " << node.getName();

        auto label_str = label.str();
        dumpNode(&node, label_str.c_str());
    }

    void visit(const ast::node::UnOp &node) override {
        std::ostringstream label;

        label << "Un op: " << unOpKindToStr(node.getKind());

        auto label_str = label.str();
        dumpNode(&node, label_str.c_str());

        const auto *expr = node.getExpr();
        ASSERT(expr != nullptr);
        expr->accept(*this);

        dumpEdge(&node, expr, "expr");
    }

    void visit(const ast::node::BinOp &node) override {
        std::ostringstream label;

        label << "Binary op: " << binOpKindToStr(node.getKind());

        auto label_str = label.str();
        dumpNode(&node, label_str.c_str());

        const auto *left = node.getLeft();
        ASSERT(left != nullptr);
        left->accept(*this);

        const auto *right = node.getRight();
        ASSERT(right != nullptr);
        right->accept(*this);

        dumpEdge(&node, left, "left");
        dumpEdge(&node, right, "right");
    }

    void visit(const ast::node::Assign &node) override {
        std::ostringstream label;

        label << "Assign; dest: " << node.getDest();

        auto label_str = label.str();
        dumpNode(&node, label_str.c_str());

        const auto *expr = node.getExpr();
        ASSERT(expr != nullptr);
        expr->accept(*this);

        dumpEdge(&node, expr, "expr");
    }

    void visit(const ast::node::CallArg &node) override {
        std::ostringstream label;

        label << "Call arg";

        auto label_str = label.str();
        dumpNode(&node, label_str.c_str());

        const auto *expr = node.getExpr();
        ASSERT(expr != nullptr);
        expr->accept(*this);

        const auto *next = node.getNext();
        visitP(next);

        dumpEdge(&node, next, "next");
        dumpEdge(&node, expr, "expr");
    }

    void visit(const ast::node::ExprStmt &node) override {
        std::ostringstream label;

        label << "Expr statement";

        auto label_str = label.str();
        dumpNode(&node, label_str.c_str());

        const auto *expr = node.getExpr();
        ASSERT(expr != nullptr);
        expr->accept(*this);

        const auto *next = node.getNext();
        visitP(next);

        dumpEdge(&node, next, "next");
        dumpEdge(&node, expr, "expr");
    }

    void visit(const ast::node::VarDef &node) override {
        std::ostringstream label;

        label << "Var def; " << valTypeToStr(node.getType()) << " "
              << node.getName();

        auto label_str = label.str();
        dumpNode(&node, label_str.c_str());

        const auto *expr = node.getExpr();
        ASSERT(expr != nullptr);
        expr->accept(*this);

        const auto *next = node.getNext();
        visitP(next);

        dumpEdge(&node, next, "next");
        dumpEdge(&node, expr, "expr");
    }

    void visit(const ast::node::If &node) override {
        std::ostringstream label;

        label << "If";

        auto label_str = label.str();
        dumpNode(&node, label_str.c_str());

        const auto *expr = node.getExpr();
        ASSERT(expr != nullptr);
        expr->accept(*this);

        const auto *statements = node.getStatements();
        visitP(statements);

        const auto *next = node.getNext();
        visitP(next);

        dumpEdge(&node, next, "next");
        dumpEdge(&node, expr, "expr");
        dumpEdge(&node, statements, "stmts");
    }

    void visit(const ast::node::While &node) override {
        std::ostringstream label;

        label << "While";

        auto label_str = label.str();
        dumpNode(&node, label_str.c_str());

        const auto *expr = node.getExpr();
        ASSERT(expr != nullptr);
        expr->accept(*this);

        const auto *statements = node.getStatements();
        visitP(statements);

        const auto *next = node.getNext();
        visitP(next);

        dumpEdge(&node, next, "next");
        dumpEdge(&node, expr, "expr");
        dumpEdge(&node, statements, "stmts");
    }

    void visit(const ast::node::Return &node) override {
        std::ostringstream label;

        label << "Return";

        auto label_str = label.str();
        dumpNode(&node, label_str.c_str());

        const auto *expr = node.getExpr();
        ASSERT(expr != nullptr);
        expr->accept(*this);

        const auto *next = node.getNext();
        visitP(next);

        dumpEdge(&node, next, "next");
        dumpEdge(&node, expr, "expr");
    }

    void visit(const ast::node::FuncArg &node) override {
        std::ostringstream label;

        label << "Func arg: " << valTypeToStr(node.getType()) << " "
              << node.getName();

        auto label_str = label.str();
        dumpNode(&node, label_str.c_str());

        const auto *next = node.getNext();
        visitP(next);

        dumpEdge(&node, next, "next");
    }

    void visit(const ast::node::FuncDef &node) override {
        std::ostringstream label;

        label << "Func def; " << valTypeToStr(node.getRetType()) << " "
              << node.getName();

        auto label_str = label.str();
        dumpNode(&node, label_str.c_str());

        const auto *arg = node.getArgs();
        visitP(arg);

        const auto *body = node.getBody();
        visitP(body);

        const auto *next = node.getNext();
        visitP(next);

        dumpEdge(&node, next, "next");
        dumpEdge(&node, arg, "arg");
        dumpEdge(&node, body, "body");
    }

    void visit(const ast::node::Call &node) override {
        std::ostringstream label;

        label << "Call " << node.getCallee();

        auto label_str = label.str();
        dumpNode(&node, label_str.c_str());

        const auto *args = node.getArgs();
        visitP(args);

        dumpEdge(&node, args, "args");
    }
};

} // namespace lang

#endif // DOT_DUMP_HPP
