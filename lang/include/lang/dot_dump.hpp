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

    static const char *unOpTypeToStr(ast::UnOpType type) {
        switch (type) {
        case ast::UnOpType::UN_MINUS:
            return "-";
        case ast::UnOpType::UN_PLUS:
            return "+";
        }

        assert(0);
    }

    static const char *binOpTypeToStr(ast::BinOpType type) {
        switch (type) {
        case ast::BinOpType::ADD:
            return "+";
        case ast::BinOpType::SUB:
            return "-";
        case ast::BinOpType::MUL:
            return "*";
        case ast::BinOpType::DIV:
            return "/";
        case ast::BinOpType::CMP_LESS:
            return "<";
        case ast::BinOpType::CMP_LESS_EQUAL:
            return "<=";
        case ast::BinOpType::CMP_GREATER:
            return ">";
        case ast::BinOpType::CMP_GREATER_EQUAL:
            return ">=";
        case ast::BinOpType::CMP_EQUAL:
            return "==";
        case ast::BinOpType::CMP_NOT_EQUAL:
            return "!=";
        }

        assert(0);
    }

    static const char *varTypeToStr(ast::VarType type) {
        switch (type) {
        case ast::VarType::INT:
            return "int";
        case ast::VarType::FIXED:
            return "fixed";
        }

        assert(0);
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

    void visit(const ast::node::UnaryOp &node) override {
        std::ostringstream label;

        label << "Unary op: " << unOpTypeToStr(node.getType());

        auto label_str = label.str();
        dumpNode(&node, label_str.c_str());

        const auto *expr = node.getExpr();
        expr->accept(*this);

        dumpEdge(&node, expr, "expr");
    }

    void visit(const ast::node::BinaryOp &node) override {
        std::ostringstream label;

        label << "Binary op: " << binOpTypeToStr(node.getType());

        auto label_str = label.str();
        dumpNode(&node, label_str.c_str());

        const auto *left = node.getLeft();
        left->accept(*this);

        const auto *right = node.getRight();
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
        expr->accept(*this);

        dumpEdge(&node, expr, "expr");
    }

    void visit(const ast::node::CallArg &node) override {
        std::ostringstream label;

        label << "Call arg";

        auto label_str = label.str();
        dumpNode(&node, label_str.c_str());

        const auto *expr = node.getExpr();
        expr->accept(*this);

        const auto *next = node.getNext();
        visitP(next);

        dumpEdge(&node, next, "next");
        dumpEdge(&node, expr, "expr");
    }

    void visit(const ast::node::ExprStatement &node) override {
        std::ostringstream label;

        label << "Expr statement";

        auto label_str = label.str();
        dumpNode(&node, label_str.c_str());

        const auto *expr = node.getExpr();
        expr->accept(*this);

        const auto *next = node.getNext();
        visitP(next);

        dumpEdge(&node, next, "next");
        dumpEdge(&node, expr, "expr");
    }

    void visit(const ast::node::VarDef &node) override {
        std::ostringstream label;

        label << "Var def; " << varTypeToStr(node.getType()) << " "
              << node.getName();

        auto label_str = label.str();
        dumpNode(&node, label_str.c_str());

        const auto *expr = node.getExpr();
        expr->accept(*this);

        const auto *next = node.getNext();
        visitP(next);

        dumpEdge(&node, next, "next");
        dumpEdge(&node, expr, "expr");
    }

    void visit(const ast::node::IfStatement &node) override {
        std::ostringstream label;

        label << "If statement";

        auto label_str = label.str();
        dumpNode(&node, label_str.c_str());

        const auto *expr = node.getExpr();
        expr->accept(*this);

        const auto *statements = node.getStatements();
        visitP(statements);

        const auto *next = node.getNext();
        visitP(next);

        dumpEdge(&node, next, "next");
        dumpEdge(&node, expr, "expr");
        dumpEdge(&node, statements, "stm");
    }

    void visit(const ast::node::WhileStatement &node) override {
        std::ostringstream label;

        label << "While statement";

        auto label_str = label.str();
        dumpNode(&node, label_str.c_str());

        const auto *expr = node.getExpr();
        expr->accept(*this);

        const auto *statements = node.getStatements();
        visitP(statements);

        const auto *next = node.getNext();
        visitP(next);

        dumpEdge(&node, next, "next");
        dumpEdge(&node, expr, "expr");
        dumpEdge(&node, statements, "stm");
    }

    void visit(const ast::node::Return &node) override {
        std::ostringstream label;

        label << "Return";

        auto label_str = label.str();
        dumpNode(&node, label_str.c_str());

        const auto *expr = node.getExpr();
        expr->accept(*this);

        const auto *next = node.getNext();
        visitP(next);

        dumpEdge(&node, next, "next");
        dumpEdge(&node, expr, "expr");
    }

    void visit(const ast::node::FuncArg &node) override {
        std::ostringstream label;

        label << "Func arg: " << varTypeToStr(node.getType()) << " "
              << node.getName();

        auto label_str = label.str();
        dumpNode(&node, label_str.c_str());

        const auto *next = node.getNext();
        visitP(next);

        dumpEdge(&node, next, "next");
    }

    void visit(const ast::node::FuncDef &node) override {
        std::ostringstream label;

        label << "Func def; " << varTypeToStr(node.getRetType()) << " "
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
