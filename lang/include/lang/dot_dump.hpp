#ifndef DOT_DUMP_HPP
#define DOT_DUMP_HPP

#include <cassert>
#include <fstream>
#include <sstream>

#include <lang/ast.hpp>

namespace lang {

class ASTDotDumper final : public InterfaceASTNodeVisitor {
    std::ostream &m_out;

    std::string get_node_name(const InterfaceASTNode *node) const {
        std::stringstream out;
        out << "n" << node;
        return out.str();
    }

    void dump_node(const InterfaceASTNode *node, const char *label) {
        m_out << get_node_name(node) << "[label = \"" << label << "\"]"
              << std::endl;
    }

    void dump_edge(const InterfaceASTNode *from, const InterfaceASTNode *to) {
        m_out << get_node_name(from) << " -> " << get_node_name(to)
              << std::endl;
    }

    using UnOpType = ASTNode::UnOpType;

    static const char *unOpTypeToStr(UnOpType type) {
        switch (type) {
        case UnOpType::UN_MINUS:
            return "-";
        case UnOpType::UN_PLUS:
            return "+";
        }

        assert(0);
    }

    using BinOpType = ASTNode::BinOpType;

    static const char *binOpTypeToStr(BinOpType type) {
        switch (type) {
        case BinOpType::ADD:
            return "+";
        case BinOpType::SUB:
            return "-";
        case BinOpType::MUL:
            return "*";
        case BinOpType::DIV:
            return "/";
        case BinOpType::CMP_LESS:
            return "<";
        case BinOpType::CMP_LESS_EQUAL:
            return "<=";
        case BinOpType::CMP_GREATER:
            return ">";
        case BinOpType::CMP_GREATER_EQUAL:
            return ">=";
        case BinOpType::CMP_EQUAL:
            return "==";
        case BinOpType::CMP_NOT_EQUAL:
            return "!=";
        }

        assert(0);
    }

    using VarType = ASTNode::VarType;

    static const char *varTypeToStr(VarType type) {
        switch (type) {
        case VarType::INT:
            return "int";
        case VarType::FIXED:
            return "fixed";
        }

        assert(0);
    }

  public:
    ASTDotDumper(std::ostream &out) : m_out(out) {}

    void dump_ast(const InterfaceASTNode *ast_root) {
        m_out << "digraph {" << std::endl;

        ast_root->accept(*this);

        m_out << "}" << std::endl;
    }

    void visit(const ASTNode::IntVal &node) override {
        std::ostringstream label;

        label << "IntVal = " << node.getValue();

        auto label_str = label.str();
        dump_node(&node, label_str.c_str());
    }

    void visit(const ASTNode::FixedVal &node) override {
        std::ostringstream label;

        label << "FixedVal = " << node.getFloatValue();

        auto label_str = label.str();
        dump_node(&node, label_str.c_str());
    }

    void visit(const ASTNode::Id &node) override {
        std::ostringstream label;

        label << "Id: " << node.getId();

        auto label_str = label.str();
        dump_node(&node, label_str.c_str());
    }

    void visit(const ASTNode::UnaryOp &node) override {
        std::ostringstream label;

        label << "Unary op: " << unOpTypeToStr(node.getType());

        auto label_str = label.str();
        dump_node(&node, label_str.c_str());

        const auto *expr = node.getExpr();
        expr->accept(*this);

        dump_edge(&node, expr);
    }

    void visit(const ASTNode::BinaryOp &node) override {
        std::ostringstream label;

        label << "Binary op: " << binOpTypeToStr(node.getType());

        auto label_str = label.str();
        dump_node(&node, label_str.c_str());

        const auto *left = node.getLeft();
        left->accept(*this);

        const auto *right = node.getRight();
        right->accept(*this);

        dump_edge(&node, left);
        dump_edge(&node, right);
    }

    void visit(const ASTNode::Assign &node) override {
        std::ostringstream label;

        label << "Assign";

        auto label_str = label.str();
        dump_node(&node, label_str.c_str());

        const auto *lval = node.getLval();
        lval->accept(*this);

        const auto *expr = node.getExpr();
        expr->accept(*this);

        dump_edge(&node, lval);
        dump_edge(&node, expr);
    }

    void visit(const ASTNode::ExprStatement &node) override {
        std::ostringstream label;

        label << "Expr statement";

        auto label_str = label.str();
        dump_node(&node, label_str.c_str());

        const auto *expr = node.getExpr();
        expr->accept(*this);

        const auto *next = node.next();
        if (next != nullptr) {
            next->accept(*this);
            dump_edge(&node, next);
        }

        dump_edge(&node, expr);
    }

    void visit(const ASTNode::VarDef &node) override {
        std::ostringstream label;

        label << "Var definition: " << varTypeToStr(node.getType());

        auto label_str = label.str();
        dump_node(&node, label_str.c_str());

        const auto *id = node.getId();
        id->accept(*this);

        const auto *expr = node.getExpr();
        expr->accept(*this);

        const auto *next = node.next();
        if (next != nullptr) {
            next->accept(*this);
            dump_edge(&node, next);
        }

        dump_edge(&node, id);
        dump_edge(&node, expr);
    }

    void visit(const ASTNode::IfStatement &node) override {
        std::ostringstream label;

        label << "If statement";

        auto label_str = label.str();
        dump_node(&node, label_str.c_str());

        const auto *expr = node.getExpr();
        expr->accept(*this);

        const auto statements = node.getStatements();
        statements->accept(*this);

        const auto *next = node.next();
        if (next != nullptr) {
            next->accept(*this);
            dump_edge(&node, next);
        }

        dump_edge(&node, expr);
        dump_edge(&node, statements);
    }
};

} // namespace lang

#endif // DOT_DUMP_HPP
