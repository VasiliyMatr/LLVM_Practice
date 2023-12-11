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

    using UnaryOpType = ASTNode::UnaryOp::UnaryOpType;

    static const char *unOpTypeToStr(UnaryOpType type) {
        switch (type) {
        case UnaryOpType::UN_MINUS:
            return "-";
        case UnaryOpType::UN_PLUS:
            return "+";
        }

        assert(0);
    }

    using BinaryOpType = ASTNode::BinaryOp::BinaryOpType;

    static const char *binOpTypeToStr(BinaryOpType type) {
        switch (type) {
        case BinaryOpType::ADD:
            return "+";
        case BinaryOpType::SUB:
            return "-";
        case BinaryOpType::MUL:
            return "*";
        case BinaryOpType::DIV:
            return "/";
        case BinaryOpType::CMP_LESS:
            return "<";
        case BinaryOpType::CMP_LESS_EQUAL:
            return "<=";
        case BinaryOpType::CMP_GREATER:
            return ">";
        case BinaryOpType::CMP_GREATER_EQUAL:
            return ">=";
        case BinaryOpType::CMP_EQUAL:
            return "==";
        case BinaryOpType::CMP_NOT_EQUAL:
            return "!=";
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
};

} // namespace lang

#endif // DOT_DUMP_HPP
