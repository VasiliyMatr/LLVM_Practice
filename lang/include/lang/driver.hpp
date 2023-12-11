#ifndef DRIVER_HPP
#define DRIVER_HPP

#include <memory>
#include <vector>

#ifndef yyFlexLexer
#include <FlexLexer.h>
#endif

#include <lang_parser.tab.hpp>

namespace yy {

class Driver final {
    using TType = parser::token_type;
    using INode = lang::InterfaceASTNode;
    using INodeUPtr = std::unique_ptr<INode>;

    FlexLexer *m_lexer = nullptr;

    std::vector<INodeUPtr> m_ast_nodes{};

    const lang::InterfaceASTNode *m_ast_root;

    template <class Node> const Node *obtainNode(std::unique_ptr<Node> &&ptr) {
        return static_cast<const Node *>(
            m_ast_nodes.emplace_back(std::move(ptr)).get());
    }

  public:
    parser::token_type yylex(parser::semantic_type *yylval) {
        auto ttype = static_cast<TType>(m_lexer->yylex());

        switch (ttype) {
        case TType::ID: {
            const auto *id = obtainNode(
                std::make_unique<lang::ASTNode::Id>(m_lexer->YYText()));

            yylval->emplace<const lang::ASTNode::Id *>(id);
            break;
        }
        case TType::FIXED_VAL: {
            const auto *fixed_val =
                obtainNode(std::make_unique<lang::ASTNode::FixedVal>(
                    std::atof(m_lexer->YYText())));

            yylval->emplace<const lang::ASTNode::FixedVal *>(fixed_val);
            break;
        }
        case TType::INT_VAL: {
            const auto *int_val =
                obtainNode(std::make_unique<lang::ASTNode::IntVal>(
                    std::atoi(m_lexer->YYText())));

            yylval->emplace<const lang::ASTNode::IntVal *>(int_val);
            break;
        }
        default:
            break; // no semantic value
        }

        return ttype;
    }

    const auto *createAssign(const lang::ASTNode::Id *id, const INode *expr) {
        return obtainNode(std::make_unique<lang::ASTNode::Assign>(id, expr));
    }

    using BinaryOpType = lang::ASTNode::BinaryOp::BinaryOpType;
    const auto *createBinOp(BinaryOpType type, const INode *left,
                            const INode *right) {
        return obtainNode(
            std::make_unique<lang::ASTNode::BinaryOp>(type, left, right));
    }

    using UnaryOpType = lang::ASTNode::UnaryOp::UnaryOpType;
    const auto *createUnOp(UnaryOpType type, const INode *expr) {
        return obtainNode(std::make_unique<lang::ASTNode::UnaryOp>(type, expr));
    }

    const auto *createIntVal(int32_t value) {
        return obtainNode(std::make_unique<lang::ASTNode::IntVal>(value));
    }

    const auto *createFixedVal(float value) {
        return obtainNode(std::make_unique<lang::ASTNode::FixedVal>(value));
    }

    const auto *createId(std::string id) {
        return obtainNode(std::make_unique<lang::ASTNode::Id>(std::move(id)));
    }

    void setASTRoot(const INode *ast_root) { m_ast_root = ast_root; }

    const INode *buildAST(std::istream &in) {
        yyFlexLexer lexer;
        lexer.switch_streams(&in);

        m_lexer = &lexer;

        parser parser{this};

        parser.parse();
        m_lexer = nullptr;

        return m_ast_root;
    }
};

} // namespace yy

#endif // DRIVER_HPP
