#ifndef DRIVER_HPP
#define DRIVER_HPP

#include <memory>
#include <vector>

#ifndef yyFlexLexer
#include <FlexLexer.h>
#endif

#include <yy_parser.tab.hpp>

namespace yy {

class Driver final {
    using TType = parser::token_type;
    using INode = toy::InterfaceASTNode;
    using INodeUPtr = std::unique_ptr<INode>;

    FlexLexer *m_lexer = nullptr;

    std::vector<INodeUPtr> m_ast_nodes{};

    const toy::InterfaceASTNode *m_ast_root;

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
                std::make_unique<toy::ASTNode::Id>(m_lexer->YYText()));

            yylval->emplace<const toy::ASTNode::Id *>(id);
            break;
        }
        case TType::FIXED_VAL:
            yylval->emplace<float>(std::atof(m_lexer->YYText()));
            break;
        case TType::INT_VAL:
            yylval->emplace<int32_t>(std::atoi(m_lexer->YYText()));
            break;
        default:
            break; // no semantic value
        }

        return ttype;
    }

    const auto *createAssign(const toy::ASTNode::Id *id, const INode *expr) {
        return obtainNode(std::make_unique<toy::ASTNode::Assign>(id, expr));
    }

    using BinaryOpType = toy::ASTNode::BinaryOp::BinaryOpType;
    const auto *createBinOp(BinaryOpType type, const INode *left,
                            const INode *right) {
        return obtainNode(
            std::make_unique<toy::ASTNode::BinaryOp>(type, left, right));
    }

    using UnaryOpType = toy::ASTNode::UnaryOp::UnaryOpType;
    const auto *createUnOp(UnaryOpType type, const INode *expr) {
        return obtainNode(std::make_unique<toy::ASTNode::UnaryOp>(type, expr));
    }

    const auto *createIntVal(int32_t value) {
        return obtainNode(std::make_unique<toy::ASTNode::Int>(value));
    }

    const auto *createFixedVal(float value) {
        return obtainNode(std::make_unique<toy::ASTNode::Fixed>(value));
    }

    const auto *createId(std::string id) {
        return obtainNode(std::make_unique<toy::ASTNode::Id>(std::move(id)));
    }

    void setASTRoot(const toy::InterfaceASTNode *ast_root) {
        m_ast_root = ast_root;
    }

    const toy::InterfaceASTNode *buildAST(std::istream &in) {
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
