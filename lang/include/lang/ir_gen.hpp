#ifndef IR_GEN_HPP
#define IR_GEN_HPP

#include <unordered_map>
#include <vector>

#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/GenericValue.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"

#include <lang/ast.hpp>

namespace lang {

class IRGen final : ast::InterfaceNodeVisitor {
    class FuncInfo final {
        llvm::Function *m_llvm_func = nullptr;
        const ast::node::FuncDef *m_ast_func = nullptr;

      public:
        FuncInfo(llvm::Function *llvm_func, const ast::node::FuncDef *ast_func)
            : m_llvm_func(llvm_func), m_ast_func(ast_func) {}

        NODISCARD auto *llvmFunc() noexcept { return m_llvm_func; }
        NODISCARD const auto *astFunc() const noexcept { return m_ast_func; }
    };

    class VarInfo final {
        ast::VarType m_type;
        llvm::Value *m_alloca = nullptr;

      public:
        VarInfo(ast::VarType type, llvm::Value *alloca)
            : m_type{type}, m_alloca(alloca) {}

        NODISCARD auto type() const noexcept { return m_type; }
        NODISCARD auto *alloca() noexcept { return m_alloca; }
    };

    using ScopeVars = std::unordered_map<std::string, VarInfo>;

    class ExprInfo final {
        llvm::Value *m_llvm_value = nullptr;
        ast::VarType m_type;

      public:
        ExprInfo(llvm::Value *llvm_value, ast::VarType type)
            : m_llvm_value(llvm_value), m_type(type) {}

        NODISCARD auto *llvmValue() noexcept { return m_llvm_value; }
        NODISCARD auto type() noexcept { return m_type; }
    };

    llvm::LLVMContext m_context;
    llvm::Module m_module{"top", m_context};

    llvm::IRBuilder<> m_builder{m_context};

    std::unordered_map<std::string, FuncInfo> m_funcs{};

    // Top scope include global variables
    std::vector<ScopeVars> m_curr_scopes{{}};

    FuncInfo *m_curr_func = nullptr;
    llvm::BasicBlock *m_curr_bb = nullptr;

    ExprInfo m_last_expr_info{nullptr, ast::VarType::INT};

    llvm::Type *m_i32_t = llvm::Type::getInt32Ty(m_context);
    llvm::Type *m_i64_t = llvm::Type::getInt64Ty(m_context);

    llvm::ConstantInt *m_fixed_shift = m_builder.getInt32(16);

    auto *castValue(llvm::Value *value, ast::VarType from, ast::VarType to) {
        if (from == ast::VarType::INT) {
            if (to == ast::VarType::FIXED) {
                return m_builder.CreateShl(value, m_fixed_shift);
            }
        } else {
            if (to == ast::VarType::INT) {
                return m_builder.CreateLShr(value, m_fixed_shift);
            }
        }

        return value;
    }

    VarInfo *findVar(const std::string &var_name) {
        for (auto &&scope : m_curr_scopes) {
            auto it = scope.find(var_name);
            if (it != scope.end()) {
                return &it->second;
            }
        }

        return nullptr;
    }

    void visit(const ast::node::FuncDef &node) override {
        // Get args number
        const auto *args = node.getArgs();
        size_t num_args = args == nullptr ? 0 : args->getNextCount() + 1;

        // Create function
        std::vector<llvm::Type *> args_types(num_args, m_i32_t);
        auto *func_type = llvm::FunctionType::get(m_i32_t, args_types, false);

        auto *llvm_func =
            llvm::Function::Create(func_type, llvm::Function::ExternalLinkage,
                                   node.getName(), m_module);

        auto [it, status] =
            m_funcs.emplace(node.getName(), FuncInfo{llvm_func, &node});
        m_curr_func = &it->second;

        // Add first bb
        m_curr_bb = llvm::BasicBlock::Create(m_context, "", llvm_func);

        // Add func scope
        m_curr_scopes.push_back({});
        // Visit func args
        visitP(args);

        // Visit function body
        visitP(node.getBody());

        // Visit next
        m_curr_scopes.pop_back();
        visitP(node.getNext());
    }

    void visit(const ast::node::FuncArg &node) override {
        auto *alloca = m_builder.CreateAlloca(m_i32_t);
        auto *arg_value =
            m_curr_func->llvmFunc()->arg_begin() + node.getNextCount();

        m_builder.CreateStore(arg_value, alloca);

        m_curr_scopes.back().emplace(node.getName(),
                                     VarInfo{node.getType(), alloca});
        visitP(node.getNext());
    }

    void visit(const ast::node::VarDef &node) override {
        node.getExpr()->accept(*this);

        auto *expr_value = m_last_expr_info.llvmValue();
        auto expr_type = m_last_expr_info.type();

        auto *value = castValue(expr_value, expr_type, node.getType());

        auto *alloca = m_builder.CreateAlloca(m_i32_t);
        m_builder.CreateStore(value, alloca);

        visitP(node.getNext());
    }

    void visit(const ast::node::ExprStatement &node) override {
        visitP(node.getExpr());
    }

    void visit(const ast::node::IfStatement &node) override {
        // Visit cond expr
        visitP(node.getExpr());
        auto *expr_val = m_last_expr_info.llvmValue();

        // Add cond branch
        auto *true_bb =
            llvm::BasicBlock::Create(m_context, "", m_curr_func->llvmFunc());
        auto *false_bb =
            llvm::BasicBlock::Create(m_context, "", m_curr_func->llvmFunc());

        m_builder.CreateCondBr(expr_val, true_bb, false_bb);
        m_curr_bb = true_bb;

        // Visit true statements
        m_curr_scopes.push_back({});

        visitP(node.getStatements());
        m_builder.CreateBr(false_bb);

        m_curr_scopes.pop_back();

        // Visit next
        m_curr_bb = false_bb;
        visitP(node.getNext());
    }

    void visit(const ast::node::WhileStatement &node) override {
        // Add cond bb
        auto *cond_bb =
            llvm::BasicBlock::Create(m_context, "", m_curr_func->llvmFunc());
        m_builder.CreateBr(cond_bb);
        m_curr_bb = cond_bb;

        // Visit cond expr
        node.getExpr()->accept(*this);
        auto *expr_val = m_last_expr_info.llvmValue();

        // Add cond branch
        auto *true_bb =
            llvm::BasicBlock::Create(m_context, "", m_curr_func->llvmFunc());
        auto *false_bb =
            llvm::BasicBlock::Create(m_context, "", m_curr_func->llvmFunc());

        m_builder.CreateCondBr(expr_val, true_bb, false_bb);
        m_curr_bb = true_bb;

        // Visit true statements
        m_curr_scopes.push_back({});

        visitP(node.getStatements());
        m_builder.CreateBr(cond_bb);

        m_curr_scopes.pop_back();

        // Visit next
        m_curr_bb = false_bb;
        visitP(node.getNext());
    }

    void visit(const ast::node::Return &node) override {
        node.getExpr()->accept(*this);
        auto *value = m_last_expr_info.llvmValue();
        auto type = m_last_expr_info.type();

        value = castValue(value, type, m_curr_func->astFunc()->getRetType());

        m_builder.CreateRet(value);
    }

    void visit(const ast::node::Assign &node) override {
        node.getExpr()->accept(*this);
        auto *expr_val = m_last_expr_info.llvmValue();
        auto expr_type = m_last_expr_info.type();

        auto *var = findVar(node.getDest());
        assert(var != nullptr);

        auto var_type = var->type();

        auto *value = castValue(expr_val, expr_type, var_type);
        m_builder.CreateStore(value, var->alloca());

        m_last_expr_info = {value, var_type};
    }

    static auto cmpTypeToPred(ast::BinOpType cmp_type) noexcept {
        using CmpT = ast::BinOpType;
        using Pred = llvm::CmpInst::Predicate;

        switch (cmp_type) {
        case CmpT::CMP_LESS:
            return Pred::ICMP_SLT;
        case CmpT::CMP_LESS_EQUAL:
            return Pred::ICMP_SLE;
        case CmpT::CMP_GREATER:
            return Pred::ICMP_SGT;
        case CmpT::CMP_GREATER_EQUAL:
            return Pred::ICMP_SGE;
        case CmpT::CMP_EQUAL:
            return Pred::ICMP_EQ;
        case CmpT::CMP_NOT_EQUAL:
            return Pred::ICMP_NE;
        default:
            assert(0);
        }

        assert(0);
    }

    void visit(const ast::node::BinaryOp &node) override {
        // Visit left
        node.getLeft()->accept(*this);
        auto *left_val = m_last_expr_info.llvmValue();
        auto left_type = m_last_expr_info.type();

        // Visit right
        node.getRight()->accept(*this);
        auto *right_val = m_last_expr_info.llvmValue();
        auto right_type = m_last_expr_info.type();

        // Cast operands
        auto operands_type = left_type;
        if (left_type != right_type) {
            left_val = castValue(left_val, left_type, ast::VarType::FIXED);
            right_val = castValue(right_val, right_type, ast::VarType::FIXED);
            operands_type = ast::VarType::FIXED;
        }

        // Add operation
        llvm::Value *res = nullptr;
        ast::VarType res_type = operands_type;

        auto op_type = node.getType();

        switch (op_type) {
        case ast::BinOpType::MUL:
            if (operands_type == ast::VarType::FIXED) {
                auto *l_ext = m_builder.CreateSExt(left_val, m_i64_t);
                auto *r_ext = m_builder.CreateSExt(right_val, m_i64_t);
                auto *mul = m_builder.CreateMul(l_ext, r_ext);
                auto *shr = m_builder.CreateLShr(mul, m_fixed_shift);
                res = m_builder.CreateTrunc(shr, m_i32_t);
            } else {
                res = m_builder.CreateMul(left_val, right_val);
            }

            break;

        case ast::BinOpType::DIV:
            if (operands_type == ast::VarType::FIXED) {
                auto *l_ext = m_builder.CreateSExt(left_val, m_i64_t);
                auto *l_shl = m_builder.CreateShl(l_ext, m_fixed_shift);
                auto *r_ext = m_builder.CreateSExt(right_val, m_i64_t);
                auto *div = m_builder.CreateSDiv(l_shl, r_ext);
                res = m_builder.CreateTrunc(div, m_i32_t);
            } else {
                res = m_builder.CreateSDiv(left_val, right_val);
            }

            break;

        case ast::BinOpType::ADD:
            res = m_builder.CreateAdd(left_val, right_val);
            break;

        case ast::BinOpType::SUB:
            res = m_builder.CreateSub(left_val, right_val);
            break;

        case ast::BinOpType::CMP_LESS:
        case ast::BinOpType::CMP_LESS_EQUAL:
        case ast::BinOpType::CMP_GREATER:
        case ast::BinOpType::CMP_GREATER_EQUAL:
        case ast::BinOpType::CMP_EQUAL:
        case ast::BinOpType::CMP_NOT_EQUAL:
            res = m_builder.CreateCmp(cmpTypeToPred(op_type), left_val,
                                      right_val);
            res_type = ast::VarType::INT;

            break;

        default:
            assert(0);
        }

        m_last_expr_info = {res, res_type};
    }

    void visit(const ast::node::UnaryOp &node) override {
        // Visit expr
        node.getExpr()->accept(*this);
        auto *expr_val = m_last_expr_info.llvmValue();
        auto expr_type = m_last_expr_info.type();

        // Add operation
        llvm::Value *res = nullptr;

        switch (node.getType()) {
        case ast::UnOpType::UN_MINUS:
            res = m_builder.CreateNeg(expr_val);
            break;
        case ast::UnOpType::UN_PLUS:
            res = expr_val;
            break;
        default:
            assert(0);
        }

        m_last_expr_info = {res, expr_type};
    }

    void visit(const ast::node::Call &node) override {
        std::vector<llvm::Value *> args_values{};
        std::vector<ast::VarType> args_types{};

        for (const ast::ChainNode *arg = node.getArgs(); arg != nullptr;
             arg = arg->getNext()) {
            arg->accept(*this);
            args_values.push_back(m_last_expr_info.llvmValue());
            args_types.push_back(m_last_expr_info.type());
        }

        auto it = m_funcs.find(node.getCallee());
        assert(it != m_funcs.end());
        auto *callee_info = &it->second;

        std::vector<ast::VarType> callee_args_types{};
        for (const ast::node::FuncArg *callee_arg =
                 callee_info->astFunc()->getArgs();
             callee_arg != nullptr; callee_arg = callee_arg->getNext()) {
            callee_args_types.push_back(callee_arg->getType());
        }

        assert(callee_args_types.size() == args_types.size());

        for (size_t i = 0, end = args_types.size(); i != end; ++i) {
            castValue(args_values[i], args_types[i], callee_args_types[i]);
        }

        auto *value =
            m_builder.CreateCall(callee_info->llvmFunc(), args_values);

        m_last_expr_info = {value, callee_info->astFunc()->getRetType()};
    }

    void visit(const ast::node::CallArg &node) override {
        node.getExpr()->accept(*this);

        /* Dont visit next here! Will visit in ast::node::Call visitor */
    }

    void visit(const ast::node::VarVal &node) override {
        auto *var = findVar(node.getName());
        assert(var != nullptr);

        auto *val = m_builder.CreateLoad(m_i32_t, var->alloca());

        m_last_expr_info = {val, var->type()};
    }

    void visit(const ast::node::FixedVal &node) override {
        m_last_expr_info = {m_builder.getInt32(node.getValue()),
                            ast::VarType::FIXED};
    }

    void visit(const ast::node::IntVal &node) override {
        m_last_expr_info = {m_builder.getInt32(node.getValue()),
                            ast::VarType::INT};
    }

  public:
    llvm::Module &genIR(const ast::InterfaceNode *ast_root) {
        visitP(ast_root);

        return m_module;
    }
};

} // namespace lang

#endif // IR_GEN_HPP
