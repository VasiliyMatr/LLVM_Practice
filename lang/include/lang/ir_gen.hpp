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

#include <engine_llvm.hpp>

#include <lang/ast.hpp>

namespace lang {

struct SemanticError final : public std::runtime_error {
    static std::string dump_func_info(const ast::node::FuncDef *ast_func) {
        if (ast_func == nullptr) {
            return "Unknown func";
        }

        return ast_func->getName();
    }

    SemanticError(const ast::node::FuncDef *ast_func, const std::string &what)
        : std::runtime_error("Semantic error in " + dump_func_info(ast_func) +
                             ": " + what) {}
};

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
        ValType m_type;
        llvm::Value *m_alloca = nullptr;

      public:
        VarInfo(ValType type, llvm::Value *alloca)
            : m_type{type}, m_alloca(alloca) {}

        NODISCARD auto type() const noexcept { return m_type; }
        NODISCARD auto *alloca() noexcept { return m_alloca; }
    };

    using ScopeVars = std::unordered_map<std::string, VarInfo>;

    class ExprInfo final {
        llvm::Value *m_llvm_value = nullptr;
        ValType m_type;

      public:
        ExprInfo(llvm::Value *llvm_value, ValType type)
            : m_llvm_value(llvm_value), m_type(type) {}

        NODISCARD auto *llvmValue() noexcept { return m_llvm_value; }
        NODISCARD auto type() noexcept { return m_type; }
    };

    llvm::LLVMContext &m_context;
    llvm::Module &m_module;

    llvm::IRBuilder<> m_builder{m_context};

    std::unordered_map<std::string, FuncInfo> m_funcs{};

    llvm::FunctionCallee m_window_create =
        addWindowCreate(m_context, &m_module);
    llvm::FunctionCallee m_window_set_pixel =
        addWindowSetPixel(m_context, &m_module);
    llvm::FunctionCallee m_window_update =
        addWindowUpdate(m_context, &m_module);

    // Top scope include global variables
    std::vector<ScopeVars> m_curr_scopes{{}};

    FuncInfo *m_curr_func = nullptr;
    llvm::BasicBlock *m_curr_alloca_bb = nullptr;

    ExprInfo m_last_expr_info{nullptr, ValType::INT};

    llvm::Type *m_i32_t = llvm::Type::getInt32Ty(m_context);
    llvm::Type *m_i64_t = llvm::Type::getInt64Ty(m_context);

    llvm::ConstantInt *m_fixed_shift = m_builder.getInt32(16);

    auto *castValue(llvm::Value *value, ValType from, ValType to) {
        if (from == ValType::INT) {
            if (to == ValType::FIXED) {
                return m_builder.CreateShl(value, m_fixed_shift);
            }
        } else {
            if (to == ValType::INT) {
                return m_builder.CreateLShr(value, m_fixed_shift);
            }
        }

        return value;
    }

    VarInfo &findVar(const std::string &var_name) {
        for (auto &&scope : m_curr_scopes) {
            auto it = scope.find(var_name);
            if (it != scope.end()) {
                return it->second;
            }
        }

        throw SemanticError(m_curr_func->astFunc(),
                            "Use of undefined variable: " + var_name);
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

        // Add alloca and first bb
        m_curr_alloca_bb = llvm::BasicBlock::Create(m_context, "", llvm_func);
        auto *first_bb = llvm::BasicBlock::Create(m_context, "", llvm_func);
        m_builder.SetInsertPoint(m_curr_alloca_bb);

        // Add func scope
        m_curr_scopes.emplace_back();
        // Visit func args
        visitP(args);

        m_builder.SetInsertPoint(first_bb);

        // Visit function body
        visitP(node.getBody());

        m_builder.SetInsertPoint(m_curr_alloca_bb);
        m_builder.CreateBr(first_bb);

        // Visit next
        m_curr_scopes.pop_back();
        visitP(node.getNext());
    }

    void visit(const ast::node::FuncArg &node) override {
        auto *alloca = m_builder.CreateAlloca(m_i32_t);
        auto *llvm_func = m_curr_func->llvmFunc();
        auto *arg_value = llvm_func->arg_begin() + llvm_func->arg_size() -
                          node.getNextCount() - 1;

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

        auto prev_insert_bb = m_builder.GetInsertBlock();
        m_builder.SetInsertPoint(m_curr_alloca_bb);
        auto *alloca = m_builder.CreateAlloca(m_i32_t);

        m_builder.SetInsertPoint(prev_insert_bb);
        m_builder.CreateStore(value, alloca);

        m_curr_scopes.back().emplace(node.getName(),
                                     VarInfo{node.getType(), alloca});

        visitP(node.getNext());
    }

    void visit(const ast::node::ExprStmt &node) override {
        visitP(node.getExpr());
        visitP(node.getNext());
    }

    void visit(const ast::node::If &node) override {
        // Visit cond expr
        visitP(node.getExpr());
        auto *expr_val = m_last_expr_info.llvmValue();

        // Add cond branch
        auto *true_bb =
            llvm::BasicBlock::Create(m_context, "", m_curr_func->llvmFunc());
        auto *false_bb =
            llvm::BasicBlock::Create(m_context, "", m_curr_func->llvmFunc());

        m_builder.CreateCondBr(expr_val, true_bb, false_bb);

        // Visit true statements
        m_builder.SetInsertPoint(true_bb);
        m_curr_scopes.emplace_back();

        visitP(node.getStatements());
        m_builder.CreateBr(false_bb);

        m_curr_scopes.pop_back();

        // Visit next
        m_builder.SetInsertPoint(false_bb);
        visitP(node.getNext());
    }

    void visit(const ast::node::While &node) override {
        // Add cond bb
        auto *cond_bb =
            llvm::BasicBlock::Create(m_context, "", m_curr_func->llvmFunc());
        m_builder.CreateBr(cond_bb);

        // Visit cond expr
        m_builder.SetInsertPoint(cond_bb);
        node.getExpr()->accept(*this);
        auto *expr_val = m_last_expr_info.llvmValue();

        // Add cond branch
        auto *true_bb =
            llvm::BasicBlock::Create(m_context, "", m_curr_func->llvmFunc());
        auto *false_bb =
            llvm::BasicBlock::Create(m_context, "", m_curr_func->llvmFunc());

        m_builder.CreateCondBr(expr_val, true_bb, false_bb);

        // Visit true statements
        m_builder.SetInsertPoint(true_bb);
        m_curr_scopes.emplace_back();

        visitP(node.getStatements());
        m_builder.CreateBr(cond_bb);

        m_curr_scopes.pop_back();

        // Visit next
        m_builder.SetInsertPoint(false_bb);
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

        auto &var = findVar(node.getDest());

        auto var_type = var.type();

        auto *value = castValue(expr_val, expr_type, var_type);
        m_builder.CreateStore(value, var.alloca());

        m_last_expr_info = {value, var_type};
    }

    static auto cmpKindToPred(BinOpKind cmp_type) noexcept {
        using CmpT = BinOpKind;
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

    void visit(const ast::node::BinOp &node) override {
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
            left_val = castValue(left_val, left_type, ValType::FIXED);
            right_val = castValue(right_val, right_type, ValType::FIXED);
            operands_type = ValType::FIXED;
        }

        // Add operation
        llvm::Value *res = nullptr;
        ValType res_type = operands_type;

        auto op_kind = node.getKind();

        switch (op_kind) {
        case BinOpKind::MUL:
            if (operands_type == ValType::FIXED) {
                auto *l_ext = m_builder.CreateSExt(left_val, m_i64_t);
                auto *r_ext = m_builder.CreateSExt(right_val, m_i64_t);
                auto *mul = m_builder.CreateMul(l_ext, r_ext);
                auto *shr = m_builder.CreateLShr(mul, m_fixed_shift);
                res = m_builder.CreateTrunc(shr, m_i32_t);
            } else {
                res = m_builder.CreateMul(left_val, right_val);
            }

            break;

        case BinOpKind::DIV:
            if (operands_type == ValType::FIXED) {
                auto *l_ext = m_builder.CreateSExt(left_val, m_i64_t);
                auto *l_shl = m_builder.CreateShl(l_ext, m_fixed_shift);
                auto *r_ext = m_builder.CreateSExt(right_val, m_i64_t);
                auto *div = m_builder.CreateSDiv(l_shl, r_ext);
                res = m_builder.CreateTrunc(div, m_i32_t);
            } else {
                res = m_builder.CreateSDiv(left_val, right_val);
            }

            break;

        case BinOpKind::ADD:
            res = m_builder.CreateAdd(left_val, right_val);
            break;

        case BinOpKind::SUB:
            res = m_builder.CreateSub(left_val, right_val);
            break;

        case BinOpKind::CMP_LESS:
        case BinOpKind::CMP_LESS_EQUAL:
        case BinOpKind::CMP_GREATER:
        case BinOpKind::CMP_GREATER_EQUAL:
        case BinOpKind::CMP_EQUAL:
        case BinOpKind::CMP_NOT_EQUAL:
            res = m_builder.CreateCmp(cmpKindToPred(op_kind), left_val,
                                      right_val);
            res_type = ValType::INT;

            break;

        default:
            assert(0);
        }

        m_last_expr_info = {res, res_type};
    }

    void visit(const ast::node::UnOp &node) override {
        // Visit expr
        node.getExpr()->accept(*this);
        auto *expr_val = m_last_expr_info.llvmValue();
        auto expr_type = m_last_expr_info.type();

        // Add operation
        llvm::Value *res = nullptr;

        switch (node.getKind()) {
        case UnOpKind::MINUS:
            res = m_builder.CreateNeg(expr_val);
            break;
        case UnOpKind::PLUS:
            res = expr_val;
            break;
        default:
            assert(0);
        }

        m_last_expr_info = {res, expr_type};
    }

    bool tryCallEngineFunc(const std::string &name,
                           std::vector<llvm::Value *> args_values,
                           const std::vector<ValType> args_types) {
        if (name == WINDOW_CREATE_NAME) {
            if (args_values.size() != 0) {
                throw SemanticError(
                    m_curr_func->astFunc(),
                    name + " is called with invalid number of arguments");
            }

            m_builder.CreateCall(m_window_create);
            return true;
        }

        if (name == WINDOW_UPDATE_NAME) {
            if (args_values.size() != 0) {
                throw SemanticError(
                    m_curr_func->astFunc(),
                    name + " is called with invalid number of arguments");
            }

            m_builder.CreateCall(m_window_update);
            return true;
        }

        if (name == WINDOW_SET_PIXEL) {
            static constexpr size_t SET_PIXEL_ARGS_NUM = 5;

            if (args_values.size() != SET_PIXEL_ARGS_NUM) {
                throw SemanticError(
                    m_curr_func->astFunc(),
                    name + " is called with invalid number of arguments");
            }

            for (size_t i = 0; i != SET_PIXEL_ARGS_NUM; ++i) {
                args_values[i] =
                    castValue(args_values[i], args_types[i], ValType::INT);
            }

            m_builder.CreateCall(m_window_set_pixel, args_values);
            return true;
        }

        return false;
    }

    void visit(const ast::node::Call &node) override {
        // Collect call args info
        std::vector<llvm::Value *> args_values{};
        std::vector<ValType> args_types{};

        for (const ast::ChainNode *arg = node.getArgs(); arg != nullptr;
             arg = arg->getNext()) {
            arg->accept(*this);
            args_values.push_back(m_last_expr_info.llvmValue());
            args_types.push_back(m_last_expr_info.type());
        }

        // Collect callee args info
        const auto &callee_name = node.getCallee();

        if (tryCallEngineFunc(callee_name, args_values, args_types)) {
            m_last_expr_info = {m_builder.getInt32(1), ValType::INT};
            return;
        }

        auto it = m_funcs.find(callee_name);
        if (it == m_funcs.end()) {
            throw SemanticError(m_curr_func->astFunc(),
                                "Call of undefined func: " + callee_name);
        }

        auto &callee_info = it->second;

        std::vector<ValType> callee_args_types{};
        for (const ast::node::FuncArg *callee_arg =
                 callee_info.astFunc()->getArgs();
             callee_arg != nullptr; callee_arg = callee_arg->getNext()) {
            callee_args_types.push_back(callee_arg->getType());
        }

        if (callee_args_types.size() != args_types.size()) {
            throw SemanticError(
                m_curr_func->astFunc(),
                callee_name + " is called with invalid number of arguments");
        }

        // Cast arguments
        for (size_t i = 0, end = args_types.size(); i != end; ++i) {
            castValue(args_values[i], args_types[i], callee_args_types[i]);
        }

        // Create call
        auto *value = m_builder.CreateCall(callee_info.llvmFunc(), args_values);

        m_last_expr_info = {value, callee_info.astFunc()->getRetType()};
    }

    void visit(const ast::node::CallArg &node) override {
        node.getExpr()->accept(*this);

        /* Dont visit next here! Will visit in ast::node::Call visitor */
    }

    void visit(const ast::node::VarVal &node) override {
        auto &var = findVar(node.getName());

        auto *val = m_builder.CreateLoad(m_i32_t, var.alloca());

        m_last_expr_info = {val, var.type()};
    }

    void visit(const ast::node::FixedVal &node) override {
        m_last_expr_info = {m_builder.getInt32(node.getValue()),
                            ValType::FIXED};
    }

    void visit(const ast::node::IntVal &node) override {
        m_last_expr_info = {m_builder.getInt32(node.getValue()), ValType::INT};
    }

  public:
    IRGen(llvm::LLVMContext &context, llvm::Module &module)
        : m_context(context), m_module(module) {}

    llvm::Function *genIR(const ast::InterfaceNode *ast_root) {
        visitP(ast_root);
        auto it = m_funcs.find("app");
        if (it == m_funcs.end()) {
            throw SemanticError(m_curr_func->astFunc(),
                                "Can not found entry function");
        }

        return it->second.llvmFunc();
    }
};

} // namespace lang

#endif // IR_GEN_HPP
