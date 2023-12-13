#ifndef ENGINE_LLVM_HPP
#define ENGINE_LLVM_HPP

#include "llvm/IR/Function.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"

constexpr const char *WINDOW_CREATE_NAME = "engine_windowCreate";
inline llvm::FunctionCallee addWindowCreate(llvm::LLVMContext &ctx,
                                            llvm::Module *module) {
    auto *void_type = llvm::Type::getVoidTy(ctx);
    auto *i32_type = llvm::Type::getInt32Ty(ctx);

    // declare: void engine_windowCreate();
    auto *engine_windowCreate_type =
        llvm::FunctionType::get(void_type, {void_type}, false);

    return module->getOrInsertFunction(WINDOW_CREATE_NAME,
                                       engine_windowCreate_type);
}

constexpr const char *WINDOW_UPDATE_NAME = "engine_windowUpdate";
inline llvm::FunctionCallee addWindowUpdate(llvm::LLVMContext &ctx,
                                            llvm::Module *module) {
    auto *void_type = llvm::Type::getVoidTy(ctx);
    auto *i32_type = llvm::Type::getInt32Ty(ctx);

    // declare: void engine_windowUpdate();
    auto *engine_windowUpdate_type =
        llvm::FunctionType::get(void_type, {void_type}, false);

    return module->getOrInsertFunction(WINDOW_UPDATE_NAME,
                                       engine_windowUpdate_type);
}

constexpr const char *WINDOW_SET_PIXEL = "engine_windowSetPixel";
inline llvm::FunctionCallee addWindowSetPixel(llvm::LLVMContext &ctx,
                                              llvm::Module *module) {
    auto *void_type = llvm::Type::getVoidTy(ctx);
    auto *i32_type = llvm::Type::getInt32Ty(ctx);

    // declare: void engine_windowSetPixel(int x, int y, int red, int green, int
    // blue);
    llvm::ArrayRef<llvm::Type *> engine_windowSetPixel_param_types = {
        llvm::Type::getInt32Ty(ctx), // x
        llvm::Type::getInt32Ty(ctx), // y
        llvm::Type::getInt32Ty(ctx), // red
        llvm::Type::getInt32Ty(ctx), // green
        llvm::Type::getInt32Ty(ctx), // blue
    };

    auto *engine_windowSetPixel_type = llvm::FunctionType::get(
        void_type, engine_windowSetPixel_param_types, false);

    return module->getOrInsertFunction(WINDOW_SET_PIXEL,
                                       engine_windowSetPixel_type);
}

#endif // ENGINE_LLVM_HPP
