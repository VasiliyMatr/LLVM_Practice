#include <memory>
#include <string>

#include <engine.hpp>

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/GenericValue.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

void app() {
    LLVMContext context;

    auto module = std::make_unique<Module>("app.cpp", context);
    IRBuilder<> builder(context);

    Type *void_type = Type::getVoidTy(context);
    Type *i32_type = Type::getInt32Ty(context);

    // declare: void engine_windowSetPixel(int x, int y, int red, int green, int
    // blue);
    ArrayRef<Type *> engine_windowSetPixel_param_types = {
        Type::getInt32Ty(context), // x
        Type::getInt32Ty(context), // y
        Type::getInt32Ty(context), // red
        Type::getInt32Ty(context), // green
        Type::getInt32Ty(context), // blue
    };

    FunctionType *engine_windowSetPixel_type =
        FunctionType::get(void_type, engine_windowSetPixel_param_types, false);
    FunctionCallee engine_windowSetPixel_func = module->getOrInsertFunction(
        "engine_windowSetPixel", engine_windowSetPixel_type);

    // declare: void engine_windowUpdate();
    FunctionType *engine_windowUpdate_type =
        FunctionType::get(void_type, {void_type}, false);
    FunctionCallee engine_windowUpdate_func = module->getOrInsertFunction(
        "engine_windowUpdate", engine_windowUpdate_type);

    // declare: void app();
    FunctionType *app_type = FunctionType::get(void_type, {}, false);
    Function *app_func = Function::Create(app_type, Function::ExternalLinkage,
                                          "app", module.get());

    // app basic blocks
    BasicBlock *bb00 = BasicBlock::Create(context, "", app_func);
    BasicBlock *bb01 = BasicBlock::Create(context, "", app_func);
    BasicBlock *bb02 = BasicBlock::Create(context, "", app_func);
    BasicBlock *bb10 = BasicBlock::Create(context, "", app_func);
    BasicBlock *bb17 = BasicBlock::Create(context, "", app_func);
    BasicBlock *bb22 = BasicBlock::Create(context, "", app_func);
    BasicBlock *bb25 = BasicBlock::Create(context, "", app_func);
    BasicBlock *bb30 = BasicBlock::Create(context, "", app_func);
    BasicBlock *bb44 = BasicBlock::Create(context, "", app_func);
    BasicBlock *bb54 = BasicBlock::Create(context, "", app_func);

    // 0:
    builder.SetInsertPoint(bb00);
    //  br label %1
    builder.CreateBr(bb01);

    // 1:
    builder.SetInsertPoint(bb01);
    //  br label %2
    builder.CreateBr(bb02);

    // 2:
    builder.SetInsertPoint(bb02);
    //  %3 = phi i32 [ 196608, %1 ], [ %12, %10 ]
    PHINode *val03 = builder.CreatePHI(i32_type, 2);
    //  %4 = phi i32 [ 196608, %1 ], [ %14, %10 ]
    PHINode *val04 = builder.CreatePHI(i32_type, 2);
    //  %5 = phi i32 [ 0, %1 ], [ %15, %10 ]
    PHINode *val05 = builder.CreatePHI(i32_type, 2);
    //  %6 = sdiv i32 %3, -2
    Value *val06 = builder.CreateSDiv(val03, builder.getInt32(-2));
    //  %7 = add nsw i32 %6, -47625
    Value *val07 = builder.CreateNSWAdd(val06, builder.getInt32(-47625));
    //  %8 = sdiv i32 %4, -2
    Value *val08 = builder.CreateSDiv(val04, builder.getInt32(-2));
    //  %9 = add nsw i32 %8, 16874
    Value *val09 = builder.CreateNSWAdd(val08, builder.getInt32(16874));
    //  br label %17
    builder.CreateBr(bb17);

    // 10:
    builder.SetInsertPoint(bb10);
    //  tail call void @engine_windowUpdate()
    builder.CreateCall(engine_windowUpdate_func);
    //  %11 = mul nsw i32 %3, 3
    Value *val11 = builder.CreateNSWMul(val03, builder.getInt32(3));
    //  %12 = sdiv i32 %11, 4
    Value *val12 = builder.CreateSDiv(val11, builder.getInt32(4));
    //  %13 = mul nsw i32 %12, 3
    Value *val13 = builder.CreateNSWMul(val12, builder.getInt32(3));
    //  %14 = sdiv i32 %13, 4
    Value *val14 = builder.CreateSDiv(val13, builder.getInt32(4));
    //  %15 = add nuw nsw i32 %5, 1
    Value *val15 = builder.CreateAdd(val05, builder.getInt32(1), "", true, true);
    //  %16 = icmp eq i32 %15, 20
    Value *val16 = builder.CreateICmpEQ(val15, builder.getInt32(20));
    //  br i1 %16, label %1, label %2, !llvm.loop !5
    builder.CreateCondBr(val16, bb01, bb02);

    // 17:
    builder.SetInsertPoint(bb17);
    //  %18 = phi i32 [ 0, %2 ], [ %23, %22 ]
    PHINode *val18 = builder.CreatePHI(i32_type, 2);
    //  %19 = mul nsw i32 %18, %3
    Value *val19 = builder.CreateNSWMul(val18, val03);
    //  %20 = sdiv i32 %19, 400
    Value *val20 = builder.CreateSDiv(val19, builder.getInt32(400));
    //  %21 = add nsw i32 %7, %20
    Value *val21 = builder.CreateNSWAdd(val07, val20);
    //  br label %25
    builder.CreateBr(bb25);

    // 22:
    builder.SetInsertPoint(bb22);
    //  %23 = add nuw nsw i32 %18, 1
    Value *val23 = builder.CreateAdd(val18, builder.getInt32(1), "", true, true);
    //  %24 = icmp eq i32 %23, 400
    Value *val24 = builder.CreateICmpEQ(val23, builder.getInt32(400));
    //  br i1 %24, label %10, label %17, !llvm.loop !7
    builder.CreateCondBr(val24, bb10, bb17);

    // 25:
    builder.SetInsertPoint(bb25);
    //  %26 = phi i32 [ 0, %17 ], [ %56, %54 ]
    PHINode *val26 = builder.CreatePHI(i32_type, 2);
    //  %27 = mul nsw i32 %26, %4
    Value *val27 = builder.CreateNSWMul(val26, val04);
    //  %28 = sdiv i32 %27, 400
    Value *val28 = builder.CreateSDiv(val27, builder.getInt32(400));
    //  %29 = add nsw i32 %9, %28
    Value *val29 = builder.CreateNSWAdd(val09, val28);
    //  br label %30
    builder.CreateBr(bb30);

    // 30:
    builder.SetInsertPoint(bb30);
    //  %31 = phi i32 [ 0, %25 ], [ %52, %44 ]
    PHINode *val31 = builder.CreatePHI(i32_type, 2);
    //  %32 = phi i32 [ 0, %25 ], [ %51, %44 ]
    PHINode *val32 = builder.CreatePHI(i32_type, 2);
    //  %33 = phi i32 [ 0, %25 ], [ %46, %44 ]
    PHINode *val33 = builder.CreatePHI(i32_type, 2);
    //  %34 = sext i32 %33 to i64
    Value *val34 = builder.CreateSExt(val33, builder.getInt64Ty());
    //  %35 = mul nsw i64 %34, %34
    Value *val35 = builder.CreateNSWMul(val34, val34);
    //  %36 = lshr i64 %35, 16
    Value *val36 = builder.CreateLShr(val35, builder.getInt32(16));
    //  %37 = trunc i64 %36 to i32
    Value *val37 = builder.CreateTrunc(val36, i32_type);
    //  %38 = sext i32 %32 to i64
    Value *val38 = builder.CreateSExt(val32, builder.getInt64Ty());
    //  %39 = mul nsw i64 %38, %38
    Value *val39 = builder.CreateNSWMul(val38, val38);
    //  %40 = lshr i64 %39, 16
    Value *val40 = builder.CreateLShr(val39, builder.getInt32(16));
    //  %41 = trunc i64 %40 to i32
    Value *val41 = builder.CreateTrunc(val40, i32_type);
    //  %42 = add nsw i32 %41, %37
    Value *val42 = builder.CreateNSWAdd(val41, val37);
    //  %43 = icmp sgt i32 %42, 262144
    Value *val43 = builder.CreateICmpSGT(val42, builder.getInt32(262144));
    //  br i1 %43, label %54, label %44
    builder.CreateCondBr(val43, bb54, bb44);

    // 44:
    builder.SetInsertPoint(bb44);
    //  %45 = add i32 %21, %37
    Value *val45 = builder.CreateAdd(val21, val37);
    //  %46 = sub i32 %45, %41
    Value *val46 = builder.CreateSub(val45, val41);
    //  %47 = mul nsw i64 %38, %34
    Value *val47 = builder.CreateNSWMul(val38, val34);
    //  %48 = lshr i64 %47, 15
    Value *val48 = builder.CreateLShr(val47, builder.getInt32(15));
    //  %49 = trunc i64 %48 to i32
    Value *val49 = builder.CreateTrunc(val48, i32_type);
    //  %50 = and i32 %49, -2
    Value *val50 = builder.CreateAnd(val49, builder.getInt32(-2));
    //  %51 = add nsw i32 %29, %50
    Value *val51 = builder.CreateNSWAdd(val29, val50);
    //  %52 = add nuw nsw i32 %31, 1
    Value *val52 = builder.CreateAdd(val31, builder.getInt32(1), "", true, true);
    //  %53 = icmp eq i32 %52, 256
    Value *val53 = builder.CreateICmpEQ(val52, builder.getInt32(256));
    //  br i1 %53, label %54, label %30, !llvm.loop !8
    builder.CreateCondBr(val53, bb54, bb30);

    // 54:
    builder.SetInsertPoint(bb54);
    //  %55 = phi i32 [ %31, %30 ], [ 256, %44 ]
    PHINode *val55 = builder.CreatePHI(i32_type, 2);
    //  tail call void @engine_windowSetPixel(i32 noundef %18, i32 noundef %26,
    //  i32 noundef %55, i32 noundef %55, i32 noundef %55)
    Value *args[] = {val18, val26, val55, val55, val55};
    builder.CreateCall(engine_windowSetPixel_func, args);
    //  %56 = add nuw nsw i32 %26, 1
    Value *val56 = builder.CreateAdd(val26, builder.getInt32(1), "", true, true);
    //  %57 = icmp eq i32 %56, 400
    Value *val57 = builder.CreateICmpEQ(val56, builder.getInt32(400));
    //  br i1 %57, label %22, label %25, !llvm.loop !9
    builder.CreateCondBr(val57, bb22, bb25);

    // Link PHI nodes
    //  %3 = phi i32 [ 196608, %1 ], [ %12, %10 ]
    val03->addIncoming(builder.getInt32(196608), bb01);
    val03->addIncoming(val12, bb10);

    //  %4 = phi i32 [ 196608, %1 ], [ %14, %10 ]
    val04->addIncoming(builder.getInt32(196608), bb01);
    val04->addIncoming(val14, bb10);

    //  %5 = phi i32 [ 0, %1 ], [ %15, %10 ]
    val05->addIncoming(builder.getInt32(0), bb01);
    val05->addIncoming(val15, bb10);

    //  %18 = phi i32 [ 0, %2 ], [ %23, %22 ]
    val18->addIncoming(builder.getInt32(0), bb02);
    val18->addIncoming(val23, bb22);

    //  %26 = phi i32 [ 0, %17 ], [ %56, %54 ]
    val26->addIncoming(builder.getInt32(0), bb17);
    val26->addIncoming(val56, bb54);

    //  %31 = phi i32 [ 0, %25 ], [ %52, %44 ]
    val31->addIncoming(builder.getInt32(0), bb25);
    val31->addIncoming(val52, bb44);

    //  %32 = phi i32 [ 0, %25 ], [ %51, %44 ]
    val32->addIncoming(builder.getInt32(0), bb25);
    val32->addIncoming(val51, bb44);

    //  %33 = phi i32 [ 0, %25 ], [ %46, %44 ]
    val33->addIncoming(builder.getInt32(0), bb25);
    val33->addIncoming(val46, bb44);

    //  %55 = phi i32 [ %31, %30 ], [ 256, %44 ]
    val55->addIncoming(builder.getInt32(256), bb44);
    val55->addIncoming(val31, bb30);

    // Dump IR
    module->print(outs(), nullptr);

    // Interpret IR
    outs() << "Running code ...\n";
    InitializeNativeTarget();
    InitializeNativeTargetAsmPrinter();

    ExecutionEngine *ee = EngineBuilder(std::move(module)).create();
    ee->InstallLazyFunctionCreator([&](const std::string &func_name) -> void * {
        if (func_name == "engine_windowSetPixel") {
            return reinterpret_cast<void *>(engine_windowSetPixel);
        }
        if (func_name == "engine_windowUpdate") {
            return reinterpret_cast<void *>(engine_windowUpdate);
        }

        return nullptr;
    });
    ee->finalizeObject();

    ArrayRef<GenericValue> noargs;
    GenericValue v = ee->runFunction(app_func, noargs);
    outs() << "Code was run\n";
}
