#include <iostream>

#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/Pass.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

using namespace llvm;

extern "C" {

void traceInstr(char *instr_name);

} // extern "C"

namespace {

struct TracePass : public FunctionPass {
    static char ID;

    TracePass() : FunctionPass(ID) {}

    static bool isTracer(StringRef func_name) noexcept {
        return func_name == "traceInstr";
    }

    bool runOnFunction(Function &func) override {
        // if (isTracer(func.getName())) {
        //     return false;
        // }

        LLVMContext &ctx = func.getContext();
        IRBuilder<> builder(ctx);
        Type *tracer_ret_type = Type::getVoidTy(ctx);

        ArrayRef<Type *> tracer_instr_param_types = {
            builder.getInt8Ty()->getPointerTo(),
        };

        FunctionType *tracer_instr_func_type =
            FunctionType::get(tracer_ret_type, tracer_instr_param_types, false);
        FunctionCallee tracer_instr_func = func.getParent()->getOrInsertFunction(
            "traceInstr", tracer_instr_func_type);

        outs() << "trace: Instrumenting " << func.getName() << "\n";

        for (auto &BB : func) {
            for (auto &instr : BB) {
                if (dyn_cast<PHINode>(&instr) != nullptr) {
                    continue;
                }

                if (auto *call = dyn_cast<CallInst>(&instr)) {
                    Function *callee = call->getCalledFunction();
                    if (!callee || isTracer(callee->getName())) {
                        continue;
                    }
                }

                builder.SetInsertPoint(&instr);
                // builder.SetInsertPoint(&BB, ++builder.GetInsertPoint());

                Value *instr_name =
                    builder.CreateGlobalStringPtr(instr.getOpcodeName());
                Value *args[] = {instr_name};
                builder.CreateCall(tracer_instr_func, args);
            }
        }

        return true;
    }
};

char TracePass::ID = 0;

static void registerTracePass(const PassManagerBuilder &,
                              legacy::PassManagerBase &PM) {
    PM.add(new TracePass());
}

static RegisterStandardPasses
    RegisterTracePass(PassManagerBuilder::EP_OptimizerLast, registerTracePass);

} // namespace
