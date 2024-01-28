#include <CLI/CLI.hpp>

#include <engine.hpp>

#include <lang/dot_dump.hpp>
#include <lang/driver.hpp>
#include <lang/ir_gen.hpp>

using namespace llvm;

int main(int argc, char **argv) {
    CLI::App app("Toy lang compiler");

    std::string dot_file_name{};
    auto *dot_arg = app.add_option("-d,--dot", dot_file_name, "Dot dump file");
    dot_arg->required(false);

    std::string input_file_name{};
    auto *input_arg = app.add_option("--in", input_file_name, "Input file");
    input_arg->required();

    CLI11_PARSE(app, argc, argv);

    yy::Driver driver{};

    // Build AST
    std::ifstream in_stream {input_file_name};
    const auto *ast_root = driver.buildAST(in_stream);

    // Dump AST if needed
    if (!dot_arg->empty()) {
        std::ofstream dot_file{dot_file_name};
        lang::ASTDotDumper dot_dumper{dot_file};
        dot_dumper.dump_ast(ast_root);
    }

    // Generate IR
    LLVMContext context{};
    auto module = std::make_unique<Module>("top", context);

    lang::IRGen ir_gen{context, *module};
    auto *app_func = ir_gen.genIR(ast_root);
    module->print(llvm::outs(), nullptr);

    std::vector<llvm::GlobalVariable *> globs{};

    for (auto &&glob : module->getGlobalList()) {
        globs.push_back(&glob);
    }

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
        if (func_name == "engine_windowCreate") {
            return reinterpret_cast<void *>(engine_windowCreate);
        }

        return nullptr;
    });

    std::vector<uint32_t> globs_vals{};
    for (auto &&glob : globs) {
        globs_vals.push_back(0);
        ee->addGlobalMapping(glob, globs.back());
    }

    ee->finalizeObject();

    ArrayRef<GenericValue> noargs;
    GenericValue v = ee->runFunction(app_func, noargs);
    outs() << "Code was run\n";

    return 0;
}
