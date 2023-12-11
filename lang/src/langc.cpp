#include <CLI/CLI.hpp>

#include <lang/dot_dump.hpp>
#include <lang/driver.hpp>

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

    std::ifstream in_stream {input_file_name};
    const auto *ast_root = driver.buildAST(in_stream);

    if (!dot_arg->empty()) {
        std::ofstream dot_file{dot_file_name};
        lang::ASTDotDumper dot_dumper{dot_file};
        dot_dumper.dump_ast(ast_root);
    }

    return 0;
}
