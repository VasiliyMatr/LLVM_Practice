#include <iostream>

extern "C" {

void traceInstr(char *instr_name) { std::cout << instr_name << ","; }

} // extern "C"
