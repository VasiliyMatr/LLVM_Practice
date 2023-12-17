#ifndef COMMON_HPP
#define COMMON_HPP

#include <iostream>

#define NODISCARD [[nodiscard]]

#define ASSERT(cond)                                                           \
    do {                                                                       \
        if (!(cond)) {                                                         \
            std::cerr << "Assertion failed: " << #cond << std::endl            \
                      << "Location: " << __FILE__ << ":" << __LINE__           \
                      << std::endl;                                            \
            std::terminate();                                                  \
        }                                                                      \
    } while (0)

#define UNREACHABLE()                                                          \
    do {                                                                       \
        std::cerr << "Encounter unreachable code" << std::endl                 \
                  << "Location: " << __FILE__ << ":" << __LINE__ << std::endl; \
        std::terminate();                                                      \
    } while (0)

namespace lang {

enum class ValType { INT, FIXED };

enum class UnOpKind { PLUS, MINUS };

enum class BinOpKind {
    MUL,
    DIV,
    ADD,
    SUB,
    CMP_LESS,
    CMP_LESS_EQUAL,
    CMP_GREATER,
    CMP_GREATER_EQUAL,
    CMP_EQUAL,
    CMP_NOT_EQUAL,
};

} // namespace lang

#endif // COMMON_HPP
