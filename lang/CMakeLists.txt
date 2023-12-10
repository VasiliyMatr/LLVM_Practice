
find_package(FLEX REQUIRED)
find_package(BISON REQUIRED)

flex_target(lexer
    lexer.ll
    ${CMAKE_CURRENT_BINARY_DIR}/lexer.cpp
)

bison_target(parser
    parser.yy
    ${CMAKE_CURRENT_BINARY_DIR}/parser.cpp
    COMPILE_FLAGS "--defines=${CMAKE_CURRENT_BINARY_DIR}/yy_parser.tab.hpp"
)

add_flex_bison_dependency(lexer parser)

add_executable(toyc
    src/toyc.cpp
    ${FLEX_lexer_OUTPUTS}
    ${BISON_parser_OUTPUTS}
)

target_include_directories(toyc
PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/include
    ${CMAKE_CURRENT_BINARY_DIR}
)

target_link_libraries(toyc
PRIVATE
    CLI11
)
