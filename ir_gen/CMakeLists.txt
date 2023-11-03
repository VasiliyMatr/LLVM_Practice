# Define IR generator build

add_executable(ir_gen src/main.cpp)

target_include_directories(ir_gen
PRIVATE
    ${LLVM_INCLUDE_DIRS}
)

target_link_libraries(ir_gen
PRIVATE
    LLVM
    engine
)
