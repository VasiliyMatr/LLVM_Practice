# Describe project build

cmake_minimum_required(VERSION 3.11)

project(llvm_course VERSION 0.1 LANGUAGES C CXX)

find_package(LLVM REQUIRED)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")

set(CMAKE_C_COMPILER "clang")
set(CMAKE_CXX_COMPILER "clang++")

# Compiler commands export for language server
set(CMAKE_EXPORT_COMPILE_COMMANDS 1)

# C++20 is the default standard
set(CMAKE_CXX_STANDARD 20)

add_subdirectory(third_party)

add_subdirectory(trace)
add_subdirectory(app)
add_subdirectory(ir_gen)

add_subdirectory(lang)
