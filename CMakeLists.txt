# Describe project build

cmake_minimum_required(VERSION 3.11)

project(yalp VERSION 0.1 LANGUAGES CXX)

# Compiler commands export for language server
set(CMAKE_EXPORT_COMPILE_COMMANDS 1)

# C++20 is the default standard
set(CMAKE_CXX_STANDARD 20)

add_subdirectory(examples)

