# Use stripped cli11 with basic features

add_library(CLI11 INTERFACE)

target_include_directories(CLI11 INTERFACE ${CMAKE_CURRENT_SOURCE_DIR}/include)
