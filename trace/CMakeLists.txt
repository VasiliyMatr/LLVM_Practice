# Define trace lib build

# Tracing instrumentation is added with llvm pass
add_library(trace SHARED)

target_include_directories(trace
PRIVATE
    ${LLVM_INCLUDE_DIRS}
)

target_link_libraries(trace
PRIVATE
    LLVM
)

target_sources(trace
PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/src/trace.cpp
)

# Add llvm pass for tracing instrumentation
add_library(trace_add_pass INTERFACE)
add_library(trace::add_pass ALIAS trace_add_pass)

target_compile_options(trace_add_pass
INTERFACE
    "SHELL:-Xclang -load"
    "SHELL:-Xclang ${CMAKE_CURRENT_BINARY_DIR}/libtrace.so"
    -flegacy-pass-manager
)

# Add tracers sources
add_library(trace_tracers_src INTERFACE)
add_library(trace::tracers_src ALIAS trace_tracers_src)

target_sources(trace_tracers_src
INTERFACE
    ${CMAKE_CURRENT_SOURCE_DIR}/src/tracers.cpp
)
