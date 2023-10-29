# Define traced app build

# Traced app
add_executable(mandel_trace)

# Target for traced code
add_library(mandel_obj OBJECT ${APP_SRC_DIR}/app.cpp)

# Apply optimizations for trace to be representive
target_compile_options(mandel_obj
PRIVATE
    -O2
)

target_link_libraries(mandel_obj
PRIVATE
    engine::include
    trace::add_pass
)

add_dependencies(mandel_obj engine trace)

target_link_libraries(mandel_trace
PRIVATE
    engine
    trace::tracers_src
    mandel_obj
)
