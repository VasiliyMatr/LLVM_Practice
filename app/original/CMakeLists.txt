# Define original app build

add_executable(mandel_cpp ${APP_SRC_DIR}/app.cpp)

target_link_libraries(mandel_cpp
PRIVATE
    engine
)

# Also dump llvm IR
set(MANDEL_LL ${CMAKE_CURRENT_BINARY_DIR}/mandel.ll)

add_custom_command(
    OUTPUT ${MANDEL_LL}
    COMMAND ${CMAKE_CXX_COMPILER} -S -emit-llvm -O2 -std=c++20 -I${ENGINE_INCLUDE_DIR} -o ${MANDEL_LL} ${APP_SRC_DIR}/app.cpp 
    DEPENDS ${APP_SRC_DIR}/app.cpp 
)
add_custom_target(mandel_ll DEPENDS ${MANDEL_LL})

add_dependencies(mandel_cpp mandel_ll)
