# Define target app implementations and engine build

set(APP_SRC_DIR ${CMAKE_CURRENT_SOURCE_DIR}/src)
set(ENGINE_INCLUDE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/engine/include)

add_subdirectory(engine)
add_subdirectory(original)
add_subdirectory(instrumented)
