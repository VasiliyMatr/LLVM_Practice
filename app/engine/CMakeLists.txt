# Define engine lib build

# SFML library is used for graphics
find_package(SFML COMPONENTS system window graphics REQUIRED)

# Add engine includes
add_library(engine_include INTERFACE)
add_library(engine::include ALIAS engine_include)

target_include_directories(engine_include
INTERFACE
    ${CMAKE_CURRENT_SOURCE_DIR}/include
    ${LLVM_INCLUDE_DIRS}
)

add_library(engine STATIC)

target_link_libraries(engine
PUBLIC
    LLVM
    sfml-graphics
    engine::include
)

target_sources(engine
PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/src/engine.cpp
)
