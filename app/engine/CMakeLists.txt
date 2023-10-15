# Define engine lib build

# SFML library is used for graphics
find_package(SFML COMPONENTS system window graphics REQUIRED)

add_library(engine STATIC)

target_link_libraries(engine
PUBLIC
    sfml-graphics
)

target_include_directories(engine PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)

target_sources(engine PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/src/engine.cpp)
