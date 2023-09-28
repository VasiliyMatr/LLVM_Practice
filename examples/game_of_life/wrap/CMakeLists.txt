# Define wrap lib build

# SFML library is used for graphics
find_package(SFML COMPONENTS system window graphics REQUIRED)

add_library(wrap STATIC)

target_link_libraries(wrap
PUBLIC
    sfml-graphics
)

target_include_directories(wrap PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)

target_sources(wrap PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/src/wrap.cpp)
