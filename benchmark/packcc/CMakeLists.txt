cmake_minimum_required(VERSION 3.14)
project(packcc_bench C)

include(FetchContent)
FetchContent_Declare(
    packcc
    GIT_REPOSITORY https://github.com/arithy/packcc.git
    GIT_TAG 78e65e69784a9696f065ed7a9c453a66f8685409
)
FetchContent_GetProperties(packcc)
if(NOT packcc_POPULATED)
    FetchContent_Populate(packcc)
endif()

# Build the PackCC generator, then generate the JSON parser with it
add_executable(packcc ${packcc_SOURCE_DIR}/src/packcc.c)
add_custom_command(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/json.c ${CMAKE_CURRENT_BINARY_DIR}/json.h
    COMMAND packcc -o ${CMAKE_CURRENT_BINARY_DIR}/json ${CMAKE_CURRENT_SOURCE_DIR}/json.peg
    DEPENDS packcc ${CMAKE_CURRENT_SOURCE_DIR}/json.peg
)

add_executable(packcc_bench main.c ${CMAKE_CURRENT_BINARY_DIR}/json.c)
target_include_directories(packcc_bench PRIVATE ../common ${CMAKE_CURRENT_BINARY_DIR})
target_compile_options(packcc_bench PRIVATE -O3)
