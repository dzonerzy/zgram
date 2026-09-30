cmake_minimum_required(VERSION 3.14)
project(lexy_bench CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

include(FetchContent)
FetchContent_Declare(
    lexy
    GIT_REPOSITORY https://github.com/foonathan/lexy.git
    GIT_TAG c1358c4117752393e9ce27d6c885d9146846f2f9
)
FetchContent_MakeAvailable(lexy)

add_executable(lexy_bench main.cpp)
target_link_libraries(lexy_bench PRIVATE foonathan::lexy)
target_include_directories(lexy_bench PRIVATE ../common)
target_compile_options(lexy_bench PRIVATE -O3)

# Typed AST (lexy's official JSON example)
add_executable(lexy_ast_bench ast.cpp)
target_link_libraries(lexy_ast_bench PRIVATE foonathan::lexy)
target_include_directories(lexy_ast_bench PRIVATE ../common)
target_compile_options(lexy_ast_bench PRIVATE -O3)

# Expression grammar (expression_production)
add_executable(lexy_expr_bench expr.cpp)
target_link_libraries(lexy_expr_bench PRIVATE foonathan::lexy)
target_include_directories(lexy_expr_bench PRIVATE ../common)
target_compile_options(lexy_expr_bench PRIVATE -O3)
