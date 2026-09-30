cmake_minimum_required(VERSION 3.14)
project(spirit_x3_bench CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Boost.Spirit X3 is header-only
find_package(Boost 1.70 REQUIRED)

add_executable(spirit_x3_bench main.cpp)
target_link_libraries(spirit_x3_bench PRIVATE Boost::boost)
target_include_directories(spirit_x3_bench PRIVATE ../common)
target_compile_options(spirit_x3_bench PRIVATE -O3)

# Typed AST (idiomatic attribute-based parsing into values)
add_executable(spirit_x3_ast_bench ast.cpp)
target_link_libraries(spirit_x3_ast_bench PRIVATE Boost::boost)
target_include_directories(spirit_x3_ast_bench PRIVATE ../common)
target_compile_options(spirit_x3_ast_bench PRIVATE -O3)

# Expression grammar (validation + flat tree)
add_executable(spirit_x3_expr_bench expr.cpp)
target_link_libraries(spirit_x3_expr_bench PRIVATE Boost::boost)
target_include_directories(spirit_x3_expr_bench PRIVATE ../common)
target_compile_options(spirit_x3_expr_bench PRIVATE -O3)
