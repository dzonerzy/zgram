cmake_minimum_required(VERSION 3.14)
project(pegtl_bench CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

include(FetchContent)
FetchContent_Declare(
    pegtl
    GIT_REPOSITORY https://github.com/taocpp/PEGTL.git
    GIT_TAG 3.2.8
)
FetchContent_MakeAvailable(pegtl)

add_executable(pegtl_bench main.cpp)
target_link_libraries(pegtl_bench PRIVATE taocpp::pegtl)
target_compile_options(pegtl_bench PRIVATE -O3)
