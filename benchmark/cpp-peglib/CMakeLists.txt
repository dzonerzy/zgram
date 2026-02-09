cmake_minimum_required(VERSION 3.14)
project(cpppeg_bench CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

include(FetchContent)
FetchContent_Declare(
    cpp-peglib
    GIT_REPOSITORY https://github.com/yhirose/cpp-peglib.git
    GIT_TAG v1.9.1
)
FetchContent_MakeAvailable(cpp-peglib)

add_executable(cpppeg_bench main.cpp)
target_include_directories(cpppeg_bench PRIVATE ${cpp-peglib_SOURCE_DIR})
target_compile_options(cpppeg_bench PRIVATE -O3)
