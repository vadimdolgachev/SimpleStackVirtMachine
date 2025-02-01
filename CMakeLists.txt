cmake_minimum_required(VERSION 3.30)
project(stack_vm)

set(CMAKE_CXX_STANDARD 23)

add_executable(stack_vm main.cpp)
target_compile_options(stack_vm PRIVATE -Wall -Wextra -Wpedantic -Werror -Wstrict-aliasing)