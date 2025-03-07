cmake_minimum_required(VERSION 3.20)

project(owl)

set(CMAKE_CXX_STANDARD 20)

find_package(fmt CONFIG REQUIRED)
find_path(RANG_INCLUDE_DIRS "rang.hpp")
find_package(LLVM CONFIG REQUIRED)
find_package(argparse CONFIG REQUIRED)

include_directories(SYSTEM ${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})
llvm_map_components_to_libnames(llvm_libs core support passes analysis target codegen mc native asmprinter)

set(SOURCES
	src/main.cpp
	src/token.cpp
)

include_directories(src)
add_compile_options(-fsanitize=address -fsanitize=undefined -fsanitize=leak)
add_link_options(-fsanitize=address -fsanitize=undefined -fsanitize=leak)

add_compile_options(-Wall -Wextra -Wpedantic)

add_executable(owl ${SOURCES})

target_include_directories(owl PRIVATE ${RANG_INCLUDE_DIRS})
target_link_libraries(owl PRIVATE fmt::fmt)
target_link_libraries(owl PRIVATE argparse::argparse)
target_link_libraries(owl PRIVATE ${llvm_libs})
