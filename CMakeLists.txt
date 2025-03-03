cmake_minimum_required(VERSION 3.20)

project(stuff)

set(CMAKE_CXX_STANDARD 20)

find_package(fmt CONFIG REQUIRED)
find_package(LLVM CONFIG REQUIRED)

include_directories(SYSTEM ${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})
llvm_map_components_to_libnames(llvm_libs core support passes analysis target codegen mc native asmprinter)

set(SOURCES
	src/main.cpp
	src/token.cpp
)

include_directories(src)
add_compile_options(-fsanitize=address)
add_link_options(-fsanitize=address)

# all compiler warnings but only for my code
add_compile_options(-Wall -Wextra -Wpedantic)

add_executable(stuff ${SOURCES})

target_link_libraries(stuff PRIVATE fmt::fmt)
target_link_libraries(stuff PRIVATE ${llvm_libs})
