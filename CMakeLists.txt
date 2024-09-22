cmake_minimum_required(VERSION 3.28)
project(gba CXX)

enable_testing()

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

if (NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE "Debug")
endif()

add_compile_options(-std=c++23)
add_compile_options(-g)
add_compile_options(-Wall)
# add_compile_options(-fmodules-ts)
add_compile_options(-Wno-deprecated-declarations)
add_compile_options(-fdiagnostics-color=always)
add_compile_options(-v)

if (CMAKE_BUILD_TYPE STREQUAL "Debug")
    message("-- Use flag -fsanitize=address")
    list(APPEND CMAKE_CXX_FLAGS "-fsanitize=address")
endif()


set(CMAKE_CXX_FLAGS_DEBUG "-O0 -g")
set(CMAKE_CXX_FLAGS_RELEASE "-O2 -DNDEBUG")

find_package(spdlog CONFIG REQUIRED)
find_package(SDL3 REQUIRED CONFIG REQUIRED COMPONENTS SDL3)

add_subdirectory(src)

find_package(Catch2 3)
# Only build tests if Catch2 is available
IF (Catch2_FOUND)
    add_subdirectory(test)
ENDIF()

# Exports compile_commands.json to project root for ide support
add_custom_target(
    copy-compile-commands ALL
    ${CMAKE_COMMAND} -E copy_if_different
        ${CMAKE_BINARY_DIR}/compile_commands.json
        ${CMAKE_CURRENT_LIST_DIR}
)
