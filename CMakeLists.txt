cmake_minimum_required(VERSION 3.28)
project(gba CXX)

enable_testing()

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

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

add_subdirectory(src)

find_package(Catch2 3)

IF (Catch2_FOUND)
    add_subdirectory(test)
ENDIF()
