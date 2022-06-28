cmake_minimum_required(VERSION 3.10)
project(copywrite)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_COMPILER clang++-13)
set(SYSTEM_INCLUDE_PATH "${CMAKE_SYSTEM_INCLUDE_PATH}/../")
set(SYSTEM_LOCAL_INCLUDE_PATH "${SYSTEM_INCLUDE_PATH}../local/include")
set(CMAKE_CXX_FLAGS -fsanitize=address)

add_executable(copywrite copywrite.cpp copywrite.hpp colors_defs.hpp easing_defs.hpp)

target_include_directories( copywrite PRIVATE "${SYSTEM_LOCAL_INCLUDE_PATH}/freetype2/" "${SYSTEM_LOCAL_INCLUDE_PATH}/freetype2/freetype")

target_link_libraries( copywrite PRIVATE -lfreetype -lfontconfig -lpng)