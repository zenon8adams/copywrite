cmake_minimum_required(VERSION 3.15)
project(copywrite)

set(CMAKE_CXX_STANDARD 14)
set(SYSTEM_INCLUDE_PATH "${CMAKE_SYSTEM_INCLUDE_PATH}/..")

add_executable(copywrite copywrite.cpp copywrite.hpp colors_defs.hpp easing_defs.hpp)

target_include_directories( copywrite PRIVATE "${SYSTEM_INCLUDE_PATH}/freetype2/" "${SYSTEM_INCLUDE_PATH}/freetype2/freetype")

target_link_libraries( copywrite PRIVATE -lfreetype -lfontconfig -lpng)