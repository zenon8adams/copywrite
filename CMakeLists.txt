#/***************************************************************************/
#/*                                                                         */
#/*  CMakeList.txt                                                          */
#/*                                                                         */
#/*    Project configuration script                  					    */
#/*                                                                         */
#/*  Copyright 2022 by Adesina Meekness                                     */
#/*                                                                         */
#/*                                                                         */
#/*       ##    ## ##                                                       */
#/*       ##    ##  #                                                       */
#/*       ###  ###  #  ##                                                   */
#/*       # # # ##  # #                                                     */
#/* ####  # ### ##  ###                                                     */
#/*       #  #  ##  # ##                                                    */
#/*       #  #  ##  #  ##                                                   */
#/*                                                                         */
#/*                                                                         */
#/*  This file is part of the Copywrite project, and may only be used,      */
#/*  modified, and distributed under the terms of the GNU project           */
#/*  license, LICENSE.TXT.  By continuing to use, modify, or distribute     */
#/*  this file you indicate that you have read the license and              */
#/*  understand and accept it fully.                                        */
#/*                                                                         */
#/***************************************************************************/

cmake_minimum_required(VERSION 3.5.0)
project(copywrite CXX)

set(APP_NAME copywrite)
enable_testing()

# Read the version information from the VERSION file
file(STRINGS "${PROJECT_SOURCE_DIR}/VERSION" PACKAGE_VERSION)
string(REGEX REPLACE "([0-9]+)\\.[0-9]+\\.[0-9]+.*" "\\1" CPACK_PACKAGE_VERSION_MAJOR ${PACKAGE_VERSION})
string(REGEX REPLACE "[0-9]+\\.([0-9]+)\\.[0-9]+.*" "\\1" CPACK_PACKAGE_VERSION_MINOR ${PACKAGE_VERSION})
string(REGEX REPLACE "[0-9]+\\.[0-9]+\\.([0-9]+).*" "\\1" CPACK_PACKAGE_VERSION_PATCH ${PACKAGE_VERSION})

set(PROJECT_VERSION ${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}.${CPACK_PACKAGE_VERSION_PATCH})

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_LIST_DIR}/cmake)

################################################################################################################
# Setup C/C++ compiler options
# C++17 compiler support
################################################################################################################
include(CXX17)
check_for_cxx17_compiler( CXX17_COMPILER)

if(CXX17_COMPILER)
    set(CMAKE_CXX_COMPILER "clang++")
    set(CMAKE_CXX_STANDARD 17)
    set(CMAKE_CXX_STANDARD_REQUIRED ON)
else()
    message(FATAL_ERROR "Compiler does not support C++17.")
endif()

option(CUSTOM_FONT_SUPPORTED "Install and Uninstall fonts locally" OFF)
option(PNG_SUPPORTED         "Export design as PNG"                OFF)
option(JPG_SUPPORTED         "Export design as JPG"                OFF)
option(RUN_ALL_TESTS         "Compile and run tests"               OFF)

##################################################
# Setup the default font to be used in the project
##################################################
set(DEFAULT_PROJECT_FONT "${CMAKE_CURRENT_LIST_DIR}/assets/common-fonts/Times New Roman/times new roman bold.ttf")

set(OPERATING_SYSTEM "${CMAKE_SYSTEM_NAME}")

if(CMAKE_SYSTEM_NAME MATCHES Linux)
    set(IS_LINUX ON)
    include(CheckIncludeFiles)
    check_include_files( "sys/types.h;sys/stat.h;unistd.h" HAVE_SYS_STAT_H)
elseif(CMAKE_SYSTEM_NAME MATCHES Windows)
    set(IS_WINDOWS ON)
elseif(CMAKE_SYSTEM_NAME MATCHES Darwin)
    set(IS_DARWIN ON)
endif()

if($<CONFIG:Debug>)
    string(APPEND CMAKE_CXX_FLAGS " -pg -ggdb -fsanitize=address")
elseif($<CONFIG:Release>)
    string(APPEND CMAKE_CXX_FLAGS " -o2")
endif()

find_library( FONTCONFIG_LIB_PATH NAMES fontconfig libfontconfig REQUIRED)
find_library( FREETYPE_LIB_PATH NAMES freetype libfreetype freetype2 libfreetype2 REQUIRED)
find_library( PNG_LIB_PATH NAMES png libpng++)
find_library( JPG_LIB_PATH NAMES jpeg libjpeg)

find_path( FONTCONFIG_INCLUDE_PATH fontconfig.h PATH_SUFFIXES fontconfig REQUIRED)
find_path( FREETYPE_INCLUDE_PATH NAMES ft2build.h PATH_SUFFIXES freetype2 freetype REQUIRED)

add_library(FontConfig INTERFACE IMPORTED)
#set_target_properties(FontConfig PROPERTIES IMPORTED_LOCATION ${FONTCONFIG_LIB_PATH})
target_link_libraries(FontConfig INTERFACE ${FONTCONFIG_LIB_PATH})
target_include_directories(FontConfig INTERFACE ${FONTCONFIG_INCLUDE_PATH})

add_library(Freetype INTERFACE IMPORTED)
#set_target_properties(Freetype PROPERTIES IMPORTED_LOCATION ${FONTCONFIG_LIB_PATH})
target_link_libraries(Freetype INTERFACE ${FREETYPE_LIB_PATH})
target_include_directories(Freetype INTERFACE ${FREETYPE_INCLUDE_PATH})

if(NOT JPG_LIB_PATH)
    message( WARNING "jpeglib not found: Features requiring jpeg are disabled.")
else()
    set(JPG_SUPPORTED ON)
    add_library(JPEG INTERFACE IMPORTED)
#    set_target_properties(JPEG PROPERTIES IMPORTED_LOCATION ${JPG_LIB_PATH})
    target_link_libraries(JPEG INTERFACE ${JPG_LIB_PATH})
    find_path( JPG_INCLUDE_PATH NAMES jpeglib.h REQUIRED)
    target_include_directories(JPEG INTERFACE ${JPG_INCLUDE_PATH})
endif()

if(NOT PNG_LIB_PATH)
    message( WARNING "libpng++ not found: Features requiring png are disabled.")
else()
    set(PNG_SUPPORTED ON)
    add_library(PNG INTERFACE IMPORTED)
#    set_target_properties(PNG PROPERTIES IMPORTED_LOCATION ${PNG_LIB_PATH})
    target_link_libraries(PNG INTERFACE ${PNG_LIB_PATH})
    find_path( PNG_INCLUDE_PATH NAMES image.hpp PATH_SUFFIXES png++ REQUIRED)
    target_include_directories(PNG INTERFACE ${PNG_INCLUDE_PATH})
endif()

include(autoloader)
###################################################
# Move through all directories in source all source
# files, include files
# and include paths into the following variables:
# SOURCE_FILES
# INCLUDE_FILES
# INCLUDE_PATHS
###################################################
include_subdirs( src)

add_executable( ${APP_NAME} src/copywrite.cpp include/copywrite.hpp ${SOURCE_FILES} ${INCLUDE_FILES})
target_include_directories( ${APP_NAME} PRIVATE include ${INCLUDE_PATHS})

if(CUSTOM_FONT_SUPPORTED)
    find_package(Zip REQUIRED)
endif()

configure_file(config.h.in config.h)
target_compile_features( ${APP_NAME} PRIVATE cxx_std_17 cxx_constexpr
                                                 cxx_decltype cxx_lambdas
                                                 cxx_variable_templates)
target_link_libraries( ${APP_NAME} PUBLIC Freetype FontConfig $<TARGET_NAME_IF_EXISTS:JPEG>
                                                                  $<TARGET_NAME_IF_EXISTS:PNG>
                                                                  $<TARGET_NAME_IF_EXISTS:Zip::Zip>)
target_include_directories( ${APP_NAME} PUBLIC ${CMAKE_CURRENT_BINARY_DIR})

if(RUN_ALL_TESTS)
    add_subdirectory(tests)
endif()

print_configuration()