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

cmake_minimum_required(VERSION 3.10)
project(copywrite)

set(CMAKE_CXX_STANDARD 17)
# set(CMAKE_CXX_FLAGS -fcoroutines)
option(CUSTOM_FONT_SUPPORTED OFF CACHE)
option(PNG_SUPPORTED OFF)
option(JPG_SUPPORTED OFF)
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

if(CMAKE_BUILD_TYPE MATCHES "Debug")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pg -ggdb -fsanitize=address")
elseif(CMAKE_BUILD_TYPE MATCHES "Release")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -o2")
endif()

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-c99-designator")

find_library( FONTCONFIG_LIB_PATH NAMES fontconfig libfontconfig REQUIRED)
if(NOT FONTCONFIG_LIB_PATH)
    message( FATAL_ERROR "libfontconfig required for ${PROJECT_NAME} to function")
endif()

find_library( FREETYPE_LIB_PATH NAMES freetype libfreetype freetype2 libfreetype2 REQUIRED)
if(NOT FREETYPE_LIB_PATH)
    message( FATAL_ERROR "libfreetype required for ${PROJECT_NAME} to function")
endif()

find_library( PNG_LIB_PATH NAMES png libpng++)
find_library( JPG_LIB_PATH NAMES jpeg libjpeg)
if(NOT JPG_LIB_PATH)
    message( WARNING "jpeglib not found: Features requiring jpeg are disabled.")
    unset(JPG_LIB_PATH)
else()
    set(JPG_SUPPORTED ON)
endif()

if(NOT PNG_LIB_PATH)
    message( WARNING "libpng++ not found: Features requiring png are disabled.")
    unset( PNG_LIB_PATH)
else()
    set(PNG_SUPPORTED ON)
endif()

find_path( FONTCONFIG_INCLUDE_PATH NAMES fontconfig fontconfig2)
find_path( FREETYPE_INCLUDE_PATH NAMES freetype freetype2)
find_path( PNG_INCLUDE_PATH NAMES png++)
find_path( JPG_INCLUDE_PATH NAMES jpeglib.h)

if(NOT PNG_INCLUDE_PATH)
    set(PNG_INCLUDE_PATH "")
else()
    set(PNG_INCLUDE_PATH "${PNG_INCLUDE_PATH}/png++")
endif()

if(NOT JPG_INCLUDE_PATH)
    set(JPG_INCLUDE_PATH "")
endif()

if(EXISTS  "${FONTCONFIG_INCLUDE_PATH}/fontconfig")
    set( FONTCONFIG_INCLUDE_PATH "${FONTCONFIG_INCLUDE_PATH}/fontconfig/")
endif()

if(EXISTS "${FREETYPE_INCLUDE_PATH}/freetype2")
    set( FONTCONFIG_INCLUDE_PATH "${FREETYPE_INCLUDE_PATH}/freetype2/")
endif()

add_executable( ${PROJECT_NAME} copywrite.cpp copywrite.hpp colors_defs.hpp easing_defs.hpp
                                geo_vector.cpp geo_vector.hpp composition_defs.hpp blend_defs.hpp timer.hpp special_effects.hpp snap_defs.hpp)

if(CUSTOM_FONT_SUPPORTED)
    find_library(ZIP_LIB_PATH NAMES zip libzip)
    find_path(ZIP_INCLUDE_PATH NAMES zip.h)
    if(NOT ZIP_LIB_PATH AND ZIP_INCLUDE_PATH)
        message(FATAL_ERROR "libzip installation is required for custom font feature to be available")
    endif()
endif()

configure_file(config.h.in config.h)
target_include_directories( ${PROJECT_NAME} PRIVATE ${FONTCONFIG_INCLUDE_PATH}
        ${FREETYPE_INCLUDE_PATH} ${PNG_INCLUDE_PATH} ${JPG_INCLUDE_PATH} ${ZIP_INCLUDE_PATH}
        ${CMAKE_CURRENT_BINARY_DIR})
target_link_libraries( ${PROJECT_NAME} PUBLIC ${FONTCONFIG_LIB_PATH} ${FREETYPE_LIB_PATH}
        ${PNG_LIB_PATH} ${JPG_LIB_PATH} ${ZIP_LIB_PATH})