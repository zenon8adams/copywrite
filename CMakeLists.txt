#/***************************************************************************/#
#/*                                                                         */#
#/*  CMakeList.txt                                                          */#
#/*                                                                         */#
#/*    Project configuration script                  					    */#
#/*                                                                         */#
#/*  Copyright 2022 by Adesina Meekness                                     */#
#/*                                                                         */#
#/*                                                                         */#
#/*       ##    ## ##                                                       */#
#/*       ##    ##  #                                                       */#
#/*       ###  ###  #  ##                                                   */#
#/*       # # # ##  # #                                                     */#
#/* ####  # ### ##  ###                                                     */#
#/*       #  #  ##  # ##                                                    */#
#/*       #  #  ##  #  ##                                                   */#
#/*                                                                         */#
#/*                                                                         */#
#/*  This file is part of the Copywrite project, and may only be used,      */#
#/*  modified, and distributed under the terms of the GNU project           */#
#/*  license, LICENSE.TXT.  By continuing to use, modify, or distribute     */#
#/*  this file you indicate that you have read the license and              */#
#/*  understand and accept it fully.                                        */#
#/*                                                                         */#
#/***************************************************************************/#

cmake_minimum_required(VERSION 3.19.0)
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

##############################
# Setup C/C++ compiler options
# C++17 compiler support
##############################
include(CXX17)
check_for_cxx17_compiler( CXX17_COMPILER)

if(CXX17_COMPILER)
    set(CMAKE_CXX_STANDARD 17)
    set(CMAKE_CXX_STANDARD_REQUIRED ON)
    set(CMAKE_CXX_COMPILER "clang++-14")
else()
    message(FATAL_ERROR_ERROR "Compiler does not support C++17.")
endif()

option(CUSTOM_FONT_SUPPORTED "Install and Uninstall fonts locally" OFF)
option(PNG_SUPPORTED         "Export design as PNG"                OFF)
option(JPG_SUPPORTED         "Export design as JPG"                OFF)
option(BUILD_TESTS           "Compile and run tests"               OFF)

set(ASSETS_DIR ${CMAKE_CURRENT_LIST_DIR}/assets)
##################################################
# Setup the default font to be used in the project
##################################################
set(PROJECT_DEFAULT_FONT "/default_font/times-new-roman.ttf")
#######################################################
# Setup directory for installation of project resources
#######################################################
include(GNUInstallDirs)
set(RESOURCE_INSTALL_DIR ${CMAKE_INSTALL_LOCALSTATEDIR}/${APP_NAME})
set(ASSETS_INSTALL_DIR ${RESOURCE_INSTALL_DIR}/assets)
set(OPERATING_SYSTEM "${CMAKE_SYSTEM_NAME}")

if(CMAKE_SYSTEM_NAME MATCHES Linux)
    set(IS_LINUX ON)
    include(CheckIncludeFiles)
    check_include_files("sys/types.h;sys/stat.h;unistd.h" HAVE_SYS_STAT_H)
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

find_package(Fontconfig)
if(NOT Fontconfig_FOUND)
    message(FATAL_ERROR "Fontconfig is required")
endif()

find_package(Freetype)
if(NOT Freetype_FOUND)
    message(FATAL_ERROR "Freetype is required")
endif()

find_package(PNG)
find_package(JPEG)

if(NOT JPEG_FOUND)
    message(WARNING "jpeglib not found: Features requiring jpeg are disabled.")
else()
    set(JPG_SUPPORTED ON)
endif()

if(NOT PNG_FOUND)
    message(WARNING "libpng++ not found: Features requiring png are disabled.")
else()
    set(PNG_SUPPORTED ON)
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

add_executable(${APP_NAME} src/copywrite.cpp include/copywrite.hpp ${SOURCE_FILES} ${INCLUDE_FILES})
target_include_directories(${APP_NAME} PRIVATE include ${INCLUDE_PATHS})
target_compile_definitions(${APP_NAME} PRIVATE
                           PROJECT_DEFAULT_FONT="${ASSETS_DIR}${PROJECT_DEFAULT_FONT}"
                           RESOURCE_DIR="${CMAKE_BINARY_DIR}/")

if(CUSTOM_FONT_SUPPORTED)
    find_package(Zip QUIET)
    if(NOT Zip_FOUND)
        message(FATAL_ERROR "libzip is required. You can turn off option.")
    endif()
endif()

set(BIN_MODIFIER patch)
add_library(${BIN_MODIFIER} STATIC ${CMAKE_SOURCE_DIR}/src/injection.cpp)
target_compile_definitions(${BIN_MODIFIER} PRIVATE
        PROJECT_DEFAULT_FONT="/${ASSETS_INSTALL_DIR}${PROJECT_DEFAULT_FONT}"
        RESOURCE_DIR="/${ASSETS_INSTALL_DIR}/")

configure_file(patch.cmake.in patch.cmake)
configure_file(config.h.in config.h)
target_compile_features(${APP_NAME} PRIVATE cxx_std_17 cxx_constexpr cxx_decltype cxx_lambdas cxx_variable_templates)
target_link_libraries(${APP_NAME} PUBLIC Freetype::Freetype
                                         Fontconfig::Fontconfig
                                         $<$<BOOL:JPEG_FOUND>:JPEG::JPEG>
                                         $<$<BOOL:PNG_FOUND>:PNG::PNG>
                                         $<$<BOOL:Zip_FOUND>:Zip::Zip>)
target_include_directories(${APP_NAME} PUBLIC ${CMAKE_CURRENT_BINARY_DIR})

if($<CONFIG:Debug> AND BUILD_TESTS)
    add_subdirectory(tests)
endif()

install(TARGETS ${APP_NAME} CONFIGURATIONS Release RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
install(DIRECTORY ${ASSETS_DIR} DESTINATION ${ASSETS_INSTALL_DIR} PATTERN docs EXCLUDE)

add_subdirectory(packaging)

print_configuration()