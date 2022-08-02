cmake_minimum_required(VERSION 3.10)
project(copywrite)

option(DEBUG ON)

set(CMAKE_CXX_STANDARD 17)
# set(CMAKE_CXX_FLAGS -fcoroutines)
if(CMAKE_BUILD_TYPE MATCHES "Debug")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -ggdb -fsanitize=address")
elseif(CMAKE_BUILD_TYPE MATCHES "Release")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O2")
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
if(NOT JPG_LIB_PATH OR NOT PNG_LIB_PATH)
    message( WARNING "Features requiring png or jpeg are going to be disabled.")
    if(JPG_LIB_PATH)
        set( PNG_LIB_PATH "")
    else()
        set( JPG_LIB_PATH "")
    endif()
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
                                geo_vector.cpp geo_vector.hpp composition_defs.hpp)

target_include_directories( ${PROJECT_NAME} PRIVATE ${FONTCONFIG_INCLUDE_PATH}
                            ${FREETYPE_INCLUDE_PATH} ${PNG_INCLUDE_PATH} ${JPG_INCLUDE_PATH})
target_link_libraries( ${PROJECT_NAME} PUBLIC ${FONTCONFIG_LIB_PATH} ${FREETYPE_LIB_PATH}
                       ${PNG_LIB_PATH} ${JPG_LIB_PATH})