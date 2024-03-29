###########################
# Print build configuration
###########################
macro( print_configuration)
    message("\n======== Build Configuration ========\n")
    message(STATUS "CUSTOM_FONT_SUPPORTED        = " ${CUSTOM_FONT_SUPPORTED})
    message(STATUS "PNG_SUPPORTED                = "         ${PNG_SUPPORTED})
    message(STATUS "JPG_SUPPORTED                = "         ${JPG_SUPPORTED})
    message(STATUS "BUILD_TESTS                  = "           ${BUILD_TESTS})
    message("\n=============== Done ================\n")
endmacro()

################################################################################
# For all subdirectory in the specified directory, search for all CMakeLists.txt
# and include them.
################################################################################
macro( include_subdirs PARENT)
    file(GLOB SUBDIRS RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "${PARENT}/*")
    foreach(SUBDIR ${SUBDIRS})
        if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/${SUBDIR}/CMakeLists.txt")
            add_subdirectory("${CMAKE_CURRENT_SOURCE_DIR}/${SUBDIR}")
        endif()
    endforeach()
endmacro()

macro( load_matching_include_files INCLUDE_FILES_PATH INCLUDE_ROOT SOURCE_FILES)
    foreach(SOURCE ${${SOURCE_FILES}})
        get_filename_component( FILENAME ${SOURCE} NAME_WE)
        file(GLOB SELECTION "${CMAKE_SOURCE_DIR}/include/${INCLUDE_ROOT}/${FILENAME}.*")
        list(APPEND ${INCLUDE_FILES_PATH} ${SELECTION})
    endforeach()
endmacro()

##########################################################################################################
# Group all files related to this current directory into SOURCE_FILES, INCLUDE_FILES and INCLUDE_PATHS.
##########################################################################################################
macro( autoload)
    include_guard()
    get_filename_component(MODULE_NAME ${CMAKE_CURRENT_SOURCE_DIR} NAME)
    file(GLOB PROJECT_SOURCE_FILES "*.cpp")
    file(GLOB EXTRA_DEFS "${CMAKE_SOURCE_DIR}/include/*.h*")
    load_matching_include_files( PROJECT_INCLUDE_FILES ${MODULE_NAME} PROJECT_SOURCE_FILES)
    set(PROJECT_INCLUDE_PATHS ${CMAKE_SOURCE_DIR}/include/${MODULE_NAME})
    set(SOURCE_FILES  ${SOURCE_FILES}  ${PROJECT_SOURCE_FILES} PARENT_SCOPE)
    set(INCLUDE_FILES ${INCLUDE_FILES} ${PROJECT_INCLUDE_FILES} PARENT_SCOPE)
    set(INCLUDE_PATHS ${INCLUDE_PATH} ${PROJECT_INCLUDE_PATHS}  PARENT_SCOPE)
endmacro()

function(get_all_targets VAR)
    set(TARGETS)
    get_all_targets_recursive(TARGETS ${CMAKE_CURRENT_SOURCE_DIR})
    set(${VAR} ${TARGETS} PARENT_SCOPE)
endfunction()

macro(get_all_targets_recursive TARGETS DIR)
    get_property(SUBDIRS DIRECTORY ${DIR} PROPERTY SUBDIRECTORIES)
    foreach(SUBDIR ${SUBDIRS})
        get_all_targets_recursive(${TARGETS} ${SUBDIR})
    endforeach()

    get_property(CURRENT_TARGETS DIRECTORY ${DIR} PROPERTY BUILDSYSTEM_TARGETS)
    list(APPEND ${TARGETS} ${CURRENT_TARGETS})
endmacro()

macro(filter_directories DEST DIR_LISTING)
    set(DIRS "")
    foreach(DIR ${${DIR_LISTING}})
        if(IS_DIRECTORY ${DIR})
            list(APPEND DIRS ${DIR})
        endif()
    endforeach()
    set(${DEST} ${DIRS})
endmacro()