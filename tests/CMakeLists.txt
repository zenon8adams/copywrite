set( CopywriteTests_src
     test_command_line_args.cpp)

include(FetchContent)
FetchContent_Declare(
    CppUTest
    GIT_REPOSITORY https://github.com/cpputest/cpputest.git
    GIT_TAG        latest-passing-build
)
#FetchContent_MakeAvailable( CppUTest)

#add_executable( Copywrite_Tests ${CopywriteTests_src})

#target_link_libraries( Copywrite_Tests PRIVATE CppUTest)

#   Test if help is displayed
add_test(NAME ${APP_NAME}_displays_help COMMAND ${APP_NAME} -h)
set_tests_properties(${APP_NAME}_displays_help PROPERTIES WILL_FAIL YES)
#   Test for basic console output
add_test(NAME ${APP_NAME}_basic_command COMMAND ${APP_NAME} "Hello")
#   Test setting of drawing character
add_test(NAME ${APP_NAME}_change_drawing_character COMMAND ${APP_NAME} --cD=\# "Hello")
#   Test linear gradient
add_test(NAME ${APP_NAME}_set_linear_gradient COMMAND ${APP_NAME} --rC=[1..]{Red->Blue+} "Hello World!")
#   Test image file generation
add_test(NAME ${APP_NAME}_generate_image COMMAND ${APP_NAME} --rC=[1..]{Red->Blue+} -iA -o basic_image.png "Hello")
#  Test color-test
add_test(NAME ${APP_NAME}_decodes_color_appropriately COMMAND ${APP_NAME} --cT=\(Lime/Red+/-Magenta/Cyan\))
set_tests_properties(${APP_NAME}_decodes_color_appropriately PROPERTIES
                     PASS_REGULAR_EXPRESSION ".*Background colors:.+White")
set_tests_properties(${APP_NAME}_decodes_color_appropriately PROPERTIES
        PASS_REGULAR_EXPRESSION ".*Foreground colors:.+DarkSlateGray")