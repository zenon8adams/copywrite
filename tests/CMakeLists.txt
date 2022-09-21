set( CopywriteTests_src
     test_command_line_args.cpp)

include(FetchContent)
FetchContent_Declare(
    CppUTest
    GIT_REPOSITORY https://github.com/cpputest/cpputest.git
    GIT_TAG        latest-passing-build
)
FetchContent_MakeAvailable( CppUTest)

add_executable( Copywrite_Tests ${CopywriteTests_src})

target_link_libraries( Copywrite_Tests PRIVATE CppUTest)