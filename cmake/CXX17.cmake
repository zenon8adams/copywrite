# Copyright (c) 2013 Nathan Osman

# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.

## Modified to account for support for structured binding and constexpr if

# Determines whether the compiler supports C++17
macro(check_for_cxx17_compiler _VAR)
    message(STATUS "Checking for C++17 compiler")
    set(${_VAR})
    try_compile(_COMPILER_TEST_RESULT ${PROJECT_BINARY_DIR} ${PROJECT_SOURCE_DIR}/cmake/test_compiler.cpp CMAKE_FLAGS -DCMAKE_CXX_STANDARD=17 -DCMAKE_CXX_STANDARD_REQUIRED=ON)
    if(NOT _COMPILER_TEST_RESULT AND CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
        try_compile(_COMPILER_TEST_RESULT ${PROJECT_BINARY_DIR} ${PROJECT_SOURCE_DIR}/cmake/test_compiler.cpp CMAKE_FLAGS -DCMAKE_CXX_FLAGS="-stdlib=libc++" -DCMAKE_CXX_STANDARD=17 -DCMAKE_CXX_STANDARD_REQUIRED=ON)
        if(_COMPILER_TEST_RESULT)
            set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -stdlib=libc++")
        else()
            message(STATUS "To enable C++17 install libc++ standard library from https://libcxx.llvm.org/")
        endif()
    endif()
    if(_COMPILER_TEST_RESULT AND ((MSVC AND (MSVC17)) OR                                                        # See https://learn.microsoft.com/en-us/cpp/overview/visual-cpp-language-conformance?view=msvc-170
    (CMAKE_COMPILER_IS_GNUCXX AND ${CMAKE_CXX_COMPILER_VERSION} VERSION_GREATER_EQUAL 7.0) OR                   # See https://gcc.gnu.org/projects/cxx-status.html#cxx17
    (CMAKE_CXX_COMPILER_ID STREQUAL "Clang" AND ${CMAKE_CXX_COMPILER_VERSION} VERSION_GREATER_EQUAL 8.0) OR     # See https://clang.llvm.org/cxx_status.html
    (CMAKE_CXX_COMPILER_ID STREQUAL "AppleClang")))
        set(${_VAR} 1)
        message(STATUS "Checking for C++17 compiler - available")
    else()
        message(STATUS "Checking for C++17 compiler - unavailable")
    endif()
endmacro()