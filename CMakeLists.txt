##
# @file
#
# @brief CMakeList.txt
#
# @author  Wei Tang <gauchyler@uestc.edu.cn>
# @date    2025-03-18
#
# @copyright Copyright (c) 2025.
#            National Key Laboratory of Science and Technology on Communications,
#            University of Electronic Science and Technology of China.
#            All rights reserved.
#
cmake_minimum_required(VERSION 3.15)

project(proxy-cpp17 LANGUAGES CXX)

add_executable(test-proxy test-proxy.cpp)

# C++17 is required.
target_compile_features(test-proxy PUBLIC cxx_std_17)

include(CTest)
enable_testing()

add_test(NAME    test-proxy
         COMMAND test-proxy)

install(TARGETS     test-proxy
        DESTINATION bin)
