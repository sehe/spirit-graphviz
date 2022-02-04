cmake_minimum_required(VERSION 3.5)
project(spirit-graphviz)

ADD_EXECUTABLE(spirit-graphviz spirit-graphviz.cpp)

SET(CMAKE_EXPORT_COMPILE_COMMANDS ON)
SET(CMAKE_CXX_COMPILER g++-10)

SET(BOOST_DIR /home/sehe/custom/boost_1_78_0)

SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} \
    -isystem ${BOOST_DIR}/ \
    -Wall -Wextra -pedantic \
    -std=c++2a \
    -g -O3 -pthread -march=native")

    #-fsanitize=address,undefined \
