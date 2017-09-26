cmake_minimum_required(VERSION 3.5)

ADD_EXECUTABLE(spirit-graphviz spirit-graphviz.cpp)

SET(CMAKE_CXX_COMPILER g++-7)
#SET(CMAKE_CXX_COMPILER clang++-3.8)
#SET(CMAKE_CXX_COMPILER g++-5)

SET(BOOST_DIR /home/sehe/custom/boost_1_65_0)

SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} \
    -isystem ${BOOST_DIR}/ \
    -Wall -Wextra -pedantic \
    -std=c++1z \
    -fsanitize=address,undefined \
    -g -Og -pthread -march=native")
