cmake_minimum_required(VERSION 3.5)

ADD_EXECUTABLE(sotest test.cpp)
#ADD_LIBRARY(sotest SHARED test.cpp)
#ADD_EXECUTABLE(test1 test1.cpp)
#ADD_EXECUTABLE(qi qi.cpp)
#SET(CMAKE_CXX_COMPILER g++-7)
SET(CMAKE_CXX_COMPILER clang++-3.8)
#SET(CMAKE_CXX_COMPILER g++-5)

SET(BOOST_DIR /home/sehe/custom/boost_1_65_0)

#ADD_EXECUTABLE(gen gen.cpp)
#TARGET_COMPILE_DEFINITIONS(gen PUBLIC "GENERATE_RANDOM_GRAPHS=1")

SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} \
    -I /usr/include/python2.7 \
    -isystem ${BOOST_DIR}/ \
    -Wl,-rpath,${BOOST_DIR}/stage/lib \
    -L ${BOOST_DIR}/stage/lib \
    -Wall -Wextra -pedantic \
    -std=c++11 \
    -ftemplate-depth=19000 \
    -isystem /usr/include/mpich \
    -fsanitize=address,undefined \
    -g -Os -pthread -march=native")
    #-isystem /home/sehe/custom/nonius/include \
    #-I /home/sehe/custom/spirit/include \
    #-isystem /home/sehe/custom/beast/include \
    #-isystem /home/sehe/custom/uri/include \
TARGET_LINK_LIBRARIES(sotest
    sfml-system sfml-window sfml-graphics
    boost_chrono
    #boost_context
    #boost_coroutine
    boost_locale
    boost_random
    boost_filesystem
    boost_iostreams
    boost_program_options
    boost_regex
    boost_serialization
    boost_system
    bz2
    boost_thread
    #curses
    #form
    ##gmp
    #menu
    #ncurses
    #ncurses++
    #panel
    ##png
    #readline
    rt
    ssl
    crypto
    ##tiff
    #-lstdc++fs
    ##avformat
    ##avcodec
    #avutil
    #swscale
    #boost_mpi
    #boost_graph_parallel
    boost_graph
    #mpich
    #mpi
    #mpi_cxx
    pugixml
    python2.7
    boost_python
    #/home/sehe/custom/uri/build/src/libnetwork-uri.a
    icudata icui18n icuio icule icuuc
)
#ADD_EXECUTABLE(bmi test1.cpp)
#TARGET_LINK_LIBRARIES(bmi rt)
