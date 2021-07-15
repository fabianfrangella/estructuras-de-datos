cmake_minimum_required(VERSION 3.19)
project(main)

set(CMAKE_CXX_STANDARD 14)

add_executable(main main.cpp Tree.cpp Tree.h TreeFunctions.cpp TreeFunctions.h ArrayList.cpp ArrayList.h TreeQueue.cpp TreeQueue.h)