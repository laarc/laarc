cmake_minimum_required(VERSION 3.10)
project(bcrypt)

set(CMAKE_CXX_STANDARD 14)

add_library(bcrypt SHARED
        bcrypt.cc blowfish.cc)
