.PHONY: default
default: main

CXXFLAGS := -Wall -Werror --std=c++20
CXX = g++-10

CFLAGS := -Wall -Werror --std=c18
CC = g++-10

%: %.cpp
	${CXX} ${CXXFLAGS} $< -o $@
