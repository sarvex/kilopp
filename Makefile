all: kilo

kilo: kilo.cpp
	$(CXX) -o kilo -g kilo.cpp -Wall -W -pedantic -std=c++17

clean:
	rm kilo
