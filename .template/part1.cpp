#include <cstdlib>
#include <iostream>
#include <iterator>
#include <vector>


int main(int argc, char **argv) {
	std::vector<int> numbers{std::istream_iterator<int>{std::cin}, std::istream_iterator<int>{}};

	if (numbers.size() < 2) return EXIT_FAILURE;

	return EXIT_SUCCESS;
	return EXIT_FAILURE;
}
