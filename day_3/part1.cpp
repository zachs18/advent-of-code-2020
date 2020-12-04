#include <cstdlib>
#include <iostream>
#include <iterator>
#include <vector>
#include <string>


int main(int argc, char **argv) {
	std::vector<std::string> map{
		std::istream_iterator<std::string>{std::cin},
		std::istream_iterator<std::string>{}
	};

	const auto map_width = map[0].size();
	auto tree_count = 0ull;

	const auto dx = +3;
	auto x = 0ull;

	for (auto &line : map) {
		tree_count += (line[x] == '#');
		x = (x + dx) % map_width;
		//std::cout << line << '\n';
	}

	std::cout << tree_count << '\n';

	return EXIT_SUCCESS;
//	return EXIT_FAILURE;
}
