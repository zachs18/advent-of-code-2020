#include <cstdlib>
#include <iostream>
#include <iterator>
#include <vector>
#include <string>

unsigned long count_trees(const auto &map, long dx, long dy) {
	const auto map_width = map[0].size();
	auto tree_count = 0ul;

	auto x = 0ul;

	for (unsigned long y = 0; y < map.size(); y += dy) {
		tree_count += (map[y][x] == '#');
		x = (x + dx) % map_width;
		//std::cout << line << '\n';
	}

	return tree_count;
}


int main(int argc, char **argv) {
	std::vector<std::string> map{
		std::istream_iterator<std::string>{std::cin},
		std::istream_iterator<std::string>{}
	};

	auto test = [&map](auto dx, auto dy) {
		auto count = count_trees(map, dx, dy);
		std::cout << "Right " << dx << ", down " << dy << ": " << count << " trees\n";
		return count;
	};

	auto product = test(1,1) * test(3,1) * test(5,1) * test(7,1) * test(1,2);
	std::cout << "Product: " << product << '\n';

	return EXIT_SUCCESS;
//	return EXIT_FAILURE;
}
