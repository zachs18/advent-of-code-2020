#include <cstdlib>
#include <iostream>
#include <set>
#include <ranges>
#include <iterator>
#include <algorithm>

unsigned pass_to_number(std::string const &pass) {
	unsigned num = 0;
	for (auto c : pass) {
		num *= 2;
		if (c == 'B' || c == 'R') ++num;
	}
	return num;
}

int main(int argc, char **argv) {
	std::set<unsigned> passes;
	std::ranges::copy(
		std::ranges::subrange{
			std::istream_iterator<std::string>{std::cin},
			std::istream_iterator<std::string>{}
		} | std::views::transform(pass_to_number),
		std::inserter(passes, passes.end())
	);

	std::cout << "Part 1: " << *--passes.end() << '\n';

	for (auto i = *passes.begin(); i < (1 << 10); ++i) {
		if (passes.find(i) == passes.end()) {
			std::cout << "Part 2: " << i << '\n';
			return 0;
		}
	}

	return EXIT_SUCCESS;
//	return EXIT_FAILURE;
}
