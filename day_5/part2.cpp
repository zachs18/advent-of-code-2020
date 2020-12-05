#include <cstdlib>
#include <iostream>
#include <iterator>
#include <vector>
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
	auto passes__ = std::ranges::subrange{
		std::istream_iterator<std::string>{std::cin},
		std::istream_iterator<std::string>{}
	};
	auto passes_ = std::views::transform(passes__, pass_to_number);
	std::set<unsigned> passes;
	std::ranges::copy(
		passes_,
		std::inserter(passes, passes.end())
	);

	for (auto i = *passes.begin(); i < (1 << 10); ++i) {
		if (passes.find(i) == passes.end()) {
			std::cout << i << '\n';
			return 0;
		}
	}

	return EXIT_SUCCESS;
//	return EXIT_FAILURE;
}
