#include <cstdlib>
#include <iostream>
#include <ranges>
#include <unordered_map>
#include <algorithm>


int main(int argc, char **argv) {
	std::unordered_map<int, unsigned> numbers;
	for (auto i : std::ranges::subrange{std::istream_iterator<int>{std::cin}, std::istream_iterator<int>{}}) {
		++numbers[i];
	}

	if (numbers.size() < 3) {
		unsigned size = 0;
		for (auto [k, v] : numbers) {
			size += v;
		}
		if (size < 3) return EXIT_FAILURE;
	}

	for (auto [i1, count1] : numbers) {
		for (auto [i2, count2] : numbers) {
			if (i1 == i2 && count1 < 2) continue;
			auto i3 = 2020 - i1 - i2;
			auto it3 = numbers.find(i3);
			if (it3 != numbers.end()) {
				auto count3 = it3->second;
				if (
					(i1 != i2 && i3 != i1 && i3 != i2) ||
					(i1 != i2 && (i3 == i1 || i3 == i2) && count3 >= 2) ||
					(i1 == i2 && i3 == i1 && count3 >= 3)
				) {
					std::cout << i1 << " + " << i2 << " + " << i3 << " = " << 2020 << "\n";
					std::cout << i1 << " * " << i2 << " * " << i3 << " = " << (i1 * i2 * i3) << "\n";
					return EXIT_SUCCESS;
				}
			}
		}
	}
	return EXIT_FAILURE;
}
