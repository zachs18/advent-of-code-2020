#include <cstdlib>
#include <iostream>
#include <iterator>
#include <vector>
#include <algorithm>


int main(int argc, char **argv) {
	std::vector<int> numbers{std::istream_iterator<int>{std::cin}, std::istream_iterator<int>{}};

	if (numbers.size() < 2) return EXIT_FAILURE;

	std::sort(numbers.begin(), numbers.end());

	auto lower = numbers.begin();
	auto higher = numbers.end()-1;
	while (lower < higher) {
		auto sum = *lower + *higher;
		if (sum == 2020) {
			std::cout << *lower << " + " << *higher << " = " << sum << '\n';
			std::cout << *lower << " * " << *higher << " = " << (*lower * *higher) << '\n';
			return EXIT_SUCCESS;
		} else if (sum > 2020) {
			--higher;
		} else {
			++lower;
		}
	}
	return EXIT_FAILURE;
}
