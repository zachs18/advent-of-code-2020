#include <cstdlib>
#include <iostream>
#include <iterator>
#include <vector>
#include <set>
#include <map>

unsigned long long part1(std::set<int> const &digraph) {
	if (digraph.size() < 2) return 0;
	std::map<int, unsigned long long> difference_counts;
	++difference_counts[*digraph.begin()]; // start
	++difference_counts[3]; // end
	auto current_1 = digraph.begin();
	auto current_2 = ++digraph.begin();
	while (current_2 != digraph.end()) {
		++difference_counts[*current_2 - *current_1];
		++current_1;
		++current_2;
	}
	return difference_counts[1] * difference_counts[3];
}

unsigned long long part2(std::set<int> const &digraph) {
	if (!digraph.size()) return 1;
	auto current = --digraph.end();
	std::map<int, unsigned long long> partial_solutions;
	partial_solutions[*current] = 1;
	while (current != digraph.begin()) {
		--current;
		auto value = *current;
		partial_solutions[value] =
			partial_solutions[value+1] +
			partial_solutions[value+2] +
			partial_solutions[value+3];
	}
	return partial_solutions[1] + partial_solutions[2] + partial_solutions[3];
}

int main(int argc, char **argv) {
	std::set<int> numbers{std::istream_iterator<int>{std::cin}, std::istream_iterator<int>{}};

	std::cout << part1(numbers) << '\n';
	std::cout << part2(numbers) << '\n';

	return EXIT_SUCCESS;
}
