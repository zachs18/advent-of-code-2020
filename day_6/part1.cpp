#include <cstdlib>
#include <iostream>
#include <iterator>
#include <vector>
#include <map>
#include <set>
#include <string>
#include <sstream>
#include <ranges>
#include <functional>

struct declaration_form_group {
	std::vector<std::set<char>> m_yesses;

	auto count(void) const {
		std::set<char> yes;
		for (auto s : m_yesses) {
			yes.merge(std::move(s));
		}
		return yes.size();
	};
};

std::istream &operator>>(std::istream &str, declaration_form_group &form) {
	form.m_yesses.clear();
	std::string line;
	bool any = false;
	while (getline(str, line) && line.size() > 0) {
		any = true;
		form.m_yesses.emplace_back(line.begin(), line.end());
	}
	if (any) str.clear();
	return str;
}

int main(int argc, char **argv) {
	std::ranges::subrange declaration_form_groups{
		std::istream_iterator<declaration_form_group>{std::cin},
		std::istream_iterator<declaration_form_group>{}
	};

	unsigned count = 0;
	for (const auto &group : declaration_form_groups) {
		count += group.count();
	}

	std::cout << count << '\n';

	return EXIT_SUCCESS;
//	return EXIT_FAILURE;
}
