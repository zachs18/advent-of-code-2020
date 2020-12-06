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

template<typename T>
std::set<T> &operator&=(std::set<T> &a, std::set<T> const &b) {
	for (auto const &i : std::set<T>{a}) {
		if (b.find(i) == b.end()) a.erase(i);
	}
	return a;
}

struct declaration_form_group {
	std::vector<std::set<char>> m_yesses;

	auto count(void) const {
		char letters[] = "qwertyuioopasdfghjklzxcvbnm";
		std::set<char> yes{std::begin(letters), std::end(letters)};
		for (auto const &s : m_yesses) {
			yes &= s;
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
