#include <cstdlib>
#include <iostream>
#include <iterator>
#include <ranges>
#include <string>
#include <sstream>

struct password {
	unsigned min, max;
	char letter;
	std::string password;

	bool valid(void) const {
		auto count = 0u;
		for (char c : password) if (c == letter) ++count;
		return count >= min && count <= max;
	}
};

std::istream &operator>>(std::istream &str, const char &&c) {
	char given = 0;
	str >> given;
	if (given != c) {
		str.setstate(str.failbit);
	}
	return str;
}

std::istream &operator>>(std::istream &str, struct password &pw) {
	str >> pw.min >> '-' >> pw.max >> pw.letter >> ':' >> std::ws;
	std::getline(str, pw.password);
	return str;
}

int main(int argc, char **argv) {
	auto count = 0u;
	std::ranges::subrange passwords{
		std::istream_iterator<password>{std::cin},
		std::istream_iterator<password>{}
	};

	for (auto pw : passwords) {
		if (pw.valid()) ++count;
	}

	std::cout << count << '\n';

	return EXIT_SUCCESS;
}
