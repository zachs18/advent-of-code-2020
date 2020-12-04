#include <cstdlib>
#include <iostream>
#include <iterator>
#include <ranges>
#include <string>
#include <sstream>

struct password {
	unsigned index1, index2; // 1-indexed
	char letter;
	std::string password;

	bool valid(void) const {
		return (password.size() >= index1 && password[index1-1] == letter) ^
		       (password.size() >= index2 && password[index2-1] == letter);
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
	str >> pw.index1 >> '-' >> pw.index2 >> pw.letter >> ':' >> std::ws;
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
