#include <cstdlib>
#include <iostream>
#include <iterator>
#include <vector>
#include <map>
#include <string>
#include <sstream>
#include <ranges>
#include <functional>

class passport {
	static const std::vector<std::string> required_fields;
	static const std::vector<std::string> optional_fields;
public:
	std::map<std::string, std::string> m_fields;

	bool valid(void) const {
		if (m_fields.size() < required_fields.size()) return false;
		for (const auto &field : required_fields) {
			if (m_fields.find(field) == m_fields.end()) return false;
		}
		unsigned optional_count = 0;
		for (const auto &field : optional_fields) {
			if (m_fields.find(field) != m_fields.end()) ++optional_count;
		}
		return m_fields.size() == required_fields.size() + optional_count;
	}
};

const std::vector<std::string> passport::required_fields{
	"byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"
};
const std::vector<std::string> passport::optional_fields{
	"cid"
};

std::istream &operator>>(std::istream &str, passport &ppt) {
	ppt.m_fields.clear();
	std::string line;
	bool any = false;
	while (getline(str, line) && line.size() > 0) {
		any = true;
		std::istringstream iss{line};
		std::string kv_pair;
		while (iss >> kv_pair) {
			auto colon_index = kv_pair.find(':');
			ppt.m_fields[kv_pair.substr(0, colon_index)] = kv_pair.substr(1+colon_index);
		}
	}
	if (any) str.clear();
	return str;
}

int main(int argc, char **argv) {
	std::ranges::subrange passports{
		std::istream_iterator<passport>{std::cin},
		std::istream_iterator<passport>{}
	};

	unsigned valid_count = 0;
	for (const auto &ppt : passports) {
		if (ppt.valid()) ++valid_count;
	}

	std::cout << valid_count << '\n';

	return EXIT_SUCCESS;
//	return EXIT_FAILURE;
}
