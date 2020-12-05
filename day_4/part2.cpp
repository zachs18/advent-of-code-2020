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

class passport {
	static const std::map<std::string, std::function<bool(std::string_view)>> required_fields;
	static const std::map<std::string, std::function<bool(std::string_view)>> optional_fields;
public:
	std::map<std::string, std::string> m_fields;

	bool valid(void) const {
		if (m_fields.size() < required_fields.size()) return false;
		for (const auto &[field, validator] : required_fields) {
			if (auto it = m_fields.find(field); it == m_fields.end() || !validator(it->second)) {
				return false;
			}
		}
		unsigned optional_count = 0;
		for (const auto &[field, validator] : optional_fields) {
			if (auto it = m_fields.find(field); it != m_fields.end() && validator(it->second)) {
				++optional_count;
			}
		}
		return m_fields.size() == required_fields.size() + optional_count;
	}
};

std::optional<unsigned> to_number_no_sign(std::string_view val) {
	unsigned v = 0;
	for (auto c : val) {
		if (!std::isdigit(c)) return {};
		v = v * 10 + (c - '0');
	}
	return v;
}

std::optional<int> to_number(std::string_view val) {
	if (val.size() == 0) return {};
	if (val[0] == '-') {
		val.remove_prefix(1);
		if (auto i = to_number_no_sign(val)) return -*i;
	} else {
		if (auto i = to_number_no_sign(val)) return *i;
	}
	return {};
}

const std::map<std::string, std::function<bool(std::string_view)>> passport::required_fields{
	{"byr", [](std::string_view byr) -> bool {
		const auto birth_year = to_number(byr);
		return birth_year.has_value() &&
			birth_year >= 1920 &&
			birth_year <= 2002;
	}},
	{"iyr", [](std::string_view iyr) -> bool {
		const auto issue_year = to_number(iyr);
		return issue_year.has_value() &&
			issue_year >= 2010 &&
			issue_year <= 2020;
	}},
	{"eyr", [](std::string_view eyr) -> bool {
		const auto expiration_year = to_number(eyr);
		return expiration_year.has_value() &&
			expiration_year >= 2020 &&
			expiration_year <= 2030;
	}},
	{"hgt", [](std::string_view hgt) -> bool {
		if (hgt.size() < 2) return false;
		auto units = hgt;
		units.remove_prefix(units.size()-2);
		hgt.remove_suffix(2);
		const auto height = to_number(hgt);
		return height.has_value() && (
			(units == "cm" && height >= 150 && height <= 193) ||
			(units == "in" && height >= 59 && height <= 76)
		);
	}},
	{"hcl", [](std::string_view hcl) -> bool {
		return hcl.size() == 7 && hcl[0] == '#' && std::all_of(
			hcl.begin()+1, hcl.end(),
			[](char c) -> bool {
				return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f');
			}
		);
	}},
	{"ecl", [](std::string_view ecl) -> bool {
		static const std::set<std::string_view> valid_eye_colors{
			"amb", "blu", "brn", "gry", "grn", "hzl", "oth"
		};
		return valid_eye_colors.find(ecl) != valid_eye_colors.end();
	}},
	{"pid", [](std::string_view pid) -> bool {
		if (pid.size() != 9) return false;
		const auto passport_id = to_number_no_sign(pid);
		return passport_id.has_value();
	}},
};
const std::map<std::string, std::function<bool(std::string_view)>> passport::optional_fields{
	{"cid", [](auto) {
		return true;
	}}
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
