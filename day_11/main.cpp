#include <cstdlib>
#include <iostream>
#include <iterator>
#include <vector>
#include <string>

using Board = std::vector<std::string>;

std::ostream &operator<<(std::ostream &str, Board const &board) {
	for (auto &line : board) str << line << '\n';
	return str;
}

unsigned long count_adj(Board const &board, char c, unsigned long y, unsigned long x) {
	signed long dy1, dy2, dx1, dx2;
	if (y == 0) { dy1 = 0; dy2 = 1; }
	else if (y == board.size()-1) { dy1 = -1; dy2 = 0; }
	else { dy1 = -1; dy2 = +1; }

	if (x == 0) { dx1 = 0; dx2 = 1; }
	else if (x == board[0].size()-1) { dx1 = -1; dx2 = 0; }
	else { dx1 = -1; dx2 = +1; }

	unsigned long count = 0;
	for (signed long dy = dy1; dy <= dy2; ++dy) {
		for (signed long dx = dx1; dx <= dx2; ++dx) {
			if ((dx != 0 || dy != 0) && board[y+dy][x+dx] == c) ++count;
		}
	}
	return count;
}

Board iterate_1(Board const &board) {
	auto board_ = board;
	for (unsigned long y = 0; y < board.size(); ++y) {
		for (unsigned long x = 0; x < board[y].size(); ++x) {
			if (board[y][x] == 'L' && count_adj(board, '#', y, x) == 0) {
				board_[y][x] = '#';
			} else if (board[y][x] == '#' && count_adj(board, '#', y, x) >= 4) {
				board_[y][x] = 'L';
			}
		}
	}
	return board_;
}

unsigned long count_visible(Board const &board, char c, unsigned long y, unsigned long x) {
	unsigned long count = 0;
	for (signed long dy = -1; dy <= 1; ++dy) {
		if ((y==0 && dy<0) || (y+dy >= board.size())) continue;
		for (signed long dx = -1; dx <= +1; ++dx) {
			if (dy == 0 && dx == 0) continue;
			if ((x==0 && dx<0) || (x+dx >= board[0].size())) continue;
			for (int s = 1; ; ++s) {
				if (dy < 0 && static_cast<unsigned long>(-(s*dy)) > y) break;
				if (dy > 0 && static_cast<unsigned long>(s*dy) >= board.size()-y) break;
				if (dx < 0 && static_cast<unsigned long>(-(s*dx)) > x) break;
				if (dx > 0 && static_cast<unsigned long>(s*dx) >= board[0].size()-x) break;
				if (board[y+s*dy][x+s*dx] == c) ++count;
				if (board[y+s*dy][x+s*dx] != '.') break;
			}
		}
	}
	return count;
}

Board iterate_2(Board const &board) {
	auto board_ = board;
	for (unsigned long y = 0; y < board.size(); ++y) {
		for (unsigned long x = 0; x < board[y].size(); ++x) {
			if (board[y][x] == 'L' && count_visible(board, '#', y, x) == 0) {
				board_[y][x] = '#';
			} else if (board[y][x] == '#' && count_visible(board, '#', y, x) >= 5) {
				board_[y][x] = 'L';
			}
		}
	}
	return board_;
}


unsigned long part1(Board board) {
	unsigned long iterations = 0;
	while (true) {
//		std::cout << board << '\n';
		Board b = iterate_1(board);
		if (b == board) break;
		++iterations;
		board = std::move(b);
	}
	unsigned long occupied = 0;
	for (auto const &line : board) {
		for (auto c : line) {
			if (c == '#') ++occupied;
		}
	}
	return occupied;
}

unsigned long part2(Board board) {
	unsigned long iterations = 0;
	while (true) {
//		std::cout << board << '\n';
		Board b = iterate_2(board);
		if (b == board) break;
		++iterations;
		board = std::move(b);
	}
	unsigned long occupied = 0;
	for (auto const &line : board) {
		for (auto c : line) {
			if (c == '#') ++occupied;
		}
	}
	return occupied;
}

int main(int argc, char **argv) {
	Board lines{
		std::istream_iterator<std::string>{std::cin},
		std::istream_iterator<std::string>{}
	};

	std::cout << part1(lines) << '\n';
	std::cout << part2(lines) << '\n';

	return EXIT_SUCCESS;
}
