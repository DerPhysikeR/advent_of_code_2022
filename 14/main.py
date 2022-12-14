from itertools import pairwise
from typing import NamedTuple


class Coords(NamedTuple):
    row: int
    col: int

    @classmethod
    def from_string(cls, string):
        col, row = [int(v) for v in string.split(",")]
        return cls(row, col)


def path_to_rocks(path: list[Coords]) -> set[Coords]:
    rocks = set()
    for (start, end) in pairwise(path):
        if start.row == end.row:
            cols = [start.col, end.col]
            for col in range(min(cols), max(cols) + 1):
                rocks.add(Coords(start.row, col))
    for (start, end) in pairwise(path):
        if start.col == end.col:
            rows = [start.row, end.row]
            for row in range(min(rows), max(rows) + 1):
                rocks.add(Coords(row, start.col))
    return rocks


def parse_rock_paths(filename):
    with open(filename) as stream:
        lines = stream.read().strip().split("\n")
    paths = [[Coords.from_string(s) for s in line.split(" -> ")] for line in lines]
    return paths


def game_to_str(rocks: set[Coords], sand: set[Coords] = set()) -> str:
    to_draw = rocks | sand
    rows = [t.row for t in to_draw]
    cols = [t.col for t in to_draw]
    minrc = Coords(min(rows), min(cols))
    maxrc = Coords(max(rows), max(cols))
    lines = []
    for row in range(minrc.row, maxrc.row + 1):
        lines.append([])
        for col in range(minrc.col, maxrc.col + 1):
            if Coords(row, col) in rocks:
                lines[-1].append("#")
            elif Coords(row, col) in sand:
                lines[-1].append("O")
            else:
                lines[-1].append(".")
    return "\n".join("".join(line) for line in lines)


def get_fall_targets(coords: Coords):
    row, col = coords
    return [Coords(row + 1, col), Coords(row + 1, col - 1), Coords(row + 1, col + 1)]


def add_sand(rocks: set[Coords], sand: set[Coords]):
    if Coords(0, 500) in sand:
        return sand
    bottom = max(r.row for r in rocks)
    block = Coords(0, 500)
    while block.row < bottom:
        for target in get_fall_targets(block):
            if target not in rocks | sand:
                block = target
                break
        else:
            return sand | {block}
    else:
        return sand


def fill_with_sand(rocks: set[Coords], sand_adder):
    sand = set()
    lenbefore = -1
    while len(sand) > lenbefore:
        lenbefore = len(sand)
        sand = sand_adder(rocks, sand)
    return sand


def add_sand_with_bottom(rocks: set[Coords], sand: set[Coords]):
    if Coords(0, 500) in sand:
        return sand
    bottom = max(r.row for r in rocks) + 2
    block = Coords(0, 500)
    while True:
        for target in get_fall_targets(block):
            if target.row < bottom and target not in rocks and target not in sand:
                block = target
                break
        else:
            sand.add(block)
            return sand


if __name__ == "__main__":
    rock_paths = parse_rock_paths("input.txt")
    rocks = set().union(*[path_to_rocks(p) for p in rock_paths])
    sand = fill_with_sand(rocks, sand_adder=add_sand)
    print(game_to_str(rocks, sand))
    print(len(sand))
    sand = fill_with_sand(rocks, sand_adder=add_sand_with_bottom)
    print(game_to_str(rocks, sand))
    print(len(sand))
