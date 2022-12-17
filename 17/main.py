from __future__ import annotations

import time
from itertools import cycle, islice
from typing import NamedTuple


class C(NamedTuple):
    x: int
    y: int

    def __add__(self, other: C):
        return C(self.x + other.x, self.y + other.y)

    def dist(self, other: C):
        return abs(self.x - other.x) + abs(self.y - other.y)


Rock = set[C]


def move(rock: Rock, by: C) -> Rock:
    return set(r + by for r in rock)


def maxx(rock: Rock) -> int:
    return max(r.x for r in rock)


def minx(rock: Rock) -> int:
    return min(r.x for r in rock)


def maxy(rock: Rock) -> int:
    return max(r.y for r in rock)


def miny(rock: Rock) -> int:
    return min(r.y for r in rock)


def move_to_side(rock: Rock, tower: Rock, dir: str, width: int = 7) -> Rock:
    if dir == ">":
        new_rock = move(rock, C(1, 0))
        if maxx(new_rock) >= width:
            return rock
        if tower.intersection(new_rock):
            return rock
        return new_rock
    if dir == "<":
        new_rock = move(rock, C(-1, 0))
        if minx(new_rock) < 0:
            return rock
        if tower.intersection(new_rock):
            return rock
        return new_rock
    raise ValueError(f"invalid direction '{dir}'")


def move_down(rock: Rock, tower: Rock) -> Rock:
    new_rock = move(rock, C(0, -1))
    if miny(new_rock) <= 0:
        return rock
    if tower.intersection(new_rock):
        return rock
    return new_rock


def print_game(tower: Rock, rock: Rock):
    top = maxy(rock)
    for y in range(top, 0, -1):
        row = ["|"]
        for x in range(7):
            coord = C(x, y)
            if coord in rock:
                row.append("@")
                continue
            if coord in tower:
                row.append("#")
                continue
            row.append(".")
        print("".join(row + ["|"]))
    print("+-------+")


if __name__ == "__main__":
    with open("input.txt") as stream:
        jets = stream.read().strip()

    rocks = [
        set([C(0, 0), C(1, 0), C(2, 0), C(3, 0)]),
        set([C(1, 0), C(0, 1), C(1, 1), C(2, 1), C(1, 2)]),
        set([C(0, 0), C(1, 0), C(2, 0), C(2, 1), C(2, 2)]),
        set([C(0, 0), C(0, 1), C(0, 2), C(0, 3)]),
        set([C(0, 0), C(1, 0), C(0, 1), C(1, 1)]),
    ]

    directions = cycle(jets)
    top = 0
    tower = set()
    for rock in islice(cycle(rocks), 2022):
        rock = move(rock, C(2, top + 4))
        old_rock = set()
        while old_rock != rock:
            # print_game(tower, rock)
            rock = move_to_side(rock, tower, next(directions))
            old_rock = rock
            rock = move_down(rock, tower)
            # time.sleep(0.3)
        tower = tower.union(rock)
        top = maxy(tower)
    print(top)
