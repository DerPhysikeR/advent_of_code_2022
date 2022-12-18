from __future__ import annotations

from typing import NamedTuple


class Cube(NamedTuple):
    x: int
    y: int
    z: int

    @classmethod
    def from_string(cls, s: str) -> Cube:
        coords = [int(part) for part in s.split(",")]
        return cls(*coords)

    @property
    def neighbors(self) -> Droplet:
        return set(
            [
                Cube(self.x - 1, self.y, self.z),
                Cube(self.x + 1, self.y, self.z),
                Cube(self.x, self.y - 1, self.z),
                Cube(self.x, self.y + 1, self.z),
                Cube(self.x, self.y, self.z - 1),
                Cube(self.x, self.y, self.z + 1),
            ]
        )


Droplet = set[Cube]


def calc_surface_area(droplet: Droplet) -> int:
    surface_area = 0
    for cube in droplet:
        surface_area += 6 - len(cube.neighbors.intersection(droplet))
    return surface_area


def parse_input(input_file: str) -> Droplet:
    with open(input_file) as stream:
        return set(Cube.from_string(line) for line in stream.read().strip().split("\n"))


if __name__ == "__main__":
    droplet = parse_input("input.txt")
    print(calc_surface_area(droplet))
