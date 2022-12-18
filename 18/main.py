from __future__ import annotations

from typing import NamedTuple


class Vector(NamedTuple):
    x: float
    y: float
    z: float

    def __add__(self, other):
        return Vector(self.x + other.x, self.y + other.y, self.z + other.z)

    def __sub__(self, other):
        return Vector(self.x - other.x, self.y - other.y, self.z - other.z)

    def __mul__(self, scalar):
        return Vector(self.x * scalar, self.y * scalar, self.z * scalar)

    def __rmul__(self, scalar):
        return self.__mul__(scalar)

    def __div__(self, scalar):
        return Vector(self.x / scalar, self.y / scalar, self.z / scalar)


NORMAL_VECTORS = set(
    [
        Vector(-1, 0, 0),
        Vector(1, 0, 0),
        Vector(0, -1, 0),
        Vector(0, 1, 0),
        Vector(0, 0, -1),
        Vector(0, 0, 1),
    ]
)


class Side(NamedTuple):
    position: Vector
    orientation: Vector

    @property
    def outwards_vectors(self):
        outwards_vectors = NORMAL_VECTORS.difference(
            set([self.orientation, -1 * self.orientation])
        )
        assert len(outwards_vectors) == 4
        return outwards_vectors

    @property
    def concave_neighbors(self) -> Surface:
        return set(
            [
                Side(self.position + 0.5 * self.orientation + 0.5 * ov, -1 * ov)
                for ov in self.outwards_vectors
            ]
        )

    @property
    def flat_neighbors(self) -> Surface:
        return set(
            [Side(self.position + ov, self.orientation) for ov in self.outwards_vectors]
        )

    @property
    def convex_neighbors(self) -> Surface:
        return set(
            [
                Side(self.position - 0.5 * self.orientation + 0.5 * ov, ov)
                for ov in self.outwards_vectors
            ]
        )

    @property
    def neighbors(self) -> Surface:
        return set().union(
            *[self.concave_neighbors, self.flat_neighbors, self.convex_neighbors]
        )

    def get_connected_sides(self, sides: Surface, droplet: Droplet) -> Surface:
        concave_neighbors = self.concave_neighbors.intersection(sides)
        flat_neighbors = self.flat_neighbors.intersection(sides)
        all_convex_neighbors = self.convex_neighbors.intersection(sides)
        convex_neighbors = set([])
        for cv in all_convex_neighbors:
            cutoff_cube_center = self.position + 0.5 * self.orientation + cv.orientation
            cutoff_cube = Cube(*[int(c) for c in cutoff_cube_center])
            if cv in sides and (cutoff_cube not in droplet):
                convex_neighbors.add(cv)
        return set().union(*[concave_neighbors, flat_neighbors, convex_neighbors])


Surface = set[Side]


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

    @property
    def sides(self) -> Surface:
        left = Side(Vector(self.x - 1 / 2, self.y, self.z), Vector(-1, 0, 0))
        right = Side(Vector(self.x + 1 / 2, self.y, self.z), Vector(1, 0, 0))
        back = Side(Vector(self.x, self.y - 1 / 2, self.z), Vector(0, -1, 0))
        front = Side(Vector(self.x, self.y + 1 / 2, self.z), Vector(0, 1, 0))
        bottom = Side(Vector(self.x, self.y, self.z - 1 / 2), Vector(0, 0, -1))
        top = Side(Vector(self.x, self.y, self.z + 1 / 2), Vector(0, 0, 1))
        return set([left, right, top, bottom, front, back])


Droplet = set[Cube]


def calc_surface_area(droplet: Droplet) -> int:
    surface_area = 0
    for cube in droplet:
        surface_area += 6 - len(cube.neighbors.intersection(droplet))
    return surface_area


def extract_surface(start_side: Side, sides: Surface, droplet: Droplet) -> Surface:
    extracted_surface = set([start_side])
    to_check = set([start_side])
    if start_side in sides:
        sides.remove(start_side)
    while to_check:
        new_to_check = set([])
        for side in to_check:
            for neighbor in side.get_connected_sides(sides, droplet):
                sides.remove(neighbor)
                extracted_surface.add(neighbor)
                new_to_check.add(neighbor)
        to_check = new_to_check
    return extracted_surface


def group_sides(sides: Surface, droplet: Droplet) -> list[Surface]:
    groups = []
    sides_to_check = sides.copy()
    while sides_to_check:
        groups.append(extract_surface(sides_to_check.pop(), sides_to_check, droplet))
    return groups


def calc_real_surface_area(droplet: Droplet) -> int:
    sides = set().union(*(cube.sides for cube in droplet))
    unpaired_sides = set()
    for side in sides:
        mirrored = Side(side.position, -1 * side.orientation)
        if mirrored not in sides:
            unpaired_sides.add(side)
    surfaces = group_sides(unpaired_sides, droplet)
    return max(len(s) for s in surfaces)


def parse_input(input_file: str) -> Droplet:
    with open(input_file) as stream:
        return set(Cube.from_string(line) for line in stream.read().strip().split("\n"))


if __name__ == "__main__":
    droplet = parse_input("input.txt")
    print(calc_surface_area(droplet))
    print(calc_real_surface_area(droplet))
