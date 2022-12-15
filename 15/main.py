from __future__ import annotations

from typing import NamedTuple


class C(NamedTuple):
    x: int
    y: int

    def __add__(self, other):
        return C(self.x + other.x, self.y + other.y)

    def dist(self, other):
        return abs(self.x - other.x) + abs(self.y - other.y)


def parse_coords(string):
    xstr, ystr = string.split(", ")
    x = int(xstr.partition("=")[-1])
    y = int(ystr.partition("=")[-1])
    return C(x, y)


def parse_input(fp):
    with open(fp) as stream:
        lines = stream.read().strip().split("\n")
    parts = [l.split(": closest beacon is at ") for l in lines]
    return {
        parse_coords(p[0].split("Sensor at ")[-1]): parse_coords(p[-1]) for p in parts
    }


def get_neighbors(coords: C) -> set[C]:
    x, y = coords
    return set([C(x + 1, y), C(x, y + 1), C(x - 1, y), C(x, y - 1)])


def coord_is_cleared(coord: C, sensors_beacons: dict[C, C]) -> bool:
    for sensor, beacon in sensors_beacons.items():
        if coord == sensor or coord == beacon:
            return False
        if sensor.dist(coord) <= sensor.dist(beacon):
            return True
    return False


def cleared_in_line(y: int, sensors_beacons: dict[C, C]) -> int:
    bounds = [
        calc_x_bounds_for_sensor_beacon(y, s, b) for s, b in sensors_beacons.items()
    ]
    minx = min(b[0] for b in bounds)
    maxx = max(b[1] for b in bounds)
    return sum(
        coord_is_cleared(C(x, y), sensors_beacons) for x in range(minx, maxx + 1)
    )


def calc_x_bounds_for_sensor_beacon(y: int, sensor: C, beacon: C) -> tuple[int, int]:
    distance = sensor.dist(beacon)
    dx = distance - abs(y - sensor.y)
    if dx > 0:
        return sensor.x - dx, sensor.x + dx
    return 0, 0


def calc_tuning_frequency(coords: C) -> int:
    return coords.x * 4_000_000 + coords.y


def calc_height(sensor, distance_to_beacon, coords):
    sc = sensor.dist(coords)
    if sc > distance_to_beacon:
        return 0
    return distance_to_beacon - sc + 1


def boundary_generator(s, beacon):
    d = s.dist(beacon) + 1
    top = C(s.x, s.y + d)
    right = C(s.x + d, s.y)
    bottom = C(s.x, s.y - d)
    left = C(s.x - d, s.y)
    point = top
    while point != right:
        yield point
        point = point + C(1, -1)
    while point != bottom:
        yield point
        point = point + C(-1, -1)
    while point != left:
        yield point
        point = point + C(-1, 1)
    while point != top:
        yield point
        point = point + C(1, 1)


class Box(NamedTuple):
    minxy: C
    maxxy: C

    def __contains__(self, coords: C):
        inx = self.minxy.x <= coords.x <= self.maxxy.x
        iny = self.minxy.y <= coords.y <= self.maxxy.y
        return inx and iny


def main(input_file: str, line_to_check: int, bb: C):
    sensors_beacons = parse_input(input_file)
    print(cleared_in_line(line_to_check, sensors_beacons))

    sensor_beacon_distances = {s: s.dist(b) for s, b in sensors_beacons.items()}

    def height_fun(c: C):
        if c.x < 0 or c.x > bb.x or c.y < 0 or c.y > bb.y:
            return 4_000_000
        return sum(calc_height(s, d, c) for s, d in sensor_beacon_distances.items())

    b = False
    for sensor, beacon in sensors_beacons.items():
        print(sensor, beacon)
        for point in boundary_generator(sensor, beacon):
            if point not in Box(C(0, 0), bb):
                continue
            if height_fun(point) == 0:
                print(point, height_fun(point), calc_tuning_frequency(point))
                b = True
                break
        if b:
            break


if __name__ == "__main__":
    input_file = "input.txt"
    # input_file = "test_input.txt"
    line_to_check = {"test_input.txt": 10, "input.txt": 2_000_000}[input_file]
    bounding_box = {"test_input.txt": C(20, 20), "input.txt": C(4_000_000, 4_000_000)}[
        input_file
    ]
    main(input_file, line_to_check, bounding_box)
