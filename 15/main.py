from typing import NamedTuple


class C(NamedTuple):
    x: int
    y: int


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


def dist(c1, c2):
    return abs(c1.x - c2.x) + abs(c1.y - c2.y)


def get_neighbors(coords: C) -> set[C]:
    x, y = coords
    return set([C(x + 1, y), C(x, y + 1), C(x - 1, y), C(x, y - 1)])


def generate_circumferences(coords: C, distance: int) -> set[C]:
    cleared = [set(), set([coords])]
    for _ in range(distance):
        neighbors = set()
        for c in cleared[-1]:
            filtered_neighbors = get_neighbors(c) - cleared[-2]
            for fn in filtered_neighbors:
                neighbors.add(fn)
        cleared.append(neighbors)
    return set().union(*cleared)


def calc_cleared_c(sensors_beacons: dict[C, C]) -> dict[C, set[C]]:
    cleared: dict[C, set[C]] = {}
    for sensor, beacon in sensors_beacons.items():
        distance: int = dist(sensor, beacon)
        cleared[sensor] = generate_circumferences(sensor, distance)
    return cleared


def count_cleared_c_in_line(lineidx: int, cleared_c: set[C]):
    return sum(c.y == lineidx for c in cleared_c)


def coord_is_cleared(coord: C, sensors_beacons: dict[C, C]) -> bool:
    for sensor, beacon in sensors_beacons.items():
        if coord == sensor or coord == beacon:
            return False
        if dist(sensor, coord) <= dist(sensor, beacon):
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


def calc_x_bounds(sensors_beacons: dict[C, C]) -> C:
    sb = set(s for s in sensors_beacons.keys()).union(
        b for b in sensors_beacons.values()
    )
    minx = min(c.x for c in sb)
    maxx = max(c.x for c in sb)
    return C(minx, maxx)


def calc_x_bounds_for_sensor_beacon(y: int, sensor: C, beacon: C) -> tuple[int, int]:
    distance = dist(sensor, beacon)
    dx = distance - abs(y - sensor.y)
    if dx > 0:
        return sensor.x - dx, sensor.x + dx
    return 0, 0


if __name__ == "__main__":
    input_file = "input.txt"
    sensors_beacons = parse_input(input_file)
    line_to_check = {"test_input.txt": 10, "input.txt": 2_000_000}[input_file]
    print(cleared_in_line(line_to_check, sensors_beacons))
