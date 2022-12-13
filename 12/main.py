from typing import NamedTuple


class Coords(NamedTuple):
    row: int
    col: int


class Maze(NamedTuple):
    height_map: dict[Coords, str]
    start: Coords
    end: Coords


def parse_input(filename):
    with open(filename) as stream:
        content = stream.read().split()
    maze = {
        Coords(rowIdx, colIdx): ch
        for rowIdx, row in enumerate(content)
        for colIdx, ch in enumerate(row)
    }
    inverted_maze = {value: key for key, value in maze.items()}
    start = inverted_maze["S"]
    end = inverted_maze["E"]
    maze[start] = "a"
    maze[end] = "z"
    return Maze(maze, start, end)


def popmin(to_check):
    inverted_to_check = {value: key for key, value in to_check.items()}
    inverted_to_check.pop("infinity")
    minimum = min(inverted_to_check)
    coords = inverted_to_check[minimum]
    return coords, to_check.pop(coords)


def get_neighbors(coords):
    row, col = coords
    return set(
        [
            Coords(row, col + 1),
            Coords(row - 1, col),
            Coords(row, col - 1),
            Coords(row + 1, col),
        ]
    )


def reconstruct_path(predecessors, position):
    path = [position]
    while path[-1] in predecessors:
        path.append(predecessors[path[-1]])
    return path[::-1]


def get_valid_neighbors(maze, position):
    height = ord(maze.height_map[position])
    neighbors = get_neighbors(position)
    return set(
        n
        for n in neighbors
        if n in maze.height_map and ord(maze.height_map[n]) <= height + 1
    )


def dijkstra(maze, to_check, predecessors):
    while set(to_check.values()) > set(["infinity"]):
        position, distance = popmin(to_check)
        neighbors = get_valid_neighbors(maze, position).intersection(to_check)
        for neighbor in neighbors:
            if neighbor == maze.end:
                predecessors[maze.end] = position
                return predecessors, reconstruct_path(predecessors, maze.end)
            if to_check[neighbor] == "infinity" or to_check[neighbor] > distance + 1:
                to_check[neighbor] = distance + 1
                predecessors[neighbor] = position
    return "no path found"


def solve_maze(maze):
    to_check: dict[Coords, int | str] = {k: "infinity" for k in maze.height_map}
    to_check[maze.start] = 0
    predecessors: dict[Coords, Coords] = {}
    return dijkstra(maze, to_check, predecessors)


def get_valid_neighbors_2(maze, position):
    height = ord(maze.height_map[position])
    neighbors = get_neighbors(position)
    return set(
        n
        for n in neighbors
        if n in maze.height_map and ord(maze.height_map[n]) >= height - 1
    )


def dijkstra2(maze, to_check, predecessors):
    while len({v: k for k, v in to_check.items()}) > 1:
        position, distance = popmin(to_check)
        neighbors = get_valid_neighbors_2(maze, position).intersection(to_check)
        for neighbor in neighbors:
            if to_check[neighbor] == "infinity" or to_check[neighbor] > distance + 1:
                to_check[neighbor] = distance + 1
                predecessors[neighbor] = position
    return predecessors, reconstruct_path(predecessors, maze.end)


def solve_maze2(maze):
    to_check: dict[Coords, int | str] = {k: "infinity" for k in maze.height_map}
    to_check[maze.start] = 0
    predecessors: dict[Coords, Coords] = {}
    return dijkstra2(maze, to_check, predecessors)


if __name__ == "__main__":
    maze = parse_input("input.txt")
    predecessors, path = solve_maze(maze)
    print(len(path) - 1)

    inverted_maze = Maze(maze.height_map, maze.end, maze.start)
    predecessors2, path2 = solve_maze2(inverted_maze)
    assert path == path2[::-1]

    all_as = set([k for k, v in inverted_maze.height_map.items() if v == "a"])
    path_lenghts = []
    for a in all_as:
        path_length = len(reconstruct_path(predecessors2, a)) - 1
        if path_length > 0:
            path_lenghts.append(path_length)
    print(min(path_lenghts))
