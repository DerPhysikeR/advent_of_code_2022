from typing import Iterable


def parse_input(input_file: str):
    with open(input_file) as stream:
        lines = stream.read().strip().split("\n")
    flow_dict: dict = {}
    connection_dict: dict = {}
    for line in lines:
        if "tunnels" in line:
            split_str = "; tunnels lead to valves "
        else:
            split_str = "; tunnel leads to valve "
        valve_str, _, connection_str = line.partition(split_str)
        _, name, _, _, rate_eq_str = valve_str.split(" ")
        _, _, rate_str = rate_eq_str.partition("=")
        flow_dict[name] = int(rate_str)
        connections = connection_str.split(", ")
        connection_dict[name] = connections
    return flow_dict, connection_dict


def get_min_value(d: dict[str, int]) -> tuple[int, str]:
    minvalve, minpriority = next(iter(d.items()))
    for valve, priority in d.items():
        if priority < minpriority:
            minpriority = priority
            minvalve = valve
    del d[minvalve]
    return minpriority, minvalve


def find_shortest_paths(
    connections: dict[str, list[str]], from_: str, tos: Iterable[str]
):
    to_check = {valve: 10_000 for valve in connections}
    to_check[from_] = 0
    distances = {}
    while to_check:
        distance, valve = get_min_value(to_check)
        distances[valve] = distance
        for neighbor in connections[valve]:
            if neighbor not in to_check:
                continue
            if (new_distance := distance + 1) < to_check[neighbor]:
                to_check[neighbor] = new_distance
    return {t: distances[t] for t in tos}


def simplify(flow_dict, connection_dict) -> dict[str, dict[str, int]]:
    important_valves = set(["AA"] + [v for v, f in flow_dict.items() if f > 0])
    distance_dict = {
        valve: find_shortest_paths(connection_dict, valve, important_valves)
        for valve in important_valves
    }
    for key, value in distance_dict.items():
        del value[key]
    return distance_dict


def value_path(
    path: list[str],
    distance_dict: dict[str, dict[str, int]],
    flow_dict: dict[str, int],
    time: int,
) -> int:
    elapsed_time = 0
    iterator = iter(path)
    released_pressure = 0
    from_valve = next(iterator)
    for to_valve in iterator:
        distance = distance_dict[from_valve][to_valve]
        elapsed_time += distance + 1
        if elapsed_time > time:
            break
        released_pressure += (time - elapsed_time) * flow_dict[to_valve]
        from_valve = to_valve
    return released_pressure


def maximize_flow(
    distance_dict: dict[str, dict[str, int]],
    flow_dict: dict[str, int],
    path=["AA"],
    all_paths=[],
    time=30,
):
    paths = sorted(
        [
            path + [valve]
            for valve in distance_dict[path[-1]]
            if valve not in path and path_duration(path + [valve], distance_dict) < time
        ],
        key=lambda p: -value_path(p, distance_dict, flow_dict, time),
    )
    if not paths:
        value = value_path(path, distance_dict, flow_dict, time=time)
        all_paths.append((tuple(path), value))
        return value
    return max(
        [
            maximize_flow(distance_dict, flow_dict, path, all_paths, time)
            for path in paths
        ]
    )


def path_duration(path, distance_dict):
    duration = 0
    iterator = iter(path)
    previous_valve = next(iterator)
    for valve in iterator:
        duration += distance_dict[previous_valve][valve] + 1
        previous_valve = valve
    return duration


def find_best_path_combination(paths: list[tuple[tuple[str, ...], int]]) -> int:
    compare_set = set(["AA"])
    for start_idx, (path1, value1) in enumerate(paths, 1):
        for path2, value2 in paths[start_idx:]:
            if set(path1).intersection(set(path2)) == compare_set:
                return value1 + value2
    return 0


if __name__ == "__main__":
    flow_dict, connection_dict = parse_input("input.txt")
    distances = simplify(flow_dict, connection_dict)
    flow = maximize_flow(distances, flow_dict)
    print(flow)

    all_paths = []
    maximize_flow(distances, flow_dict, all_paths=all_paths, time=26)
    all_paths.sort(key=lambda x: x[1], reverse=True)
    print(find_best_path_combination(all_paths))
