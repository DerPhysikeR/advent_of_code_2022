from dataclasses import dataclass
from enum import Enum, auto
from typing import NamedTuple


class State(Enum):
    CONTINUE = auto()
    ORDERED = auto()
    UNORDERED = auto()


class Pair(NamedTuple):
    left: list
    right: list

    @classmethod
    def from_string(cls, string):
        parts = string.split("\n")
        return cls(eval(parts[0]), eval(parts[1]))

    def is_ordered(self):
        state = is_ordered_recursive(self.left, self.right)
        if state == State.ORDERED:
            return True
        elif state == State.UNORDERED:
            return False
        raise ValueError("Could not determine if ordered or unordered")


def compress(states):
    for state in states:
        if state == State.ORDERED:
            return State.ORDERED
        if state == State.UNORDERED:
            return State.UNORDERED
    return State.CONTINUE


def is_ordered_recursive(left, right) -> State:
    if isinstance(left, int) and isinstance(right, int):
        if left < right:
            return State.ORDERED
        elif left > right:
            return State.UNORDERED
        else:
            return State.CONTINUE
    elif isinstance(left, int) and isinstance(right, list):
        return is_ordered_recursive([left], right)
    elif isinstance(left, list) and isinstance(right, int):
        return is_ordered_recursive(left, [right])

    if len(left) == len(right) == 0:
        return State.CONTINUE
    elif len(left) == 0 and len(right) > 0:
        return State.ORDERED
    elif len(left) > 0 and len(right) == 0:
        return State.UNORDERED
    return compress(
        [is_ordered_recursive(left[0], right[0])]
        + [is_ordered_recursive(left[1:], right[1:])]
    )


def ordered_indices_sum(pairs):
    s = 0
    for idx, pair in enumerate(pairs, 1):
        # print(f"Pair {idx} is_ordered = {pair.is_ordered()}, {pair}")
        if pair.is_ordered():
            s += idx
    return s


def read_input(inputfile):
    with open(inputfile) as stream:
        content = stream.read().split("\n\n")
    return [Pair.from_string(ps) for ps in content]


@dataclass
class List:
    data: list

    def __eq__(self, other):
        return self.data == other.data

    def __lt__(self, other):
        return Pair(self.data, other.data).is_ordered()


def read_input_2(inputfile):
    with open(inputfile) as stream:
        lines = stream.read().split("\n")
    lists = [eval(line) for line in lines if line != ""]
    lists.append([[2]])
    lists.append([[6]])
    return [List(l) for l in lists]


if __name__ == "__main__":
    input_file = "input.txt"
    pairs = read_input(input_file)
    print(ordered_indices_sum(pairs))
    lists = read_input_2(input_file)
    sorted_lists = sorted(lists)
    print((sorted_lists.index(List([[2]])) + 1) * (sorted_lists.index(List([[6]])) + 1))
