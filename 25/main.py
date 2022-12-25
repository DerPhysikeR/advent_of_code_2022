FROM_SNAFU_LOOKUP = {"=": -2, "-": -1, "0": 0, "1": 1, "2": 2}
TO_SNAFU_LOOKUP = {v: k for k, v in FROM_SNAFU_LOOKUP.items()}


def from_snafu(snafu: str) -> int:
    return sum(FROM_SNAFU_LOOKUP[d] * 5**i for i, d in enumerate(reversed(snafu)))


def to_snafu(num) -> str:
    s = []
    while num > 0:
        digit = num % 5
        num = (num - digit) // 5
        if digit > 2:
            digit -= 5
            num += 1
        s.append(TO_SNAFU_LOOKUP[digit])
    return "".join(reversed(s))


if __name__ == "__main__":
    with open("input.txt") as stream:
        snafu_nums = stream.read().strip().split("\n")

    s = sum(from_snafu(n) for n in snafu_nums)
    assert s == from_snafu(to_snafu(s))
    print(to_snafu(s))
