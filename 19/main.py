from __future__ import annotations

from functools import cache
from typing import NamedTuple


class Blueprint(NamedTuple):
    id: int
    ore_robot_ore_cost: int
    clay_robot_ore_cost: int
    obsidian_robot_ore_cost: int
    obsidian_robot_clay_cost: int
    geode_robot_ore_cost: int
    geode_robot_obsidian_cost: int

    def can_buy_ore_robot(self, balance: Balance) -> bool:
        return balance.ore >= self.ore_robot_ore_cost

    def buy_ore_robot(self, fleet: Fleet, balance: Balance) -> tuple[Fleet, Balance]:
        return (
            Fleet(
                fleet.ore_robots + 1,
                fleet.clay_robots,
                fleet.obsidian_robots,
                fleet.geode_robots,
            ),
            Balance(
                balance.ore - self.ore_robot_ore_cost,
                balance.clay,
                balance.obsidian,
                balance.geodes,
            ),
        )

    def can_buy_clay_robot(self, balance: Balance) -> bool:
        return balance.ore >= self.clay_robot_ore_cost

    def buy_clay_robot(self, fleet: Fleet, balance: Balance) -> tuple[Fleet, Balance]:
        return (
            Fleet(
                fleet.ore_robots,
                fleet.clay_robots + 1,
                fleet.obsidian_robots,
                fleet.geode_robots,
            ),
            Balance(
                balance.ore - self.clay_robot_ore_cost,
                balance.clay,
                balance.obsidian,
                balance.geodes,
            ),
        )

    def can_buy_obsidian_robot(self, balance: Balance) -> bool:
        return (
            balance.ore >= self.obsidian_robot_ore_cost
            and balance.clay >= self.obsidian_robot_clay_cost
        )

    def buy_obsidian_robot(
        self, fleet: Fleet, balance: Balance
    ) -> tuple[Fleet, Balance]:
        return (
            Fleet(
                fleet.ore_robots,
                fleet.clay_robots,
                fleet.obsidian_robots + 1,
                fleet.geode_robots,
            ),
            Balance(
                balance.ore - self.obsidian_robot_ore_cost,
                balance.clay - self.obsidian_robot_clay_cost,
                balance.obsidian,
                balance.geodes,
            ),
        )

    def can_buy_geode_robot(self, balance: Balance) -> bool:
        return (
            balance.ore >= self.geode_robot_ore_cost
            and balance.obsidian >= self.geode_robot_obsidian_cost
        )

    def buy_geode_robot(self, fleet: Fleet, balance: Balance) -> tuple[Fleet, Balance]:
        return (
            Fleet(
                fleet.ore_robots,
                fleet.clay_robots,
                fleet.obsidian_robots,
                fleet.geode_robots + 1,
            ),
            Balance(
                balance.ore - self.geode_robot_ore_cost,
                balance.clay,
                balance.obsidian - self.geode_robot_obsidian_cost,
                balance.geodes,
            ),
        )


class Balance(NamedTuple):
    ore: int = 0
    clay: int = 0
    obsidian: int = 0
    geodes: int = 0


class Fleet(NamedTuple):
    ore_robots: int = 1
    clay_robots: int = 0
    obsidian_robots: int = 0
    geode_robots: int = 0


def do_nothing(fleet: Fleet, balance: Balance) -> tuple[Fleet, Balance]:
    return fleet, balance


@cache
def maximize_geodes(bp: Blueprint, remaining_time: int, f: Fleet, b: Balance):
    if remaining_time <= 0:
        return b.geodes

    nb = Balance(
        b.ore + f.ore_robots,
        b.clay + f.clay_robots,
        b.obsidian + f.obsidian_robots,
        b.geodes + f.geode_robots,
    )

    options = []
    if bp.can_buy_clay_robot(b):
        options.append(bp.buy_clay_robot)
    if bp.can_buy_ore_robot(b):
        options.append(bp.buy_ore_robot)
    options.append(do_nothing)

    if bp.can_buy_obsidian_robot(b):
        options = [bp.buy_obsidian_robot]
    if bp.can_buy_geode_robot(b):
        options = [bp.buy_geode_robot]

    return max(maximize_geodes(bp, remaining_time - 1, *buy(f, nb)) for buy in options)


def get_num_openend_geodes(blueprint: Blueprint) -> int:
    balance = Balance()
    fleet = Fleet()
    return maximize_geodes(blueprint, 24, fleet, balance)


def calc_quality_level(blueprint: Blueprint) -> int:
    return blueprint.id * get_num_openend_geodes(blueprint)


def parse_input(filepath):
    with open(filepath) as stream:
        lines = stream.read().strip().split("\n")

    blueprints = []
    for line in lines:
        nums = []
        for part in line.split():
            num = []
            for char in part:
                if char in "0123456789":
                    num.append(char)
            if num:
                nums.append(int("".join(num)))
        blueprints.append(Blueprint(*nums))
    return blueprints


if __name__ == "__main__":
    blueprints = parse_input("test_input.txt")
    print(sum(calc_quality_level(bp) for bp in blueprints))
