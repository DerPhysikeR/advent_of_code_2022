from __future__ import annotations

from dataclasses import dataclass


@dataclass
class Node:
    value: int
    pred: Node | None = None
    succ: Node | None = None


if __name__ == "__main__":
    decryption_key = 811589153
    # decryption_key = 1
    # with open("test_input.txt") as stream:
    with open("input.txt") as stream:
        numbers = [int(l) * decryption_key for l in stream.readlines()]

    # print(numbers)

    nodes = [Node(value) for value in numbers]
    for idx, node in enumerate(nodes):
        try:
            node.pred = nodes[idx - 1]
            node.succ = nodes[idx + 1]
        except IndexError:
            pass
        nodes[-1].succ = nodes[0]

    length = len(numbers) - 1
    # for node in nodes:
    #     print(node.pred.value, node.value, node.succ.value)

    head = nodes[0]

    for _ in range(10):
        # for _ in range(1):
        # to_print = nodes[0]
        print("---------------------")
        to_print = head
        for _ in nodes:
            print(to_print.value)
            to_print = to_print.succ

        for node in nodes:
            # print(head.value)
            # if node.value == 0:
            if node.value % length == 0:
                continue
            if node == head and node.value > 0:
                head = head.succ
            if node == head and node.value < 0:
                head = head.pred

            # remove node from linked list
            node.pred.succ = node.succ
            node.succ.pred = node.pred
            if node.value > 0:
                new_pred = node.succ
                # for _ in range((node.value % length) - 1):
                for _ in range((node.value % length) - 1):
                    new_pred = new_pred.succ
            if node.value < 0:
                new_pred = node.pred
                # for _ in range(abs((node.value % length))):
                for _ in range(abs((node.value % length) - length)):
                    new_pred = new_pred.pred
            node.succ = new_pred.succ
            node.succ.pred = node
            new_pred.succ = node
            node.pred = new_pred

    # to_print = nodes[0]
    print("---------------------")
    to_print = head
    for _ in nodes:
        print(to_print.value)
        to_print = to_print.succ

    # print("---------------------")
    for node in nodes:
        if node.value == 0:
            zero_node = node

    to_print = zero_node
    for _ in range(1000):
        to_print = to_print.succ
    first = to_print.value
    print(to_print.value)

    to_print = zero_node
    for _ in range(2000):
        to_print = to_print.succ
    second = to_print.value
    print(to_print.value)

    to_print = zero_node
    for _ in range(3000):
        to_print = to_print.succ
    third = to_print.value
    print(to_print.value)

    print(first + second + third)
