#!/usr/bin/python3

import functools
import io
import sys

def parseInput(lines):
    initialState = list(lines[0][15:len(lines[0])])
    rules = []
    for ruleLine in lines[2:]:
        rules.append((list(ruleLine[:5]), ruleLine[9]))
    return initialState, rules

def applyRules(state, p, rules):
    stateLen = len(state)
    L2  = state[p-2] if p >= 2 and p < stateLen+2 else "."
    L1  = state[p-1] if p >= 1 and p < stateLen+1 else "."
    C   = state[p] if p >= 0 and p < stateLen else "."
    R1  = state[p+1] if p >= -1 and p < stateLen-1 else "."
    R2  = state[p+2] if p >= -2 and p < stateLen-2 else "."
    val = [L2, L1, C, R1, R2]
    for rule in rules:
        if val == rule[0]:
            return rule[1]
    return "."

def count(state, originIndex):
    val = 0
    for i, p in enumerate(state):
        if p != ".":
           val += i - originIndex
    return val

def part1(lines):
    state, rules = parseInput(lines)

    originIndex = 0
    plantCount = count(state, originIndex)
    print("[%d] %s :: %d" % (0, "".join(state), plantCount))

    simLength = 20
    for n in range(1, simLength+1):
        newState = []
        for p in range(-5, len(state)+5):
            newValue = applyRules(state, p, rules)
            if newValue != "." or (len(newState) > 0 and p < len(state)):
                newState.append(newValue)
                if p < 0:
                    originIndex += 1
            elif p >= 0 and p < len(state):
                originIndex -= 1
        state = newState
        plantCount = count(state, originIndex)
        print("[%d] %s :: %d / %d" % (n, "".join(state), plantCount, originIndex))
    print("[Part1] %d" % plantCount)

def part2(initialState, rules):
    pass

testInput = """initial state: #..#.#..##......###...###

...## => #
..#.. => #
.#... => #
.#.#. => #
.#.## => #
.##.. => #
.#### => #
#.#.# => #
#.### => #
##.#. => #
##.## => #
###.. => #
###.# => #
####. => #"""

def run(inputfile):
    lines = []
    with open(inputfile, "r") as file:
        lines = file.readlines()

    part1(lines)
    
def main():
    if (len(sys.argv) != 2):
        print("usage: plants [inputfile]")
        return

    run(sys.argv[1])

if __name__ == "__main__":
    """ This is executed when run from the command line """
    main()
