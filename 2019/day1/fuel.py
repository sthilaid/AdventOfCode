#!/usr/bin/python3

import functools
import io
import math
import sys

def part1(inputfile):
    moduleWeights = []
    with open(inputfile, "r") as file:
        for line in file:
            moduleWeights += [int(line)]

    moduleFuel = functools.reduce(lambda a,x: a + math.floor(x/3)-2, moduleWeights, 0)
    
    print("Module Fuel: %d" % (moduleFuel))

def part2(inputfile):
    moduleWeights = []
    with open(inputfile, "r") as file:
        for line in file:
            moduleWeights += [int(line)]

    totalFuel = 0
    for moduleMass in moduleWeights:
        currentModuleFuel = math.floor(moduleMass / 3) - 2
        totalModuleFuel = currentModuleFuel
        while True:
            currentModuleFuel = math.floor(currentModuleFuel / 3) - 2
            if currentModuleFuel <= 0:
                break
            totalModuleFuel += currentModuleFuel
        totalFuel += totalModuleFuel

    print("Total Rocket Fuel: %d" % (totalFuel))
    
def main():
    if (len(sys.argv) != 2):
        print("usage: lights [inputfile]")
        return

    part1(sys.argv[1])

if __name__ == "__main__":
    """ This is executed when run from the command line """
    main()
