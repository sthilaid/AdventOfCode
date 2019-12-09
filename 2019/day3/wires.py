#!/usr/bin/python3

import functools
import itertools
import io
import math
import sys

def manhattanDistance(p1, p2=(0,0)):
    return abs(p2[0] - p1[0]) + abs(p2[1] - p1[1])

def walkWire(wire, posFn, stepCountSearchFn=False):
    currentPos  = (0,0)
    stepCount   = 0
    for (dir, steps) in wire:
        delta = (0,0)
        if dir == "U":
            delta = (0,1)
        elif dir == "D":
            delta = (0,-1)
        elif dir == "L":
            delta = (-1,0)
        elif dir == "R":
            delta = (1,0)
        for i in range(steps):
            stepCount += 1
            currentPos = (currentPos[0] + delta[0], currentPos[1] + delta[1])
            posFn(currentPos)
            if stepCountSearchFn:
                stepCountSearchFn(currentPos, stepCount)

def part1and2(inputfile):
    wires = []
    with open(inputfile, "r") as file:
        for line in file:
            wires += [[(el[0], int(el[1:])) for el in line.split(",")]]

    wire1Nodes = dict()
    def addPos(pos):
        if not pos[0] in wire1Nodes:
            wire1Nodes[pos[0]] = dict()
        wire1Nodes[pos[0]][pos[1]] = True
    walkWire(wires[0], addPos)

    collisions = set()
    def addCollision(pos):
        if pos[0] not in wire1Nodes:
            return
        if pos[1] not in wire1Nodes[pos[0]]:
            return
        collisions.add(pos)
    walkWire(wires[1], addCollision)
    
    distances = [manhattanDistance(p) for p in collisions]
    print("closest inersection distance: %d" % min(distances))

    collisionsStepCounts = list(itertools.repeat(0, len(collisions)))
    def countStepsToIntersections(pos, stepCount):
        for i, col in enumerate(collisions):
            if col[0] == pos[0] and col[1] == pos[1]:
                collisionsStepCounts[i] = collisionsStepCounts[i] + stepCount
    walkWire(wires[0], lambda x: x, countStepsToIntersections)
    walkWire(wires[1], lambda x: x, countStepsToIntersections)
    print("closest intersection in wire distance: %d" % min(collisionsStepCounts))

def main():
    if (len(sys.argv) != 2):
        print("usage: lights [inputfile]")
        return

    part1and2(sys.argv[1])

if __name__ == "__main__":
    """ This is executed when run from the command line """
    main()
