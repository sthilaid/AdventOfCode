#!/usr/bin/python3

import io
import sys

def getCoordsAndSizes(inputfile):
    coordinates = []
    with open(inputfile, "r") as file:
        for line in file:
            numbers = line.split(",")
            coordinates.append((int(numbers[0]), int(numbers[1])))

    gridSizes    = [0,0]
    for c in coordinates:
        gridSizes[0] = max(gridSizes[0], c[0])
        gridSizes[1] = max(gridSizes[1], c[1])

    return coordinates, gridSizes

def part1(inputfile):
    coordinates, gridSizes = getCoordsAndSizes(inputfile)
    gridSize    = gridSizes[0] * gridSizes[1]
    grid        = [(-1, sys.maxsize)] * gridSize

    for cell in range(gridSize):
        x = cell % gridSizes[0]
        y = int(cell / gridSizes[0])
        bestDist    = sys.maxsize
        bestCoord   = -1
        for i, c in enumerate(coordinates):
            dist = abs(x - c[0]) + abs(y - c[1])
            if dist < bestDist:
                bestDist = dist
                bestCoord= i
        grid[cell] = (bestCoord, bestDist)

    coordinateCounts = []
    for c in range(len(coordinates)):
        count = 0
        for cell in grid:
            if cell[0] == c:
                count += 1
        coordinateCounts.append((c, count))
    coordinateCounts.sort(key=lambda x: x[1], reverse=True)

    infiniteCells = set()
    for i in range(gridSizes[0]):
        infiniteCells.add(grid[i][0])
    for i in range(gridSizes[0]):
        infiniteCells.add(grid[(gridSizes[0] * (gridSizes[1]-1)) + i][0])
    for i in range(gridSizes[1]):
        infiniteCells.add(grid[gridSizes[0] * i][0])
    for i in range(gridSizes[1]):
        infiniteCells.add(grid[gridSizes[0] * i + gridSizes[0]-1][0])

    largest = None
    for c in coordinateCounts:
        if c[0] not in infiniteCells:
            print("[Part1] (%d,%d) in area of %d cells" % (coordinates[c[0]][0], coordinates[c[0]][1], c[1]))
            return
    print("failed to find cell...")

def part2(inputfile):
    coordinates, gridSizes  = getCoordsAndSizes(inputfile)
    regionCount             = 0
    for y in range(gridSizes[1]):
        for x in range(gridSizes[0]):
            distSum = 0
            for c in coordinates:
                distSum += abs(x - c[0]) + abs(y - c[1])
            if distSum < 10000:
                regionCount += 1

    print("[Part2] region size is : %d" % regionCount)

def main():
    if (len(sys.argv) != 2):
        print("usage: freq [inputfile]")
        return

    part1(sys.argv[1])
    part2(sys.argv[1])

if __name__ == "__main__":
    """ This is executed when run from the command line """
    main()
