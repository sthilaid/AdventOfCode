#!/usr/bin/python3

import functools
import io
import sys

def getCellLevel(x, y, gridSerialNumber):
    rackId      = x+10
    powerLevel  = rackId * y
    powerLevel += gridSerialNumber
    powerLevel *= rackId
    powerLevel  = int((powerLevel % 1000) / 100)
    powerLevel -= 5
    return powerLevel

def part1(input):
    powerLevels = [0] * (301 * 301)
    for y in range(1, 301):
        for x in range(1, 301):
            powerLevels[x + y*301] = getCellLevel(x, y, input)

    bestCellPower = -sys.maxsize-1
    bestCellX, bestCellY = 0,0
    for y in range(1, 301):
        for x in range(1, 301):
            cellSquarePowerLevel = 0
            for dy in range(3):
                for dx in range(3):
                    cellX, cellY = x+dx, y+dy
                    if cellX > 300 or cellY > 300:
                        cellSquarePowerLevel += -sys.maxsize-1
                    else:
                        cellSquarePowerLevel += powerLevels[cellX + cellY*301]
            if cellSquarePowerLevel > bestCellPower:
                bestCellPower = cellSquarePowerLevel
                bestCellX, bestCellY = x, y
    print("[Part1] Best 3x3 fuel cell located at %d,%d with %d total power" % (bestCellX, bestCellY, bestCellPower))

def part2(input):
    powerLevels = [0] * (301 * 301)
    for y in range(1, 301):
        for x in range(1, 301):
            powerLevels[x + y*301] = getCellLevel(x, y, input)

    bestCellPower           = -sys.maxsize-1
    bestCellSize            = 0
    bestCellX, bestCellY    = 0,0
    prevIteration           = [0] * 301 * 301
    for size in range(1,301):
        for y in range(1, 301):
            for x in range(1, 301):
                if x+size > 300 or y+size > 300:
                    prevIteration[x + y*301] = -sys.maxsize-1
                else:
                    cellSquarePowerLevel = prevIteration[x+y*301]
                    for dy in range(size-1): # note: last element included in dx
                        cellX, cellY = x+size-1, y+dy
                        cellSquarePowerLevel += powerLevels[cellX + cellY*301]
                    cellSquarePowerLevel += sum(powerLevels[x+(y+size-1)*301:x+size+(y+size-1)*301])

                    prevIteration[x + y*301]    = cellSquarePowerLevel
                    if cellSquarePowerLevel > bestCellPower:
                        bestCellPower           = cellSquarePowerLevel
                        bestCellSize            = size
                        bestCellX, bestCellY    = x, y

        print("[%d] best cell of grid size %dx%d is %d,%d with %d total power"
              % (size, bestCellSize, bestCellSize, bestCellX, bestCellY, bestCellPower))
    print("[Part2] Best fuel cell located at %d,%d with %d total power of size %d"
          % (bestCellX, bestCellY, bestCellPower, bestCellSize))

def main():
    input = 9995 # my input for this problem instance
    part1(input)

if __name__ == "__main__":
    """ This is executed when run from the command line """
    main()
