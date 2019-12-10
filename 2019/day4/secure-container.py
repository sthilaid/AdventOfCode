#!/usr/bin/python3

import functools
import itertools
import io
import math
import sys

def findNextValid(val):
    strVal = str(val+1)
    missingDigits = max(0, 6 - len(strVal))
    strVal += "0" * missingDigits

    if len(strVal) != 6:
        raise Exception("invalid digit count for pwd: %s" % strVal)

    intVals = [int(x) for x in list(strVal)]
    for i in range(1,len(intVals)):
        if intVals[i] < intVals[i-1]:
            intVals[i] = intVals[i-1]

    return int("".join([str(x) for x in intVals]))

def isValidForm(val):
    intVals = [int(x) for x in list(str(val))]
    count = 0
    digit = 999
    for i in range(len(intVals)):
        if intVals[i] == digit:
            count = count + 1
        else:
            if count == 2:
                return True
            count = 1
            digit = intVals[i]
    return count == 2
    
def part1(rangemin, rangemax):
    part1SolutionCount = -2
    part2SolutionCount = 0
    currentPwd = rangemin

    while currentPwd <= rangemax:
        part1SolutionCount = part1SolutionCount + 1
        currentPwd = findNextValid(currentPwd)
        if isValidForm(currentPwd):
            part2SolutionCount = part2SolutionCount + 1 

    print("found %d part1 solutions and %d part2 solutions for range [%d, %d]"
          % (part1SolutionCount, part2SolutionCount, rangemin, rangemax))

def main():
    if (len(sys.argv) != 3):
        print("usage: secure-container [rangemin] [rangemax]")
        return

    part1(int(sys.argv[1]), int(sys.argv[2]))

if __name__ == "__main__":
    """ This is executed when run from the command line """
    main()
