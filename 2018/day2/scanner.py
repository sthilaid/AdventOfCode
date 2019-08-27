#!/usr/bin/python3

import io
import sys

def part1(inputfile):
    doubles=0
    triples=0

    with open(inputfile, "r") as file:
        for code in file:
            letterCounts = {}
            for letter in code:
                letterCounts[letter] = letterCounts.get(letter, 0) + 1
            if any([val == 2 for val in letterCounts.values()]):
                doubles = doubles + 1
            if any([val == 3 for val in letterCounts.values()]):
                triples = triples + 1
    checksum = doubles * triples
    print("[Part1] checksum: %d x %d = %d" % (doubles, triples, checksum))
    return checksum

def part2(inputfile):
    codes = []
    with open(inputfile, "r") as file:
        codes = file.readlines()
        codes = [c[:len(c)-1] for c in codes]
    for i in range(len(codes)):
        for j in range(i+1, len(codes)):
            # assuming all words of same length
            diffIndices = []
            for l in range(len(codes[i])):
                if codes[i][l] != codes[j][l]:
                    diffIndices += [l]
            if len(diffIndices) == 1:
                commonIndex = diffIndices[0]
                commonId = codes[i][:commonIndex] + codes[i][commonIndex+1:]
                print("[Part2] common id: %s (indices: %d, %d), letter: %s and %s"
                      % (commonId, i, j, codes[i][commonIndex], codes[j][commonIndex]))
                return commonId
    print("[Part2] Failed to find common id in the input files...")

def main():
    if (len(sys.argv) != 2):
        print("usage: freq [inputfile]")
        return

    part1(sys.argv[1])
    part2(sys.argv[1])

if __name__ == "__main__":
    """ This is executed when run from the command line """
    main()

