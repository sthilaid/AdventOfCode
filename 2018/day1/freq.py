#!/usr/bin/python3

import io
import sys

def part1(inputfile):
    freq = 0
    with open(inputfile, "r") as file:
        for line in file:
            freq += int(line)
    print("[Part1] freq: %d" % freq)
    return freq

def part2(inputfile, maxTries):
    freq        = 0
    knownFreqs  = set()
    inputData   = []

    with open(inputfile, "r") as file:
        lines = file.readlines()
        inputData = [int(x) for x in lines]

    if len(inputData) == 0:
        return

    dataIndex = 0
    safetyNet = 0
    while safetyNet < maxTries:
        knownFreqs.add(freq)
        freq += inputData[dataIndex]
        if freq in knownFreqs:
            print("[Part2] freq: %d (tries: %d)" % (freq, safetyNet))
            return freq
        dataIndex = (dataIndex+1) % len(inputData)
        safetyNet = safetyNet+1
    print("[Part2] Coudln't find a repeating frequency after %d tries..." % safetyNet)

def main():
    if (len(sys.argv) != 2):
        print("usage: freq [inputfile]")

    part1(sys.argv[1])
    part2(sys.argv[1], 10000000)

if __name__ == "__main__":
    """ This is executed when run from the command line """
    main()

