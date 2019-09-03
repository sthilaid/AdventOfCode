#!/usr/bin/python3

import io
import sys

def parseNode(data, f):
    if len(data) < 2:
        raise Exception("invalid node data for '%s'" % data)
    childCount      = data[0]
    metaDataCount   = data[1]
    childResults    = []
    offset = 2
    for c in range(childCount):
        childOffset, childResult = parseNode(data[offset:], f)
        offset += childOffset
        childResults.append(childResult)
    metadata = data[offset:offset+metaDataCount]
    offset += metaDataCount
    return offset, f(data, childResults, metadata)

def part1(inputfile):
    data = ""
    with open(inputfile, "r") as file:
        dataStr = file.read()
        data = [int(x) for x in dataStr.split()]
    
    def accumulateChecksum(data, childResults, metadata):
        return sum(childResults) + sum(metadata)

    _,checksum = parseNode(data, accumulateChecksum)
    print("[Part1] checksum: %d" % checksum)

def part2(inputfile):
    data = ""
    with open(inputfile, "r") as file:
        dataStr = file.read()
        data = [int(x) for x in dataStr.split()]

    def nodeValue(data, childResults, metadata):
        if len(childResults) == 0:
            return sum(metadata)
        value = 0
        for m in metadata:
            childIndex = m-1 # 1 first child, ...
            if childIndex >= 0 and childIndex < len(childResults):
                value += childResults[childIndex]
        return value

    _, rootValue = parseNode(data, nodeValue)
    print("[Part2] tree root value: %d" % rootValue)

def main():
    if (len(sys.argv) != 2):
        print("usage: freq [inputfile]")
        return

    part1(sys.argv[1])
    part2(sys.argv[1])

if __name__ == "__main__":
    """ This is executed when run from the command line """
    main()
