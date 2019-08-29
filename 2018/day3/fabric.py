#!/usr/bin/python3

import array
import io
import sys

class order():
    def __init__(self, id, x, y, w, h):
        self.id = id
        self.x  = x
        self.y  = y
        self.w  = w
        self.h  = h

    def intersects(self, x, y):
        return ((x >= self.x and x < self.x + self.w)
                and (y >= self.y and y < self.y + self.h))

    def parse(line):
        data    = line.split()
        id      = int(data[0][1:])
        offset  = data[2].split(",")
        xoffset = int(offset[0])
        yoffset = int(offset[1][:len(offset[1])-1])
        coord   = data[3].split("x")
        xcoord  = int(coord[0])
        ycoord  = int(coord[1])
        return order(id, xoffset, yoffset, xcoord, ycoord)

    def __repr__(self):
        return "order(%d, %d, %d, %d, %d)" % (self.id, self.x, self.y, self.w, self.h)
    def __str__(self):
        return "#%d @ %d,%d: %dx%d" % (self.id, self.x, self.y, self.w, self.h)

def part1And2(inputfile):
    orders = []
    with open(inputfile, "r") as file:
        for line in file:
            orders += [order.parse(line)]

    fabricWidth     = 1000
    fabricHeight    = 1000
    fabric          = bytearray(fabricWidth * fabricHeight)
    overlappingTileCount = 0
    for currentOrder in orders:
        for y in range(currentOrder.h):
            for x in range(currentOrder.w):
                index = (currentOrder.y+y) * fabricHeight + (currentOrder.x+x)
                fabric[index] += 1
                if fabric[index] == 2:
                    overlappingTileCount += 1
    print("[Part1] %d overlapping fabric tiles" % overlappingTileCount)

    singleClaimOrder = None
    for currentOrder in orders:
        singleClaim = True
        for y in range(currentOrder.h):
            for x in range(currentOrder.w):
                index = (currentOrder.y+y) * fabricHeight + (currentOrder.x+x)
                singleClaim = singleClaim and fabric[index] == 1
                if not singleClaim:
                    break
            if not singleClaim:
                break
        if singleClaim:
            singleClaimOrder = currentOrder
            break

    if singleClaimOrder == None:
        raise Exception("Couldn't find intersecting single claim order with index %d" % singleClaimIndex)

    print("[Part2] single claim order: %s" % singleClaimOrder)

def main():
    if (len(sys.argv) != 2):
        print("usage: freq [inputfile]")
        return

    part1And2(sys.argv[1])

if __name__ == "__main__":
    """ This is executed when run from the command line """
    main()

