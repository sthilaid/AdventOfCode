#!/usr/bin/python3

import io
import sys

class order():
    def __init__(self, id, x, y, w, h):
        self.id = id
        self.x  = x
        self.y  = y
        self.w  = w
        self.h  = h

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

def part1(inputfile):
    orders = []
    with open(inputfile, "r") as file:
        for line in file:
            orders += [order.parse(line)]
    print("%s" % orders[0])

def part2(inputfile):
    with open(inputfile, "r") as file:
        pass

def main():
    if (len(sys.argv) != 2):
        print("usage: freq [inputfile]")
        return

    part1(sys.argv[1])
    part2(sys.argv[1])

if __name__ == "__main__":
    """ This is executed when run from the command line """
    main()

