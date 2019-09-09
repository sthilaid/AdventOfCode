#!/usr/bin/python3

import functools
import io
import sys

class lightPoint():
    def __init__(self, p, v):
        self.pos = p
        self.vel = v

    def update(self, dt):
        self.pos[0] += dt * self.vel[0]
        self.pos[1] += dt * self.vel[1]
        
    def distSqr(self, pos):
        return pow(pos[0] - self.pos[0], 2) + pow(pos[1] - self.pos[1], 2)

    def samePos(self, pos):
        return self.pos[0] == pos[0] and self.pos[1] == pos[1] 

    def updateMinMax(self, minCornter, maxCorner):
        if self.pos[0] < minCorner[0]:
            minCorner[0] = self.pos[0]
        elif self.pos[1] < minCorner[1]:
            minCorner[1] = self.pos[1]
        if self.pos[0] > maxCorner[0]:
            maxCorner[0] = self.pos[0]
        elif self.pos[1] > maxCorner[1]:
            maxCorner[1] = self.pos[1]
        return (minCorner, maxCorner)

    def __repr__(self):
        return "lightPoint(%s, %s)" % (self.pos, self.vel)

def part1and2(inputfile):
    def drawPoints(points):
        minCorner = [sys.maxsize, sys.maxsize]
        maxCorner = [-sys.maxsize-1, -sys.maxsize-1]
        for light in points:
            minCorner, maxCorner = light.updateMinMax(minCorner, maxCorner)

        str = ""
        for y in range(minCorner[1], maxCorner[1]+1):
            for x in range(minCorner[0], maxCorner[0]+1):
                if any([p.samePos([x,y]) for p in points]):
                    str += "#"
                else:
                    str += "."
            str += "\n"
        return str
        
    points = []
    with open(inputfile, "r") as file:
        for line in file:
            pos = [int(x) for x in line[10:24].split(',')]
            vel = [int(x) for x in line[36:42].split(',')]
            points += [lightPoint(pos, vel)]

    divergence = sys.float_info.max
    t = 0
    while True:
        center = [0,0]
        minCorner = [sys.maxsize, sys.maxsize]
        maxCorner = [-sys.maxsize-1, -sys.maxsize-1]
        for light in points:
            light.update(1)
            minCorner, maxCorner = light.updateMinMax(minCorner, maxCorner)

        newDiv = (maxCorner[0] - minCorner[0]) + (maxCorner[1] - minCorner[1])
        if newDiv >= divergence:
            for light in points:
                light.update(-1)
            break
        divergence = newDiv
        t += 1

    print("Divergence: %.2f after %d sec" % (divergence, t))
    print(drawPoints(points))

def main():
    if (len(sys.argv) != 2):
        print("usage: freq [inputfile]")
        return

    part1and2(sys.argv[1])

if __name__ == "__main__":
    """ This is executed when run from the command line """
    main()
