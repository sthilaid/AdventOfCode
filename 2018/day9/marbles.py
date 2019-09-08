#!/usr/bin/python3

import functools
import io
import sys

class dlist():
    def __init__(self, val):
        self.prev   = self
        self.next   = self
        self.val    = val

    def append(self, node):
        prevNext    = self.next
        self.next   = node
        node.prev   = self
        node.next   = prevNext
        prevNext.prev = node
        return self.next

    def prepend(self, node):
        prevPrev = self.prev
        self.prev = node
        node.next = self
        node.prev = prevPrev
        prevPrev.next = node
        return self.prev

    def remove(self):
        self.prev.next = self.next
        self.next.prev = self.prev
        return self.next

    def __getitem__(self, key):
        def getter(obj):
            if key < 0:
                return obj.prev
            else:
                return obj.next
        if not isinstance(key, int):
            raise Exception("non-int key for __getitem__: %s" % key)
        obj = self
        for i in range(abs(key)):
            next = getter(obj)
            if next == None:
                break
            else:
                obj = next
        return obj

    def printList(self, highlightNode=None):
        str = "%s " % self.val
        i = 1
        while True:
            node = self[i]
            if self == node:
                break
            if node == highlightNode:
                str += "(%s) " % self[i].val
            else:
                str += "%s " % self[i].val
            i += 1
        return str
    
    def __str__(self):
        return self.printList()

    def __repr__(self):
        return "dlist(%d)" % self.val

def computeHighScore2(playerCount, marbleCount):
    marbles         = dlist(0)
    playerScores    = [0] * playerCount
    currentMarble   = marbles
    for currentMarbleValue in range(1,marbleCount+1):
        if currentMarbleValue % 23 == 0:
            playerIndex = currentMarbleValue % playerCount
            currentMarble = currentMarble[-7]
            playerScores[playerIndex] += currentMarbleValue + currentMarble.val
            currentMarble = currentMarble.remove()
            # print("scores: %s" % playerScores)
        else:
            currentMarble = currentMarble[1].append(dlist(currentMarbleValue))

        # print("[%d] %s" % (currentMarbleValue % playerCount, marbles.printList(currentMarble)))
    return functools.reduce(lambda a,x: max(x,a), playerScores, 0)

def part1and2(inputfile):
    playerCount = 0
    marbleCount = 0
    with open(inputfile, "r") as file:
        data = file.read().split()
        playerCount = int(data[0])
        marbleCount = int(data[6])

    highscore = computeHighScore2(playerCount, marbleCount)
    print("[Part1] PlayerCount: %d, MarbleCount: %d, highscore: %d"
          % (playerCount, marbleCount, highscore))

    highscore = computeHighScore2(playerCount, marbleCount*100)
    print("[Part2] PlayerCount: %d, MarbleCount: %d, highscore: %d"
          % (playerCount, marbleCount*100, highscore))

def main():
    if (len(sys.argv) != 2):
        print("usage: freq [inputfile]")
        return

    part1and2(sys.argv[1])

if __name__ == "__main__":
    """ This is executed when run from the command line """
    main()
