#!/usr/bin/python3

import io
import sys

def react(molecule):
    newMolecule     = [molecule[0]]
    i               = 1
    while True:
        isCaseDifferent     = len(newMolecule) > 0 and molecule[i].islower() != newMolecule[-1].islower()
        isReactingElements  = isCaseDifferent and (molecule[i].casefold() == newMolecule[-1].casefold())
        if isReactingElements:
            del newMolecule[-1]
        else:
            newMolecule.append(molecule[i])
        i += 1

        if i >= len(molecule):
            break
    return newMolecule

def part1(inputfile):
    moleculeStr = "dabAcCaCBAcCcaDA" # test case
    with open(inputfile, "r") as file:
        moleculeStr = file.read()

    molecule        = list(moleculeStr)
    newMolecule     = react(molecule)
    print("[Part1] units left after reaction: %d" % (len(newMolecule)))

def part2(inputfile):
    moleculeStr = "dabAcCaCBAcCcaDA" # test case
    with open(inputfile, "r") as file:
        moleculeStr = file.read()

    bestMoleculeSize= sys.maxsize
    bestMolecule    = []
    worstElement    = ""
    molecule = list(moleculeStr)
    for i in range(ord('a'), ord('z')):
        l = chr(i)
        cleanedMolecule = [el for el in molecule if el.casefold() != l.casefold()]
        newMolecule = react(cleanedMolecule)
        if len(newMolecule) < bestMoleculeSize:
            bestMoleculeSize= len(newMolecule)
            bestMolecule    = newMolecule
            worstElement    = l
    print("[Part2] shortest polymer lenght is %d by removing all '%s'" % (bestMoleculeSize, worstElement))

def main():
    if (len(sys.argv) != 2):
        print("usage: freq [inputfile]")
        return

    part1(sys.argv[1])
    part2(sys.argv[1])

if __name__ == "__main__":
    """ This is executed when run from the command line """
    main()

