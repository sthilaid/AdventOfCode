#!/usr/bin/python3

import functools
import io
import math
import sys

def eval(op, a, b, c, mem):
    if a >= len(mem) or b >= len(mem) or c >= len(mem):
        raise Exception("invalid addresses for %d or %d or %d (size: %d)" %(a, b, c, len(mem)))
    
    aVal = mem[a]
    bVal = mem[b]
    if op == 1:
        mem[c] = aVal + bVal
        return True
    elif op == 2:
        mem[c] = aVal * bVal
        return True
    elif op == 99:
        return False
    else:
        raise Exception("invalid opcode %d" % (op))

def run(noun, verb, prog):
    mem = prog.copy()
    mem[1] = noun
    mem[2] = verb

    pc = 0
    while eval(mem[pc], mem[pc+1], mem[pc+2], mem[pc+3], mem):
        pc += 4
    return mem[0]
    
def part1(inputfile):
    mem = []
    with open(inputfile, "r") as file:
        mem = [int(x) for x in file.read().split(",")]

    answer = run(12, 2, mem)
    print("Answer: %d" % (answer))

def part2(inputfile):
    def search(targetAnswer):
        for noun in range(100):
            for verb in range(100):
                answer = run(noun, verb, prog)
                if answer == targetAnswer:
                    return (noun, verb)
        return False
        
    prog = []
    with open(inputfile, "r") as file:
        prog = [int(x) for x in file.read().split(",")]

    targetAnswer = 19690720
    result = search(targetAnswer)
    if result:
        noun, verb = result
        print("inputs %d, %d yield target answer: %d. part 2 solution: %d" % (noun, verb, targetAnswer, noun * 100 + verb))
    else:
        print("couldn't find answer yielding %d" % targetAnswer)
    
def main():
    if (len(sys.argv) != 2):
        print("usage: lights [inputfile]")
        return

    part1(sys.argv[1])

if __name__ == "__main__":
    """ This is executed when run from the command line """
    main()
