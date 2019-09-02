#!/usr/bin/python3

import io
import sys

def getStepsAndDeps(inputfile):
    steps           = set()
    dependencies    = dict()
    with open(inputfile, "r") as file:
        for line in file:
            dep     = line[5]
            step    = line[36]
            steps.add(step)
            steps.add(dep)
            
            if step in dependencies:
                dependencies[step].add(dep)
            else:
                dependencies[step] = set(dep)
    return steps, dependencies

def freeDependencies(dependencies, finishedStep):
    freeSteps = []
    for step in dependencies.keys():
        if finishedStep in dependencies[step]:
            dependencies[step].remove(finishedStep)
        if len(dependencies[step]) == 0:
            freeSteps.append(step)
    for s in freeSteps:
        del dependencies[s]

def part1(inputfile):
    steps, dependencies = getStepsAndDeps(inputfile)
    availableSteps  = [s for s in steps if s not in dependencies.keys()]
    doneSteps       = []
    while availableSteps != []:
        availableSteps.sort()
        currentStep = availableSteps[0]
        doneSteps.append(currentStep)
        freeDependencies(dependencies, currentStep)
        availableSteps = [s for s in steps if s not in dependencies.keys() and s not in doneSteps]

    print("[Part1] steps: %s" % "".join(doneSteps))

def part2(inputfile):
    def getJobTime(step):
        return 61 + ord(step) - ord('A')

    workerCount         = 5
    t                   = 0
    steps, dependencies = getStepsAndDeps(inputfile)
    availableSteps, doneSteps, jobs = [], [], []
    while len(doneSteps) != len(steps):
        if len(jobs) > 0:
            doneStep, t = jobs[0]
            doneSteps.append(doneStep)
            freeDependencies(dependencies, doneSteps[-1])
            del jobs[0]

        availableSteps = sorted([s for s in steps if (s not in dependencies.keys()
                                                      and s not in doneSteps
                                                      and not any(job[0] == s for job in jobs))])
        while len(jobs) < workerCount and len(availableSteps) > 0:
            jobs.append((availableSteps[0], t+getJobTime(availableSteps[0])))
            del availableSteps[0]
        jobs.sort(key=lambda x: x[1])
    print("[Part2] steps: %s finished after %d secs" % ("".join(doneSteps), t))

def main():
    if (len(sys.argv) != 2):
        print("usage: freq [inputfile]")
        return

    part1(sys.argv[1])
    part2(sys.argv[1])

if __name__ == "__main__":
    """ This is executed when run from the command line """
    main()
