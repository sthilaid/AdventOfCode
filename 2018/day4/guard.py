
#!/usr/bin/python3

import array
import datetime
import functools
import io
import sys

class Guard():
    def __init__(self, id):
        self.id             = id
        self.shifts         = []
        self.sleepIntervals = []

    def registerShift(self, shift):
        self.shifts += [shift.date]
        self.sleepIntervals += shift.sleepIntervals

    def mostSleptMinute(self):
        def isInRange(minute, start, end):
            time = datetime.datetime(start.year, start.month, start.day, 0, minute)
            return time >= start and time <= end

        def accIfInRange(minute):
            def accum(a, x):
                if isInRange(minute, x[0], x[1]):
                    return a+1
                else:
                    return a
            return accum
        
        bestMinute = -1
        bestCount  = 0
        for m in range(59):
            for sleep in self.sleepIntervals:
                count = functools.reduce(accIfInRange(m), self.sleepIntervals, 0)
                if count > bestCount:
                    bestMinute = m
                    bestCount = count

        if bestMinute < 0:
            if self.sleepIntervals:
                raise Exception("Invalid sleep intervals analysis: %s" % self.sleepIntervals)
            else:
                return False
        return bestMinute, bestCount

    def totalSleepTime(self):
        return functools.reduce(lambda a,x: a + (x[1]-x[0]), self.sleepIntervals, datetime.timedelta())

    def __repr__(self):
        return "Guard(%d)" % (self.id)
    def __str__(self):
        mostSleptMinuteResult = self.mostSleptMinute()
        if mostSleptMinuteResult:
            return ("Guard #%d: shifts: %d bestMinute: %d bestCount: %d snoozeTime: %s"
                    % (self.id, len(self.shifts), *mostSleptMinuteResult, self.totalSleepTime()))
        else:
            return "Guard #%d: shifts: %d !no snooze!" % (self.id, len(self.shifts))

class Shift():
    def __init__(self, guardId, date, sleepIntervals):
        self.guardId        = guardId
        self.date           = date
        self.sleepIntervals = sleepIntervals

    def parse(lines):
        def parseDate(line):
            if isinstance(line, str):
                if len(line) > 18 and line[0] == "[" and line[17] == "]":
                    dateAndTime = line[1:17].split()
                    date = [int(x) for x in dateAndTime[0].split('-')]
                    time = [int(x) for x in dateAndTime[1].split(':')]
                    return datetime.datetime(*date, *time)
            raise Exception("parseDate: invalid line %s" % line)

        def parseShiftStart(line):
            if isinstance(line, str) and len(line) > 27:
                if line[19:24] == "Guard":
                    text = line[26:]
                    num = ""
                    for i in range(1,len(text)):
                        if not text[:i].isdigit():
                            break
                        else:
                            num = text[:i]
                    if num.isdigit():
                        return int(num)
            return False

        def parseShiftSnoozeStart(line):
            return "falls asleep" in line

        def parseShiftSnoozeEnd(line):
            return "wakes up" in line
            
        if not lines:
            return (None, [])
        
        index   = 0
        line    = lines[index]
        date    = parseDate(line)
        guardId = parseShiftStart(line)
        if not guardId:
            raise Exception("invalid start of shift: %s" % line)

        isSnoozing = False
        snoozeList = []
        while True:
            index += 1
            if index >= len(lines) or parseShiftStart(lines[index]):
                break
            
            if (isSnoozing and not parseShiftSnoozeEnd(lines[index]) or
                not isSnoozing and not parseShiftSnoozeStart(lines[index])):
                raise Exception("invalid snooze sequence...")

            date = parseDate(lines[index])
            if not isSnoozing:
                isSnoozing = date
            else:
                snoozeList += [(isSnoozing, date)] # start, end
                isSnoozing      = False
        
        return (Shift(guardId, date, snoozeList), lines[index:])

    def __repr__(self):
        return "Shift(%d, %s, %s)" % (self.guardId, self.date, self.sleepIntervals)

def part1(inputfile):
    def greatestSnoozer(a, g):
        if g.totalSleepTime() > a.totalSleepTime():
            return g
        else:
            return a

    def greatestMinuteSleeper(a, g):
        res = g.mostSleptMinute()
        if res and res[1] > a[0][1]:
            return [(*res, g)]
        elif res and res[1] == a[0][1]:
            return a + [(*res, g)]
        else:
            return a
        
    lines = []
    with open(inputfile, "r") as file:
        lines = file.readlines()
        lines.sort()

    guards = []
    while True:
        shift, lines = Shift.parse(lines)
        shiftRegistered = False
        for guard in guards:
            if guard.id == shift.guardId:
                guard.registerShift(shift)
                shiftRegistered = True
                break
        if not shiftRegistered:
            guards += [Guard(shift.guardId)]
            guards[len(guards)-1].registerShift(shift)
            
        if not lines:
            break

    # for g in guards:
    #     print(g)

    snoozer = functools.reduce(greatestSnoozer, guards, Guard(-1))
    print("[Part1] Best Snoozer: %s" % snoozer)
    
    minuteSnoozer = functools.reduce(greatestMinuteSleeper, guards, [(None, 0, None)])
    print("[Part2] %s" % list(map(lambda x: str(x[2]), minuteSnoozer)))

def main():
    if (len(sys.argv) != 2):
        print("usage: freq [inputfile]")
        return

    part1(sys.argv[1])

if __name__ == "__main__":
    """ This is executed when run from the command line """
    main()

