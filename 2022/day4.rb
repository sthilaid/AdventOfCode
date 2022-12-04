#!/usr/bin/ruby

def parseLine(line)
  nums = line.scan(/\d+/).map{|str| Integer(str)}
end

def isFullOverlap(elvesData)
  minA, maxA, minB, maxB = elvesData[0], elvesData[1], elvesData[2], elvesData[3]
  return (minA >= minB && maxA <= maxB) || (minB >= minA && maxB <= maxA)
end

def isOverlap(elvesData)
  minA, maxA, minB, maxB = elvesData[0], elvesData[1], elvesData[2], elvesData[3]
  return (minA <= minB && maxA >= minB) || (minB <= minA && maxB >= minA)
end

fullOverlapCount = 0
File.foreach(ARGV[0]) { |line| if isFullOverlap(parseLine(line)) then fullOverlapCount += 1 end }
puts "part1: #{fullOverlapCount}"

overlapCount = 0
File.foreach(ARGV[0]) { |line| if isOverlap(parseLine(line)) then overlapCount += 1 end }
puts "part2: #{overlapCount}"
