package main

import (
    "os"
	"strings"
	"strconv"
)

type File struct {
	name string
	size int
}

func (f File) getSize() int {
	return f.size
}

type Directory struct {
	name string
	childFiles []*File
	childDirs []*Directory
	parent *Directory
	isRoot bool
}

func (d Directory) getSize() int {
	totalSize := 0
	for _,d := range d.childDirs {
		totalSize += d.getSize()
	}
	for _,f := range d.childFiles {
		totalSize += f.getSize()
	}
	return totalSize
}

func (d Directory) getAbsPath() string {
	if d.isRoot {
		return "/"
	}
	return d.parent.getAbsPath() + d.name + "/"
}

func (d Directory) print(indent int) {
	path := d.getAbsPath()
	size := d.getSize()
	println(path + " " + strconv.Itoa(size))
	for _,d := range d.childDirs {
		d.print(indent+2)
	}
}

func makeRoot() Directory {
	root := Directory{name: ""}
	root.parent = &root
	root.childFiles = make([]*File, 0)
	root.childDirs = make([]*Directory, 0)
	root.isRoot = true
	return root
}

func makeSubDir(current *Directory, subdirName string) *Directory {
	if subdirName == "/" {
		for !current.isRoot {
			current = current.parent
		}
		return current
	} else if subdirName == ".." {
		return current.parent
	} else if subdirName == "." {
		return current
	}
	for _, childDir := range (*current).childDirs {
		if (*childDir).name == subdirName {
			return childDir
		}
	}
	newDir := Directory{name: subdirName}
	newDir.parent = current
	newDir.childFiles = make([]*File, 0)
	newDir.childDirs = make([]*Directory, 0)
	newDir.isRoot = false
	(*current).childDirs = append((*current).childDirs, &newDir)
	return &newDir
}

func parseFileTree(data string) Directory {
	root := makeRoot()
	current := &root

	lines := strings.Split(data, "\n")
	for _, line := range lines {
		cmd := line[0:4]
		if cmd == "$ cd" {
			subdir := line[5:]
			current = makeSubDir(current, subdir)
		} else if cmd == "$ ls" {
			// ignore
		} else {
			lsinfo := strings.Split(line, " ")
			size, _ := strconv.Atoi(lsinfo[0])
			name := lsinfo[1]
			file := File{name: name, size: size}
			(*current).childFiles = append((*current).childFiles, &file)
		}
	}
	return root
}

func part1(dir Directory) int {
	sum := 0
	size := dir.getSize()
	if (size <= 100000) {
		sum += size
	}
	for _,d := range dir.childDirs {
		sum += part1(*d)
	}
	return sum
}

func gatherPart2Candidate(missing int, dir Directory) []int {
	size := dir.getSize()
	options := make([]int, 0)
	if size >= missing {
		options = append(options, size)
		for _,d := range dir.childDirs {
			options = append(options, gatherPart2Candidate(missing, *d)...)
		}
	}
	return options
}

func part2(root Directory) int {
	size := root.getSize()
	available := 70000000 - size
	missing := 30000000 - available
	min := 100000000000000
	for _,candidate := range gatherPart2Candidate(missing, root) {
		if candidate < min {
			min = candidate
		}
	}
	return min
}

func main() {
    //dat, _ := os.ReadFile("day7.testinput")
    dat, _ := os.ReadFile("day7.input")
    root := parseFileTree(string(dat))
	println("part1: "+strconv.Itoa(part1(root)))
	println("part2: "+strconv.Itoa(part2(root)))
}
