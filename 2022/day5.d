import std.container, std.stdio, std.file, std.array, std.conv, std.format;

struct CraneInstruction {
    int from;
    int to;
    int count;
}

alias Stack = char[];

Stack[] parseStacks(string data) {
    Stack[] stacks = new Stack[](0,0);
    for (int i=0; i<9; ++i) {
        stacks ~= new Stack(0);
    }

    foreach(string line; data.split("\n")) {
        for(int lineIndex=0; lineIndex<line.length; ++lineIndex) {
            char c = line[lineIndex];
            if (c >= 'A' && c <= 'Z') {
                int stackId = (lineIndex - 1) / 4;
                stacks[stackId] ~= c;
            }
        }
    }
    return stacks;
}

CraneInstruction[] parseInstructions(string data) {
    CraneInstruction[] instructions = new CraneInstruction[0];
    foreach(string line; data.split("\n")) {
        string[] words = line.split(" ");
        if (words.length == 6 && words[0] == "move") {
            CraneInstruction instruction;
            instruction.from = to!int(words[3]);
            instruction.to = to!int(words[5]);
            instruction.count = to!int(words[1]);
            instructions ~= instruction;
        }
    }
    return instructions;
}

void push(ref Stack s, char c) {
    s ~= c;
    int len = cast(int) s.length;
    for (int i=len-1; i > 0; --i) {
        char tmp = s[i-1];
        s[i-1] = s[i];
        s[i] = tmp;
    }
}

void pop(ref Stack s) {
    for (int i=1; i<s.length; ++i) {
        s[i-1] = s[i];
    }
    s.length = s.length - 1;
}

void apply9000(CraneInstruction instruction, ref Stack[] stacks) {
    Stack* fromStack = &stacks[instruction.from-1];
    Stack* toStack = &stacks[instruction.to-1];

    for (int i=0; i<instruction.count; ++i) {
        char c = cast(char) (*fromStack).front();
        push(*toStack, c);
        pop(*fromStack);
    }
}

void apply9001(CraneInstruction instruction, ref Stack[] stacks) {
    Stack* fromStack = &stacks[instruction.from-1];
    Stack* toStack = &stacks[instruction.to-1];

    Stack tmp;
    for (int i=0; i<instruction.count; ++i) {
        char c = cast(char) (*fromStack).front();
        push(tmp, c);
        pop(*fromStack);
    }
    for (int i=0; i<instruction.count; ++i) {
        char c = cast(char) tmp[i];
        push(*toStack, c);
    }
}

void main()
{
    string data = readText("day5.input");
    Stack[] stacks = parseStacks(data);
    CraneInstruction[] instructions = parseInstructions(data);
    foreach(CraneInstruction instruction; instructions) {
        apply9000(instruction, stacks);
    }

    string part1 = "";
    foreach(Stack s; stacks) {
        if (!s.empty()) {
            part1 ~= s.front();
        }
    }
    writeln(format("part1: %s", part1));

    Stack[] stacksAlt = parseStacks(data);
    foreach(CraneInstruction instruction; instructions) {
        apply9001(instruction, stacksAlt);
    }

    string part2 = "";
    foreach(Stack s; stacksAlt) {
        if (!s.empty()) {
            part2 ~= s.front();
        }
    }
    writeln(format("part2: %s", part2));
}
