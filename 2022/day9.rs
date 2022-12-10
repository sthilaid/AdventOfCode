use std::fs;

struct Rope {
    head: (i32, i32),
    tail: (i32, i32),
    tail_mem: Vec<(i32, i32)>,
}

fn is_touching(r: &Rope) -> bool {
    let dx = r.tail.0 - r.head.0;
    let dy = r.tail.1 - r.head.1;
    return dx.abs() <= 1 && dy.abs() <= 1;
}

fn fix_tail(r: &mut Rope) -> () {
    if is_touching(r) {
        return;
    }
    let dx = r.head.0 - r.tail.0;
    let dy = r.head.1 - r.tail.1;
    if dx != 0 && dy != 0 {
        r.tail.0 += dx.signum();
        r.tail.1 += dy.signum();
    } else {
        if dx.abs() > 1 {
            r.tail.0 += dx.signum();
        } else if dy.abs() > 1 {
            r.tail.1 += dy.signum();
        }
    }
    if !r.tail_mem.contains(&r.tail) {
        r.tail_mem.push(r.tail);
    }
}

fn move_rope(r: &mut Rope, dx: i32, dy: i32) -> () {
    r.head.0 += dx;
    r.head.1 += dy;
    while !is_touching(r) {
        fix_tail(r);
    }
    println!(
        "|rope| head: {},{} tail: {}, {}",
        r.head.0, r.head.1, r.tail.0, r.tail.1
    );
}

fn main() {
    let mut rope = Rope {
        head: (0, 0),
        tail: (0, 0),
        tail_mem: vec![(0, 0)],
    };
    let data = fs::read_to_string("day9.input").expect("Should have been able to read the file");
    let lines = data.split("\n");
    for line in lines {
        let linedata: Vec<&str> = line.split(" ").collect();
        let delta = linedata[1].parse::<i32>().unwrap();
        match linedata[0] {
            "U" => move_rope(&mut rope, 0, delta),
            "D" => move_rope(&mut rope, 0, -delta),
            "L" => move_rope(&mut rope, -delta, 0),
            "R" => move_rope(&mut rope, delta, 0),
            _ => panic!("unhandled command"),
        }
        //println!("{}", line);
    }
    println!("part1: {}", rope.tail_mem.len());
}
