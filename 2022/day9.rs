use std::fs;

struct Rope {
    knots: Vec<(i32, i32)>,
    tail_mem: Vec<(i32, i32)>,
}

fn is_touching(r: &Rope, h: usize, t: usize) -> bool {
    let dx = r.knots[t].0 - r.knots[h].0;
    let dy = r.knots[t].1 - r.knots[h].1;
    return dx.abs() <= 1 && dy.abs() <= 1;
}

fn update(r: &mut Rope, h: usize, t: usize, is_tail: bool) -> () {
    if is_touching(r, h, t) {
        return;
    }
    let dx = r.knots[h].0 - r.knots[t].0;
    let dy = r.knots[h].1 - r.knots[t].1;
    if dx != 0 && dy != 0 {
        r.knots[t].0 += dx.signum();
        r.knots[t].1 += dy.signum();
    } else {
        if dx.abs() > 1 {
            r.knots[t].0 += dx.signum();
        } else if dy.abs() > 1 {
            r.knots[t].1 += dy.signum();
        }
    }
    if is_tail && !r.tail_mem.contains(&r.knots[t]) {
        r.tail_mem.push(r.knots[t]);
    }
}

fn move_rope(r: &mut Rope, dx: i32, dy: i32) -> () {
    let delta = if dx != 0 { dx } else { dy };
    for _ in 0..delta.abs() {
        if dx != 0 {
            r.knots[0].0 += dx.signum();
        } else {
            r.knots[0].1 += dy.signum();
        }
        for t in 1..r.knots.len() {
            let h = t - 1;
            while !is_touching(r, h, t) {
                update(r, h, t, t == r.knots.len() - 1);
            }
        }
    }
    // let mut msg: String = "|rope|".to_owned();
    // for i in 0..r.knots.len() {
    //     msg.push_str(&format!(
    //         " knots[{}]: {},{} |",
    //         i, r.knots[i].0, r.knots[i].1
    //     ));
    // }
    // println!("{}", msg);
}

fn simulate(rope: &mut Rope, data: &str) -> () {
    let lines = data.split("\n");
    for line in lines {
        let linedata: Vec<&str> = line.split(" ").collect();
        let delta = linedata[1].parse::<i32>().unwrap();
        match linedata[0] {
            "U" => move_rope(rope, 0, delta),
            "D" => move_rope(rope, 0, -delta),
            "L" => move_rope(rope, -delta, 0),
            "R" => move_rope(rope, delta, 0),
            _ => panic!("unhandled command"),
        }
    }
}

fn main() {
    let data = fs::read_to_string("day9.input").expect("Should have been able to read the file");

    let mut rope = Rope {
        knots: vec![(0, 0), (0, 0)],
        tail_mem: vec![(0, 0)],
    };
    simulate(&mut rope, &data);
    println!("part1: {}", rope.tail_mem.len());

    let mut long_rope = Rope {
        knots: vec![
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
        ],
        tail_mem: vec![(0, 0)],
    };
    simulate(&mut long_rope, &data);
    println!("part2: {}", long_rope.tail_mem.len());
}
