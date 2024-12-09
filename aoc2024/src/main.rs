use bitvec::prelude::*;
use itertools::{Itertools, MinMaxResult};
use regex::{Regex};
use std::iter::zip;

fn day1(part: u8, input: &str) -> String {
    let mut lefts: Vec<i64> = vec![];
    let mut rights: Vec<i64> = vec![];
    for line in input.trim().lines() {
        let strs: Vec<&str> = line.split_whitespace().collect();
        let [left, right] = &strs[..] else {panic!("cannot parse {} as two things", line)};
        lefts.push(left.parse().expect(left));
        rights.push(right.parse().expect(right));
    }

    if part == 1 {
        lefts.sort_unstable();
        rights.sort_unstable();
        zip(lefts, rights).map(|(left, right)| (left - right).abs()).sum::<i64>().to_string()
    } else {
        lefts.iter().map(|left|
            left * rights.iter().filter(|&right| left == right).count() as i64
        ).sum::<i64>().to_string()
    }
}

fn day2(part: u8, input: &str) -> String {
    input.trim().lines().filter(|line| {
        let diffs =
            line.split_ascii_whitespace()
                .map(|n| n.parse::<isize>().unwrap())
                .tuple_windows()
                .map(|(x, y)| x - y);
        if part == 1 {
            let MinMaxResult::MinMax(min, max) = diffs.minmax() else {
                panic!("expected more than one element in {}", line)
            };
            min > -4 && max < 0 || min > 0 && max < 4
        } else {
            let mut diffs: Vec<isize> = diffs.collect();
            let trend = match diffs[0..3].iter().map(|x| x.signum()).collect::<Vec<_>>()[..] {
                [-1, -1, _] | [-1, _, -1] | [_, -1, -1] => -1,
                [ 1,  1, _] | [ 1, _,  1] | [_,  1,  1] =>  1,
                _ => return false
            };
            let trend_breaker = diffs.iter().positions(|&x| x.signum() != trend).exactly_one();
            if trend_breaker.is_err() {
                // either vanilla safe, too large gap without opposite step, or too wobbly
                // ... but that can still be fine if the gap is at one end
                if diffs[0].abs() > 3 {
                    diffs.swap_remove(0);
                } else if diffs[diffs.len() - 1].abs() > 3 {
                    diffs.remove(diffs.len() - 1);
                }
                let MinMaxResult::MinMax(&min, &max) = diffs.iter().minmax() else {
                    panic!("expected more than one element in {}", line)
                };
                return min > -4 && max < 0 || min > 0 && max < 4;
            };

            let trend_breaker = trend_breaker.unwrap();
            let diff_left = if trend_breaker == 0 {None} else {Some(diffs[trend_breaker - 1])};
            let diff_right = diffs.get(trend_breaker + 1).copied();
            let compensate_at = match (diff_left, diff_right) {
                (None, _) => trend_breaker + 1,
                (_, None) => trend_breaker - 1,
                (Some(left), Some(right)) if left.abs() < right.abs() => trend_breaker + 1,
                _ => trend_breaker - 1
            };
            if diff_left.is_some() && diff_right.is_some() || diffs[compensate_at].abs() > 3 {
                diffs[compensate_at] += diffs[trend_breaker];
            }
            diffs.swap_remove(trend_breaker);
            let MinMaxResult::MinMax(&min, &max) = diffs.iter().minmax() else {
                panic!("expected more than one element in {}", line)
            };
            min > -4 && max < 0 || min > 0 && max < 4
        }
    }).count().to_string()
}

fn day3(part: u8, input: &str) -> String {
    let mut enabled = true;
    Regex::new(r"mul\((\d+),(\d+)\)|do\(\)|don't\(\)").unwrap()
        .captures_iter(input)
        .map(|m| {
            match &m[0] {
                "do()" => {enabled = true; 0},
                "don't()" => {if part == 2 {enabled = false}; 0},
                _ => if enabled {
                    m[1].parse::<u64>().unwrap() * m[2].parse::<u64>().unwrap()
                } else {0}
            }
        })
        .sum::<u64>().to_string()
}

fn day4(part: u8, input: &str) -> String {
    let input = input.trim().lines().map(|line| line.as_bytes()).collect::<Vec<_>>();
    let input = &input;
    if part == 1 {
        (0 .. input.len()).flat_map(|ri| {
            (0 .. input[ri].len()).map(move |ci| {
                if input[ri][ci] == b'X' {
                    (-1i8 ..= 1).flat_map(|rd| {
                        (-1i8 ..= 1).filter(move |&cd| {
                            input.get(ri + rd as usize).and_then(|row|
                                row.get(ci + cd as usize)
                            ) == Some(&b'M') &&
                            input.get(ri + 2 * rd as usize).and_then(|row|
                                row.get(ci + 2 * cd as usize)
                            ) == Some(&b'A') &&
                            input.get(ri + 3 * rd as usize).and_then(|row|
                                row.get(ci + 3 * cd as usize)
                            ) == Some(&b'S')
                        })
                    }).count()
                } else {0}
            })
        }).sum::<usize>().to_string()
    } else {
        (1 .. input.len() - 1).flat_map(|ri| {
            (1 .. input[ri].len() - 1).filter(move |&ci| {
                input[ri][ci] == b'A' && (
                    (input[ri - 1][ci - 1] == b'M' && input[ri + 1][ci + 1] == b'S') ||
                    (input[ri + 1][ci + 1] == b'M' && input[ri - 1][ci - 1] == b'S')
                ) && (
                    (input[ri - 1][ci + 1] == b'M' && input[ri + 1][ci - 1] == b'S') ||
                    (input[ri + 1][ci - 1] == b'M' && input[ri - 1][ci + 1] == b'S')
                )
            })
        }).count().to_string()
    }
}

fn day5(part: u8, input: &str) -> String {
    let mut lines = input.trim().lines();
    let mut rules = bitarr![0; 10000];
    for line in lines.by_ref().take_while(|&line| !line.is_empty()){
        let [x, y] = line.split("|").map(|s|
            s.parse::<usize>().unwrap()
        ).collect::<Vec<_>>()[..] else {
            panic!("cannot parse line {}", line)
        };
        rules.set(100 * x + y, true);
    }

    let lines = lines.map(|line|
        line.split(",").map(|s| s.parse::<usize>().unwrap()).collect::<Vec<_>>()
    );

    if part == 1 {
        lines.filter(|line|
            (0 .. line.len()).all(|x| (x + 1 .. line.len()).all(|y| !rules[100 * line[y] + line[x]]))
        ).map(|line|
            line[line.len() / 2]
        ).sum::<usize>().to_string()
    } else {
        lines.filter_map(|line| {
            let mut unsorted = line.clone();
            let mut sorted = Vec::with_capacity(unsorted.len());
            while !unsorted.is_empty() {
                let x_at = unsorted.iter().position(|&x|
                    unsorted.iter().all(|&y| !rules[100 * y + x])
                ).unwrap();
                sorted.push(unsorted[x_at]);
                unsorted.swap_remove(x_at);
            }
            if sorted == line {None} else {Some(sorted[sorted.len() / 2])}
        }).sum::<usize>().to_string()
    }
}

fn day6(part: u8, input: &str) -> String {
    let input = input.trim().lines().map(|line| line.as_bytes()).collect::<Vec<_>>();
    let mut input = input.into_iter().map(|x| x.to_owned()).collect::<Vec<_>>();
    let guard_origin = (0 .. input.len()).find_map(|ri|
        input[ri].iter().position(|&x| x != b'.' && x != b'#').map(|ci| (ri, ci))
    ).unwrap();
    let (mut guard_ri, mut guard_ci) = guard_origin;
    let mut guard_dir = input[guard_ri][guard_ci];

    loop {
        input[guard_ri][guard_ci] = b'X';
        let (next_ri, next_ci) = match guard_dir {
            b'^' => (guard_ri - 1, guard_ci), b'>' => (guard_ri, guard_ci + 1),
            b'v' => (guard_ri + 1, guard_ci), b'<' => (guard_ri, guard_ci - 1),
            dir => panic!("unexpected guard direction '{}'", dir as char)
        };

        match input.get(next_ri).and_then(|row| row.get(next_ci)) {
            Some(b'#') => {
                guard_dir = match guard_dir {
                    b'^' => b'>', b'>' => b'v', b'v' => b'<', b'<' => b'^',
                    _ => unreachable!()
                };
            },
            Some(b'.') | Some(b'X') => {
                guard_ri = next_ri; guard_ci = next_ci;
            },
            None => break,
            Some(x) => panic!("unexpected board cell {}", x)
        };
    }

    if part == 1 {
        input.iter().flat_map(|row| row.iter().filter(|&&x| x == b'X')).count().to_string()
    } else {
        (0 .. input.len()).flat_map(|ri|
            (0 .. input[ri].len()).map(move |ci| (ri, ci))
        ).filter(|&(ri, ci)|
            (ri, ci) != guard_origin && input[ri][ci] == b'X'
        ).filter(|&(obstacle_ri, obstacle_ci)| {
            let mut input_clone = input.clone();
            input_clone[obstacle_ri][obstacle_ci] = b'#';
            let (mut guard_ri, mut guard_ci) = guard_origin;
            guard_dir = b'^';

            loop {
                input_clone[guard_ri][guard_ci] = match input_clone[guard_ri][guard_ci] {
                    b'.' | b'X' => guard_dir,
                    dir if dir == guard_dir => break true,
                    b'^' | b'>' | b'v' | b'<' => b'2',
                    b'2' => b'3', b'3' => b'4', b'4' => break true,
                    x => panic!("unexpected board tile {}", x as char)
                };
                let (next_ri, next_ci) = match guard_dir {
                    // TODO: memorize (obstacle x dir) => next turn
                    b'^' => (guard_ri - 1, guard_ci), b'>' => (guard_ri, guard_ci + 1),
                    b'v' => (guard_ri + 1, guard_ci), b'<' => (guard_ri, guard_ci - 1),
                    dir => panic!("unexpected guard direction '{}'", dir as char)
                };

                match input_clone.get(next_ri).and_then(|row| row.get(next_ci)) {
                    Some(b'#') => {
                        guard_dir = match guard_dir {
                            b'^' => b'>', b'>' => b'v', b'v' => b'<', b'<' => b'^',
                            _ => unreachable!()
                        };
                    },
                    Some(_) => {
                        guard_ri = next_ri; guard_ci = next_ci;
                    },
                    None => break false,
                };
            }
        }).count().to_string()
    }
}

fn day7(part: u8, input: &str) -> String {
    let mut bfs = Vec::new();
    let mut new_bfs = Vec::new();
    input.trim().lines().filter_map(|line| {
        let (goal, bits) = line.split_once(": ").unwrap();
        let goal = goal.parse::<usize>().unwrap();
        let bits = bits.split_whitespace().map(|x| x.parse::<usize>().unwrap()).collect::<Vec<_>>();
        let decade = bits.iter().map(|bit| 10usize.pow(bit.ilog10() + 1)).collect::<Vec<_>>();
        bfs.clear();
        bfs.push(bits[0]);
        for bi in 1 .. bits.len() {
            let bit = bits[bi];
            new_bfs.clear();
            for r in &bfs {
                if r + bit <= goal {new_bfs.push(r + bit)}
                if r * bit <= goal {new_bfs.push(r * bit)}
                let new_r = r * decade[bi] + bit;
                if new_r <= goal && part > 1 {new_bfs.push(new_r)};
            }
            std::mem::swap(&mut new_bfs, &mut bfs);
        }
        bfs.iter().any(|&r| r == goal).then_some(goal)
    }).sum::<usize>().to_string()
}

fn day8(part: u8, input: &str) -> String {
    let mut freqs = [const{vec![]}; 62];
    for (ri, line) in input.trim().lines().enumerate() {
        for (ci, cell) in line.as_bytes().iter().enumerate() {
            match cell {
                b'0' ..= b'9' => freqs[(cell - b'0') as usize].push((ri as isize, ci as isize)),
                b'A' ..= b'Z' => freqs[(cell - b'A' + 10) as usize].push((ri as isize, ci as isize)),
                b'a' ..= b'z' => freqs[(cell - b'a' + 36) as usize].push((ri as isize, ci as isize)),
                b'.' => (),
                _ => panic!("unexpected character {} at ({}, {})", cell, ri, ci)
            }
        }
    }
    let rows = input.trim().lines().count();
    let cols = input.lines().next().unwrap().len();

    freqs.iter().flat_map(|antennas|
        antennas.iter().flat_map(move |rci @ (ri, ci)|
            antennas.iter().flat_map(move |rcj @ (rj, cj)|
                ((if part == 1 {1} else {0}) ..).map_while(move |n| {
                          let rij = ri + n * (ri - rj);
                          let cij = ci + n * (ci - cj);
                          (
                              (0 .. rows as isize).contains(&rij) &&
                              (0 .. cols as isize).contains(&cij) &&
                              rci != rcj && (n == 1 || part > 1)
                          ).then_some((rij, cij))
                      })
            )
        )
    ).unique().count().to_string()
}

fn day9(part: u8, input: &str) -> String {
    let mut blocks = Vec::with_capacity(9 * input.len());
    let mut input = input.trim().as_bytes().iter();
    for id in 0 .. {
        let Some(&n) = input.next() else {break};
        for _ in 0 .. n - b'0' {blocks.push(id as i16)};
        let Some(&n) = input.next() else {break};
        for _ in 0 .. n - b'0' {blocks.push(-1)};
    }

    if part == 1 {
        let mut block_iter = blocks.iter_mut();
        loop {
            let Some(left) = block_iter.find(|n| **n == -1) else {break};
            let Some(right) = block_iter.rfind(|n| **n != -1) else {break};
            *left = *right;
            *right = -1;
        }
    } else {
        let mut gap_iters = [0; 9];
        let mut file_iter = blocks.len() - 1;
        let mut id_seen = -1;
        'compact: while id_seen != 0 {
            while blocks[file_iter] as u16 >= id_seen as u16 {
                if file_iter == 0 {break 'compact};
                file_iter -= 1;
            };
            let file_end = file_iter;
            id_seen = blocks[file_iter];
            while file_iter > 0 && blocks[file_iter - 1] == id_seen {file_iter -= 1};
            let file_start = file_iter;
            let file_sz = file_end - file_start + 1;

            for i in 1 .. file_sz {
                if gap_iters[file_sz - 1] > gap_iters[i] {gap_iters[file_sz - 1] = gap_iters[i]};
            }
            let gap_iter = &mut gap_iters[file_sz - 1];

            while *gap_iter < file_start {
                let Some(nongap_at) = (*gap_iter .. *gap_iter + file_sz).rfind(|&ix|
                    blocks[ix] != -1
                ) else {break};
                *gap_iter = nongap_at + 1;
            }

            if *gap_iter < file_start {
                blocks[*gap_iter ..][.. file_sz].fill(id_seen);
                blocks[file_start ..][.. file_sz].fill(-1);
            }
        }
    }

    blocks.iter().enumerate().filter(|&(_, &id)| id != -1)
          .map(|(pos, &id)| pos * id as usize).sum::<usize>().to_string()
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let days = [
      day1, day2, day3, day4, day5, day6, day7, day8, day9
    ];

    let args = std::env::args().collect::<Vec<_>>();
    let (day_arg, part_arg, fname) = match &args[..] {
        [_, day_arg, part_arg] => (day_arg, part_arg, format!("day{}.in", day_arg)),
        [_, day_arg, test_arg, part_arg] => (day_arg, part_arg, format!("day{}test{}.in", day_arg, test_arg)),
        _ => {
            println!("exactly two or three arguments expected - day number, optionally test number and 1/2 for part");
            std::process::exit(1);
        }
    };

    assert!(part_arg == "1" || part_arg == "2");
    let day: usize = day_arg.parse()?;
    let input = std::fs::read_to_string(dbg!(fname))?;
    let time = std::time::Instant::now();
    println!("{}", days[day - 1](part_arg.parse()?, &input));
    println!("{} seconds elapsed", time.elapsed().as_secs_f32());
    Ok(())
}
