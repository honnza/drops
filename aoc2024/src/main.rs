use bitvec::prelude::*;
use itertools::{Itertools, MinMaxResult};
use regex::{Regex};
use rustc_hash::{FxHashMap, FxHashSet};
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

fn day10(part: u8, input: &str) -> String {
    let input = input.trim().as_bytes();
    let w = input.iter().position(|&x| x == b'\n').unwrap() + 1;

    let mut bfs = Vec::with_capacity(400);
    let mut new_bfs = Vec::with_capacity(400);
    (0 .. input.len()).map(|ix_0| {
        if input[ix_0] != b'0' {return 0};
        bfs.clear();
        bfs.push(ix_0);
        for lv in b'1' ..= b'9' {
            new_bfs.clear();
            for &ix in &bfs {
                if ix >= w && input[ix - w] == lv {new_bfs.push(ix - w)};
                if ix >= 1 && input[ix - 1] == lv {new_bfs.push(ix - 1)};
                if ix < input.len() - 1 && input[ix + 1] == lv {new_bfs.push(ix + 1)};
                if ix < input.len() - w && input[ix + w] == lv {new_bfs.push(ix + w)};
                if part == 1 {
                    new_bfs.sort_unstable();
                    new_bfs.dedup();
                }
            }
            std::mem::swap(&mut new_bfs, &mut bfs);
        }
        bfs.len()
    }).sum::<usize>().to_string()
}

fn day11(part: u8, input: &str) -> String {
    let mut eval_cache = FxHashMap::default();

    fn eval(eval_cache: &mut FxHashMap<(u64, u8), u64>, stone: u64, steps: u8) -> u64 {
        if let Some(&r) = eval_cache.get(&(stone, steps)) {return r};
        let r = if steps == 0 {
            1
        } else if stone == 0 {
            eval(eval_cache, 1, steps - 1)
        } else {
            let nl = stone.ilog10() + 1;
            if nl % 2 == 0 {
                let d = 10u64.pow(nl / 2);
                eval(eval_cache, stone / d, steps - 1) + eval(eval_cache, stone % d, steps - 1)
            } else {
                eval(eval_cache, stone * 2024, steps - 1)
            }
        };

        eval_cache.insert((stone, steps), r);
        r
    }

    input.split_whitespace().map(|str|
        eval(&mut eval_cache, str.parse::<u64>().unwrap(), if part == 1 {25} else {75})
    ).sum::<u64>().to_string()
}

fn day12(part: u8, input: &str) -> String {
    let mut input = input.trim().as_bytes().to_owned();
    let w = input.iter().position(|&x| x == b'\n').unwrap() + 1;
    let mut dfs = Vec::new();

    let mut price_sum = 0u64;

    for ix in 0 .. input.len() {
        let plant @ b'A' ..= b'Z' = input[ix] else {continue};
        let mut area = 0;
        let mut inner_edges = 0;
        let mut straight_edges = 0;

        dfs.clear();
        dfs.push(ix);
        for dfs_ix in 0.. {
            let Some(&ix) = dfs.get(dfs_ix) else {break};
            if input[ix] != plant {continue};
            input[ix] = b',';
            area += 1;
            if ix >= w && input[ix - w] == plant {inner_edges += 1; dfs.push(ix - w)};
            if ix >= 1 && input[ix - 1] == plant {inner_edges += 1; dfs.push(ix - 1)};
            if input.get(ix + 1) == Some(&plant) {inner_edges += 1; dfs.push(ix + 1)};
            if input.get(ix + w) == Some(&plant) {inner_edges += 1; dfs.push(ix + w)};

            if part > 1 {
                let nw = ix > w && (input[ix - w - 1] == plant || input[ix - w - 1] == b',');
                let n = ix >= w && (input[ix - w] == plant || input[ix - w] == b',');
                let we = ix >= 1 && (input[ix - 1] == plant || input[ix - 1] == b',');
                if !nw && (n != we) {straight_edges += 1};
                let se = input.get(ix + w + 1) == Some(&plant) || input.get(ix + w + 1) == Some(&b',');
                let s = input.get(ix + w) == Some(&plant) || input.get(ix + w) == Some(&b',');
                let e = input.get(ix + 1) == Some(&plant) || input.get(ix + 1) == Some(&b',');
                if !se && (s != e) {straight_edges += 1};
            }
        }
        if part > 1 {for &ix in &dfs {input[ix] = b' '}};
        let fences = 4 * area - 2 * inner_edges - straight_edges;
        price_sum += fences * area;
    }

    price_sum.to_string()
}

fn day13(part: u8, input: &str) -> String {
    let mut input = input.chars();
    let input = input.by_ref();
    let mut res = 0;
    loop {
        if !input.any(|c| c == '+') {break};
        let ax = 10 * input.next().unwrap().to_digit(10).unwrap() as i64
            + input.next().unwrap().to_digit(10).unwrap() as i64;
        input.find(|&c| c == '+').unwrap();
        let ay = 10 * input.next().unwrap().to_digit(10).unwrap() as i64
            + input.next().unwrap().to_digit(10).unwrap() as i64;
        input.find(|&c| c == '+').unwrap();
        let bx = 10 * input.next().unwrap().to_digit(10).unwrap() as i64
            + input.next().unwrap().to_digit(10).unwrap() as i64;
        input.find(|&c| c == '+').unwrap();
        let by = 10 * input.next().unwrap().to_digit(10).unwrap() as i64
            + input.next().unwrap().to_digit(10).unwrap() as i64;
        input.find(|&c| c == '=').unwrap();
        let mut px = 0;
        input.take_while(|c| c.is_ascii_digit()).for_each(|c|
            px = 10 * px + c.to_digit(10).unwrap() as i64
        );
        input.find(|&c| c == '=').unwrap();
        let mut py = 0;
        input.take_while(|c| c.is_ascii_digit()).for_each(|c|
            py = 10 * py + c.to_digit(10).unwrap() as i64
        );
        if part > 1 {px += 10_000_000_000_000; py += 10_000_000_000_000}

        if ax * by == bx * ay {panic!("TODO: collinear case: {:?}", (ax, ay, bx, by, px, py))}
        let ran = px * by - py * bx;
        let rbn = ax * py - ay * px;
        let rde = ax * by - ay * bx;
        res += if ran % rde != 0 || rbn % rde != 0 || ran / rde < 0 || rbn / rde < 0 {
            0
        } else {
            3 * ran / rde + rbn / rde
        };
    }
    res.to_string()
}

fn day14(part: u8, input: &str) -> String {
    let is_test = dbg!(!input.ends_with("\n\n"));
    let w = if is_test {11} else {101};
    let h = if is_test {7} else {103};

    let mut ulq = 0;
    let mut urq = 0;
    let mut blq = 0;
    let mut brq = 0;
    let robots = input.trim().lines().map(|line| {
        let [pxy, vxy] = &line.split_whitespace().collect::<Vec<_>>()[..] else {panic!("{}", line)};
        let ["p", pxy] = &pxy.split('=').collect::<Vec<_>>()[..] else {panic!("{}", line)};
        let [px, py] = &pxy.split(',').collect::<Vec<_>>()[..] else {panic!("{}", line)};
        let px: i64 = px.parse().unwrap();
        let py: i64 = py.parse().unwrap();
        let ["v", vxy] = &vxy.split('=').collect::<Vec<_>>()[..] else {panic!("{}", line)};
        let [vx, vy] = &vxy.split(',').collect::<Vec<_>>()[..] else {panic!("{}", line)};
        let vx: i64 = vx.parse().unwrap();
        let vy: i64 = vy.parse().unwrap();
        (px, py, vx, vy)
    }).collect::<Vec<_>>();

    if part == 1 {
        for (px, py, vx, vy) in robots {
            let fpx = (px + 100 * vx).rem_euclid(w);
            let fpy = (py + 100 * vy).rem_euclid(h);
            if fpx < w / 2 && fpy < h / 2 {ulq += 1}
            else if fpx > w / 2 && fpy < h / 2 {urq += 1}
            else if fpx < w / 2 && fpy > h / 2 {blq += 1}
            else if fpx > w / 2 && fpy > h / 2 {brq += 1};
        }
        (ulq * urq * blq * brq).to_string()
    } else {
        let mut r = vec![];
        let mut buf = vec![b'.'; (w * h) as usize];
        't: for t in 0 .. w * h {
            for c in &mut buf {*c = b'.'};
            for &(px, py, vx, vy) in &robots {
                let fpx = (px + t * vx).rem_euclid(w);
                let fpy = (py + t * vy).rem_euclid(h);
                buf[(fpx + fpy * w) as usize] += 1;
                if buf[(fpx + fpy * w) as usize] > b'.' + 1 {continue 't};
            }
            r.push(t);
        }
        if r.len() == 1 {r[0].to_string()} else {"?".to_string()}
    }
}
    
fn day15(part: u8, input: &str) -> String {
    let &[map, moves] = &input.trim().split("\n\n").collect::<Vec<_>>()[..] else {
        panic!("expected exactly two paragraphs");
    };
    let mut map = map.as_bytes().to_owned();
    let mut w = map.iter().position(|&c| c == b'\n').unwrap() + 1;
    let mut bot_pos = map.iter().position(|&c| c == b'@').unwrap() as isize;
    map[bot_pos as usize] = b'.';

    if part == 1 {
        for m in moves.bytes() {
            let m = match m {
                b'^' => -(w as isize), b'>' => 1, b'v' => w as isize, b'<' => -1,
                b'\n' => continue, m => panic!("unknown move {m}")
            };
            let mut push_until = bot_pos + m;
            while map[push_until as usize] == b'O' {push_until += m};
            if map[push_until as usize] == b'#' {continue};
            map[push_until as usize] = b'O';
            map[(bot_pos + m) as usize] = b'.';
            bot_pos += m;
        }
    } else {
        map = map.into_iter().flat_map(|c|
            match c {
                b'.' | b'@' => b"..", b'O' => b"[]", b'#' => b"##", b'\n' => b"\n\n",
                _ => panic!("unknown map tile {}", c as char)
            }
        ).copied().collect::<Vec<_>>();
        bot_pos *= 2;
        w *= 2;

        let mut pushed_crates = vec![];
        'moves: for m in moves.bytes() {
            let m = match m {
                b'^' => -(w as isize), b'>' => 1, b'v' => w as isize, b'<' => -1,
                b'\n' => continue, m => panic!("unknown move {m}")
            };

            pushed_crates.clear();
            match map[(bot_pos + m) as usize] {
                b'#' => continue,
                b'[' => pushed_crates.push(bot_pos + m),
                b']' => pushed_crates.push(bot_pos + m - 1),
                _ => ()
            }
            for pc_ix in 0.. {
                let Some(&crate_pos) = pushed_crates.get(pc_ix) else {break};
                match map[(crate_pos + m) as usize] {
                    b'#' => continue 'moves,
                    b'[' => pushed_crates.push(crate_pos + m),
                    b']' if m != 1 => pushed_crates.push(crate_pos + m - 1),
                    _ => ()
                }
                match map[(crate_pos + m + 1) as usize] {
                    b'#' => continue 'moves,
                    b'[' if m != -1 => pushed_crates.push(crate_pos + m + 1),
                    _ => ()
                }
            }

            bot_pos += m;
            for &crate_pos in pushed_crates.iter().rev() {
                map[crate_pos as usize] = b'.';
                map[(crate_pos + 1) as usize] = b'.';
                map[(crate_pos + m) as usize] = b'[';
                map[(crate_pos + m + 1) as usize] = b']';
            }
        }
    }

    map.iter().positions(|&c| c == b'O' || c == b'[').map(|pos|
        100 * (pos / w) + pos % w
    ).sum::<usize>().to_string()
}

fn day16(part: u8, input: &str) -> String {
    let input = input.as_bytes();
    let w = (input.iter().position(|&c| c == b'\n').unwrap() + 1) as isize;
    let start_pos = input.iter().position(|&c| c == b'S').unwrap() as isize;

    let mut to_check = vec![(start_pos, 1)];
    let mut node_cost = FxHashMap::default();
    node_cost.insert((start_pos, 1), 0);
    let mut node_source = FxHashMap::default();
    node_source.insert((start_pos, 1), vec![]);

    let mut checked = FxHashSet::default();
    let (end_pos, end_dir) = loop {
        let node_ix = (0 .. to_check.len()).min_by_key(|&node_ix|
            node_cost[&to_check[node_ix]]
        ).expect("exit not found");
        let (pos, dir) = to_check[node_ix];
        let cost = node_cost[&(pos, dir)];
        to_check.swap_remove(node_ix);
        let dir_cw = if dir == 1 {-w}
                     else if dir == -w {-1}
                     else if dir == -1 {w}
                     else if dir == w {1}
                     else {unreachable!()};
        if checked.contains(&(pos, dir)) {continue};
        if input[pos as usize] == b'E' {break (pos, dir)};
        checked.insert((pos, dir));

        match node_cost.get(&(pos, dir_cw)) {
            Some(&old_cost) if old_cost < cost + 1000 => {}
            Some(&old_cost) if old_cost == cost + 1000 =>
                node_source.get_mut(&(pos, dir_cw)).unwrap().push((pos, dir)),
            Some(_) => {
                node_cost.insert((pos, dir_cw), cost + 1000);
                node_source.insert((pos, dir_cw), vec![(pos, dir)]);
            }
            None => {
                to_check.push((pos, dir_cw));
                node_cost.insert((pos, dir_cw), cost + 1000);
                node_source.insert((pos, dir_cw), vec![(pos, dir)]);
            }
        }

        match node_cost.get(&(pos, -dir_cw)) {
            Some(&old_cost) if old_cost < cost + 1000 => {}
            Some(&old_cost) if old_cost == cost + 1000 =>
                node_source.get_mut(&(pos, -dir_cw)).unwrap().push((pos, dir)),
            Some(_) => {
                node_cost.insert((pos, -dir_cw), cost + 1000);
                node_source.insert((pos, -dir_cw), vec![(pos, dir)]);
            }
            None => {
                to_check.push((pos, -dir_cw));
                node_cost.insert((pos, -dir_cw), cost + 1000);
                node_source.insert((pos, -dir_cw), vec![(pos, dir)]);
            }
        }

        if input[(pos + dir) as usize] == b'#' {continue};
        let mut new_pos = pos;
        let mut new_cost = cost;
        loop {
            new_pos += dir;
            new_cost += 1;
            if input[(new_pos - dir_cw) as usize] != b'#'
                || input[(new_pos + dir_cw) as usize] != b'#' 
                || input[(new_pos + dir) as usize] == b'#'
            {break};
        }
        match node_cost.get(&(new_pos, dir)) {
            Some(&old_cost) if old_cost < new_cost => {}
            Some(&old_cost) if old_cost == new_cost =>
                node_source.get_mut(&(new_pos, dir)).unwrap().push((pos, dir)),
            Some(_) => {
                node_cost.insert((new_pos, dir), new_cost);
                node_source.insert((new_pos, dir), vec![(pos, dir)]);
            }
            None => {
                to_check.push((new_pos, dir));
                node_cost.insert((new_pos, dir), new_cost);
                node_source.insert((new_pos, dir), vec![(pos, dir)]);
            }
        }
    };

    if part == 1 {
        node_cost[&(end_pos, end_dir)].to_string()
    } else {
        let mut input = input.to_owned();
        input[end_pos as usize] = b'O';
        let mut path_scan = vec![(end_pos, end_dir)];
        for ps_ix in 0 .. {
            let Some(&(pos, dir)) = path_scan.get(ps_ix) else {break};
            for &(prev_pos, prev_dir) in &node_source[&(pos, dir)] {
                path_scan.push((prev_pos, prev_dir));
                let mut pos_iter = prev_pos;
                while pos_iter != pos {
                    input[pos_iter as usize] = b'O';
                    pos_iter += prev_dir;
                }
            }
        }

        input.iter().filter(|&&c| c == b'O').count().to_string()
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let days = [
        day1, day2, day3, day4, day5, day6, day7, day8, day9, day10, day11, day12, day13, day14,
        day15, day16
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
