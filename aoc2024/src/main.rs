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

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let days = [
      day1, day2, day3, day4
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
