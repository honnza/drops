use itertools::{Itertools, MinMaxResult};
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
            let compensate_at =
                if diff_left.is_none() {trend_breaker + 1}
                else if diff_right.is_none() {trend_breaker - 1}
                else if diff_left.unwrap().abs() < diff_right.unwrap().abs() {trend_breaker + 1}
                else {trend_breaker - 1};
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

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let days = [
      day1, day2
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
