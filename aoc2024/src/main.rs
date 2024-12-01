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
    lefts.sort_unstable();
    rights.sort_unstable();

    if part == 1 {
        zip(lefts, rights).map(|(left, right)| (left - right).abs()).sum::<i64>().to_string()
    } else {
        lefts.iter().map(|left|
            left * rights.iter().filter(|&right| left == right).count() as i64
        ).sum::<i64>().to_string()
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let days = [
      day1
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
