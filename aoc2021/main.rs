use std::collections::{HashMap, HashSet, VecDeque};

fn str_split_once<'a>(s: &'a str, pat: &str) -> Option<(&'a str, &'a str)> {
    match s.split(pat).collect::<Vec<_>>()[..] {
        [x, y] => Some((x, y)),
        _ => None
    }
}

fn gcd(a: u64, b: u64) -> u64 {if b == 0 {a} else {gcd(b, a % b)}}
fn lcm(a: u64, b: u64) -> u64 {a / gcd(a, b) * b}

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
enum Op {Acc(isize), Jmp(isize), Nop(isize)}
#[derive(PartialEq)]
enum RunState {Running, Success, JumpError, Looped}
struct AocCpu<'a> {code: &'a Vec<Op>, state: RunState, ip: isize, acc: isize, lines_visited: Vec<bool>}
impl AocCpu<'_> {
    fn new(code: &Vec<Op>) -> AocCpu { AocCpu {
        lines_visited: vec![false; code.len()], code, state: RunState::Running, ip: 0, acc: 0
    }}
    
    fn parse_asm(s: &str) -> Vec<Op>{
        s.lines().map(|line| {
            let (op_str, arg_str) = str_split_once(line, " ").expect(line);
            let arg = arg_str.parse().expect(line);
            match op_str {
                "acc" => Op::Acc(arg),
                "jmp" => Op::Jmp(arg),
                "nop" => Op::Nop(arg),
                _ => panic!("{}", line)
            }
        }).collect()
    }
    
    fn run(&mut self) {while self.state == RunState::Running {self.step()}}

    fn step(&mut self){
        assert!(self.state == RunState::Running);
        self.lines_visited[self.ip as usize] = true;
        match self.code[self.ip as usize] {
            Op::Acc(arg) => {
                self.acc += arg;
                self.ip += 1
            },
            Op::Jmp(arg) => {
                self.ip += arg
            },
            Op::Nop(_) => {
                self.ip += 1
            },
        }
        if self.ip < 0 || (self.ip as usize) > self.code.len() {self.state = RunState::JumpError}
        else if (self.ip as usize) == self.code.len() {self.state = RunState::Success}
        else if self.lines_visited[self.ip as usize] {self.state = RunState::Looped};
    }
}

////////////////////////////////////////////////////////////////////////////////

fn day1(part: char, s: &str) -> String {
    let mut nums = s.lines().map(|l| l.parse().unwrap());
    let mut increases = 0u32;
    if part == 'a' {
        let mut prev: u32 = nums.next().unwrap();
        while let Some(next) = nums.next() {
            if next > prev {increases += 1};
            prev = next;
        }
    } else {
        let mut p3 = nums.next().unwrap();
        let mut p2 = nums.next().unwrap();
        let mut p1 = nums.next().unwrap();
        while let Some(next) = nums.next() {
            if next > p3 {increases += 1};
            p3 = p2;
            p2 = p1;
            p1 = next;
        }
    }
    increases.to_string()
}

fn day2(part: char, s: &str) -> String {
    let mut x = 0i32;
    let mut y = 0i32;
    let mut aim = 0i32;
    for line in s.lines() {
        let (word, num_str) = str_split_once(line, " ").unwrap();
        let num: i32 = num_str.parse().unwrap();
        match (part, word) {
            ('a', "forward") => {x += num},
            ('a', "down") => {y += num},
            ('a', "up") => {y -= num},
            (_, "forward") => {x += num; y += num * aim},
            (_, "down") => {aim += num},
            (_, "up") => {aim -= num},
            _ => panic!()
        }
    }
    (x * y).to_string()
}

fn day3(part: char, s: &str) -> String {
    let n_bits = s.lines().next().unwrap().len();
    if part == 'a' {
        let mut zero_count = vec![0; n_bits];
        let mut one_count = vec![0; n_bits];
        for line in s.lines() {
            for (ix, byte) in line.bytes().enumerate(){
                match byte {
                    b'0' => zero_count[ix] += 1,
                    b'1' => one_count[ix] += 1,
                    _ => panic!()
                }
            }
        }
        let gamma = (0 .. n_bits).fold(0, |a, ix| 2 * a + (one_count[ix] >= zero_count[ix]) as usize);
        let eps = (1 << n_bits) - 1 - gamma;
        println!("zeros: {:?}, ones: {:?}, gamma: {:b}, eps: {:b}", zero_count, one_count, gamma, eps);
        (gamma * eps).to_string()
    } else {
        let (nums_zero, nums_one): (Vec<&str>, Vec<&str>) =
            s.lines().partition(|line| line.as_bytes()[0] == b'0');
        let (mut nums_oxy, mut nums_co2) = if nums_one.len() >= nums_zero.len() {
            (nums_one, nums_zero)
        } else {
            (nums_zero, nums_one)
        };

        for ix in 1 .. n_bits {
            let n_ones = nums_oxy.iter().filter(|n| n.as_bytes()[ix] == b'1').count();
            let to_retain = if n_ones >= (nums_oxy.len() - n_ones) {b'1'} else {b'0'};
            nums_oxy.retain(|n| n.as_bytes()[ix] == to_retain);

            let n_ones = nums_co2.iter().filter(|n| n.as_bytes()[ix] == b'1').count();
            let to_retain = if n_ones == 0 {b'0'}
                            else if n_ones == nums_co2.len() {b'1'}
                            else if n_ones >= (nums_co2.len() - n_ones) {b'0'} 
                            else {b'1'};
            
            nums_co2.retain(|n| n.as_bytes()[ix] == to_retain);

            println!("{} {} {}", ix, nums_oxy.len(), nums_co2.len());
        }
        assert_eq!(nums_oxy.len(), 1);
        assert_eq!(nums_co2.len(), 1);
        println!("{} {}", nums_oxy[0], nums_co2[0]);
        (usize::from_str_radix(nums_oxy[0], 2).unwrap() * usize::from_str_radix(nums_co2[0], 2).unwrap()).to_string()
    }
}

fn day4(part: char, s: &str) -> String {
    const ROWS: [u32; 10] = [
        0b_00000_00000_00000_00000_11111,
        0b_00000_00000_00000_11111_00000,
        0b_00000_00000_11111_00000_00000,
        0b_00000_11111_00000_00000_00000,
        0b_11111_00000_00000_00000_00000,
        0b_00001_00001_00001_00001_00001,
        0b_00010_00010_00010_00010_00010,
        0b_00100_00100_00100_00100_00100,
        0b_01000_01000_01000_01000_01000,
        0b_10000_10000_10000_10000_10000,
    ];
    let mut lines = s.lines().peekable();
    let draws: Vec<u8> = lines.next().unwrap().split(','
                                ).map(|s| s.parse().unwrap()).collect();
    let mut board: Vec<u8> = Vec::with_capacity(25);

    let mut best_time = None;
    let mut best_score = 0;
    while lines.next().is_some() {
        board.clear();
        for _ in 0 .. 5 {
            board.extend(
                lines.next().unwrap().split_ascii_whitespace()
                        .map(|s| s.parse::<u8>().unwrap())
            );
        }
        assert_eq!(board.len(), 25);
        let mut board_seen = 0u32;
        for (t, &draw) in draws.iter().enumerate() {
            if let Some(ix) = board.iter().position(|&b| b == draw) {
                board_seen |= 1 << ix;
                if ROWS.iter().any(|&row| board_seen & row == row) {
                    if best_time.is_none() || (part == 'b') ^ (best_time.unwrap() >= t) {
                        best_time = Some(t);
                        best_score = 0;
                        for ix in 0 .. 25 {
                            if board_seen & (1 << ix) == 0 {
                                best_score += board[ix] as usize;
                            }
                        }
                        best_score *= draw as usize;
                    }
                    break;
                }
            }
        }
    }
    best_score.to_string()
}

fn day5(part: char, s: &str) -> String {
    let ticket_id_iter = s.lines().map(|l| u32::from_str_radix(
        &l.chars().map(|c|
            match c {'F' | 'L' => '0', 'B' | 'R' => '1', _ => panic!("{}", l)}
        ).collect::<String>()
    , 2).unwrap());
    if part == 'a' {
        ticket_id_iter.max().unwrap().to_string()
    } else {
        let (min, len, sum) = ticket_id_iter.fold((u32::MAX, 0, 0), |(min, len, sum), id|
            (std::cmp::min(min, id), len + 1, sum + id)
        );
        // the sum of all valid tickets is num_valid * (min_valid + max_valid) / 2)
        ((len + 1) * (2 * min + len) / 2 - sum).to_string()
    }
}

fn day6(part: char, s: &str) -> String {
    s.split("\n\n").map(|group|
        group.lines().skip(1).fold(
            group.lines().next().unwrap().chars().collect(),
        |acc: HashSet<char>, line: &str| {
            if part == 'a' {
                acc.union(&line.chars().collect()).cloned().collect()
            } else {
                acc.intersection(&line.chars().collect()).cloned().collect()
            }
        }).len()
    ).sum::<usize>().to_string()
}

fn day7(part: char, s: &str) -> String {
    let rules: HashMap<&str, Vec<(u8, &str)>> = s.lines().map(|line| {
        let (container_str, contents_str) = str_split_once(line, " contain ").expect(line);
        assert!(container_str.ends_with(" bags"), "{}", line);
        let contents_vec = if contents_str == "no other bags." {
            vec![]
        } else {
            contents_str.trim_end_matches(".").split(", ").map(|content_str| {
                let count_len: usize = content_str.chars().take_while(char::is_ascii_digit).count();
                let count: u8 = content_str[.. count_len].parse().expect(content_str);
                assert!(&content_str[count_len ..= count_len] == " ");
                assert!(content_str.ends_with(if count == 1 {"bag"} else {"bags"}));
                (count, &content_str[
                    count_len + 1 .. content_str.len() - (if count == 1 {4} else {5})
                ])
            }).collect()
        };
        (&container_str[.. container_str.len() - 5], contents_vec)
    }).collect();
    
    if part == 'a' {
        let mut rules_inverse: HashMap<&str, Vec<(u8, &str)>> = HashMap::new();
        for (rules_k, rules_vs) in rules {
            for (n, rules_v) in rules_vs {
                rules_inverse.entry(rules_v).or_insert_with(|| vec![]).push((n, rules_k));
            }
        }
        let mut container_options: HashSet<&str> = HashSet::new();
        let mut bags_to_check = vec!["shiny gold"];
        while !bags_to_check.is_empty() {
            if let Some(rule) = &rules_inverse.get(bags_to_check.swap_remove(0)) {
                for (_, new_container) in *rule {
                    if container_options.insert(new_container) {bags_to_check.push(new_container)}
                }
            }
        }
        container_options.len().to_string()
    } else {
        let mut cache: HashMap<&str, u32> = HashMap::with_capacity(rules.len());
        fn calc<'a>(rules: &HashMap<&str, Vec<(u8, &'a str)>>,
                    cache: &mut HashMap<&'a str, u32>,
                    bag: &'a str) -> u32
        {
            if !cache.contains_key(bag) {
                let r = rules[bag].iter().map(|(n, content)|
                    (*n as u32) * (calc(rules, cache, content) + 1)
                ).sum();
                cache.insert(bag, r);
            }
            cache[bag]
        }
        calc(&rules, &mut cache, "shiny gold").to_string()
    }
}

fn day8(part: char, s: &str) -> String{
    let mut code = AocCpu::parse_asm(s);
    if part == 'a'{
        let mut cpu = AocCpu::new(&code);
        cpu.run();
        assert!(cpu.state == RunState::Looped);
        cpu.acc.to_string()
    } else {
        for i in 0 .. code.len(){
            code[i] = match code[i] {
                Op::Jmp(n) => Op::Nop(n),
                Op::Nop(n) => Op::Jmp(n), 
                _ => continue
            };
            let mut cpu = AocCpu::new(&code);
            cpu.run();
            match cpu.state {
                RunState::Looped => (),
                RunState::JumpError => println!("jump error ({})", cpu.ip),
                RunState::Success => println!("code ran to success,  result {}", cpu.acc),
                RunState::Running => unreachable!()
            }
            code[i] = match code[i] {
                Op::Jmp(n) => Op::Nop(n),
                Op::Nop(n) => Op::Jmp(n), 
                _ => unreachable!()
            };
        }
        "done".to_string()
    }
}

fn day9 (part: char, s: &str) -> String {
    let mut buf: VecDeque<u64> = VecDeque::with_capacity(26);
    let mut a_res = None;
    'outer: for line in s.lines() {
        let n: u64 = line.parse().unwrap();
        if buf.len() == 25 {
            if !buf.iter().any(|x| buf.iter().any(|y| x + y == n)) {a_res = Some(n); break 'outer};
            buf.pop_front();
        }
        buf.push_back(n);
    }
    if part == 'a' {return format!("{:?}", a_res)};
    
    buf.clear();
    let mut sum = 0;
    let goal = a_res.unwrap();
    for line in s.lines(){
        let n: u64 = line.parse().unwrap();
        sum += n; buf.push_back(n);
        while sum > goal {sum -= buf.pop_front().unwrap()};
        if sum == goal && buf.len() > 1 {
            return (buf.iter().min().unwrap() + buf.iter().max().unwrap()).to_string()
        }
    }
    "done".to_string()
}

fn day10(part: char, s: &str) -> String {
    let mut data: Vec<usize> = s.lines().map(|line| line.parse().unwrap()).collect();
    data.sort_unstable();
    if part == 'a' {
        let mut tallies = [0; 3];
        for i in 0 .. data.len() {
            let diff = data[i] - (if i == 0 {0} else {data[i-1]});
            tallies[diff - 1] += 1;
        }
        tallies[2] += 1;
        (tallies[0] * tallies[2]).to_string()
    } else {
        let mut t3; let mut t2 = 0u64; let mut t1 = 0u64; let mut t0 = 1u64;
        for i in 0 .. data.len() {
            let diff = data[i] - (if i == 0 {0} else {data[i-1]});
            match diff {
                1 => {t3 = t2; t2 = t1; t1 = t0},
                2 => {t3 = t1; t2 = t0; t1 = 0},
                3 => {t3 = t0; t2 = 0; t1 = 0},
                _ => unreachable!()
            };
            t0 = t1 + t2 + t3;
        }
        t0.to_string()
    }
}

fn day11(part: char, s: &str) -> String {
    let rows = s.lines().next().unwrap().len();
    let cols = s.lines().count();
    
    let mut buf_wr = Vec::<u8>::with_capacity((rows + 2) * (cols + 1) + 2);
    for _ in 0 ..= cols {buf_wr.push(b'.')};
    buf_wr.push(b'\n');
    buf_wr.extend_from_slice(s.as_bytes());
    for _ in 0 ..= cols {buf_wr.push(b'.')};
    buf_wr.push(b'\n');
    let mut buf_rd = buf_wr.clone();
    if part == 'a' {
        loop {
            let mut last_iter = true;
            let mut people = 0;
            for i in cols + 2 ..= buf_rd.len() - cols - 2 {
                if buf_rd[i] != b'#' && buf_rd[i] != b'L' {continue;}
                let neighs = 
                    (buf_rd[i-cols-2] == b'#') as u8 +
                    (buf_rd[i-cols-1] == b'#') as u8 +
                    (buf_rd[i-cols  ] == b'#') as u8 +
                    (buf_rd[i-1     ] == b'#') as u8 +
                    (buf_rd[i+1     ] == b'#') as u8 +
                    (buf_rd[i+cols  ] == b'#') as u8 +
                    (buf_rd[i+cols+1] == b'#') as u8 +
                    (buf_rd[i+cols+2] == b'#') as u8;
                if      buf_rd[i] == b'L' && neighs == 0 {buf_wr[i] = b'#'; last_iter = false;}
                else if buf_rd[i] == b'#' && neighs >= 4 {buf_wr[i] = b'L'; last_iter = false;}
                else {buf_wr[i] = buf_rd[i];}
                if buf_rd[i] == b'#' {people += 1;}
            }
            if last_iter {return people.to_string();}
            let tmp = buf_wr; buf_wr = buf_rd; buf_rd = tmp;
        }
    } else {
        let mut seats_at: Vec<usize> = Vec::with_capacity(cols * rows);
        for i in cols + 2 ..= buf_rd.len() - cols - 2 {
            if buf_rd[i] == b'L' {seats_at.push(i);}
        }
        let mut seats_seen: Vec<[usize; 8]> = vec![[0; 8]; seats_at.len()];
        let dirs = [-(cols as isize) - 2, -(cols as isize) - 1, -(cols as isize),
                    -1, 1, 
                    cols as isize, cols as isize + 1, cols as isize + 2];
        for i in 0 .. seats_at.len() {
            for j in 0 .. 8{
                let mut ni = seats_at[i] as isize;
                loop {
                    ni += dirs[j];
                    if ni < 0 || ni >= buf_rd.len() as isize ||
                       buf_rd[ni as usize] == b'\n' {break;}
                    if buf_rd[ni as usize] == b'L' {seats_seen[i][j] = ni as usize; break;}
                }
            }
        }
        loop {
            let mut last_iter = true;
            let mut people = 0;
            for i in 0 .. seats_at.len() {
                let si = seats_at[i];
                let mut neighs = 0;
                for ni in &seats_seen[i] {if buf_rd[*ni] == b'#' {neighs += 1;}}
                if      buf_rd[si] == b'L' && neighs == 0 {buf_wr[si] = b'#'; last_iter = false;}
                else if buf_rd[si] == b'#' && neighs >= 5 {buf_wr[si] = b'L'; last_iter = false;}
                else {buf_wr[si] = buf_rd[si]};
                if buf_rd[si] == b'#' {people += 1};
            }
            if last_iter {return people.to_string();}
            let tmp = buf_wr; buf_wr = buf_rd; buf_rd = tmp;
        }
    }
}

fn day12(part: char, s: &str) -> String {
    let mut ship_pos: (i32, i32) = (0, 0);
    if part == 'a' {
        let mut ship_dir = (1, 0);
        for l in s.lines() {
            let op = l.chars().next().unwrap();
            let arg: i32 = l[1..].parse().unwrap();
            match op {
                'L' => {
                    assert!(arg % 90 == 0);
                    for _ in 0 .. arg / 90 {ship_dir = (-ship_dir.1, ship_dir.0);}
                },
                'R' => {
                    assert!(arg % 90 == 0);
                    for _ in 0 .. arg / 90 {ship_dir = (ship_dir.1, -ship_dir.0);}
                },
                _ => {
                    let move_dir = match op {
                        'N' => (0, 1), 'S' => (0, -1), 'E' => (1, 0), 'W' => (-1, 0),
                        'F' => ship_dir, _ => unreachable!()
                    };
                    ship_pos.0 += move_dir.0 * arg;
                    ship_pos.1 += move_dir.1 * arg;
                }
            }
        }
    } else {
        let mut ship_dir = (10, 1);
        for l in s.lines() {
            let op = l.chars().next().unwrap();
            let arg: i32 = l[1..].parse().unwrap();
            match op {
                'L' => {
                    assert!(arg % 90 == 0);
                    for _ in 0 .. arg / 90 {ship_dir = (-ship_dir.1, ship_dir.0);}
                }, 'R' => {
                    assert!(arg % 90 == 0);
                    for _ in 0 .. arg / 90 {ship_dir = (ship_dir.1, -ship_dir.0);}
                }, 'F' => {
                    ship_pos.0 += ship_dir.0 * arg;
                    ship_pos.1 += ship_dir.1 * arg;
                }, _ => {
                    let move_dir = match op {
                        'N' => (0, 1), 'S' => (0, -1), 'E' => (1, 0), 'W' => (-1, 0),
                        _ => unreachable!()
                    };
                    ship_dir.0 += move_dir.0 * arg;
                    ship_dir.1 += move_dir.1 * arg;
                }
            }
        }
    }
    (ship_pos.0.abs() + ship_pos.1.abs()).to_string()
}

fn day13(part: char, s: &str) -> String {
    let (now_str, buses_str) = str_split_once(s.trim(), "\n").unwrap();
    if part == 'a' {
        let now: u32 = now_str.parse().unwrap();
        let mut best: Option<(u32, u32)> = None;
        for bus_str in buses_str.split(",") {
            if let Ok(bus) = bus_str.parse::<u32>() {
                let next = (now - 1) / bus * bus + bus;
                if best == None || best.unwrap().0 > next {best = Some((next, bus));}
            }
        }
        if let Some((t, bus)) = best {
            ((t - now) * bus).to_string()
        } else {unreachable!()}
    } else {
        let mut first = 0;
        let mut period = 1;
        for (i, bus_str) in buses_str.split(",").enumerate() {
            if let Ok(bus) = bus_str.parse::<u64>() {
                while (first + i as u64) % bus != 0 {first += period};
                assert_eq!(1, gcd(period, bus));
                period = lcm(period, bus);
            }
        }
        first.to_string()
    }
}

fn day14(part: char, s: &str) -> String {
    let mut mem: HashMap<u64, u64> = HashMap::new();
    if part == 'a' {
        let mut bitmax = 0xfffffffffu64;
        let mut bitmin = 0x000000000u64;
        for line in s.lines() {
            if line.starts_with("mask = ") {
                let mask_str = &line[7..];
                bitmax = u64::from_str_radix(&mask_str.chars().map(|c|
                    if c == 'X' {'1'} else {c}
                ).collect::<String>(), 2).unwrap();
                bitmin = u64::from_str_radix(&mask_str.chars().map(|c|
                    if c == 'X' {'0'} else {c}
                ).collect::<String>(), 2).unwrap();
            } else {
                let (k_str, v_str) = str_split_once(line, "] = ").unwrap();
                assert!(k_str.starts_with("mem["));
                let k: u64 = k_str[4..].parse().unwrap();
                let v: u64 = v_str.parse().unwrap();
                mem.insert(k, v & bitmax | bitmin);
            }
        }
    } else {
        let mut mask = "000000000000000000000000000000000000";
        for line in s.lines() {
            if line.starts_with("mask = ") {
                mask = &line[7..];
            } else {
                let (k_str, v_str) = str_split_once(line, "] = ").unwrap();
                assert!(k_str.starts_with("mem["));
                let v: u64 = v_str.parse().unwrap();
                let mut k_str = format!("{:036b}", k_str[4..].parse::<u64>().unwrap())
                                .chars().zip(mask.chars())
                                .map(|(k_c, m_c)| match m_c {
                                    '0' => k_c, '1' => '1', 'X' => 'f', _ => panic!()
                                }).collect::<String>();
                'key_iter: loop {
                    mem.insert(u64::from_str_radix(&k_str.chars().map(|c|
                        match c {'f' => '0', 't' => '1', _ => c}
                    ).collect::<String>(), 2).unwrap(), v);
                    unsafe{
                        for i in (0..36).rev() {
                            match &k_str.as_bytes()[i] {
                                b'f' => {k_str.as_bytes_mut()[i] = b't'; continue 'key_iter},
                                b't' => {k_str.as_bytes_mut()[i] = b'f'},
                                _ => {}
                            }
                        }
                        break
                    }
                }
            }
        }
    }
    mem.values().sum::<u64>().to_string()
}

fn day15(part: char, s: &str) -> String {
    let t_max = if part == 'a' {2020} else {30000000};
    let mut seen_at: Vec<Option<usize>> = vec![None; t_max];
    let mut last : usize = 0xdead;
    let mut init = s.trim().split(',');
    for t in 1 ..= t_max {
        let next = init.next().map(|x_str| x_str.parse().unwrap())
                          .unwrap_or_else(||t - 1 - seen_at[last].unwrap_or(t - 1));
        if t > 1 {seen_at[last] = Some(t - 1)};
        last = next;
    }
    last.to_string()
}

fn day16(part: char, s: &str) -> String {
    let mut lines = s.lines();
    let rules = lines.by_ref().take_while(|line| line.len() > 0).map(|line| {
        let (name, ranges_str) = str_split_once(line, ": ").unwrap();
        let ranges = ranges_str.split(" or ").flat_map(|s| {
            let (beg, end) = str_split_once(s, "-").unwrap();
            vec![beg.parse::<u32>().unwrap(), end.parse::<u32>().unwrap() + 1]
        }).collect::<Vec<_>>();
        (name, ranges)
    }).collect::<Vec<_>>();
    let rule_union = rules.iter().map(|r| &r.1).fold(vec![], |a, d| {
        let mut ai = a.iter().peekable();
        let mut di = d.iter().peekable();
        let mut a_open = false;
        let mut d_open = false;
        let mut r = vec![];
        loop {
            if ai.peek() == None {r.extend(di); return r;}
            if di.peek() == None {r.extend(ai); return r;}
            if ai.peek().unwrap() > di.peek().unwrap() {
                d_open ^= true;
                if !a_open {r.push(**di.peek().unwrap());}
                di.next();
            } else {
                a_open ^= true;
                if !d_open {r.push(**ai.peek().unwrap());}
                ai.next();
            }
        }
    });
    assert_eq!(Some("your ticket:"), lines.next());
    let our_ticket = lines.next().unwrap().split(",").map(|s| s.parse::<u32>().unwrap())
                          .collect::<Vec<_>>();
    assert_eq!(Some(""), lines.next());
    assert_eq!(Some("nearby tickets:"), lines.next());
    if part == 'a' {
        return lines.flat_map(|s| s.split(",")).map(|s| s.parse::<u32>().unwrap()).filter(|x|
            rule_union.iter().take_while(|r| *r <= x).count() % 2 == 0
        ).sum::<u32>().to_string()
    }
    
    let mut valids = vec![(1 << rules.len()) - 1; rules.len()];
    for line in lines {
        let line_valids = line.split(",").map(|s| s.parse::<u32>().unwrap()).map(|v| 
            rules.iter().map(|r| (r.1.iter().take_while(|r| **r <= v).count() % 2))
                 .fold(0, |a, d| a * 2 + d)
        );
        let new_valids = valids.iter().zip(line_valids).map(|(a, b)| a & b).collect::<Vec<_>>();
        if *new_valids.iter().min().unwrap() > 0 {
            valids = new_valids;
        }
    }
    
    let mut field_map = HashMap::<u32, u32>::new();
    for i in (0..valids.len()).cycle() {
        if valids[i].count_ones() == 1 {
            field_map.insert(i as u32, rules.len() as u32 - valids[i].trailing_zeros() - 1);
            let mask = !valids[i];
            for v in &mut valids {*v &= mask;}
            if field_map.len() == rules.len() {break};
        }
    }
    our_ticket.iter().enumerate().filter_map(|(i, v)|
        if rules[field_map[&(i as u32)] as usize].0.starts_with("departure ") {
            Some(*v as u64)
        } else {None}
    ).product::<u64>().to_string()
}

fn day17(part: char, s: &str) -> String {
    let mut buf_rd = vec![vec![s.lines().map(|line| Vec::from(line)).collect::<Vec<_>>()]];

    for _t in 1 ..= 6 {
        let mut buf_wr = vec![vec![
            vec![vec![b'.'; buf_rd[0][0][0].len() + 2]; buf_rd[0][0].len() + 2]
        ; buf_rd[0].len() + 2]; buf_rd.len() + 2];
        
        for z in 0 .. buf_wr.len() {
            for y in 0 .. buf_wr[z].len() {
                for x in 0 .. buf_wr[z][y].len() {
                    for w in 0 .. buf_wr[z][y][x].len(){
                        let mut neighs = 0;
                        for dz in &[0, 1, 2] {
                            for dy in &[0, 1, 2] {
                                for dx in &[0, 1, 2] {
                                    for dw in &[0, 1, 2] {
                                        if z >= *dz && y >= *dy && x >= *dx && w >= *dw {
                                            if let Some(c) = buf_rd.get(z - dz)
                                                                   .and_then(|a| a.get(y - dy))
                                                                   .and_then(|a| a.get(x - dx))
                                                                   .and_then(|a| a.get(w - dw))
                                            {
                                                if *c == b'#' {neighs += 1;}
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        if neighs == 3 {buf_wr[z][y][x][w] = b'#'}
                        else if neighs == 4 && z >= 1 && y >= 1 && x >= 1 && w >= 1{
                            if let Some(c) = buf_rd.get(z - 1)
                                                   .and_then(|a| a.get(y - 1))
                                                   .and_then(|a| a.get(x - 1))
                                                   .and_then(|a| a.get(w - 1))
                            {
                                if *c == b'#' {buf_wr[z][y][x][w] = b'#';}
                            }
                        }
                    }
                }
            }
        }
        if part == 'a' {
            buf_wr.pop();
            buf_wr.remove(0);
        }
        buf_rd = buf_wr;
    }
    buf_rd.iter().flatten().flatten().flatten().filter(|c| **c == b'#').count().to_string()
}

#[derive(Clone, Copy)]
enum MathToken {Int(i64), Op(char), LPar, RPar}
impl std::fmt::Debug for MathToken {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result { match self{
        MathToken::Int(v) =>  f.write_str(&v.to_string()),
        MathToken::Op(c)  =>  f.write_str(&c.to_string()),
        MathToken::LPar   =>  f.write_str("("),
        MathToken::RPar   =>  f.write_str(")")
    }}
}

fn day18(part: char, s: &str) -> String {
    use self::MathToken::*;
    s.lines().map(|line| {
        let mut tok_stack: Vec<MathToken> = vec![];
        for c in line.chars() {
            match c {
                '0' ..= '9' => tok_stack.push(Int((c as u8 - b'0') as i64)),
                '+' | '*'   => tok_stack.push(Op(c)),
                '('         => tok_stack.push(LPar),
                ')'         => tok_stack.push(RPar),
                ' '         => (),
                _           => panic!("{}", line)
            }
            if part == 'b' {
                if let [.., Int(l), Op('*'), Int(r), next] = tok_stack[..]{
                    if matches!(next, Op('*') | RPar){
                        tok_stack.truncate(tok_stack.len() - 4);
                        tok_stack.push(Int(l * r));
                        tok_stack.push(next);
                    }
                }
            }
            if let [.., LPar, Int(v), RPar] = tok_stack[..] {
                tok_stack.truncate(tok_stack.len() - 3);
                tok_stack.push(Int(v));
            }
            if let [.., Int(l), Op('+'), Int(r)] = tok_stack[..]{
                tok_stack.truncate(tok_stack.len() - 3);
                tok_stack.push(Int(l + r));
            }
            if part == 'a' {
                if let [.., Int(l), Op('*'), Int(r)] = tok_stack[..]{
                    tok_stack.truncate(tok_stack.len() - 3);
                    tok_stack.push(Int(l * r));
                }
            }
        }
        if let [.., Int(l), Op('*'), Int(r)] = tok_stack[..]{
            tok_stack.truncate(tok_stack.len() - 3);
            tok_stack.push(Int(l * r));
        }
        if let [Int(v)] = tok_stack[..] {v} else {panic!("{} produced {:?}", line, tok_stack)}
    }).sum::<i64>().to_string()
}

fn day19(part: char, s: &str) -> String {
    let mut lines = s.lines();
    #[derive(Clone, Debug)]
    enum Rule {Unknown, Lit(char), Opt(Vec<Vec<usize>>)}
    let mut rules = vec![];
    
    for line in lines.by_ref().take_while(|line| !line.is_empty()) {
        let (id_str, def_str) = str_split_once(line, ": ").unwrap();
        let id: usize = id_str.parse().unwrap();
        if rules.len() <= id {rules.resize(id + 1, Rule::Unknown)}
        rules[id] = match def_str {
            "\"a\"" => Rule::Lit('a'),
            "\"b\"" => Rule::Lit('b'),
            _ => Rule::Opt(def_str.split(" | ").map(|opt_str|
                opt_str.split(" ").map(|num_str| num_str.parse().unwrap()).collect()
            ).collect())
        };
    }
    
    if part == 'b' {
        rules[8] = Rule::Opt(vec![vec![42], vec![42, 8]]);
        rules[11] = Rule::Opt(vec![vec![42, 31], vec![42, 11, 31]]);
    }
    
    lines.filter(|line| {
        let mut parses_rem: Vec<Vec<usize>> = vec![vec![0]];
        for c in line.chars() {
            let mut next_parses: Vec<Vec<usize>> = vec![];
            while !parses_rem.is_empty() {
                let mut parse: Vec<usize> = parses_rem.pop().unwrap();
                if parse.is_empty() {continue;}
                match &rules[parse.remove(0)] {
                    Rule::Lit(lit_c) => {
                        if c == *lit_c {
                            next_parses.push(parse);
                        }
                    },
                    Rule::Opt(opt_parses) => {
                        for opt_parse in opt_parses {
                            let mut new_parse = opt_parse.clone();
                            new_parse.extend(&parse);
                            parses_rem.push(new_parse);
                        }
                    },
                    _ => unreachable!()
                }
            }
            parses_rem = next_parses;
            if parses_rem.is_empty() {return false};
        }
        parses_rem.iter().any(|parse| parse.is_empty())
    }).count().to_string()
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Transform {Id, Cw, R2, Cc, Mx, My, Md, Ma}

trait Transformable: Sized {
    fn transpose(self) -> Self;
    fn mirror_x(self) -> Self;
    fn mirror_y(self) -> Self;
    
    fn transform(self, t: Transform) -> Self {
        use Transform::*;
        match t {
            Id => self, Md => self.transpose(), Mx => self.mirror_x(), My => self.mirror_y(),
            Cw => self.mirror_x().transpose(), Cc => self.transpose().mirror_x(),
            R2 => self.mirror_x().mirror_y(), Ma => self.mirror_x().transpose().mirror_x()
        }
    }
}
impl Transformable for char {
    fn transpose(self) -> Self { match self {
        'N' => 'W', 'E' => 'S', 'S' => 'E', 'W' => 'N', _ => panic!("can't transpose {}", self)
    }}
    fn mirror_x(self) -> Self { match self {'N' => 'S', 'S' => 'N', _ => self}}
    fn mirror_y(self) -> Self { match self {'E' => 'W', 'W' => 'E', _ => self}}
}

impl<T: Copy> Transformable for Vec<Vec<T>> {
    fn transpose(mut self) -> Self {
        for y in 0 .. self.len() {
            for x in 0 .. y {
                let tmp = self[x][y];
                self[x][y] = self[y][x];
                self[y][x] = tmp;
            }
        }
        self
    }
    fn mirror_x(mut self) -> Self {self.reverse(); self}
    fn mirror_y(mut self) -> Self {for row in &mut self {row.reverse()}; self}
}

impl Transformable for Transform {
    fn transpose(self) -> Self {use Transform::*; match self {
        Id => Md, Cw => Mx, R2 => Ma, Cc => My, Md => Id, Mx => Cw, Ma => R2, My => Cc
    }}
    fn mirror_x(self) -> Self {use Transform::*; match self {
        Id => Mx, Cw => Ma, R2 => My, Cc => Md, Md => Cc, My => R2, Ma => Cw, Mx => Id
    }}
    fn mirror_y(self) -> Self {use Transform::*; match self {
        Id => My, Cw => Md, R2 => Mx, Cc => Ma, Md => Cw, My => Id, Ma => Cc, Mx => R2
    }}
}

fn day20(part: char, s: &str) -> String {
    let tile_sz = s.lines().nth(2).unwrap().len();
    let tile_area: usize = tile_sz * tile_sz;
    let tiles: HashMap<&str, (Vec<u8>, Vec<u8>)> = s.trim().split("\n\n").map(|tile_str| {
        let mut lines = tile_str.lines();
        let id_line = lines.next().unwrap();
        let bitmap: Vec<u8> = lines.flat_map(|line| line.as_bytes()).cloned().collect();
        assert_eq!(bitmap.len(), tile_area);
        let mut bitmap_transposed = bitmap.clone();
        for i in 0 .. bitmap.len() - 1 {
            bitmap_transposed[i] = bitmap[i * tile_sz % (tile_area - 1)];
        }
        bitmap_transposed[tile_area - 1] = bitmap[tile_area - 1];
        assert!(id_line.starts_with("Tile ") && id_line.ends_with(":"), "{}", id_line);
        (&id_line[5 .. id_line.len() - 1], (bitmap, bitmap_transposed))
    }).collect();
    
    type Pair<'a> = (&'a str, char, &'a str, char);
    let mut pairs: Vec<Pair> = Vec::with_capacity(tiles.len() * 2);
    for (a_id, (a_i, a_t)) in &tiles {
        for (b_id, (b_i, b_t)) in &tiles {
            if a_id < b_id {
                for (ae_name, ae_bytes) in &[
                    ('N', &a_i[.. tile_sz]), ('S', &a_i[tile_area - tile_sz ..]),
                    ('W', &a_t[.. tile_sz]), ('E', &a_t[tile_area - tile_sz ..])
                ] {
                    for(be_name, be_bytes) in &[
                        ('N', &b_i[.. tile_sz]), ('S', &b_i[tile_area - tile_sz ..]),
                        ('W', &b_t[.. tile_sz]), ('E', &b_t[tile_area - tile_sz ..])
                    ] {
                        if ae_bytes == be_bytes || (0 .. ae_bytes.len()).all(|i|
                            ae_bytes[i] == be_bytes[be_bytes.len()-1 - i]
                        ) {
                            pairs.push((a_id, *ae_name, b_id, *be_name));
                        }
                    }
                }
            }
        }
    }
    
    let mut pairs_for: HashMap<&str, Vec<Pair>> = HashMap::with_capacity(pairs.len());
    for (ai, ae, bi, be) in pairs {
        pairs_for.entry(ai).or_insert_with(||vec![]).push((ai, ae, bi, be));
        pairs_for.entry(bi).or_insert_with(||vec![]).push((bi, be, ai, ae));
    }

    if part == 'a' {
        return pairs_for.iter().filter_map(|(k, v)|
            if v.len() == 2 {Some(k.parse::<u64>().unwrap())} else {None})
        .inspect(|k| println!("{}", k)).product::<u64>().to_string()
    }
    
    let mut tilemap = vec![vec![]];
    let mut bitmap = vec![];
    'forj: for j in 0 .. {
        'fori: for i in 0 .. {
            if i == 0 && j == 0 {
                tilemap[j].push(pairs_for.iter().find_map(|(k, v)|
                    if v.len() == 2 {Some(k)} else {None}
                ).unwrap());
            }
            let tile_id = tilemap[j][i];
            
            let pairs = &pairs_for[tile_id];
            assert!(pairs.len() <= 4 && (i > 0 || j > 0 || pairs.len() < 4));
            let mut t = Transform::Id;
            let tile_n = if j == 0 {None} else {Some(tilemap[j-1][i])};
            let tile_w = if i == 0 {None} else {Some(tilemap[j][i-1])};

            let neigh_at = |c: char, t: Transform| pairs.iter().find_map(|(_, dir, tile, _)|
                if dir.transform(t) == c {Some(tile)} else {None}
            );
            
            if i == 0 {
                for _ in 0 .. tile_sz - 2 {bitmap.push(vec![]);}
                while neigh_at('N', t) != tile_n {
                    t = t.transform(Transform::Cw);
                    assert_ne!(t, Transform::Id);
                }
                if neigh_at('W', t) != tile_w {t = t.mirror_y();}
            } else {
                while neigh_at('W', t) != tile_w {
                    t = t.transform(Transform::Cw);
                    assert_ne!(t, Transform::Id);
                }
                if neigh_at('N', t) != tile_n {t = t.mirror_x();}
            }
            
            assert_eq!(neigh_at('N', t), tile_n);
            assert_eq!(neigh_at('W', t), tile_w);
            print!("{} {:?} ", tile_id, t);

            let tile_e = neigh_at('E', t);
            if tile_e == None {
                assert!((i + 1) * (i + 1) == tiles.len());
                println!();
            } else {
                if j == 0 {
                    tilemap[j].push(tile_e.unwrap())
                } else {
                    assert!(tilemap[j][i+1] == tile_e.unwrap())
                }
            }

            let tile_s = neigh_at('S', t);
            if tile_s == None {
                assert!((j + 1) * (j + 1) == tiles.len());
            } else {
                if i == 0 {
                    tilemap.push(vec![]);
                }
                tilemap[j + 1].push(tile_s.unwrap());
            }
            
            for (tj, bits) in (1 .. tile_sz - 1).map(|tj|
                Vec::from(&tiles[tile_id].0[tj * tile_sz + 1 .. (tj + 1) * tile_sz - 1])
            ).collect::<Vec<_>>().transform(t).iter().enumerate() {
                bitmap[j * (tile_sz - 2) + tj].extend_from_slice(&bits);
            }
            
            if tile_e == None {if tile_s == None {break 'forj} else {break 'fori}}
        }
    }
    
    const NESSIE: [(usize, usize); 15] = [                   (18, 0),
    (0, 1),   (5, 1), (6, 1),   (11, 1), (12, 1),   (17, 1), (18, 1), (19, 1),
     (1, 2), (4, 2),   (7, 2), (10, 2),   (13, 2), (16, 2)
    ];
    
    let mut tacc = Transform::Id;
    for t in &[
        Transform::Ma, Transform::My, Transform::Mx, Transform::My,
        Transform::Md, Transform::My, Transform::Mx, Transform::My
    ] {
        bitmap = bitmap.transform(*t);
        tacc = tacc.transform(*t);
        println!("+ {:?} = {:?}", t, tacc);
        for hj in 0 .. bitmap.len() - 2 {
            for hi in 0 .. bitmap.len() - 19 {
                if NESSIE.iter().all(|(ni, nj)|
                    bitmap[hj + nj][hi + ni] != b'.'
                ) {
                    println!("nessie found at {}, {}", hj, hi);
                    for (ni, nj) in &NESSIE {
                        bitmap[hj + nj][hi + ni] = b'O';
                    }
                }
            }
        }
    }
    
    for line in &bitmap {println!("{}", std::str::from_utf8(&line).unwrap());}
    bitmap.iter().flatten().filter(|c| **c == b'#').count().to_string()
}

fn day21(part: char, s: &str) -> String {
    let mut ingredient_counts: HashMap<&str, u32> = HashMap::new();
    let mut allergen_ingredients: HashMap<&str, HashSet<&str>> = HashMap::new();
    for line in s.lines() {
        let (l_ing_str, l_all_str) = str_split_once(line, " (contains ").unwrap();
        let l_ing = l_ing_str.split(" ").collect::<HashSet<_>>();
        for a in l_all_str.trim_end_matches(")").split(", "){
            allergen_ingredients.entry(a).and_modify(|v| v.retain(|vv| l_ing.contains(vv)))
                                         .or_insert_with(|| l_ing.clone());
        }
        for i in l_ing {
            *ingredient_counts.entry(i).or_insert(0) += 1;
        }
    }
    
    for (_, aiv) in &allergen_ingredients {
        ingredient_counts.retain(|ick, _| !aiv.contains(ick))
    }
    if part == 'a' {
        ingredient_counts.values().sum::<u32>().to_string()
    } else {
        let mut known_pairs: Vec<(&str, &str)> = vec![];
        let mut known_ingredients: HashSet<&str> = HashSet::new();
        let ai_len = allergen_ingredients.len();
        'l: loop {
            for (aik, aiv) in allergen_ingredients.iter_mut() {
                aiv.retain(|aivv| !known_ingredients.contains(aivv));
                if aiv.len() == 1{
                    let aivv = aiv.iter().next().unwrap();
                    known_pairs.push((aik, aivv));
                    known_ingredients.insert(aivv);
                    if ai_len == known_pairs.len() {break 'l;}
                }
            }
        }
        known_pairs.sort_unstable();
        known_pairs.iter().map(|(_, i)| *i).collect::<Vec<_>>().join(",")
    }
}

fn day22(part: char, s: &str) -> String {
    let mut p1: VecDeque<usize> = VecDeque::new();
    let mut p2: VecDeque<usize> = VecDeque::new();
    let mut lines = s.lines();
    assert_eq!(lines.next(), Some("Player 1:"));
    for line in lines.by_ref().take_while(|line| !line.is_empty()) {
        p1.push_back(line.parse().unwrap());
    }
    assert_eq!(lines.next(), Some("Player 2:"));
    for line in lines {
        p2.push_back(line.parse().unwrap());
    }
    p1.reserve_exact(p1.len() + p2.len());
    p2.reserve_exact(p1.len() + p2.len());
    let winner : VecDeque<usize>;
    if part == 'a' {
        while !p1.is_empty() && !p2.is_empty() {
            let p1c = p1.pop_front().unwrap();
            let p2c = p2.pop_front().unwrap();
            if p1c > p2c {
                p1.push_back(p1c);
                p1.push_back(p2c);
            } else {
                p2.push_back(p2c);
                p2.push_back(p1c);
            }
        }
        winner = if p1.is_empty() {p2} else {p1};
    } else {
        subgame(&mut p1, &mut p2);
        winner = if p1.is_empty() {p2} else {p1};

        fn subgame(game_p1: &mut VecDeque<usize>, game_p2: &mut VecDeque<usize>) {
            let mut chaser_p1 = game_p1.clone();
            let mut chaser_p2 = game_p2.clone();
            loop {
                step_game(game_p1, game_p2);
                if game_p1.is_empty() || game_p2.is_empty() {break;}
                step_game(game_p1, game_p2);
                if game_p1.is_empty() || game_p2.is_empty() {break;}
                step_game(&mut chaser_p1, &mut chaser_p2);
                if *game_p1 == chaser_p1 && *game_p2 == chaser_p2 {break;}
            }
            
            fn step_game(step_p1: &mut VecDeque<usize>, step_p2: &mut VecDeque<usize>) {
                let p1c = step_p1.pop_front().unwrap();
                let p2c = step_p2.pop_front().unwrap();
                let p1_won_round;
                if p1c <= step_p1.len() && p2c <= step_p2.len() {
                    let mut sub_p1 = VecDeque::with_capacity(p1c + p2c);
                    sub_p1.extend(step_p1.iter().take(p1c));
                    let mut sub_p2 = VecDeque::with_capacity(p1c + p2c);
                    sub_p2.extend(step_p2.iter().take(p2c));
                    subgame(&mut sub_p1, &mut sub_p2);
                    p1_won_round = !sub_p1.is_empty();
                } else {
                    p1_won_round = p1c > p2c;
                }
                if p1_won_round {
                    step_p1.push_back(p1c);
                    step_p1.push_back(p2c);
                } else {
                    step_p2.push_back(p2c);
                    step_p2.push_back(p1c);
                }
            }
        }
    }
    winner.iter().rev().enumerate().map(|(i, c)| (i + 1) * c).sum::<usize>().to_string()
}

fn day23(part: char, s: &str) -> String {
    let n_cups = if part == 'a' {s.len()} else {1_000_000};
    let mut next_cups: Vec<usize> = (1 .. n_cups + 1).collect();
    let mut curr_cup = (s.as_bytes()[0] - b'1') as usize;
    for w in s.as_bytes().windows(2) {
        next_cups[(w[0] - b'1') as usize] = (w[1] - b'1') as usize;
    }
    if part == 'a' {
        next_cups[(s.as_bytes().last().unwrap() - b'1') as usize] = (s.as_bytes()[0] - b'1') as usize;
    } else {
        next_cups[(s.as_bytes().last().unwrap() - b'1') as usize] = s.len();
        *next_cups.last_mut().unwrap() = (s.as_bytes()[0] - b'1') as usize;
    }
    
    for _ in 0 .. (if part == 'a' {100} else {10_000_000}) {
        let cup2 = next_cups[curr_cup];
        let cup3 = next_cups[cup2];
        let cup4 = next_cups[cup3];
        let mut dst = curr_cup;
        loop {
            dst = if dst == 0 {n_cups - 1} else {dst - 1};
            if dst != cup2 && dst != cup3 && dst != cup4 {break};
        }
        next_cups[curr_cup] = next_cups[cup4];
        next_cups[cup4] = next_cups[dst];
        next_cups[dst] = cup2;
        curr_cup = next_cups[curr_cup];
    }
    
    if part == 'a' {
        curr_cup = 0;
        let mut r = String::with_capacity(9);
        loop {
            curr_cup = next_cups[curr_cup];
            if curr_cup == 0 {break;}
            r.push((curr_cup as u8 + b'1') as char);
        }
        r
    } else {
        ((next_cups[0] as u64 + 1) * (next_cups[next_cups[0]] as u64 + 1)).to_string()
    }
}

fn day24(part: char, s: &str) -> String {
    let mut bytes = s.bytes();
    let mut tiles: HashSet<(i32, i32)> = HashSet::new();
    'lines: loop {
        let mut x = 0;
        let mut y = 0;
        'chars: loop {
            match bytes.next() {
                None => {break 'lines;},
                Some(b'\n') => {break 'chars;},
                Some(b'w') => {x -= 2}, Some(b'e') => {x += 2},
                Some(b'n') => {y -= 1; match bytes.next() {
                    Some(b'w') => {x -= 1}, Some(b'e') => {x += 1}, _ => panic!()
                }},
                Some(b's') => {y += 1; match bytes.next() {
                    Some(b'w') => {x -= 1}, Some(b'e') => {x += 1}, _ => panic!()
                }},
                _ => panic!()
            }
        }
        if !tiles.remove(&(x, y)) {tiles.insert((x, y));}
    }

    if part == 'b' {
        for _ in 0 .. 100 {
            let mut neigh_counts: HashMap<(i32, i32), u8> = HashMap::new();
            for (x, y) in &tiles {
                for (dx, dy) in &[(1, 1), (2, 0), (1, -1), (-1, -1), (-2, 0), (-1, 1)] {
                    *neigh_counts.entry((x + dx, y + dy)).or_insert(0) += 1;
                }
            }
            tiles = neigh_counts.iter().filter_map(|(k, v)| match v {
                1 => tiles.get(k).cloned(),
                2 => Some(*k),
                _ => None
            }).collect();
        }
    }
    
    tiles.len().to_string()
}

fn day25(part: char, s: &str) -> String {
    const MOD: u64 = 20201227;
    assert_eq!(part, 'a');

    let (card_pk_str, door_pk_str) = str_split_once(s.trim(), "\n").unwrap();
    let card_pk: u64 = card_pk_str.parse().unwrap();
    let door_pk: u64 = door_pk_str.parse().unwrap();
    
    let mut card_ls: u64 = 0;
    let mut card_sn: u64 = 1;
    while card_sn != card_pk {card_ls += 1; card_sn = card_sn * 7 % MOD};
    let mut door_ls: u64 = 0;
    let mut door_sn: u64 = 1;
    while door_sn != door_pk {door_ls += 1; door_sn = door_sn * 7 % MOD};
    
    let mut card_ek = 1;
    for _ in 0 .. card_ls {card_ek = card_ek * door_pk % MOD};
    let mut door_ek = 1;
    for _ in 0 .. door_ls {door_ek = door_ek * card_pk % MOD};
    
    assert_eq!(card_ek, door_ek);
    card_ek.to_string()
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let days = [
        day1, day2, day3, day4, day5, day6, day7, day8, day9, day10, day11, day12, day13,
        day14, day15, day16, day17, day18, day19, day20, day21, day22, day23, day24, day25
    ];
    
    if let [_, day_arg, part_arg] = &std::env::args().collect::<Vec<_>>()[..] {
        assert!(part_arg == "a" || part_arg == "b");
        let day: usize = day_arg.parse()?;
        let input = std::fs::read_to_string(format!("day{}.txt", day))?;
        let time = std::time::Instant::now();
        println!("{}", days[day - 1](part_arg.parse()?, &input));
        println!("{} seconds elapsed", time.elapsed().as_secs_f32());
    } else {
        println!("exactly two arguments expected - day number and a/b for part")
    }
    
    Ok(())
}