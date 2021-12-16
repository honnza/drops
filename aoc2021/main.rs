use std::collections::{HashMap, HashSet, VecDeque};

fn str_split_once<'a>(s: &'a str, pat: &str) -> Option<(&'a str, &'a str)> {
    match s.split(pat).collect::<Vec<_>>()[..] {
        [x, y] => Some((x, y)),
        _ => None
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
    let mut once_set = HashSet::new();
    let mut twice_set = HashSet::new();

    for line in s.lines() {
        let (xy1, xy2) = str_split_once(line, " -> ").unwrap();
        let (x1, y1) = str_split_once(xy1, ",").unwrap();
        let x1: i32 = x1.parse().unwrap();
        let y1: i32 = y1.parse().unwrap();
        let (x2, y2) = str_split_once(xy2, ",").unwrap();
        let x2: i32 = x2.parse().unwrap();
        let y2: i32 = y2.parse().unwrap();
        if x1 == x2 {
            for y in y1.min(y2) ..= y1.max(y2) {
                let _ = once_set.insert((x1, y)) || twice_set.insert((x1, y));
            }
        } else if y1 == y2 {
            for x in x1.min(x2) ..= x1.max(x2) {
                let _ = once_set.insert((x, y1)) || twice_set.insert((x, y1));
            }
        } else if part == 'b' {
            let dx = (x2 > x1) as i32 - (x2 < x1) as i32;
            let dy = (y2 > y1) as i32 - (y2 < y1) as i32;
            let (mut x, mut y) = (x1, y1);
            while x != x2 {
                let _ = once_set.insert((x, y)) || twice_set.insert((x, y));
                x += dx; y += dy;
            }
            assert_eq!(y, y2);
            let _ = once_set.insert((x, y)) || twice_set.insert((x, y));
        }
    }

    twice_set.len().to_string()
}

fn day6(part: char, s: &str) -> String {
    let mut ns = [0u64; 9];
    for line in s.trim_end().split(",") {ns[line.parse::<usize>().unwrap()] += 1;}
    for _ in 0 .. (if part == 'a' {80} else {256}) {
        ns = [ns[1], ns[2], ns[3], ns[4], ns[5], ns[6], ns[7] + ns[0], ns[8], ns[0]];
    }
    ns.iter().sum::<u64>().to_string()
}

fn day7(part: char, s: &str) -> String {
    let mut nums = s.trim_end().split(",")
                    .map(|num| num.parse::<i32>().unwrap())
                    .collect::<Vec<_>>();
    if part == 'a' {
        nums.sort_unstable();
        let mid = nums[nums.len() / 2];
        nums.iter().map(|n| (n - mid).abs()).sum::<i32>().to_string()
    } else {
        (*nums.iter().min().unwrap() ..= *nums.iter().max().unwrap()).map(|n|
            nums.iter().map(|n2| {
                let dn = (n - n2).abs(); dn * (dn + 1) / 2
            }).sum::<i32>()
        ).min().unwrap().to_string()
    }
}

fn day8(part: char, s: &str) -> String{
    if part == 'a' {
        s.lines().flat_map(|line| {
            let (_, o) = str_split_once(line, " | ").unwrap();
            o.split(" ").filter(|word| matches!(word.len(), 2 | 3 | 4 | 7))
        }).count().to_string()
    } else {
        s.lines().map(|line| {
            // we only need to identify enough segments to distinguish 2/3/5 an 0/6/9.
            // For 2/3 we need E or F. For 3/5 we need B or C.
            // Further we need two of C for 6, D for 0 or E for 9.
            // C (top right) and E (bottom left) will suffice.
            // C appears in 1 and in 8 digits total. E appears in 4 digits total.
            let (i, o) = str_split_once(line, " | ").unwrap();
            let dig_1 = i.split(" ").find(|word| word.len() == 2).unwrap();
            let seg_c = ('a' ..= 'g').find(|&sc|
                dig_1.contains(sc) && i.chars().filter(|&ic| ic == sc).count() == 8
            ).unwrap();
            let seg_e = ('a' ..= 'g').find(|&sc|
                i.chars().filter(|&ic| ic == sc).count() == 4
            ).unwrap();

            o.split(" ").map(|word| match (word.len(), word.contains(seg_c), word.contains(seg_e)) {
                (2,  true, false) => 1, (3,  true, false) => 7, (4,  true, false) => 4, 
                (5,  true,  true) => 2, (5,  true, false) => 3, (5, false, false) => 5,
                (6,  true,  true) => 0, (6, false,  true) => 6, (6,  true, false) => 9,
                (7,  true,  true) => 8, _ => panic!("{} | {}", i, word)
            }).reduce(|a, d| 10 * a + d).unwrap()
        }).sum::<usize>().to_string()
    }
}

fn day9 (part: char, s: &str) -> String {
    let grid = &s.lines().collect::<Vec<&str>>();
    if part == 'a' {
        (0 .. grid.len()).flat_map(|i|
            (0 .. grid.len()).filter(move |&j| {
                let n_ij = grid[i].as_bytes()[j];
                grid.get(i).and_then(|row| row.as_bytes().get(j - 1))
                    .map_or(true, |&n| n > n_ij) &&
                grid.get(i).and_then(|row| row.as_bytes().get(j + 1))
                    .map_or(true, |&n| n > n_ij) &&
                grid.get(i - 1).and_then(|row| row.as_bytes().get(j))
                    .map_or(true, |&n| n > n_ij) &&
                grid.get(i + 1).and_then(|row| row.as_bytes().get(j))
                    .map_or(true, |&n| n > n_ij)
            }).map(move |j| (grid[i].as_bytes()[j] - b'0' + 1) as usize)
        ).sum::<usize>().to_string()
    } else {
        let mut grid = grid.iter().map(|row| row.as_bytes().to_owned()).collect::<Vec<_>>();
        let mut flood_stack = vec![];
        let mut biggest_basins = Vec::with_capacity(4);
        for i_root in 0 .. grid.len() {
            for j_root in 0 .. grid.len() {
                if grid[i_root][j_root] == b'9' {continue;}
                flood_stack.push((i_root, j_root));
                let mut basin_size = 0;
                while let Some((i, j)) = flood_stack.pop() {
                    if grid.get(i).and_then(|row| row.get(j))
                            .map_or(true, |&n| n == b'9') {continue;}
                    basin_size += 1;
                    grid[i][j] = b'9';
                    flood_stack.push((i, j - 1));
                    flood_stack.push((i, j + 1));
                    flood_stack.push((i - 1, j));
                    flood_stack.push((i + 1, j));
                }
                biggest_basins.push(basin_size);
                biggest_basins.sort_unstable_by_key(|&sz| std::cmp::Reverse(sz));
                if biggest_basins.len() > 3 {biggest_basins.pop();}
            }
        }
        biggest_basins.iter().product::<usize>().to_string()
    }
}

fn day10(part: char, s: &str) -> String {
    let mut to_match = Vec::new();
    let mut completions = Vec::new();
    let iter = s.lines().map(|line| {
        to_match.clear();
        for &c in line.as_bytes() {match (to_match.last(), c) {
            (_, b'(' | b'[' | b'{' | b'<') => {to_match.push(c);},
            (Some(b'('), b')') | (Some(b'['), b']') |
            (Some(b'{'), b'}') | (Some(b'<'), b'>') => {to_match.pop();},
            (_, b')') => return    3, (_, b']') => return    57,
            (_, b'}') => return 1197, (_, b'>') => return 25137,
            _ => panic!()
        }};
        completions.push(to_match.iter().rev().fold(0u64, |a, d|
            5 * a + match d {
                b'(' => 1, b'[' => 2, b'{' => 3, b'<' => 4, _ => unreachable!()
            }
        ));
        0
    });

    if part == 'a' {
        iter.sum::<usize>().to_string()
    } else {
        for _ in iter {};
        completions.sort_unstable();
        completions[completions.len() / 2].to_string()
    }
}

fn day11(part: char, s: &str) -> String {
    let mut grid: Vec<Vec<u8>> = s.lines().map(|line| line.as_bytes().to_owned()).collect();
    let mut n_flashes = 0;
    let w = grid[0].len() - 1;
    let h = grid.len() - 1;
    let mut t = 0;
    while part == 'a' && t < 100 || part == 'b' && n_flashes < (w+1) * (h+1) {
        for line in &mut grid {for cell in line {*cell += 1;}}
        t += 1;
        let mut needs_propagate = true;
        if part == 'b' {n_flashes = 0;}
        while needs_propagate {
            needs_propagate = false;
            for i in 0 ..= h {
                for j in 0 ..= w {
                    if grid[i][j] > b'9' {
                        n_flashes += 1;
                        needs_propagate = true;
                        grid[i][j] = b'0';
                        if i > 0 && j > 0 && grid[i-1][j-1] != b'0' {grid[i-1][j-1] += 1};
                        if i > 0          && grid[i-1][j  ] != b'0' {grid[i-1][j  ] += 1};
                        if i > 0 && j < w && grid[i-1][j+1] != b'0' {grid[i-1][j+1] += 1};
                        if          j > 0 && grid[i  ][j-1] != b'0' {grid[i  ][j-1] += 1};
                        if          j < w && grid[i  ][j+1] != b'0' {grid[i  ][j+1] += 1}
                        if i < h && j > 0 && grid[i+1][j-1] != b'0' {grid[i+1][j-1] += 1}
                        if i < h          && grid[i+1][j  ] != b'0' {grid[i+1][j  ] += 1}
                        if i < h && j < w && grid[i+1][j+1] != b'0' {grid[i+1][j+1] += 1}
                    }
                }
            }
        }
    }
    (if part == 'a' {n_flashes} else {t}).to_string()
}

fn day12(part: char, s: &str) -> String {
    let mut node_ids: HashMap<&str, usize> = HashMap::new();
    let mut edges: Vec<Vec<usize>> = Vec::new();
    let mut is_small = 0usize;
    for line in s.lines() {
        let (from, to) = str_split_once(line, "-").unwrap();
        let from_id = *node_ids.entry(from).or_insert_with(||{
            is_small |= (from.as_bytes()[0].is_ascii_lowercase() as usize) << edges.len();
            edges.push(vec![]);
            edges.len() - 1
        });
        let to_id = *node_ids.entry(to).or_insert_with(||{
            is_small |= (to.as_bytes()[0].is_ascii_lowercase() as usize) << edges.len();
            edges.push(vec![]);
            edges.len() - 1
        });
        edges[from_id].push(to_id);
        edges[to_id].push(from_id);
    }

    let n_nodes = node_ids.len();
    assert!(n_nodes <= 32); 
    let start_node = node_ids["start"];
    let end_node = node_ids["end"];
    let mut cache = vec![None; n_nodes << n_nodes + 1];

    fn recurse (
        prevs: usize, last: usize, start_node: usize, end_node: usize, is_small: usize,
        edges: &Vec<Vec<usize>>, cache: &mut Vec<Option<usize>>, revisit_budget: bool
    ) -> usize {
        let n_nodes = edges.len();
        if last == end_node {return 1;}
        let cache_ix = (prevs << 1) + (last << (n_nodes + 1)) + (revisit_budget as usize);
        if let Some(res) = cache[cache_ix] {return res;}

        let res = edges[last].iter().map(|&next| {
            if prevs & (1 << next) == 0 {
                recurse(prevs | is_small & (1 << next), next,
                        start_node, end_node, is_small, edges, cache, revisit_budget)
            } else if revisit_budget && next != start_node {
                recurse(prevs | is_small & (1 << next), next,
                        start_node, end_node, is_small, edges, cache, false)
            } else {0}
        }).sum();

        cache[cache_ix] = Some(res);
        res
    }

    recurse(1 << start_node, start_node, start_node, end_node, is_small,
            &edges, &mut cache, part == 'b').to_string()
}

fn day13(part: char, s: &str) -> String {
    let mut lines = s.lines();
    let mut dots = lines.by_ref().take_while(|line| line.len() > 0).map(|line| {
        let (x, y) = str_split_once(line, ",").unwrap();
        (x.parse().unwrap(), y.parse().unwrap())
    }).collect::<Vec<(i32, i32)>>();

    let mut do_fold = |line: &str| {
        let (xy, n) = str_split_once(
            line.strip_prefix("fold along ").unwrap(), "="
        ).unwrap();
        let n: i32 = n.parse().unwrap();
        if xy == "x" {
            for dot in &mut dots {if dot.0 > n {dot.0 = 2 * n - dot.0}}
        } else {
            for dot in &mut dots {if dot.1 > n {dot.1 = 2 * n - dot.1}}
        }
    };

    if part == 'a' {
        do_fold(lines.next().unwrap());
        dots.sort_unstable();
        dots.dedup();
        dots.len().to_string()
    } else {
        for line in lines{do_fold(line)}
        dots.sort_unstable();
        dots.dedup();
        (
            dots.iter().map(|&(_, y)| y).min().unwrap() ..= 
            dots.iter().map(|&(_, y)| y).max().unwrap()
        ).map(|y|
            (
                dots.iter().map(|&(x, _)| x).min().unwrap() ..= 
                dots.iter().map(|&(x, _)| x).max().unwrap()
            ).map(|x|
                if dots.contains(&(x, y)) {"# "} else {"  "}
            ).collect::<String>()
        ).collect::<Vec<_>>().join("\n")
    }
}

fn day14(part: char, s: &str) -> String {
    let mut lines = s.lines();
    let start = lines.next().unwrap().as_bytes();
    assert_eq!(lines.next(), Some(""));
    let pair_ids = lines.clone().enumerate().map(|(id, line)|
        ([line.as_bytes()[0], line.as_bytes()[1]], id)
    ).collect::<HashMap<[u8;2], usize>>();
    let rules = lines.map(|line| {
        let line = line.as_bytes();
        [pair_ids[&[line[0], line[6]]], pair_ids[&[line[6], line[1]]]]
    }).collect::<Vec<[usize; 2]>>();

    let mut tally = vec![0; rules.len()];
    for i in 1 .. start.len() {tally[pair_ids[&[start[i-1], start[i]]]] += 1;}
    for _ in 0 .. (if part == 'a' {10} else {40}) {
        let mut new_tally = vec![0; rules.len()];
        for (from, &[to_1, to_2]) in rules.iter().enumerate() {
            new_tally[to_1] += tally[from];
            new_tally[to_2] += tally[from];
        }
        tally = new_tally;
    }

    let mut char_tally = [0usize; 26];
    for (&[_, c], &id) in &pair_ids {char_tally[(c - b'A') as usize] += tally[id]};
    char_tally[(start[0] - b'A') as usize] += 1;

    (
        char_tally.iter().max().unwrap() - 
        char_tally.iter().filter(|&&n| n > 0).min().unwrap()
    ).to_string()
}

fn day15(part: char, s: &str) -> String {
    let mut grid = s.lines().map(|line| line.as_bytes().to_owned())
                    .collect::<Vec<Vec<u8>>>();
    let mut to_check: VecDeque<Vec<(usize, usize)>> = VecDeque::new();
    let mut cur_risk = 0;
    let mut w = grid[0].len();
    let mut h = grid.len();

    if part == 'b' {
        for row in &mut grid {
            for j in 0 .. 4 * w {
                row.push(if row[j] == b'9' {b'1'} else {row[j] + 1});
            }
        }
        for i in 0 .. 4 * h {
            grid.push(grid[i].clone());
            for cell in grid.last_mut().unwrap() {
                *cell = if *cell == b'9' {b'1'} else {*cell + 1}
            }
        }
        w *= 5;
        h *= 5;
    }

    to_check.push_back(vec![(0, 0)]);
    for _ in 0 .. 8 {to_check.push_back(Vec::new());}
    loop {
        to_check.push_back(Vec::new());
        let row = to_check.pop_front().unwrap();
        for (i, j) in row {
            grid[i][j] = b'#';
            if i == h - 1 && j == w - 1 {
                return cur_risk.to_string();
            }
            for &(ni, nj) in &[(i, j-1), (i, j+1), (i-1, j), (i+1, j)] {
                if let Some(&c @ b'1' ..= b'9') = 
                        grid.get(ni).and_then(|row| row.get(nj)) {
                    grid[ni][nj] = b'.';
                    to_check[(c - b'1') as usize].push((ni, nj));
                }
            }
        }
        cur_risk += 1;
    }
}

mod bits {
    pub enum Lti {Bits, Packets}
    pub enum Opcode {Sum = 0, Prod, Min, Max, Gt = 5, Lt, Eq}
    impl From<u8> for Opcode {fn from(n: u8) -> Opcode {use bits::Opcode::*; match n {
        0 => Sum, 1 => Prod, 2 => Min, 3 => Max, 5 => Gt, 6 => Lt, 7 => Eq, _ => panic!()
    }}}
    pub enum Data {Lit(usize), Op{op: Opcode, lti: Lti, args: Vec<Packet>}}
    pub struct Packet {pub v: u8, pub data: Data}

    trait Read {
        fn read_int(&mut self, bits: usize) -> usize;
        fn take(&mut self, bits: usize) -> Take where Self: Sized {Take(self, bits)}
    }

    struct Take<'a> (&'a mut dyn Read, usize);
    impl Take<'_> {fn is_empty(&self) -> bool {self.1 == 0}}
    impl Read for Take<'_> {
        fn read_int(&mut self, bits: usize) -> usize {
            self.1 = self.1.checked_sub(bits).unwrap();
            self.0.read_int(bits)
        }
    }

    impl Packet {
        fn from_bits<R: Read> (bits: &mut R) -> Self {
            let v = bits.read_int(3) as u8;
            let op = bits.read_int(3) as u8;
            if op == 4 {
                let mut acc = 0;
                loop {
                    let word = bits.read_int(5) as u8;
                    acc = 16 * acc + (word & 0xf) as usize;
                    if word & 0x10 == 0 {break;}
                }
                Packet{v, data: Data::Lit(acc)}
            } else {
                let lti = bits.read_int(1) as u8;
                let mut args = Vec::new();
                if lti == 0 {
                    let tlen = bits.read_int(15);
                    let mut arg_bits = bits.take(tlen);
                    while !arg_bits.is_empty() {args.push(Packet::from_bits(&mut arg_bits));}
                } else {
                    let argc = bits.read_int(11);
                    for _ in 0 .. argc {args.push(Packet::from_bits(&mut *bits));}
                }
                Packet{v, data: Data::Op{
                    op: op.into(), args, 
                    lti: if lti == 0 {Lti::Bits} else {Lti::Packets}
                }}
            }
        }
        pub fn eval(&self) -> usize {
            match &self.data {
                Data::Lit(n) => *n,
                Data::Op{args, op, ..} => {
                    let mut args = args.iter().map(|arg| Packet::eval(&arg));
                    match op {
                        Opcode::Sum  => args.sum(),
                        Opcode::Prod => args.product(),
                        Opcode::Min  => args.min().unwrap(),
                        Opcode::Max  => args.max().unwrap(),
                        Opcode::Gt   => (args.next().unwrap() >  args.next().unwrap()) as usize,
                        Opcode::Lt   => (args.next().unwrap() <  args.next().unwrap()) as usize,
                        Opcode::Eq   => (args.next().unwrap() == args.next().unwrap()) as usize,
                        
                    }
                }
            }
        }
    }

    struct HexRead<Src: std::io::Read> {src: std::io::Bytes<Src>, buf: u8, buf_len: u8}
    impl<Src: std::io::Read> HexRead<Src> {
        pub fn new(src: Src) -> Self {Self{src: src.bytes(), buf: 0, buf_len: 0}}
    }
    impl<Src: std::io::Read> Read for HexRead<Src> {
        fn read_int(&mut self, bits: usize) -> usize{
            let mut acc = 0;
            for _ in 0 .. bits {
                if self.buf_len == 0 {
                    self.buf = match self.src.next(){
                        Some(Ok(c @ b'0' ..= b'9')) => c - b'0',
                        Some(Ok(c @ b'A' ..= b'F')) => c - b'A' + 10,
                        e => panic!("{:?}", e)
                    };
                    self.buf_len = 4;
                }
                self.buf_len -= 1;
                acc = 2 * acc + (self.buf >> self.buf_len & 1) as usize;
            }
            acc
        }
    }

    impl From<&str> for Packet {
        fn from(s: &str) -> Packet {
            Packet::from_bits(&mut HexRead::new(s.as_bytes()))
        }
    }
}

fn day16(part: char, s: &str) -> String {
    fn versum (p: &bits::Packet) -> usize {
        p.v as usize + match &p.data {
            bits::Data::Lit(..) => 0, 
            bits::Data::Op{args, ..} => args.iter().map(|arg| versum(arg)).sum()
        }
    }
    
    let p = &bits::Packet::from(s);
    if part == 'a' {
        versum(&p).to_string()
    } else {
        bits::Packet::eval(&p).to_string()
    }
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