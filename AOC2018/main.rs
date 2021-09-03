use std::cell::RefCell;
use std::cmp::*;
use std::collections::*;
use std::convert::*;
use std::iter::once;

////////////////////////////////////////////////////////////////////////////////

mod intcode {
    use self::RunState::*;
    use std::error::Error;
    use std::fmt;
    use VecDeque;
    
    #[derive(Debug)]
    pub enum RunState {Running, NeedsInput, Success, Errored(CpuError)}

    #[derive(Debug)]
    pub struct CpuError{desc: String}
    impl Error for CpuError {}
    impl fmt::Display for CpuError {fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.desc)
    }}
    
    pub fn parse_intcode(s: &str) -> Vec<isize> {
        s.trim().split(",").map(|s| s.parse().unwrap()).collect()
    }

    pub struct Cpu {
        pub mem: Vec<isize>, ip: isize, rb: isize,
        input: VecDeque<isize>, output: VecDeque<isize>,
        pub state: RunState
    }    
    type MemResult = Result<isize, CpuError>;
    impl Cpu {
        pub fn new(mem: Vec<isize>) -> Self {Self {
            mem, ip: 0, rb: 0, state: Running,
            input: VecDeque::new(), output: VecDeque::new()
        }}
        
        pub fn run(&mut self) -> Result<(), &dyn Error> {
            while matches!(self.state, Running) {
                if let Err(e) = self.try_step() {self.state = Errored(e)}
            }
            match &self.state {Errored(e) => Err(e), _ => Ok(())}
        }
        
        pub fn send(&mut self, data: isize) {
            self.input.push_back(data);
            if matches!(self.state, NeedsInput) {self.state = Running}
        }
        
        pub fn send_ascii(&mut self, data: &str) {
            for c in data.chars() {self.send(c as isize);}
        }
        
        pub fn recv_ascii(&mut self) -> String {
            let mut r = String::with_capacity(self.output.len());
            while matches!(self.output.front(), Some(0 ..= 127)) {
                r.push(self.output.pop_front().unwrap() as u8 as char);
            }
            r
        }
        
        fn get_mem(&self, addr: isize) -> MemResult {
            Ok(self.mem.get(addr as usize).copied().unwrap_or(0))
        }
        
        fn get_op(&mut self) -> MemResult {
            self.ip += 1; self.get_mem(self.ip - 1)
        }

        fn get_arg(&mut self, mode: u8) -> MemResult {
            let addr = self.get_op()?;
            match mode {
                0 => self.get_mem(addr),
                1 => Ok(addr),
                2 => self.get_mem(addr + self.rb),
                _ => Err(CpuError{desc: format!("invalid get mode {}", mode)}),
            }
        }
        
        fn set_mem(&mut self, addr: isize, val: isize) -> Result<(), CpuError> {
            if addr < 0 {
                return Err(CpuError{desc: format!("attempt to set mem[{}]", addr)})
            } else if addr as usize >= self.mem.len() {self.mem.resize(addr as usize + 1, 0)}
            self.mem[addr as usize] = val;
            Ok(())
        }
        
        fn set_arg(&mut self, val: isize, mode: u8) -> Result<(), CpuError> {
            let addr = self.get_op()?;
            match mode {
                0 => self.set_mem(addr, val),
                2 => self.set_mem(addr + self.rb, val),
                _ => Err(CpuError{desc: format!("invalid set mode {}", mode)}),
            }
        }

        fn try_step(&mut self) -> Result<(), CpuError> {
            assert!(matches!(self.state, Running));
            let op = self.get_op()?;
            let m = ((op / 100 % 10) as u8, (op / 1000 % 10) as u8, (op / 10000) as u8);
            match op % 100 {
                1  => {let r = self.get_arg(m.0)? + self.get_arg(m.1)?; self.set_arg(r, m.2)?}
                2  => {let r = self.get_arg(m.0)? * self.get_arg(m.1)?; self.set_arg(r, m.2)?}
                3  => {match self.input.pop_front() {
                    Some(v) => self.set_arg(v, m.0)?, 
                    None => {self.state = NeedsInput; self.ip -= 1}
                }}
                4  => {let r = self.get_arg(m.0)?; self.output.push_back(r)}

                5  => {
                    let c = self.get_arg(m.0)? != 0;
                    let ip = self.get_arg(m.1)?;
                    if c {self.ip = ip}
                }
                6  => {
                    let c = self.get_arg(m.0)? == 0;
                    let ip = self.get_arg(m.1)?;
                    if c {self.ip = ip}
                }
                7  => {
                    let c = self.get_arg(m.0)? < self.get_arg(m.1)?;
                    self.set_arg(if c {1} else {0}, m.2)?
                }
                8  => {
                    let c = self.get_arg(m.0)? == self.get_arg(m.1)?;
                    self.set_arg(if c {1} else {0}, m.2)?
                }
                9  => {self.rb += self.get_arg(m.0)?}

                99 => {self.state = Success},
                _ => return Err(CpuError{desc: format!("invalid opcode {}", op)}),
            }
            Ok(())
        }
    }
}

////////////////////////////////////////////////////////////////////////////////

fn day1(part: char, s: &str) -> String {
    let freqs = s.lines().map(|line| {
        line.parse::<i32>().unwrap()
    }).collect::<Vec<i32>>();
    if part == 'a' {
        freqs.iter().sum::<i32>().to_string()
    } else {
        let mut freqs_seen = HashSet::new();
        let mut r = 0i32;
        loop { for f in &freqs {
            r += f;
            if !freqs_seen.insert(r) {return r.to_string()}
        }}
    }
}

fn day2(part: char, s: &str) -> String {
    if part == 'a'{
        let mut has_2 = 0u32;
        let mut has_3 = 0u32;
        for line in s.lines() {
            let mut char_counts = [0u8; 26];
            for c in line.bytes() {char_counts[(c - b'a') as usize] += 1};
            has_2 += char_counts.iter().any(|&c| c == 2) as u32;
            has_3 += char_counts.iter().any(|&c| c == 3) as u32;
        }
        (has_2 * has_3).to_string()
    } else {
        let lines = s.lines().collect::<Vec<&str>>();
        for (ixa, &a) in lines.iter().enumerate() {
            for &b in &lines[0 .. ixa] {
                if a.bytes().zip(b.bytes()).filter(|(ca, cb)| ca != cb).count() == 1 {
                    return a.bytes().zip(b.bytes()).filter_map(|(ca, cb)|
                        if ca == cb {Some(ca as char)} else {None}
                    ).collect::<String>()
                }
            }
        }
        panic!()
    }
}

fn day3(part: char, s: &str) -> String {
    let mut fabric = [[0u8; 1000]; 1000];
    let claims = s.lines().map(|line| {
        let mut bytes = line.bytes().peekable(); 
        assert_eq!(bytes.next(), Some(b'#'));
        let mut id = 0;
        while matches!(bytes.peek(), Some(&(b'0' ..= b'9'))) {
           id = 10 * id + (bytes.next().unwrap() - b'0') as usize;
        }
        assert_eq!(bytes.next(), Some(b' '));
        assert_eq!(bytes.next(), Some(b'@'));
        assert_eq!(bytes.next(), Some(b' '));
        let mut x = 0;
        while matches!(bytes.peek(), Some(&(b'0' ..= b'9'))) {
           x = 10 * x + (bytes.next().unwrap() - b'0') as usize;
        }
        assert_eq!(bytes.next(), Some(b','));
        let mut y = 0;
        while matches!(bytes.peek(), Some(&(b'0' ..= b'9'))) {
            y = 10 * y + (bytes.next().unwrap() - b'0') as usize;
        }
        assert_eq!(bytes.next(), Some(b':'));
        assert_eq!(bytes.next(), Some(b' '));
        let mut w = 0;
        while matches!(bytes.peek(), Some(&(b'0' ..= b'9'))) {
           w = 10 * w + (bytes.next().unwrap() - b'0') as usize;
        }
        assert_eq!(bytes.next(), Some(b'x'));
        let mut h = 0;
        while matches!(bytes.peek(), Some(&(b'0' ..= b'9'))) {
           h = 10 * h + (bytes.next().unwrap() - b'0') as usize;
        }
        for i in (y..).take(h) {for j in (x..).take(w) {if fabric[i][j] < 2 {fabric[i][j] += 1;}}}
        
        (id, x, y, w, h)
    }).collect::<Vec<_>>();
    
    if part == 'a' {
        fabric.iter().flatten().filter(|&&c| c == 2).count().to_string()
    } else {
        claims.iter().find_map(|&(id, x, y, w, h)|
            if (y..).take(h).all(|i| (x..).take(w).all(|j| fabric[i][j] == 1))
                {Some(id)} else {None}
        ).unwrap().to_string()
    }
}

fn day4(part: char, s: &str) -> String {
    const MIN_AT: usize = "[????-??-?? ??:".len();
    const EVT_AT: usize = "[????-??-?? ??:??] ".len();
    const GUARD_ID_AT: usize = "[????-??-?? ??:??] Guard #".len();
    let mut lines = s.lines().collect::<Vec<_>>();
    lines.sort_unstable();
    
    let mut guard_id = None;
    let mut t_fell_asleep = None;
    let mut tallies: HashMap<&str, [u32; 60]> = HashMap::new();
    for line in lines {
        let min = line[MIN_AT .. MIN_AT + 2].parse::<usize>().unwrap();
        match &line[EVT_AT .. EVT_AT + 1] {
            "G"/*uard begins shift*/ => guard_id = Some(
                line[GUARD_ID_AT ..].split(' ').next().unwrap()
            ),
            "f"/*alls asleep*/ => t_fell_asleep = Some(min),
            "w"/*akes up*/ => {
                let gt = tallies.entry(guard_id.unwrap()).or_insert([0; 60]);
                for t in t_fell_asleep.unwrap() .. min {gt[t] += 1;}
                t_fell_asleep = None;
            }
            _ => panic!()
        }
    }
    
    if part == 'a' {
        let (id, gt) = tallies.iter().max_by_key(|(_, gt)| gt.iter().sum::<u32>()).unwrap();
        (id.parse::<usize>().unwrap() * (0 .. 60).max_by_key(|&min| gt[min]).unwrap()).to_string()
    } else {
        let (id, gt) = tallies.iter().max_by_key(|(_, gt)| gt.iter().max()).unwrap();
        (id.parse::<usize>().unwrap() * (0 .. 60).max_by_key(|&min| gt[min]).unwrap()).to_string()
    }
}

fn day5(part: char, s: &str) -> String {
    let mut s2 = vec![0u8; 0];
    for &c in s.trim().as_bytes() {
        if s2.last() == Some(&(c ^ 0x20)) {s2.pop();} else {s2.push(c);}
    }

    if part == 'a' {return s2.len().to_string()}

    let mut best = s2.len();
    let mut s3 = vec![0u8; 0];
    for rem in b'A' ..= b'Z' {
        for &c in &s2 {
            if c.to_ascii_uppercase() != rem {
                if s3.last() == Some(&(c ^ 0x20)) {s3.pop();} else {s3.push(c);}
            }
        }
        best = best.min(s3.len());
        s3.clear();
    }
    
    best.to_string()
}

fn day6(part: char, s: &str) -> String {
    let pts = s.lines().map(|line|
        line.split(", ").map(|c| c.parse::<i32>().unwrap())
            .collect::<Vec<_>>().try_into().unwrap()
    ).collect::<Vec<[i32; 2]>>();
    
    let mut minx = pts[0][0];
    let mut maxx = pts[0][0];
    let mut miny = pts[0][1];
    let mut maxy = pts[0][1];
    for p in &pts {
        minx = minx.min(p[0]);
        maxx = maxx.max(p[0]);
        miny = miny.min(p[1]);
        maxy = maxy.max(p[1]);
    }
    
    if part == 'a'{
        let mut tallies = vec![0; pts.len()];
        let mut infinites = vec![false; pts.len()];
        for y in miny ..= maxy {for x in minx ..= maxx {
            let mut best_pt = None;
            let mut best_dist = i32::MAX;
            for i in 0 .. pts.len() {
                let dist = (pts[i][0] - x).abs() + (pts[i][1] - y).abs();
                if dist < best_dist {
                    best_pt = Some(i);
                    best_dist = dist;
                } else if dist == best_dist {
                    best_pt = None;
                }
            }
            if let Some(i) = best_pt {
                tallies[i] += 1;
                if y == miny || y == maxy || x == minx || x == maxx {infinites[i] = true}
            }
        }}
        tallies.iter().zip(infinites).filter_map(|(t, i)|
            if i {None} else {Some(t)}
        ).max().unwrap().to_string()
    } else {
        let pts_ref = &pts;
        (miny ..= maxy).flat_map(|y|
            (minx ..= maxx).filter(move |x| {
                let dist = pts_ref.iter().map(|p| (p[0] - x).abs() + (p[1] - y).abs()).sum::<i32>();
                dist < 10000
            })
        ).count().to_string()
    }
}

fn day7(part: char, s: &str) -> String {
    const BEFORE_AT: usize = "Step ".len();
    const AFTER_AT: usize = "Step ? must be finished before step ".len();
    let mut pairs = s.lines().map(|line|
        (line[BEFORE_AT ..].chars().next().unwrap(), line[AFTER_AT ..].chars().next().unwrap())
    ).collect::<Vec<_>>();
    let mut to_collect = ('A' ..= 'Z').collect::<Vec<_>>();
    
    if part == 'a' {
        let mut collected = String::with_capacity(26);
        while let Some(&c) = to_collect.iter().find(|&&c| pairs.iter().all(|&(_, a)| a != c)){
            collected.push(c);
            pairs.retain(|&(b, _)| b != c);
            to_collect.retain(|&tr| tr != c);
        }
        collected
    } else {
        let mut ongoing = [None, None, None, None, None];
        let mut t = 0u32;
        loop {
            for o in &mut ongoing {if let Some((oc, ot)) = *o {if ot == t {
                *o = None;
                pairs.retain(|&(b, _)| b != oc);
            }}}
            for o in &mut ongoing {if *o == None {
                if let Some(&c) = to_collect.iter().find(|&&c| pairs.iter().all(|&(_, a)| a != c)){
                    *o = Some((c, t + (c as u8 - b'A') as u32 + 61));
                    pairs.retain(|&(_, a)| a != c);
                    to_collect.retain(|&tr| tr != c);
                }
            }}
            if let Some(next_t) = ongoing.iter().filter_map(|o| o.map(|(_, ot)| ot)).min() {
                t = next_t;
            } else {
                break;
            }
        }
        t.to_string()
    }
}

fn day8(part: char, s: &str) -> String {
    let mut nums = s.trim().split(" ").map(|num_str| num_str.parse::<usize>().unwrap());
    enum RToken {Child, Meta, End(usize)}
    let mut parse_stack_r = vec![RToken::Child];
    enum LToken {Node(usize), Meta(usize)}
    
    if part == 'a' {
        let mut meta_sum = 0usize;
        loop {match parse_stack_r.pop() {
            Some(RToken::Child) => {
                let children = nums.next().unwrap();
                for _ in 0 .. nums.next().unwrap() {parse_stack_r.push(RToken::Meta);}
                for _ in 0 .. children {parse_stack_r.push(RToken::Child);}
            }
            Some(RToken::Meta) => {meta_sum += nums.next().unwrap()}
            Some(RToken::End(_)) => unreachable!(),
            None => break
        }}
        assert_eq!(nums.next(), None);
        meta_sum.to_string()
    } else {
        let mut parse_stack_l = vec![];
        loop {match parse_stack_r.pop() {
            Some(RToken::Child) => {
                let children = nums.next().unwrap();
                let metas = nums.next().unwrap();
                parse_stack_r.push(RToken::End(children + metas));
                for _ in 0 .. metas {parse_stack_r.push(RToken::Meta);}
                for _ in 0 .. children {parse_stack_r.push(RToken::Child);}
            }
            Some(RToken::Meta) => {parse_stack_l.push(LToken::Meta(nums.next().unwrap()))}
            Some(RToken::End(toks)) => {
                let mut node_toks = parse_stack_l.split_off(parse_stack_l.len() - toks);
                if matches!(node_toks[0], LToken::Meta(_)) {
                    parse_stack_l.push(LToken::Node(node_toks.iter().map(|t|
                        match t {LToken::Meta(m) => m, _ => unreachable!()}
                    ).sum::<usize>()));
                } else {
                    let mut val = 0;
                    while let Some(LToken::Meta(m)) = node_toks.pop() {
                        if let Some(LToken::Node(n)) = node_toks.get(m - 1) {val += n;}
                    }
                    parse_stack_l.push(LToken::Node(val));
                }
            }
            None => break
        }}
        assert_eq!(nums.next(), None);
        if let [LToken::Node(n)] = parse_stack_l[..] {n.to_string()} else {panic!()}
    }
}

fn day9(part: char, s: &str) -> String {
    let players = s.split(' ').next().unwrap().parse().unwrap();
    let mut scores = vec![0; players];
    let mut nexts = vec![0];
    let mut prevs = vec![0];
    let mut curr = 0;
    let marbles = 
        s.split(' ').nth(6).unwrap().parse::<usize>().unwrap() * (if part == 'a' {1} else {100});
    for t in 1 ..= marbles {
        if t % 23 == 0 {
            for _ in 0 .. 6 {curr = prevs[curr];}
            scores[t % players] += prevs[curr] + t;
            prevs[curr] = prevs[prevs[curr]];
            nexts[prevs[curr]] = curr;
            nexts.push(0xdead);
            prevs.push(0xdead);
        } else {
            curr = nexts[curr];
            nexts.push(nexts[curr]);
            prevs.push(curr);
            nexts[curr] = t;
            prevs[nexts[t]] = t;
            curr = t;
        }
    }
    scores.iter().max().unwrap().to_string()
}



fn day10(part: char, s: &str) -> String {
    // position=<-?????, -?????> velocity=<-?, -?>
    // 0123456789012345678901234567890123456789012
    // 0000000000111111111122222222223333333333444
    let pts: Vec<(i32, i32, i32, i32)> = s.lines().map(|line| (
        line[10 .. 16].trim().parse().unwrap(),
        line[18 .. 24].trim().parse().unwrap(),
        line[36 .. 38].trim().parse().unwrap(),
        line[40 .. 42].trim().parse().unwrap()
    )).collect();
    // we ballpark the time of intersection as the time the top left particle
    // crosses the x axis, then back up until we minimize the y extent of the bundle
    let height_at = |t: i32| -> i32 {
        let mut miny = pts[0].1 + pts[0].3 * t;
        let mut maxy = miny;
        for pt in &pts {
            miny = miny.min(pt.1 + pt.3 * t);
            maxy = maxy.max(pt.1 + pt.3 * t);
        }
        maxy - miny
    };
    let (_, py, _, vy) = pts.iter().min().unwrap();
    let mut t = -py / vy;
    let mut h = height_at(t);
    loop {
        let nh = height_at(t - 1);
        if nh > h {break;}
        t = t - 1;
        h = nh;
    }

    if part == 'b' {return t.to_string()}

    let mut miny = pts[0].1 + pts[0].3 * t;
    let mut maxy = miny;
    let mut minx = pts[0].0 + pts[0].2 * t;
    let mut maxx = minx;
    for pt in &pts {
        miny = miny.min(pt.1 + pt.3 * t);
        maxy = maxy.max(pt.1 + pt.3 * t);
        minx = minx.min(pt.0 + pt.2 * t);
        maxx = maxx.max(pt.0 + pt.2 * t);
    }
    let mut buf = (miny ..= maxy).map(|_|
        (minx ..= maxx).map(|_| b'.').collect()
    ).collect::<Vec<Vec<u8>>>();
    for pt in pts {
        buf[(pt.1 + pt.3 * t - miny) as usize][(pt.0 + pt.2 * t - minx) as usize] = b'#'
    }
    String::from_utf8(buf.join(&b'\n')).unwrap()
}

fn day11(part: char, s: &str) -> String {
    let gsn: i32 = s.trim().parse().unwrap();
    let pl = |x: i32, y: i32| -> i32 {((x + 10) * y + gsn) * (x + 10) / 100 % 10 - 5};

    if part == 'a' {
        let (x, y) = (1 .. 298).flat_map(|y| (1 .. 298).map(move |x| (x, y))).max_by_key(|&(x, y)|
            pl(x, y) + pl(x, y + 1) + pl(x, y + 2) + pl(x + 1, y) + pl(x + 1, y + 1) +
            pl(x + 1, y + 2) + pl(x + 2, y) + pl(x + 2, y + 1) + pl(x + 2, y + 2)
        ).unwrap();
        format!("{},{}", x, y)
    } else {
        let row_sums: Vec<Vec<i32>> = (0 ..= 300).map(|y|
            once(0).chain((0 ..= 300).scan(0, |s, x| {*s += pl(x, y); Some(*s)})).collect()
        ).collect();
        let (x, y, d, _) = (1 ..= 300).map(|d| {
            let d_row_sums: Vec<Vec<i32>> = row_sums.iter().map(|row|
                (0 ..= 300 - d + 1).map(|x| row[x + d] - row[x]).collect()
            ).collect();
            let mut col_sums = vec![vec![0i32; 300 - d + 2]];
            for y in 1 ..= 301 {
                col_sums.push(vec![]);
                for x in 0 ..= 300 - d + 1 {
                    let ns = col_sums[y - 1][x] + d_row_sums[y - 1][x];
                    col_sums[y].push(ns);
                }
            }
            let csr = &col_sums;
            (1 ..= 300 - d + 1).flat_map(|y|
                (1 ..= 300 - d + 1).map(move |x| (x, y, d, csr[y + d][x] - csr[y][x]))
            ).max_by_key(|&(_, _, _, score)| score).unwrap()
        }).max_by_key(|&(_, _, _, score)| score).unwrap();
        format!("{},{},{}", x, y, d)
    }
}

fn day12(part: char, s: &str) -> String {
    let mut lines = s.lines();
    let line = lines.next().unwrap();
    assert!(line.starts_with("initial state: "));
    let mut state: Vec<u8> = line["initial state: ".len() ..].bytes()
        .map(|c| match c {b'.' => 0, b'#' => 1, _ => panic!()}).collect();
    assert_eq!(lines.next(), Some(""));
    let mut rules = [255; 32];
    for line in lines{
        let rule_ix = line[0..5].bytes()
                                .map(|c| match c {b'.' => 0, b'#' => 1, _ => panic!()})
                                .fold(0, |a, b| 2 * a + b);
        rules [rule_ix] = match line.bytes().rev().next().unwrap() {
            b'.' => 0, b'#' => 1, _ => panic!()
        };
    }
    let mut state_at = 0i64;
    if state[0] != 0 {state_at -= 1; state.insert(0, 0);}
    if state[1] != 0 {state_at -= 1; state.insert(0, 0);}
    if state[state.len() - 1] != 0 {state.push(0);}
    if state[state.len() - 2] != 0 {state.push(0);}
    for t in 1 .. {
        if state[2] != 0 {state_at -= 1; state.insert(0, 0);}
        if state[3] != 0 {state_at -= 1; state.insert(0, 0);}
        while state[4] == 0 {state_at += 1; state.remove(0);}
        if state[state.len() - 3] != 0 {state.push(0);}
        if state[state.len() - 4] != 0 {state.push(0);}
        while state[state.len() - 5] == 0 {state.pop();}
        let mut state_new = state.clone();
        for i in 2 .. state.len() - 2 {
            let rule_ix = state[i - 2 ..= i + 2].iter().fold(0, |a, b| 2 * a + b);
            state_new[i] = rules[rule_ix as usize];
        }
        if part == 'a' && t == 20 {
            return (state_at ..).zip(state_new).fold(0, |a, (i, v)| a + i * v as i64).to_string();
        }
        if part == 'b' && trimmed(&state_new) == trimmed(&state) {
            let t_rem = 50_000_000_000 - t as i64;
            let r_prev = (state_at ..).zip(state)
                                      .fold(0, |a, (i, v)| a + i * v as i64);
            let r_curr = (state_at ..).zip(state_new)
                                      .fold(0, |a, (i, v)| a + i * v as i64);
            return (r_curr + (r_curr - r_prev) * t_rem).to_string();
        }
        state = state_new;
    }
    unreachable!();

    fn trimmed(mut a: &[u8]) -> &[u8] {
        while a.first() == Some(&0) {a = &a[1 ..];}
        while a.last() == Some(&0) {a = &a[.. a.len() - 1];}
        a
    }
}

fn day13(part: char, s: &str) -> String {
    #[derive(Clone, Copy)]
    enum Dir {North, South, West, East}
    #[derive(Clone, Copy)]
    enum Turn {Left, Straight, Right}
    impl Turn {
        fn iter() -> impl Iterator<Item = Turn> {
            [Turn::Left, Turn::Straight, Turn::Right].iter().cycle().cloned()
        }
        fn apply(self, d: Dir) -> Dir { match (self, d) {
            (Turn::Left, Dir::North) => Dir::West, (Turn::Left, Dir::South) => Dir::East,
            (Turn::Left, Dir::East) => Dir::North, (Turn::Left, Dir::West) => Dir::South,
            (Turn::Right, Dir::North) => Dir::East, (Turn::Right, Dir::South) => Dir::West,
            (Turn::Right, Dir::West) => Dir::North, (Turn::Right, Dir::East) => Dir::South,
            (Turn::Straight, d) => d
        }}
    }
    let mut map: Vec<Vec<u8>> = s.lines().map(|line| line.as_bytes().to_vec()).collect();
    let mut carts: Vec<(usize, usize, Dir, _, bool)> = vec![];
    for (y, row) in map.iter_mut().enumerate() {
        for (x, c) in row.iter_mut().enumerate() {
            match *c {
                b'^' => {*c = b'|'; carts.push((x, y, Dir::North, Turn::iter(), false))},
                b'v' => {*c = b'|'; carts.push((x, y, Dir::South, Turn::iter(), false))},
                b'<' => {*c = b'-'; carts.push((x, y, Dir::West,  Turn::iter(), false))},
                b'>' => {*c = b'-'; carts.push((x, y, Dir::East,  Turn::iter(), false))},
                _ => {}
            }
        }
    }

    loop {
        carts.sort_unstable_by_key(|&(x, y, _, _, _)| (y, x));
        for i in 0 .. carts.len() {
            let cart = &mut carts[i];
            if cart.4 {continue;}
            match cart.2 {
                Dir::North => cart.1 -= 1,
                Dir::South => cart.1 += 1,
                Dir::West  => cart.0 -= 1,
                Dir::East  => cart.0 += 1,
            }
            match map[cart.1][cart.0] {
                b'/' => cart.2 = match cart.2 { 
                    Dir::North => Dir::East, Dir::South => Dir::West,
                    Dir::East => Dir::North, Dir::West => Dir::South,
                },
                b'\\' => cart.2 = match cart.2 { 
                    Dir::North => Dir::West, Dir::South => Dir::East,
                    Dir::West => Dir::North, Dir::East => Dir::South,
                },
                b'+' => cart.2 = cart.3.next().unwrap().apply(cart.2),
                _ => {}
            }
            for j in 0 .. carts.len() {
                if i != j && !carts[j].4 && carts[i].0 == carts[j].0 && carts[i].1 == carts[j].1 {
                    if part == 'a' {
                        return format!("{},{}", carts[i].0, carts[i].1)
                    } else {
                        carts[i].4 = true;
                        carts[j].4 = true;
                    }
                }
            }
        }
        carts.retain(|&(_, _, _, _, dead)| !dead);
        if let [(x, y, _, _, _)] = carts[..] {return format!("{},{}", x, y)}
    }
}

fn day14(part: char, s: &str) -> String {
    let a_goal: usize = s.trim().parse().unwrap();
    let b_goal: Vec<u8> = s.trim().bytes().map(|c| c - b'0').collect();
    let mut fst_at = 0;
    let mut snd_at = 1;
    let mut recipes: Vec<u8> = vec![3, 7];
    recipes.reserve(a_goal + 11);
    loop {
        let new_recipe = recipes[fst_at] + recipes[snd_at];
        if new_recipe > 9 {
            recipes.push(1);
            if part == 'b' && recipes.len() >= b_goal.len() 
                           && recipes[recipes.len() - b_goal.len() ..] == b_goal[..]
            {
                return (recipes.len() - b_goal.len()).to_string()
            }
        }
        recipes.push(new_recipe % 10);
        if part == 'b' && recipes.len() >= b_goal.len() 
                       && recipes[recipes.len() - b_goal.len() ..] == b_goal[..]
        {
            return (recipes.len() - b_goal.len()).to_string()
        }
        if part == 'a' && recipes.len() >= a_goal + 10 {
            return recipes[a_goal .. a_goal + 10].iter().map(|&c| (c + b'0') as char).collect()
        }
        fst_at = (fst_at + recipes[fst_at] as usize + 1) % recipes.len();
        snd_at = (snd_at + recipes[snd_at] as usize + 1) % recipes.len();
    }
}

fn day15(part: char, s: &str) -> String {
    'outer: for elf_power in 3 .. {
        let mut map: Vec<Vec<u8>> = s.lines().map(|line| line.as_bytes().to_vec()).collect();
        let mut units: Vec<RefCell<(usize, usize, u8, u8)>> = vec![];
        let mut goblins_rem = 0;
        let mut elves_rem = 0;
        for (y, row) in map.iter().enumerate() {for (x, &c) in row.iter().enumerate() {
            if c == b'E' || c == b'G' {units.push(RefCell::new((x, y, c, 200u8)));}
            if c == b'E' {elves_rem += 1;}
            if c == b'G' {goblins_rem += 1;}
        }}
        let elves_start = elves_rem;

        let mut prev_closest: HashSet<(usize, usize)> = HashSet::new();
        for t in 0 .. {
            units.sort_unstable_by_key(|cell| {let unit = cell.borrow(); (unit.1, unit.0)});
            for i in 0 .. units.len() {
                if units[i].borrow().3 == 0 {continue;}

                if goblins_rem == 0 || elves_rem == 0 {
                    let hp: i32 = units.iter().map(|cell| cell.borrow().3 as i32).sum();
                    if part == 'b' && elves_rem != elves_start {continue 'outer}
                    return (hp * t).to_string();
                }    
                // spec says to find all closest points adjacent to an enemy,    E....G-->E
                // take the lexicographically first of them,                        .###
                // then take the lexicograhpically first step to _that_ tile.       E E
                let mut unit = units[i].borrow_mut();
                prev_closest.clear();
                let mut closest = BTreeMap::new();
                closest.insert((unit.1, unit.0), (unit.1, unit.0));
                let next_pos = 'l: loop {
                    let mut next_closest: BTreeMap<(usize, usize), (usize, usize)> = BTreeMap::new();
                    for (&(cy, cx), &v) in &closest {
                        for &(nx, ny) in &[(cx, cy - 1), (cx - 1, cy), (cx + 1, cy), (cx, cy + 1)] {
                            match map[ny][nx] {
                                c @ b'G' | c @ b'E' if c != unit.2 => {break 'l v}
                                b'.' if !prev_closest.contains(&(ny, nx)) => {
                                    next_closest.entry((ny, nx))
                                        .and_modify(|ov| *ov = (*ov).min(v))
                                        .or_insert_with(||
                                            if v == (unit.1, unit.0) {(ny, nx)} else {v}
                                        );
                                }
                                _ => {}
                            }
                        }
                    }
                    if closest.is_empty() {break 'l (unit.1, unit.0);}
                    prev_closest.extend(closest.keys());
                    closest = next_closest;
                };
                map[unit.1][unit.0] = b'.';
                unit.0 = next_pos.1;
                unit.1 = next_pos.0;
                map[unit.1][unit.0] = unit.2;
                // next up, the attack phase
                // moving doesn't care about unit HP, but attacking does
                if let Some(cell) = units.iter().filter(|cell| {
                    cell.try_borrow().map_or(false, |enemy|
                        (enemy.0 as isize - unit.0 as isize).abs() +
                        (enemy.1 as isize - unit.1 as isize).abs() == 1 &&
                        enemy.2 != unit.2 && enemy.3 > 0
                    )
                }).min_by_key(|cell| cell.borrow().3){
                    let mut enemy = cell.borrow_mut();
                    let attack = if enemy.2 == b'G' {elf_power} else {3};
                    if enemy.3 > attack {
                        enemy.3 -= attack;
                    } else {
                        enemy.3 = 0;
                        map[enemy.1][enemy.0] = b'.';
                        if enemy.2 == b'G' {goblins_rem -= 1;} else {elves_rem -= 1;}
                    }
                }
            }
            
            units.retain(|cell| cell.borrow().3 > 0);
        }
        unreachable!();
    }
    unreachable!();
}

fn day16(part: char, s: &str) -> String {
    let ops: [(&str, &dyn Fn(usize, usize, usize, [usize; 4]) -> Option<[usize; 4]>); 16] = [
        ("addr", &|a, b, c, mut mem| {let r = *mem.get(a)? + *mem.get(b)?; *mem.get_mut(c)? = r; Some(mem)}),
        ("addi", &|a, b, c, mut mem| {let r = *mem.get(a)? + b; *mem.get_mut(c)? = r; Some(mem)}),
        ("mulr", &|a, b, c, mut mem| {let r = *mem.get(a)? * *mem.get(b)?; *mem.get_mut(c)? = r; Some(mem)}),
        ("muli", &|a, b, c, mut mem| {let r = *mem.get(a)? * b; *mem.get_mut(c)? = r; Some(mem)}),
        ("banr", &|a, b, c, mut mem| {let r = *mem.get(a)? & *mem.get(b)?; *mem.get_mut(c)? = r; Some(mem)}),
        ("bani", &|a, b, c, mut mem| {let r = *mem.get(a)? & b; *mem.get_mut(c)? = r; Some(mem)}),
        ("borr", &|a, b, c, mut mem| {let r = *mem.get(a)? | *mem.get(b)?; *mem.get_mut(c)? = r; Some(mem)}),
        ("bori", &|a, b, c, mut mem| {let r = *mem.get(a)? | b; *mem.get_mut(c)? = r; Some(mem)}),
        ("setr", &|a, _, c, mut mem| {let r = *mem.get(a)? ; *mem.get_mut(c)? = r; Some(mem)}),
        ("seti", &|a, _, c, mut mem| {let r = a; *mem.get_mut(c)? = r; Some(mem)}),
        ("gtir", &|a, b, c, mut mem| {let r = a > *mem.get(b)?; *mem.get_mut(c)? = r as usize; Some(mem)}),
        ("gtri", &|a, b, c, mut mem| {let r = *mem.get(a)? > b; *mem.get_mut(c)? = r as usize; Some(mem)}),
        ("gtrr", &|a, b, c, mut mem| {let r = *mem.get(a)? > *mem.get(b)?; *mem.get_mut(c)? = r as usize; Some(mem)}),
        ("eqir", &|a, b, c, mut mem| {let r = a == *mem.get(b)?; *mem.get_mut(c)? = r as usize; Some(mem)}),
        ("eqri", &|a, b, c, mut mem| {let r = *mem.get(a)? == b; *mem.get_mut(c)? = r as usize; Some(mem)}),
        ("eqrr", &|a, b, c, mut mem| {let r = *mem.get(a)? == *mem.get(b)?; *mem.get_mut(c)? = r as usize; Some(mem)})
    ];
    
    let mut lines = s.lines();
    let mut n_ge_3 = 0;
    let mut opcode_map = [0xffffu16; 16];
    while let Some(before_str) = lines.next().unwrap().strip_prefix("Before: [").and_then(|s| s.strip_suffix("]")) {
        let before_mem: [usize; 4] = before_str.split(", ").map(|s| s.parse().unwrap()).collect::<Vec<_>>().try_into().unwrap();
        let [opcode, a, b, c]: [usize; 4] = lines.next().unwrap().split(" ").map(|s| s.parse().unwrap()).collect::<Vec<_>>().try_into().unwrap();
        let after_mem: [usize; 4] = lines.next().unwrap().strip_prefix("After:  [").unwrap().strip_suffix("]").unwrap().split(", ")
                                        .map(|s| s.parse().unwrap()).collect::<Vec<_>>().try_into().unwrap();
        assert_eq!(lines.next(), Some(""));
        
        let sats = ops.iter().enumerate().filter_map(|(i, (_, f))| (f(a, b, c, before_mem) == Some(after_mem)).then(|| i))
                                        .fold(0u16, |a, i| a | 1 << i);
        if sats.count_ones() >= 3 {n_ge_3 += 1;}
        opcode_map[opcode] &= sats;
    }
    assert_eq!(lines.next(), Some(""));
    if part == 'a' {return n_ge_3.to_string();}

    loop {
        let mut done = true;
        for j in 0..16 {
            if opcode_map[j].is_power_of_two() {
                for i in 0..16 {if i != j {opcode_map[i] &= !opcode_map[j];}}
            } else {
                done = false;
            }
        }
        if done {break};
    }
    let opcode_map: [usize; 16] = opcode_map.iter().map(|&s| s.trailing_zeros() as usize).collect::<Vec<_>>().try_into().unwrap();

    let mut mem = [0usize; 4];
    for line in lines {
        let [opcode, a, b, c]: [usize; 4] = line.split(" ").map(|s| s.parse().unwrap()).collect::<Vec<_>>().try_into().unwrap();
        mem = ops[opcode_map[opcode]].1(a, b, c, mem).unwrap();
    }
    mem[0].to_string()
}

fn day17(part: char, s: &str) -> String {
    let mut minx = 500;
    let mut map = vec![vec![b'.'; 501]];
    map[0][500] = b'+';
    for line in s.lines() {
        let mut chars = line.chars();
        let fst_axis = chars.next().unwrap();
        assert_eq!(chars.next(), Some('='));
        let fst_coord: usize = chars.by_ref().take_while(|c| c.is_ascii_digit()).collect::<String>().parse().unwrap();
        //assert_eq!(chars.next(), Some(','));
        assert_eq!(chars.next(), Some(' '));
        let snd_axis = chars.next().unwrap();
        assert_eq!(chars.next(), Some('='));
        let snd_from: usize = chars.by_ref().take_while(|c| c.is_ascii_digit()).collect::<String>().parse().unwrap();
        //assert_eq!(chars.next(), Some('.'));
        assert_eq!(chars.next(), Some('.'));
        let snd_to: usize = chars.by_ref().take_while(|c| c.is_ascii_digit()).collect::<String>().parse().unwrap();
        assert_eq!(chars.next(), None);

        let (x_from, x_to, y_from, y_to) = match (fst_axis, snd_axis) {
            ('x', 'y') => (fst_coord, fst_coord, snd_from, snd_to),
            ('y', 'x') => (snd_from, snd_to, fst_coord, fst_coord),
            _ => panic!("{:?}", (fst_axis, snd_axis))
        };
        if map.len() <= y_to {let len = map[0].len(); map.resize_with(y_to + 1, ||vec![b'.'; len]);}
        if map[0].len() <= x_to + 1 {for row in &mut map {row.resize(x_to + 2, b'.');}}
        if minx > x_from {minx = x_from;}
        for y in y_from ..= y_to {for x in x_from ..= x_to {map[y][x] = b'#';}}
    }
    let miny = map.iter().position(|row| row.contains(&b'#')).unwrap();

    let mut flows = vec![(500, 1)];
    while let Some((flow_x, flow_y)) = flows.pop() {
        map[flow_y][flow_x] = b'|';
        if flow_y == map.len() - 1 {continue;}
        // horizontal flows go over clay and still water, but through sand and into other flows
        let mut flow_left = flow_x;
        while matches!(map[flow_y + 1][flow_left], b'#' | b'~') && map[flow_y][flow_left - 1] != b'#' {flow_left -= 1;}
        let mut flow_right = flow_x;
        while matches!(map[flow_y + 1][flow_right], b'#' | b'~') && map[flow_y][flow_right + 1] != b'#' {flow_right += 1;}

        if map[flow_y + 1][flow_left] == b'.' {flows.push((flow_left, flow_y + 1));}
        if map[flow_y + 1][flow_right] == b'.' && flow_left != flow_right {flows.push((flow_right, flow_y + 1));}
        if matches!(map[flow_y + 1][flow_left], b'.' | b'|') || matches!(map[flow_y + 1][flow_right], b'.' | b'|') {
            for x in flow_left ..= flow_right {
                map[flow_y][x] = b'|';
            }
        } else {
            for x in flow_left ..= flow_right {
                map[flow_y][x] = b'~';
                if map[flow_y - 1][x] == b'|' {flows.push((x, flow_y - 1))}
            }
        }
    }

    map[miny..].iter().flatten().filter(|&&c| c == b'~' || c == b'|' && part == 'a').count().to_string()
}

fn day18(part: char, s: &str) -> String {
    let mut map: Vec<Vec<u8>> = s.lines().map(|line| line.as_bytes().to_vec()).collect();
    let mut new_map = map.clone();
    let mut history = vec![];
    let end = if part == 'a' {10} else {1000_000_000};
    for t in 0 .. end {
        for y in 0 .. map.len() {
            for x in 0 .. map[y].len() {
                let mut trees = 0;
                let mut yards = 0;
                for &(dx, dy) in &[(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)] {
                    match map.get((y as isize + dy) as usize).and_then(|row| row.get((x as isize + dx) as usize)) {
                        Some(b'|') => trees += 1,
                        Some(b'#') => yards += 1,
                        _ => {}
                    }
                }
                new_map[y][x] = match map[y][x] {
                    b'.' => if trees >= 3 {b'|'} else {b'.'},
                    b'|' => if yards >= 3 {b'#'} else {b'|'},
                    b'#' => if yards == 0 || trees == 0 {b'.'} else {b'#'},
                    _ => panic!()
                }
            }
        }
        let old_map = map;
        map = new_map;
        new_map = old_map;
        history.push(map.clone());
        println!("{} {}", t, std::str::from_utf8(&map[0]).unwrap());
        if t > 0 && t % 2 == 0 && history[t] == history[t/2] {
            // we pull a history item such that it's an integer number of periods before the end and after the period starts
            map = history.into_iter().nth(t/2 + (end-1) % (t/2)).unwrap();
            break;
        }
}
    println!();
    for row in &map {println!("{}", std::str::from_utf8(row).unwrap());}
    (map.iter().flatten().filter(|&&c| c == b'|').count() * map.iter().flatten().filter(|&&c| c == b'#').count()).to_string()
}

fn day19(part: char, s: &str) -> String {
    let ops = |op: &str, a: usize, b: usize, c: usize, mut mem: [usize; 6]| match op {
    "addr" => {let r = *mem.get(a)? + *mem.get(b)?; *mem.get_mut(c)? = r; Some(mem)},
    "addi" => {let r = *mem.get(a)? + b; *mem.get_mut(c)? = r; Some(mem)},
    "mulr" => {let r = *mem.get(a)? * *mem.get(b)?; *mem.get_mut(c)? = r; Some(mem)},
    "muli" => {let r = *mem.get(a)? * b; *mem.get_mut(c)? = r; Some(mem)},
    "banr" => {let r = *mem.get(a)? & *mem.get(b)?; *mem.get_mut(c)? = r; Some(mem)},
    "bani" => {let r = *mem.get(a)? & b; *mem.get_mut(c)? = r; Some(mem)},
    "borr" => {let r = *mem.get(a)? | *mem.get(b)?; *mem.get_mut(c)? = r; Some(mem)},
    "bori" => {let r = *mem.get(a)? | b; *mem.get_mut(c)? = r; Some(mem)},
    "setr" => {let r = *mem.get(a)? ; *mem.get_mut(c)? = r; Some(mem)},
    "seti" => {let r = a; *mem.get_mut(c)? = r; Some(mem)},
    "gtir" => {let r = a > *mem.get(b)?; *mem.get_mut(c)? = r as usize; Some(mem)},
    "gtri" => {let r = *mem.get(a)? > b; *mem.get_mut(c)? = r as usize; Some(mem)},
    "gtrr" => {let r = *mem.get(a)? > *mem.get(b)?; *mem.get_mut(c)? = r as usize; Some(mem)},
    "eqir" => {let r = a == *mem.get(b)?; *mem.get_mut(c)? = r as usize; Some(mem)},
    "eqri" => {let r = *mem.get(a)? == b; *mem.get_mut(c)? = r as usize; Some(mem)},
    "eqrr" => {let r = *mem.get(a)? == *mem.get(b)?; *mem.get_mut(c)? = r as usize; Some(mem)},

    "sofa" => {
        let mut unfactorized = *mem.get(a)?;
        let mut r = 1;
        for factor in 2.. {
            let mut fac_pow = 1;
            let mut r_fac = 1;
            while unfactorized % factor == 0 {
                fac_pow *= factor;
                r_fac += fac_pow;
                unfactorized /= factor;
            }
            r *= r_fac;
            if factor * factor > unfactorized {break;}
        }
        if unfactorized > 1 {r *= 1 + unfactorized;}
        *mem.get_mut(c)? = r;
        Some(mem)
    }
    _ => panic!()
    };

    let mut lines = s.lines();
    let ip_reg: usize = lines.next().unwrap().strip_prefix("#ip ").unwrap().parse().unwrap();
    let mut code: Vec<(&str, usize, usize, usize)> = lines.map(|line| {
        let [op, a_str, b_str, c_str]: [&str; 4] = line.split(" ").collect::<Vec<_>>().try_into().unwrap();
        (op, a_str.parse().unwrap(), b_str.parse().unwrap(), c_str.parse().unwrap())
    }).collect();
    let mut heatmap = vec![0usize; code.len()];

    // in part b it's required to opimize the algorithm presented. It's probably not intended to write a full-blown
    // optimizing compiler ready to handle any code, just the range of actual possible inputs. Unfortunately, we don't
    // know the range of actual possible inputs, just one member of it. If we ever get more samples, _then_ we'll be
    // able to generalize. 
    // The interesting bits of the input have been manually determined to compute the sum of factors. The uninteresting
    // bits run once exactly.
    // Hypotheses on possible fuzzing: register relabeling (including IP); oprr commutation; unused opargs (handled);
    // code movement (need to adjust absolute jumps within); exotic jump instructions???
    if ip_reg != 2 || !matches!(code[1..=16], [
        ("seti", 1, _, 4), ("seti", 1, _, 1), ("mulr", 4, 1, 5), ("eqrr", 5, 3, 5),
        ("addr", 5, 2, 2), ("addi", 2, 1, 2), ("addr", 4, 0, 0), ("addi", 1, 1, 1),
        ("gtrr", 1, 3, 5), ("addr", 2, 5, 2), ("seti", 2, _, 2), ("addi", 4, 1, 4),
        ("gtrr", 4, 3, 5), ("addr", 5, 2, 2), ("seti", 1, _, 2), ("mulr", 2, 2, 2)
    ]) {todo!();}
    code[1] = ("sofa", 3, 0, 0);
    code[2] = ("seti", code.len(), 0, ip_reg);

    let mut mem = [0; 6];
    let mut t = 0;
    println!("\n");
    if part == 'b' {mem[0] = 1};
    while let Some(&(op, a, b, c)) = code.get(mem[ip_reg]) {
        t += 1;
        heatmap[mem[ip_reg]] += 1;
        if t % 0x1000000 == 0{
            println!("\x1b[2A{:?}\x1b[K | {} {} {} {}\n{:?}", mem, op, a, b, c, heatmap);
        }
        mem = ops(op, a, b, c, mem).unwrap();
        mem[ip_reg] += 1;
    }
    println!("\x1b[2A{:?}\x1b[K\n{:?}", mem, heatmap);
    mem[0].to_string()
}

fn day20(part: char, s: &str) -> String {
    // we build the map one character at a time, holding the following information:
    // for each tile, the set of doors traversed;
    // the set of tiles that directions target (heads) or that become heads after a pop;
    // the set of tiles that become heads of any new branches (tails).
    const E: u8 = 1; const S: u8 = 2; const W: u8 = 4; const N: u8 = 8; const NEAR: u8 = 16;
    let mut minx = 0; let mut miny = 0;
    let mut map = vec![vec![NEAR]];
    let mut heads = vec![vec![(0isize,0isize)]];
    let mut tails = vec![vec![(0isize,0isize)]];
    for c in s.trim_end().strip_prefix("^").unwrap().strip_suffix("$").unwrap().chars(){
        match c {
            'E' => {
                for h in heads.last_mut().unwrap() {
                    if h.0 - minx == map[0].len() as isize - 1 {
                        for row in map.iter_mut() {row.push(0)}
                    }
                    map[(h.1 - miny) as usize][(h.0 - minx) as usize] |= E;
                    h.0 += 1;
                    map[(h.1 - miny) as usize][(h.0 - minx) as usize] |= W;
                }
            },
            'N' => {
                for h in heads.last_mut().unwrap() {
                    if h.1 == miny {
                        miny -= 1;
                        map.insert(0, vec![0; map[0].len()]);
                    }
                    map[(h.1 - miny) as usize][(h.0 - minx) as usize] |= N;
                    h.1 -= 1;
                    map[(h.1 - miny) as usize][(h.0 - minx) as usize] |= S;
                }
            },
            'S' => {
                for h in heads.last_mut().unwrap() {
                    if h.1 - miny == map.len() as isize - 1 {
                        map.push(vec![0; map[0].len()]);
                    }
                    map[(h.1 - miny) as usize][(h.0 - minx) as usize] |= S;
                    h.1 += 1;
                    map[(h.1 - miny) as usize][(h.0 - minx) as usize] |= N;
                }
            },
            'W' => {
                for h in heads.last_mut().unwrap() {
                    if h.0 == minx {
                        minx -= 1;
                        for row in map.iter_mut() {row.insert(0, 0)}
                    }
                    map[(h.1 - miny) as usize][(h.0 - minx) as usize] |= W;
                    h.0 -= 1;
                    map[(h.1 - miny) as usize][(h.0 - minx) as usize] |= E;
                }
            },
            '(' => {
                heads.insert(heads.len() - 1, vec![]);
                tails.push(heads.last().unwrap().clone());
            },
            '|' => {
                let old_heads = heads.pop().unwrap();
                heads.last_mut().unwrap().extend_from_slice(&old_heads);
                heads.push(tails.last().unwrap().clone());
            },
            ')' => {
                tails.pop();
                let mut old_heads = heads.pop().unwrap();
                let new_heads = heads.last_mut().unwrap();
                new_heads.extend(old_heads.drain(..));
                new_heads.sort();
                new_heads.dedup();
            },
            _ => panic!()
        }
    }
    assert_eq!(heads.len(), 1);
    let mut ge1k = 0;
    for t in 0 .. {
        let mut done = true;
        for y in 0 .. map.len() {
            for x in 0 .. map[0].len() {
                if (miny as usize ^ minx as usize ^ t ^ y ^ x) % 2 == 0  && map[y][x] & NEAR != 0 {
                    if map[y][x] & E != 0 && map[y][x + 1] & NEAR == 0 {map[y][x + 1] |= NEAR; done = false; if t >= 999 {ge1k += 1}}
                    if map[y][x] & N != 0 && map[y - 1][x] & NEAR == 0 {map[y - 1][x] |= NEAR; done = false; if t >= 999 {ge1k += 1}}
                    if map[y][x] & S != 0 && map[y + 1][x] & NEAR == 0 {map[y + 1][x] |= NEAR; done = false; if t >= 999 {ge1k += 1}}
                    if map[y][x] & W != 0 && map[y][x - 1] & NEAR == 0 {map[y][x - 1] |= NEAR; done = false; if t >= 999 {ge1k += 1}}
                }
            }
        }
        if done {return if part == 'a' {t.to_string()} else {ge1k.to_string()}}
    }
    unreachable!();
}

fn day21(part: char, s: &str) -> String {
    let ops = |op: &str, a: usize, b: usize, c: usize, mut mem: [Option<usize>; 6]| match op {
        "addr" => {let r = (*mem.get(a)?)? + (*mem.get(b)?)?; *mem.get_mut(c)? = Some(r); Some(mem)},
        "addi" => {let r = (*mem.get(a)?)? + b; *mem.get_mut(c)? = Some(r); Some(mem)},
        "mulr" => {let r = (*mem.get(a)?)? * (*mem.get(b)?)?; *mem.get_mut(c)? = Some(r); Some(mem)},
        "muli" => {let r = (*mem.get(a)?)? * b; *mem.get_mut(c)? = Some(r); Some(mem)},
        "banr" => {let r = (*mem.get(a)?)? & (*mem.get(b)?)?; *mem.get_mut(c)? = Some(r); Some(mem)},
        "bani" => {let r = (*mem.get(a)?)? & b; *mem.get_mut(c)? = Some(r); Some(mem)},
        "borr" => {let r = (*mem.get(a)?)? | (*mem.get(b)?)?; *mem.get_mut(c)? = Some(r); Some(mem)},
        "bori" => {let r = (*mem.get(a)?)? | b; *mem.get_mut(c)? = Some(r); Some(mem)},
        "setr" => {let r = (*mem.get(a)?)? ; *mem.get_mut(c)? = Some(r); Some(mem)},
        "seti" => {let r = a; *mem.get_mut(c)? = Some(r); Some(mem)},
        "gtir" => {let r = a > (*mem.get(b)?)?; *mem.get_mut(c)? = Some(r as usize); Some(mem)},
        "gtri" => {let r = (*mem.get(a)?)? > b; *mem.get_mut(c)? = Some(r as usize); Some(mem)},
        "gtrr" => {let r = (*mem.get(a)?)? > (*mem.get(b)?)?; *mem.get_mut(c)? = Some(r as usize); Some(mem)},
        "eqir" => {let r = a == (*mem.get(b)?)?; *mem.get_mut(c)? = Some(r as usize); Some(mem)},
        "eqri" => {let r = (*mem.get(a)?)? == b; *mem.get_mut(c)? = Some(r as usize); Some(mem)},
        "eqrr" => {let r = (*mem.get(a)?)? == (*mem.get(b)?)?; *mem.get_mut(c)? = Some(r as usize); Some(mem)},
        _ => panic!()
        };
    
        let mut lines = s.lines();
        let ip_reg: usize = lines.next().unwrap().strip_prefix("#ip ").unwrap().parse().unwrap();
        let code: Vec<(&str, usize, usize, usize)> = lines.map(|line| {
            let [op, a_str, b_str, c_str]: [&str; 4] = line.split(" ").collect::<Vec<_>>().try_into().unwrap();
            (op, a_str.parse().unwrap(), b_str.parse().unwrap(), c_str.parse().unwrap())
        }).collect();

        assert!(matches!(code[code.len() - 3 ..], 
            [("eqrr", 5, 0, 1), ("addr", 1, i1, i2), ("seti", _, _, i3)] if i1 == ip_reg && i2 == ip_reg && i3 == ip_reg
        ));

        let mut mem = [None, Some(0), Some(0), Some(0), Some(0), Some(0)];
        let mut rs: Vec<usize> = vec![];
        let mut mems: Vec<[usize; 5]> = vec![];
        println!("\n");
        while let Some(&(op, a, b, c)) = code.get(mem[ip_reg].unwrap()) {
            let ip = mem[ip_reg].unwrap();
            mem = match ops(op, a, b, c, mem) {
                Some(mem) => mem,
                None => {
                    assert_eq!(ip, code.len() - 3);
                    if part == 'a' {return mem[5].unwrap().to_string();}
                    if !rs.contains(&mem[5].unwrap()) {rs.push(mem[5].unwrap());}
                    let new_mem = [mem[1].unwrap(), mem[2].unwrap(), mem[3].unwrap(), mem[4].unwrap(), mem[5].unwrap()];
                    if mems.contains(&new_mem) {return rs.last().unwrap().to_string();}
                    mems.push(new_mem);
                    print!("{} {:06x} ", rs.len( ), mem[5].unwrap());
                    mem[1] = Some(0);
                    mem
                }
            };
            *mem[ip_reg].as_mut().unwrap() += 1;
        }
        unreachable!();
    }

fn day22(part: char, s: &str) -> String {
    let mut lines = s.trim().lines();
    let depth: u32 = lines.next().unwrap().strip_prefix("depth: ").unwrap().parse().unwrap();
    let target: [usize; 2] = lines.next().unwrap().strip_prefix("target: ").unwrap()
                                .split(",").map(|s| s.parse().unwrap()).collect::<Vec<_>>().try_into().unwrap();
    assert_eq!(lines.next(), None);
    let mut level = Vec::with_capacity(target[1]);
    level.push(Vec::with_capacity(target[0]));
    level[0].push(depth % 20183);
    for x in 1 .. target[0] + 1 {level[0].push((x as u32 * 16807 + depth) % 20183);}
    for y in 1 .. target[1] + 1{
        level.push(Vec::with_capacity(target[0]));
        level[y].push((y as u32 * 48271 + depth) % 20183);
        for x in 1 .. target[0] + 1 {
            let r = if [x, y] == target {0} else {(level[y][x-1] * level[y-1][x] + depth) % 20183};
            level[y].push(r);
        }
    }

    if part == 'a' {return level.iter().flatten().map(|ix| ix % 3).sum::<u32>().to_string();}

    #[derive(Debug)]
    struct Node {key: usize, y: usize, x: usize, tool: u8, cost: usize}
    impl Ord for Node {fn cmp(&self, other: &Self) -> Ordering {
        (other.key, other.y, other.x).cmp(&(self.key, self.y, self.x))
    }}
    impl PartialOrd for Node {fn partial_cmp(&self, other: &Self) -> Option<Ordering> {Some(self.cmp(other))}}
    impl PartialEq for Node {fn eq(&self, other: &Self) -> bool {self.cmp(other) == Ordering::Equal}}
    impl Eq for Node {}
    let new_node = |y, x, tool, cost| -> Node { Node { y, x, tool, cost,
        key: cost
                + (x as isize - target[0] as isize).abs() as usize 
                + (y as isize - target[1] as isize).abs() as usize 
                + (tool != 1) as usize * 7
    }};

    let mut opens: BinaryHeap<Node> = BinaryHeap::new();
    let mut closeds: HashSet<(usize, usize, u8)> = HashSet::new();

    // we identify each tool with the terrain - level%3 - it can't be used in:
    // 0 = rocky = can't use neither; 1 = wet = can't use torch; 2 = narrow = can't use climbing gear
    opens.push(new_node(0, 0, 1, 0));
    while let Some(node) = opens.pop() {
        if node.x == level[0].len() - 1 {
            let x = node.x + 1;
            level[0].push((x as u32 * 16807 + depth) % 20183);
            for y in 1 .. level.len() {
                let r = (level[y][x-1] * level[y-1][x] + depth) % 20183; 
                level[y].push(r);
            }
        }
        if node.y == level.len() - 1 {
            let y = node.y - 1;
            level.push(Vec::with_capacity(target[0]));
            level[y].push((y as u32 * 48271 + depth) % 20183);
            for x in 1 .. target[0] + 1 {
                let r =(level[y][x-1] * level[y-1][x] + depth) % 20183;
                level[y].push(r);
            }
        }

        if !closeds.insert((node.y, node.x, node.tool)) {continue;}
        if [node.x, node.y] == target && node.tool == 1 {return node.cost.to_string();}
        if node.y > 0 && (level[node.y - 1][node.x] % 3) as u8 != node.tool {
            opens.push(new_node(node.y - 1, node.x, node.tool, node.cost + 1));
        }
        if node.x > 0 && (level[node.y][node.x - 1] % 3) as u8 != node.tool {
            opens.push(new_node(node.y, node.x - 1, node.tool, node.cost + 1));
        }
        if (level[node.y][node.x + 1] % 3) as u8 != node.tool {
            opens.push(new_node(node.y, node.x + 1, node.tool, node.cost + 1));
        }
        if (level[node.y + 1][node.x] % 3) as u8 != node.tool {
            opens.push(new_node(node.y + 1, node.x, node.tool, node.cost + 1));
        }
        opens.push(new_node(node.y, node.x, 3 - (level[node.y][node.x] % 3) as u8 - node.tool, node.cost + 7));
    }
    unreachable!();
}

fn day23(part: char, s: &str) -> String {
    let mut bots = s.lines().map(|line| {
        let mut chars = line.strip_prefix("pos=<").unwrap().chars();
        let x: i32 = chars.by_ref().take_while(|c| matches!(c, '-' | '0'..='9')).collect::<String>().parse().unwrap();
        // ,
        let y: i32 = chars.by_ref().take_while(|c| matches!(c, '-' | '0'..='9')).collect::<String>().parse().unwrap();
        // ,
        let z: i32 = chars.by_ref().take_while(|c| matches!(c, '-' | '0'..='9')).collect::<String>().parse().unwrap();
        // >
        let r: i32 = chars.as_str().strip_prefix(", r=").unwrap().parse().unwrap();
        (x, y, z, r)
    }).collect::<Vec<_>>();
    
    if part == 'a' {
        let strongest = bots.iter().max_by_key(|bot| bot.3).unwrap();
        return bots.iter().filter(|bot|
            (strongest.0 - bot.0).abs() + (strongest.1 - bot.1).abs() + (strongest.2 - bot.2).abs() <= strongest.3
        ).count().to_string();
    }

    /// Region of space bounded by up to eight planes parallel to the faces of a regular octahedron.
    #[derive(PartialEq, Hash, Eq, Debug, Clone, Copy)]
    struct Shape {cs: [i32; 8]}
    impl Shape {
        /// Shape as a set of points within a maximum distance from a center. Technically an octahedron.
        //  r - x - y - z <= cs[0] <=> 0 <= cs[0]-r + x + y + z
        fn sphere(x: i32, y: i32, z: i32, r: i32) -> Shape { Shape { cs: [
            r - x - y - z, r - x - y + z, r - x + y - z, r - x + y + z,
            r + x - y - z, r + x - y + z, r + x + y - z, r + x + y + z
        ]}}
        fn is_empty(&self) -> bool {
            self.cs[0] + self.cs[7] < 0 || self.cs[1] + self.cs[6] < 0 ||
            self.cs[2] + self.cs[5] < 0 || self.cs[3] + self.cs[4] < 0
        }
        // a - x - y + z > 0 && b - x + y - z > 0 && c + x - y - z > 0 => a + b + c - x - y - z > 0
        fn compact (self) -> Self { Shape { cs: [
            self.cs[0].min(self.cs[1] + self.cs[2] + self.cs[4]), self.cs[1].min(self.cs[0] + self.cs[3] + self.cs[5]),
            self.cs[2].min(self.cs[3] + self.cs[0] + self.cs[6]), self.cs[3].min(self.cs[2] + self.cs[1] + self.cs[7]),
            self.cs[4].min(self.cs[5] + self.cs[6] + self.cs[0]), self.cs[5].min(self.cs[4] + self.cs[7] + self.cs[1]),
            self.cs[6].min(self.cs[7] + self.cs[4] + self.cs[2]), self.cs[7].min(self.cs[6] + self.cs[5] + self.cs[3])
        ]}}
    }
    impl std::ops::BitAnd for Shape {
        type Output = Shape;
        fn bitand(self, rhs: Self) -> Self { Shape { cs: [
            self.cs[0].min(rhs.cs[0]), self.cs[1].min(rhs.cs[1]), self.cs[2].min(rhs.cs[2]), self.cs[3].min(rhs.cs[3]),
            self.cs[4].min(rhs.cs[4]), self.cs[5].min(rhs.cs[5]), self.cs[6].min(rhs.cs[6]), self.cs[7].min(rhs.cs[7])
        ]}.compact()}
    }
    impl PartialOrd for Shape {
        fn partial_cmp(&self, rhs: &Self) -> Option<Ordering> {
            self.cs.iter().zip(rhs.cs.iter()).fold(Some(Ordering::Equal), |a, (lc, rc)| match (a, lc.cmp(rc)) {
                (None, _) => None,
                (Some(Ordering::Equal), ro) => Some(ro),
                (Some(lo), Ordering::Equal) => Some(lo),
                (Some(lo), ro) if lo == ro => Some(lo),
                _ => None
            })
        }
    }

    let mut shapes: Vec<(Shape, u32)> = vec![];
    bots.sort_unstable_by_key(|bot| bot.3);
    for (bot_ix, bot) in bots.iter().enumerate() {
        let bot_shape = Shape::sphere(bot.0, bot.1, bot.2, bot.3);
        println!("bot {:?} => shape {:?}", bot, bot_shape);
        for i in 0 .. shapes.len() {
            if shapes[i].0 <= bot_shape {shapes[i].1 += 1;}
            else {
                let new_shape = shapes[i].0 & bot_shape;
                if !new_shape.is_empty() {shapes.push((new_shape, shapes[i].1));}

            }
        }
        shapes.push((bot_shape, 1));
        println!("bot {} / {}: {} shapes tracked", bot_ix, bots.len(), shapes.len());
        shapes = shapes.into_iter().collect::<HashSet<_>>().into_iter().collect();
        println!("{} after deduplication", shapes.len());
    }
    todo!();
}

fn day24(part: char, s: &str) -> String {
    let mut state = 0u32;
    for c in s.as_bytes() {match c {
        b'.' => state = state >> 1,
        b'#' => state = state >> 1 | 1 << 24,
        b'\n' => {},
        _ => panic!()
    }}

    if part == 'a' {
        let mut states_seen = HashSet::new();
        states_seen.insert(state);
        loop {
            let n = state >> 5 & 0b_00000_11111_11111_11111_11111;
            let s = state << 5 & 0b_11111_11111_11111_11111_00000;
            let w = state >> 1 & 0b_01111_01111_01111_01111_01111;
            let e = state << 1 & 0b_11110_11110_11110_11110_11110;

            let one_neigh = !n & !s & (w ^ e) | !w & !e & (n ^ s);
            let two_neigh = !n & !s & w & e | (n ^ s) & (w ^ e) | n & s & !w & !e;
            state = one_neigh | two_neigh & !state;
            if !states_seen.insert(state) {return state.to_string()}
        }
    } else {
        let mut state = vec![state];
        for _ in 0 .. 200 {
            let mut outer_n = false;
            let mut outer_s = false;
            let mut outer_w = false;
            let mut outer_e = false;
            if state[0] != 0 {state.insert(0, 0)}
            if state.last().unwrap() != &0 {state.push(0)}
            for i in 0 .. state.len() {
                let this = state[i];
                let next = state.get(i+1).copied().unwrap_or(0);

                let n = this >> 5 & 0b_00000_11011_10001_11011_11111
                    | outer_n as u32  * 0b_11111_00000_00000_00000_00000;

                let s = this << 5 & 0b_11111_11011_10001_11011_00000
                    | outer_s as u32  * 0b_00000_00000_00000_00000_11111;

                let w = this >> 1 & 0b_01111_01011_00001_01011_01111
                    | outer_w as u32  * 0b_10000_10000_10000_10000_10000;

                let e = this << 1 & 0b_11110_11010_10000_11010_11110
                    | outer_e as u32  * 0b_00001_00001_00001_00001_00001;
 
                outer_n = this & 1 << 17 != 0;
                outer_s = this & 1 <<  7 != 0;
                outer_w = this & 1 << 13 != 0;
                outer_e = this & 1 << 11 != 0;
                let mut one_neigh = !n & !s & (w ^ e) | !w & !e & (n ^ s);
                let mut two_neigh = !n & !s & w & e | (n ^ s) & (w ^ e) | n & s & !w & !e;
                match (next & 0b_00000_00000_00000_00000_11111).count_ones() +
                      (this & 0b_00000_00000_00000_01010_00100).count_ones()
                      {1 => one_neigh |= 1 <<  7, 2 => two_neigh |= 1 <<  7, _ => {}}
                match (next & 0b_11111_00000_00000_00000_00000).count_ones() +
                      (this & 0b_00100_01010_00000_00000_00000).count_ones()
                      {1 => one_neigh |= 1 << 17, 2 => two_neigh |= 1 << 17, _ => {}}
                match (next & 0b_00001_00001_00001_00001_00001).count_ones() +
                      (this & 0b_00000_00010_00001_00010_00000).count_ones()
                      {1 => one_neigh |= 1 << 11, 2 => two_neigh |= 1 << 11, _ => {}}
                match (next & 0b_10000_10000_10000_10000_10000).count_ones() +
                      (this & 0b_00000_01000_10000_01000_00000).count_ones()
                      {1 => one_neigh |= 1 << 13, 2 => two_neigh |= 1 << 13, _ => {}}
                      
                state[i] = one_neigh | two_neigh & !state[i];
            }
        }
        state.iter().copied().map(u32::count_ones).sum::<u32>().to_string()
    }
}

fn day25(_part: char, s: &str) -> String {
    // To explore, we will use depth first search: each time a room is accessed,
    // we add all of its neighbors to the list of curios. Each time we move, we
    // update the position of all curios. This can handle loops, but doesn't
    // exploit them. This is fine because there aren't any loops anyways.
    const DANGEROUS_ITEMS: [&str; 5] =
        ["escape pod", "giant electromagnet", "infinite loop", "molten lava", "photons"];
    let mut curios: Vec<String> = vec![];
    let mut rooms_seen: HashSet<String> = HashSet::new();
    let mut prev_step = None;
    let mut security_at: Option<String> = None;
    let mut safe_items: Vec<String> = vec![];

    use intcode::*;
    let mut cpu = Cpu::new(parse_intcode(s));
    loop {
        cpu.run().unwrap();
        if matches!(cpu.state, RunState::Success) {panic!("CPU exited unexpectedly")}
        let response = cpu.recv_ascii();
        
        let mut current_room: Option<&str> = None;
        let mut doors_out: Vec<char> = vec![];
        let mut items_here: Vec<&str> = vec![];
        for para in response.trim().split("\n\n") {
            let mut lines = para.split("\n");
            match lines.next() {
                Some("== Security Checkpoint ==") => {security_at = Some(String::new())}
                Some(l) if l.starts_with("== ") && l.ends_with(" ==") => {
                    current_room = Some(&l[3 .. l.len() - 3]);
                }
                Some("Doors here lead:") => {
                    doors_out.extend(lines.map(|l| l.chars().nth(2).unwrap()))
                }
                Some("Items here:") => {items_here.extend(lines.map(|l| &l[2..]))}
                Some("Command?") => {}
                Some(l) => panic!("unknown paragraph {:?}", l),
                None => panic!("empty paragraph")
            }
        } 
        
        if let Some(room) = current_room {
            if rooms_seen.insert(room.to_string()) {
                for d in doors_out {if Some(rev(d)) != prev_step {curios.push(d.to_string())}}
            }
        }
        
        for item in items_here {
            if !DANGEROUS_ITEMS.contains(&item) {
                safe_items.push(item.to_string());
                cpu.send_ascii("take ");
                cpu.send_ascii(item);
                cpu.send_ascii("\n");
                cpu.run().unwrap();
                cpu.recv_ascii();
            }
        }
        
        prev_step = curios.last().or(security_at.as_ref()).and_then(|c| c.chars().last());
        if let Some(step) = prev_step {
            for c in security_at.iter_mut().chain(curios.iter_mut()) {
                if c.chars().rev().next() == Some(step) {c.pop();} else {c.push(rev(step));}
            }
            curios.retain(|c| !c.is_empty());
            let cmd = match step {
                'n' => "north\n", 's' => "south\n", 'e' => "east\n", 'w' => "west\n",
                _ => unreachable!()
            };
            cpu.send_ascii(cmd);
        } else {break}
    }

    // We stand at the security checkpoint, our pockets holding an assortment of stuff.
    // It seems optimal to only drop items when we're too heavy and take when too light,
    // but even for very many items, this can't guarantee better savings over just
    // enumerating all options than about 3x or so. For 8 items that value is a somewhere around 2.
    let mut items_dropped = 0u32;
    for i in 0u32 .. 1 << safe_items.len() {
        if i != 0 {
            let ix = i.trailing_zeros();
            if items_dropped & 1 << ix == 0 {
                items_dropped |= 1 << ix;
                cpu.send_ascii("drop ");
            } else {
                items_dropped &= !(1 << ix);
                cpu.send_ascii("take ");
            }
            cpu.send_ascii(&safe_items[ix as usize]);
            cpu.send_ascii("\n");
            cpu.run().unwrap();
            cpu.recv_ascii();
        }
        cpu.send_ascii("west\n");
        cpu.run().unwrap();
        let response = cpu.recv_ascii();
        if matches!(cpu.state, RunState::Success) {
            return response.chars().filter(|c| *c >= '0' && *c <= '9').collect::<String>()
        }
    }
    panic!();

    fn rev(c: char) -> char {
        match c {'n' => 's', 's' => 'n', 'e' => 'w', 'w' => 'e', _ => unreachable!()}
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let days = [
        day1, day2, day3, day4, day5, day6, day7, day8, day9, day10, day11, day12, day13,
        day14, day15, day16, day17, day18, day19, day20, day21, day22, day23, day24, day25
    ];
    
    if let [_, day_arg, part_arg] = &std::env::args().collect::<Vec<_>>()[..] {
        assert!(part_arg == "a" || part_arg == "b");
        let day: usize = day_arg.parse()?;
        let input = std::fs::read_to_string(format!("day{}.in", day))?;
        let time = std::time::Instant::now();
        println!("{}", days[day - 1](part_arg.parse()?, &input));
        println!("{} seconds elapsed", time.elapsed().as_secs_f32());
    } else {
        println!("exactly two arguments expected - day number and a/b for _part")
    }
    
    Ok(())
}