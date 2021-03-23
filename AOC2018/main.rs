use std::cell::RefCell;
use std::collections::*;
use std::convert::*;
use std::fmt::Write;
use std::iter::once;

fn mod_inverse (a: u64, m: u64) -> Result<u64, u64> {
    assert!(m != 0);
    let mut prev_r = m;
    let mut prev_ca = 0;
    let mut last_r = a;
    let mut last_ca = 1;
    loop {
        if last_r == 0 {return if prev_r == 1 {
            Ok((prev_ca + m as i64) as u64 % m)
        } else {Err(prev_r)}}
        let q = prev_r / last_r;
        let next_r = prev_r - q * last_r;
        prev_r = last_r;
        last_r = next_r;
        let next_ca = prev_ca - q as i64 * last_ca;
        prev_ca = last_ca;
        last_ca = next_ca;
    }
}

fn bfs_all(map: &Vec<impl AsRef<[u8]>>,
       start: (usize, usize),
       valid: impl Fn(u8) -> bool,
) -> [Option<(Vec<u8>, (usize, usize))>; 256] {
    let mut prev: HashSet<(usize, usize)> = HashSet::new();
    let mut curr: HashMap<(usize, usize), Vec<u8>> = HashMap::new();
    let mut next: HashMap<(usize, usize), Vec<u8>> = HashMap::new();
    const NONE: Option<(Vec<u8>, (usize, usize))> = None;
    let mut r = [NONE; 256];
    curr.insert(start, vec![]);
    loop {
       prev.extend(curr.keys().copied());
       for ((x, y), path) in curr.into_iter() {
            for (nx, ny, dir) in &[
                (x, y - 1, b'N'), (x + 1, y, b'E'), (x, y + 1, b'S'), (x - 1, y, b'W')
            ] {
                let mut npath = path.clone();
                npath.push(*dir);
                let c = map[*ny].as_ref()[*nx];
                if valid(c) && !prev.contains(&(*nx, *ny)){
                    if r[c as usize] == None {r[c as usize] = Some((npath.clone(), (*nx, *ny)));}
                    next.insert((*nx, *ny), npath);
                }
            }
        }
        if next.is_empty() {return r;}
        curr = next;
        next = HashMap::new();
    }
}

////////////////////////////////////////////////////////////////////////////////

mod intcode {
    use self::RunState::*;
    use std::error::Error;
    use std::fmt;
    use std::iter::FromIterator;
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
        
        pub fn with_input<'a>(mem: Vec<isize>, input: impl IntoIterator<Item = &'a isize>) 
        -> Self {Self {
            mem, ip: 0, rb: 0, state: Running,
            input: VecDeque::from_iter(input.into_iter().copied()), output: VecDeque::new()
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
        
        pub fn recv(&mut self) -> Option<isize> {
            self.output.pop_front()
        }
        
        pub fn recv_all(&mut self) -> impl Iterator<Item = isize> + '_ {
            self.output.drain(..)
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
    let mut map: Vec<Vec<u8>> = s.lines().map(|line| line.as_bytes().to_vec()).collect();
    let mut units: Vec<RefCell<(usize, usize, u8, u8)>> = vec![];
    let mut goblins_rem = 0;
    let mut elves_rem = 0;
    for (y, row) in map.iter().enumerate() {for (x, &c) in row.iter().enumerate() {
        if c == b'E' || c == b'G' {units.push(RefCell::new((x, y, c, 200u8)));}
        if c == b'E' {elves_rem += 1;}
        if c == b'G' {goblins_rem += 1;}
    }}

    let mut prev_closest: HashSet<(usize, usize)> = HashSet::new();
    for t in 0 .. {
        units.sort_unstable_by_key(|cell| {let unit = cell.borrow(); (unit.1, unit.0)});
        for i in 0 .. units.len() {
            if units[i].borrow().3 == 0 {continue;}

            if goblins_rem == 0 || elves_rem == 0 {
                let hp: i32 = units.iter().map(|cell| cell.borrow().3 as i32).sum();
                println!("t = {}\nhp = {}", t, hp);
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
            if let Some(cell) = units.iter().find(|cell| {
                cell.try_borrow().map_or(false, |enemy|
                    (enemy.0 as isize - unit.0 as isize).abs() +
                    (enemy.1 as isize - unit.1 as isize).abs() == 1 &&
                    enemy.2 != unit.2 && enemy.3 > 0
                )
            }){
                let mut enemy = cell.borrow_mut();
                if enemy.3 > 3 {
                    enemy.3 -= 3;
                } else {
                    enemy.3 = 0;
                    map[enemy.1][enemy.0] = b'.';
                    if enemy.2 == b'G' {goblins_rem -= 1;} else {elves_rem -= 1;}
                }
            }
        }
        
        units.retain(|cell| cell.borrow().3 > 0);
        //if t > 0 {print!("\x1b[{}A", map.len());}
        for cell in &units {print!("{:?}, ", cell.borrow());}
        println!();
        for y in 0 .. map.len() {
            for x in 0 .. map[y].len() {
                print!("{} ", map[y][x] as char);
            }
            println!();
        }
        std::thread::sleep(std::time::Duration::from_millis(100));
    }
    unreachable!();
}

fn day16(part: char, s: &str) -> String {
    let mut num: Vec<i32> = s.trim().chars().map(|c| c.to_digit(10).unwrap() as i32).collect();
    let skip: usize;
    if part == 'a' {
        skip = 0;
    } else {
        skip = s[0..7].parse().unwrap();
        num = num.repeat(10000);
    }
    for t in 0 .. 100 {
        println!("{}/100\x1b[A", t);
        let sums: Vec<i32> = std::iter::once(0).chain(
            num.iter().scan(0, |sum, d| {*sum += d; Some(*sum)})
        ).collect();
        num = (0 .. num.len()).map(|i| {
            let mut r = 0;
            let mut ni = i;
            loop {
                r -= sums[ni]; //run of +1 starts
                ni = (ni + i + 1).min(num.len());
                r += sums[ni]; //run of +1 ends
                ni = (ni + i + 1).min(num.len());
                if ni == num.len() {break r}
                r += sums[ni]; //run of -1 starts
                ni = (ni + i + 1).min(num.len());
                r -= sums[ni]; //run of -1 ends
                ni = (ni + i + 1).min(num.len());
                if ni == num.len() {break r}
            }.abs() % 10
        }).collect()
    }
    num[skip..][..8].iter().map(|d| d.to_string()).collect::<Vec<_>>().join("")
}

fn day17(part: char, s: &str) -> String {
    use intcode::*;
    let mut cpu = Cpu::new(parse_intcode(s));
    
    cpu.run().unwrap();
    let map_str = cpu.recv_ascii();
    let map = map_str.lines().map(|line| line.as_bytes()).collect::<Vec<_>>();
    
    if part == 'a' {
        let mut r = 0;
        for y in 1 .. map.len() - 1 {
            for x in 1 .. map[y].len() - 1 {
                if map[y - 1][x] == b'#' && map[y][x - 1] == b'#' && map[y][x] == b'#' &&
                   map[y][x + 1] == b'#' && map[y + 1][x] == b'#'
                {
                    r += x * y;
                }
            }
        }
        r.to_string()
    } else {
        let (mut bot_pos, mut bot_dir) =
            map.iter().enumerate().find_map(|(y, line)|
                line.iter().enumerate().find_map(|(x, c)| match c {
                    b'^' => Some(((x as isize, y as isize), (0, -1))),
                    b'v' => Some(((x as isize, y as isize), (0, 1))),
                    b'<' => Some(((x as isize, y as isize), (-1, 0))),
                    b'>' => Some(((x as isize, y as isize), (1, 0))),
                    _ => None
                })
            ).unwrap();
        let mut path = String::new();
        loop {
            if map.get((bot_pos.1 - bot_dir.0) as usize).and_then(|mapy|
                mapy.get((bot_pos.0 + bot_dir.1) as usize)
            ) == Some(&b'#') {
                bot_dir = (bot_dir.1, -bot_dir.0);
                path.push_str(",L,");
            } else if map.get((bot_pos.1 + bot_dir.0) as usize).and_then(|mapy|
                mapy.get((bot_pos.0 - bot_dir.1) as usize)
            ) == Some(&b'#') {
                bot_dir = (-bot_dir.1, bot_dir.0);
                path.push_str(",R,");
            } else {
                break;
            }
            let mut steps = 0;
            while map.get((bot_pos.1 + bot_dir.1) as usize).and_then(|mapy|
                mapy.get((bot_pos.0 + bot_dir.0) as usize)
            ) == Some(&b'#') {
                steps += 1;
                bot_pos.0 += bot_dir.0;
                bot_pos.1 += bot_dir.1;
            }
            write!(path, "{}", steps).unwrap();
        }
                
        let mut cpu_cmd = None;
        'fora: for a_len in (1 ..= 21).rev() {
            'forb: for b_len in (1 ..= 21).rev() {
                'forc: for c_len in (1 ..= 21).rev() {
                    let mut a_str = None;
                    let mut b_str = None;
                    let mut c_str = None;
                    let mut main_str = String::new();
                    let mut path_rem = &path[..];
                    while !path_rem.is_empty(){
                        if a_str == None {
                            a_str = path_rem.get(.. a_len);
                            if a_str == None {continue 'fora;} //
                            path_rem = &path_rem[a_len ..];
                            if !matches!(path_rem.get(.. 2), None | Some(",L") | Some(",R"))
                                {continue 'fora;}
                            main_str.push_str(",A");
                        } else if path_rem.get(.. a_len) == a_str {
                            path_rem = &path_rem[a_len ..];
                            main_str.push_str(",A");
                        } else if b_str == None {
                            b_str = path_rem.get(.. b_len);
                            if b_str == None {continue 'forb;}
                            path_rem = &path_rem[b_len ..];
                            if !matches!(path_rem.get(.. 2), None | Some(",L") | Some(",R"))
                                {continue 'forb;}
                            main_str.push_str(",B");
                        } else if path_rem.get(.. b_len) == b_str {
                            path_rem = &path_rem[b_len ..];
                            main_str.push_str(",B");
                        } else if c_str == None {
                            c_str = path_rem.get(.. c_len);
                            if c_str == None {continue 'forc;}
                            path_rem = &path_rem[c_len ..];
                            if !matches!(path_rem.get(.. 2), None | Some(",L") | Some(",R"))
                                {continue 'forc;}
                            main_str.push_str(",C");
                        } else if path_rem.get(.. c_len) == c_str {
                            path_rem = &path_rem[c_len ..];
                            main_str.push_str(",C");
                        } else {continue 'forc;}
                    }
                    cpu_cmd = Some(format!("{}\n{}\n{}\n{}\nn\n",
                        main_str.trim_matches(','),
                        a_str.unwrap().trim_matches(','),
                        b_str.unwrap().trim_matches(','),
                        c_str.unwrap().trim_matches(',')
                    ));
                    break 'fora;
                }
            }
        }
        
        cpu = Cpu::new(parse_intcode(s));
        cpu.mem[0] = 2;
        cpu.send_ascii(&cpu_cmd.unwrap());
        cpu.run().unwrap();
        cpu.recv_ascii();
        format!("{:?}", cpu.recv_all().collect::<Vec<_>>())
    }
}

fn day18(part: char, s: &str) -> String {
    let mut map = s.lines().map(|line| line.as_bytes()).collect::<Vec<_>>();
    let start_pos = map.iter().enumerate().find_map(|(y, mapy)|
        mapy.iter().enumerate().find_map(|(x, c)| if *c == b'@' {Some((x, y))} else {None})
    ).unwrap();
    let mut keys_all = 0u32;
    let mut keys_got_ever = 0;
    for c in s.bytes() {if matches!(c, b'a' ..= b'z') {keys_all |= 1 << c - b'a';}}
    
    if part == 'a' {
        let mut opens: BinaryHeap<(i32, (usize, usize), u32)> = BinaryHeap::new();
        opens.push((0, start_pos, 0));
        let mut closeds: HashSet<((usize, usize), u32)> = HashSet::new();

        while let Some((neg_len, pos, keys_got)) = opens.pop(){
            if keys_got == keys_all {
                return (-neg_len).to_string();
            }
            if !closeds.insert((pos, keys_got)) {continue;}
            keys_got_ever |= keys_got;
            if closeds.len() % 100 == 0 {
                println!("{} {} {} {:026b}", opens.len(), closeds.len(),
                         neg_len, keys_got_ever);
            }
            let paths = bfs_all(&map, pos, |c| match c {
                b'#' => false, b'.' | b'@' | b'a' ..= b'z' => true,
                b'A' ..= b'Z'  => keys_got & 1 << c - b'A' != 0,
                _ => panic!()
            });
            for key in b'a' ..= b'z' {
                if let Some((ref npath, npos)) = paths[key as usize] {
                    if keys_got & 1 << key - b'a' == 0 {
                        opens.push((neg_len - npath.len() as i32, npos, 
                                    keys_got | 1 << key - b'a'));
                    }
                }
            }
        }
    } else {
        let (start_x, start_y) = start_pos;
        let mut nline: Vec<u8> = map[start_y - 1].into();
        nline[start_x] = b'#';
        map[start_y - 1] = &nline;
        let mut nline: Vec<u8> = map[start_y].into();
        nline[start_x - 1] = b'#';
        nline[start_x + 1] = b'#';
        map[start_y] = &nline;
        let mut nline: Vec<u8> = map[start_y + 1].into();
        nline[start_x] = b'#';
        map[start_y + 1] = &nline;
        
        let mut opens: BinaryHeap<(i32, [(usize, usize); 4], u32)> = BinaryHeap::new();
        opens.push((0, [
            (start_x - 1, start_y - 1), (start_x - 1, start_y + 1),
            (start_x + 1, start_y - 1), (start_x + 1, start_y + 1)
        ], 0));
        let mut closeds: HashSet<([(usize, usize); 4], u32)> = HashSet::new();
        
        while let Some((neg_len, poses, keys_got)) = opens.pop(){
            if keys_got == keys_all {
                return (-neg_len).to_string()
            }
            if !closeds.insert((poses, keys_got)) {continue;}
            keys_got_ever |= keys_got;
            if closeds.len() % 100 == 0 {
                println!("{} {} {} {:026b}", opens.len(), closeds.len(),
                         neg_len, keys_got_ever);
            }
            let pathses: [_; 4] = poses.iter().map(|pos| bfs_all(&map, *pos, |c| match c {
                b'#' => false, b'.' | b'@' | b'a' ..= b'z' => true,
                b'A' ..= b'Z'  => keys_got & 1 << c - b'A' != 0,
                _ => panic!()
            })).collect::<Vec<_>>().try_into().unwrap();
            
            for (vault, paths) in pathses.iter().enumerate() {
                for key in b'a' ..= b'z' {
                    if let Some((ref npath, npos)) = paths[key as usize] {
                        if keys_got & 1 << key - b'a' == 0 {
                            let mut nposes = poses;
                            nposes[vault] = npos;
                            opens.push((neg_len - npath.len() as i32, nposes, 
                                        keys_got | 1 << key - b'a'));
                        }
                    }
                }
            }
       } 
    }
    panic!();
}

fn day19(part: char, s: &str) -> String {
    use intcode::*;
    let code = parse_intcode(s);
    let test = |x: usize, y: usize| -> bool {
        let mut cpu = Cpu::with_input(code.clone(), &[x as isize, y as isize]);
        cpu.run().unwrap();
        cpu.recv().unwrap() != 0
    };

    if part == 'a' {
        (0 .. 50).map(|x|
            (0 .. 50).map(|y| {test(x, y) as u32}).sum::<u32>()
        ).sum::<u32>().to_string()
    } else {
        let mut x = 0;
        let mut y = 0;
        loop {
            while !test(x, y + 99) {x += 1;}
            if !test(x + 99, y) {y += 1;} else {break;}
        }
        (x * 10000 + y).to_string()
    }
}

fn day20(part: char, s: &str) -> String {
    let map = s.lines().map(|line| line.as_bytes().into()).collect::<Vec<Vec<u8>>>();
    let mut portals: HashMap<(u8, u8), Vec<(usize, usize)>> = HashMap::new();

    for y in 0 .. map.len() {
        for x in 0 .. map[y].len() {
            if matches!(map[y][x], b'A' ..= b'Z') {
                if matches!(map[y].get(x + 1), Some(b'A' ..= b'Z')){
                    if x != 0 && map[y].get(x - 1) == Some(&b'.') {
                        portals.entry((map[y][x], map[y][x + 1]))
                               .or_insert_with(|| vec![])
                               .push((x - 1, y));
                    }
                    if map[y].get(x + 2) == Some(&b'.') {
                        portals.entry((map[y][x], map[y][x + 1]))
                               .or_insert_with(|| vec![])
                               .push((x + 2, y));
                    }
                }
                if matches!(map.get(y + 1).and_then(|mapy| mapy.get(x)), Some(b'A' ..= b'Z')){
                    if y != 0 && map.get(y - 1).and_then(|mapy| mapy.get(x)) == Some(&b'.') {
                        portals.entry((map[y][x], map[y + 1][x]))
                               .or_insert_with(|| vec![])
                               .push((x, y - 1));
                    }
                    if map.get(y + 2).and_then(|mapy| mapy.get(x)) == Some(&b'.') {
                        portals.entry((map[y][x], map[y + 1][x]))
                               .or_insert_with(|| vec![])
                               .push((x, y + 2));
                    }
                }
            }
        }
    }

    let mut links = HashMap::new();
    for p in portals.values() {match p[..] {
        [_] => {}, [p1, p2] => {links.insert(p1, p2); links.insert(p2, p1);}, _ => panic!()
    }}

    let mut prev: HashSet<(usize, usize, usize)> = HashSet::new();
    let mut curr: HashSet<(usize, usize, usize)> = HashSet::new();
    match portals[&(b'A', b'A')][..] {[(x, y)] => {curr.insert((x, y, 0));}, _ => panic!()}
    let mut next: HashSet<(usize, usize, usize)> = HashSet::new();
    let end;
    match portals[&(b'Z', b'Z')][..] {[(x, y)] => end = (x, y, 0), _ => panic!()}

    for t in 1 .. {
        assert!(!curr.is_empty());
        for (x, y, d) in curr.drain() {
            if y > 0 && map.get(y).and_then(|mapy| mapy.get(x)) == Some(&b'.') {
                if (x, y - 1, d) == end {return t.to_string();}
                if prev.insert((x, y - 1, d)) {next.insert((x, y - 1, d));}
            }
            if x > 0 && map[y].get(x - 1) == Some(&b'.') {
                if (x - 1, y, d) == end {return t.to_string();}
                if prev.insert((x - 1, y, d)) {next.insert((x - 1, y, d));}
            }
            if map[y].get(x + 1) == Some(&b'.') {
                if (x + 1, y, d) == end {return t.to_string();}
                if prev.insert((x + 1, y, d)) {next.insert((x + 1, y, d));}
            }
            if map.get(y + 1).and_then(|mapy| mapy.get(x)) == Some(&b'.') {
                if (x, y + 1, d) == end {return t.to_string();}
                if prev.insert((x, y + 1, d)) {next.insert((x, y + 1, d));}
            }
            if let Some(&(nx, ny)) = links.get(&(x, y)) {
                let is_outward = x == 2 || y == 2 || map[y].len() - x == 3 || map.len() - y == 3;
                if d > 0 || part == 'a' || !is_outward {
                    let nd = if part == 'a' {0} else if is_outward {d - 1} else {d + 1};
                    if prev.insert((nx, ny, nd)) {next.insert((nx, ny, nd));}
                }
            }
        }
        std::mem::swap(&mut curr, &mut next);
    }
    unreachable!();
}

fn day21(part: char, s: &str) -> String {
    use intcode::*;
    let mut cpu = Cpu::new(parse_intcode(s));
    cpu.send_ascii(if part  == 'a' { /* (!a || !b || !c) && d */ "\
NOT A J
NOT B T
OR  T J
NOT C T
OR  T J
AND D J
WALK
    "} else { /* (!a || !b || !c) && d  && (e || h) */ "\
NOT A J
NOT B T
OR  T J
NOT C T
OR  T J
AND D J
NOT E T
NOT T T
OR  H T
AND T J
RUN
    "});
    cpu.run().unwrap();
    println!("{}", cpu.recv_ascii());
    format!("{:?}", cpu.recv_all().collect::<Vec<_>>())
}

fn day22(part: char, s: &str) -> String {
    let n_cards: u64 = if part == 'a' {10007} else {119315717514047};
    let mod_mul = |a: u64, b: u64| -> u64 {(a as u128 * b as u128 % n_cards as u128) as u64};
    let mut increment = 1u64;
    let mut zero_at = 0u64;
    for line in s.lines() {
        let op: Vec<u8> = line.bytes().take_while(|c| !matches!(c, b'0' ..= b'9' | b'-')).collect();
        match &op[..] {
            b"deal into new stack" => {
                increment = n_cards - increment;
                zero_at = n_cards - 1 - zero_at;
            }
            b"cut " => {
                let arg: i64 = line[op.len() ..].parse().unwrap();
                let arg_p: u64 = (n_cards as i64 - arg) as u64;
                zero_at = (zero_at + arg_p) % n_cards;
            }
            b"deal with increment " => {
                let arg: u64 = line[op.len() ..].parse().unwrap();
                increment = mod_mul(increment, arg);
                zero_at = mod_mul(zero_at, arg);
            }
            _ => panic!()
        }
    }
    
    if part == 'a' {
        return ((zero_at as u128 + 2019 * increment as u128) % n_cards as u128).to_string()
    }

    let mut total_increment = 1u64;
    let mut total_zero_at = 0u64;

    //magic number is the number of shuffles given by the challenge
    for bit in format!("{:b}", 101741582076661u64).bytes() {
        total_zero_at = (mod_mul(total_zero_at, total_increment) + total_zero_at) % n_cards;
        total_increment = mod_mul(total_increment, total_increment);
        
        if bit == b'1' {
            total_zero_at = (mod_mul(total_zero_at, increment) + zero_at) % n_cards;
            total_increment = mod_mul(total_increment, increment);
        }
    }
    //  pos = zero_at  + increment * id
    //  pos - zero_at  = increment * id
    // (pos - zero_at) / increment = id
    let ii = mod_inverse(total_increment, n_cards).unwrap();
    assert_eq!(mod_mul(ii, total_increment), 1);
    mod_mul(ii, 2020 + n_cards - total_zero_at).to_string()
}

fn day23(part: char, s: &str) -> String {
    use intcode::*;
    let code = parse_intcode(s);
    let mut cpus: Vec<Cpu> = (0 .. 50).map(|ip| Cpu::with_input(code.clone(), &[ip])).collect();
    let mut nat = None;
    let mut last_nat_sent = None;
    
    loop {
        let mut network_idle = true;
        for ip in 0 .. 50 {
            cpus[ip].send(-1);
            cpus[ip].run().unwrap();
            loop{
                match (cpus[ip].recv(), cpus[ip].recv(), cpus[ip].recv()) {
                    (Some(rip @ 0 ..= 49), Some(x), Some(y)) => {
                        cpus[rip as usize].send(x);
                        cpus[rip as usize].send(y);
                        network_idle = false;
                    }
                    (Some(255), Some(x), Some(y)) => {
                        if part == 'a' {return y.to_string()}
                        nat = Some((x, y));
                    }
                    (None, _, _) => break,
                    e => panic!("unknown response {:?} from CPU {}", e, ip)
                }
            }
        }
        if network_idle {
            let (x, y) = nat.unwrap();
            if last_nat_sent == Some(y) {return y.to_string()};
            last_nat_sent = Some(y);
            cpus[0].send(x);
            cpus[0].send(y);
        }
    }
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