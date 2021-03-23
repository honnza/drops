use std::collections::*;
use std::convert::*;
use std::fmt::Write;

fn str_split_once<'a>(s: &'a str, pat: &str) -> Option<(&'a str, &'a str)> {
    match s.split(pat).collect::<Vec<_>>()[..] {
        [x, y] => Some((x, y)),
        _ => None
    }
}

fn gcd(a: u64, b: u64) -> u64 {if b == 0 {a} else {gcd(b, a % b)}}
fn lcm(a: u64, b: u64) -> u64 {a / gcd(a, b) * b}
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


fn bfs(map: &Vec<Vec<u8>>,
       start: (usize, usize),
       valid: impl Fn(u8) -> bool,
       goal: impl Fn(u8) -> bool,
) -> Option<Vec<u8>> {
    let mut prev: HashSet<(usize, usize)> = HashSet::new();
    let mut curr: HashMap<(usize, usize), Vec<u8>> = HashMap::new();
    let mut next: HashMap<(usize, usize), Vec<u8>> = HashMap::new();
    curr.insert(start, vec![]);
    loop {
       prev.extend(curr.keys().copied());
       for ((x, y), path) in curr.into_iter() {
            for (nx, ny, dir) in &[
                (x, y - 1, b'N'), (x + 1, y, b'E'), (x, y + 1, b'S'), (x - 1, y, b'W')
            ] {
                let mut npath = path.clone();
                npath.push(*dir);
                if valid(map[*ny][*nx]) && !prev.contains(&(*nx, *ny)){
                    if goal(map[*ny][*nx]) {return Some(npath);}
                    next.insert((*nx, *ny), npath);
                }
            }
        }
        if next.is_empty() {return None;}
        curr = next;
        next = HashMap::new();
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
    if part == 'a' {
        s.lines().map(|line| {
            line.parse::<u32>().unwrap() / 3 - 2
        }).sum::<u32>().to_string()
    } else {
        s.lines().map(|line| {
            let mut fuel_tot = 0;
            let mut mass_rem = line.parse::<u32>().unwrap();
            while mass_rem > 8 {
                mass_rem = mass_rem / 3 - 2;
                fuel_tot += mass_rem;
            }
            fuel_tot
        }).sum::<u32>().to_string()
    }
}

fn day2(part: char, s: &str) -> String {
    use intcode::*;
    let code = parse_intcode(s);
    if part == 'a' {
        let mut cpu = Cpu::new(code);
        cpu.mem[1] = 12;
        cpu.mem[2] = 2;
        cpu.run().unwrap();
        assert!(matches!(cpu.state, RunState::Success));
        cpu.mem[0].to_string()
    } else {
        for noun in 0 ..= 99 {
            for verb in 0 ..= 99 {
                let mut cpu = Cpu::new(code.clone());
                cpu.mem[1] = noun;
                cpu.mem[2] = verb;
                cpu.run().unwrap();
                assert!(matches!(cpu.state, RunState::Success));
                if cpu.mem[0] == 19690720 {return (100 * noun + verb).to_string()}
            }
        }
        panic!()
    }
}

fn day3(part: char, s: &str) -> String {
    let paths = s.lines().map(|line| {
        let mut x = 0i32;
        let mut y = 0i32;
        let mut i = 0u32;
        let mut r = HashMap::new();
        for seg in line.split(",") {
            let len = seg[1..].parse().unwrap();
            match seg.as_bytes()[0] {
                b'D' => for _ in 0 .. len {y += 1; i+= 1; r.entry((x, y)).or_insert(i);},
                b'L' => for _ in 0 .. len {x -= 1; i+= 1; r.entry((x, y)).or_insert(i);},
                b'R' => for _ in 0 .. len {x += 1; i+= 1; r.entry((x, y)).or_insert(i);},
                b'U' => for _ in 0 .. len {y -= 1; i+= 1; r.entry((x, y)).or_insert(i);},
                _    => panic!()
            }
        }
        r
    }).collect::<Vec<_>>();
    
    if part == 'a' {
        paths[0].keys().filter(|k| paths[1].contains_key(k))
                       .map(|k| k.0.abs() + k.1.abs())
                       .min().unwrap().to_string()
    } else {
        paths[0].keys().filter(|k| paths[1].contains_key(k))
                       .map(|k| paths[0][k] + paths[1][k])
                       .min().unwrap().to_string()
    }
}

fn day4(part: char, s: &str) -> String {
    let (min, max) = str_split_once(s.trim(), "-").unwrap();
    let mut n = min.as_bytes().to_vec();
    for i in 0 .. n.len() - 1 {if n[i+1] < n[i] {for j in i + 1 .. n.len() {n[j] = n[i]}}}
    
    let mut count = 0u32;
    
    loop {
        for i in 0 .. n.len() - 1 {
            if n[i] == n[i+1] && (part == 'a' || 
                (i == 0 || n[i-1] != n[i]) && 
                (i == n.len() - 2 || n[i+1] != n[i+2])
            ) {count += 1; break}
        }

        let i_nn = match n.iter().rposition(|b| *b != b'9'){
            Some(i_nn) => i_nn, None => break
        };
        let nd = n[i_nn] + 1;
        for i in i_nn .. n.len() {n[i] = nd}
        if n.as_slice() > max.as_bytes() {break}
    }
    count.to_string()
}

fn day5(part: char, s: &str) -> String {
    use intcode::*;
    let mut cpu = Cpu::with_input(parse_intcode(s), &[if part == 'a' {1} else {5}]);
    cpu.run().unwrap();
    assert!(matches!(cpu.state, RunState::Success));
    format!("{:?}", cpu.recv_all().collect::<Vec<_>>())
}

fn day6(part: char, s: &str) -> String {
    let mut orbits: HashMap<&str, &str> = HashMap::new();
    for line in s.lines() {
        let (v, k) = str_split_once(line, ")").unwrap();
        orbits.insert(k, v);
    }

    let mut orbit_depths: HashMap<&str, u32> = HashMap::new();
    orbit_depths.insert("COM", 0);
    let mut eval_stack: Vec<&str> = vec![];
    for k in orbits.keys() {
        eval_stack.push(k);
        while !eval_stack.is_empty() {
            let e = eval_stack.last().unwrap();
            let e_par = orbits[e];
            if let Some(depth) = orbit_depths.get(e_par) {
                let v = depth + 1;
                orbit_depths.insert(e, v);
                eval_stack.pop();
            } else {
                eval_stack.push(e_par);
            }
        }
    }
    
    if part == 'a' {
        orbit_depths.values().sum::<u32>().to_string()
    } else {
        let mut you_pp = orbits["YOU"];
        let you_depth = orbit_depths[you_pp];
        let mut ypp_depth = you_depth;
        let mut san_pp = orbits["SAN"];
        let san_depth = orbit_depths[san_pp];
        let mut spp_depth = san_depth;
        
        while ypp_depth > spp_depth {you_pp = orbits[you_pp]; ypp_depth -= 1;}
        while ypp_depth < spp_depth {san_pp = orbits[san_pp]; spp_depth -= 1;}
        while you_pp != san_pp {you_pp = orbits[you_pp]; san_pp = orbits[san_pp]; ypp_depth -= 1;}
        (you_depth + san_depth - 2 * ypp_depth).to_string()
    }
}

fn day7(part: char, s: &str) -> String {
    use intcode::*;
    let code = parse_intcode(s);
    let mut best_out = isize::MIN;
    if part == 'a' {
        let mut cpu;
        for a in 0 .. 5 {
            cpu = Cpu::with_input(code.clone(), &[a, 0]);
            cpu.run().unwrap();
            assert!(matches!(cpu.state, RunState::Success));
            let a_res = cpu.recv().unwrap();
            for b in 0 .. 5 {if a != b {
                cpu = Cpu::with_input(code.clone(), &[b, a_res]);
                cpu.run().unwrap();
                assert!(matches!(cpu.state, RunState::Success));
                let b_res = cpu.recv().unwrap();
                for c in 0 .. 5 {if a != c && b != c {
                    cpu = Cpu::with_input(code.clone(), &[c, b_res]);
                    cpu.run().unwrap();
                    assert!(matches!(cpu.state, RunState::Success));
                    let c_res = cpu.recv().unwrap();
                    for d in 0 .. 5 {if a != d && b != d && c != d {
                        cpu = Cpu::with_input(code.clone(), &[d, c_res]);
                        cpu.run().unwrap();
                        assert!(matches!(cpu.state, RunState::Success));
                        let d_res = cpu.recv().unwrap();
                        
                        let e = 10 - a - b - c - d;
                        cpu = Cpu::with_input(code.clone(), &[e, d_res]);
                        cpu.run().unwrap();
                        assert!(matches!(cpu.state, RunState::Success));
                        let e_res = cpu.recv().unwrap();
                        
                        best_out = isize::max(best_out, e_res);
                    }}
                }}
            }}
        }
    } else {
        for a in 5 .. 10 {
            for b in 5 .. 10 {if a != b {
                for c in 5 .. 10 {if a != c && b != c {
                    for d in 5 .. 10 {if a != d && b != d && c != d {
                        let e = 35 - a - b - c - d;
                        let mut cpu_a = Cpu::with_input(code.clone(), &[a]);
                        let mut cpu_b = Cpu::with_input(code.clone(), &[b]);
                        let mut cpu_c = Cpu::with_input(code.clone(), &[c]);
                        let mut cpu_d = Cpu::with_input(code.clone(), &[d]);
                        let mut cpu_e = Cpu::with_input(code.clone(), &[e]);
                        let mut res_e = 0;
                        loop {
                            cpu_a.send(res_e);
                            cpu_a.run().unwrap();
                            match cpu_a.recv() {Some(r) => cpu_b.send(r), None => break}
                            cpu_b.run().unwrap();
                            match cpu_b.recv() {Some(r) => cpu_c.send(r), None => break}
                            cpu_c.run().unwrap();
                            match cpu_c.recv() {Some(r) => cpu_d.send(r), None => break}
                            cpu_d.run().unwrap();
                            match cpu_d.recv() {Some(r) => cpu_e.send(r), None => break}
                            cpu_e.run().unwrap();
                            match cpu_e.recv() {Some(r) => res_e = r, None => break}
                        }
                        best_out = isize::max(best_out, res_e);
                    }}
                }}
            }}
        }
    }
    best_out.to_string()
}

fn day8(part: char, s: &str) -> String {
    if part == 'a' {
        let tallies = s.trim().as_bytes().chunks(150).map(|layer|
            [b'0', b'1', b'2'].iter().map(|d|
                layer.iter().filter(|c| *c == d).count()
            ).collect::<Vec<_>>()
        ).min().unwrap();
        return (tallies[1] * tallies[2]).to_string()
    } else {
        let mut r = [b'2'; 150];
        for layer in s.trim().as_bytes().chunks(150) {
            for i in 0 .. 150 {if r[i] == b'2' {r[i] = layer[i]}}
        }
        for i in 0 .. 150 {if r[i] == b'0' {r[i] = b'.'} else if r[i] == b'1' {r[i] = b'#'}}
        for line in r.chunks(25) {println!("{}", std::str::from_utf8(line).unwrap())}
    }
    "".to_string()
}

fn day9(part: char, s: &str) -> String {
    use intcode::*;
    let mut cpu = Cpu::with_input(parse_intcode(s), &[if part == 'a' {1} else {2}]);
    cpu.run().unwrap();
    assert!(matches!(cpu.state, RunState::Success));
    format!("{:?}", cpu.recv_all().collect::<Vec<_>>())
}



fn day10(part: char, s: &str) -> String {
    let asteroids: Vec<(i32, i32)> = s.lines().enumerate().map(|(y, line)|
        line.bytes().enumerate().filter(|(_x, byte)| *byte == b'#')
                                .map(move |(x, _byte)| (x as i32, -(y as i32)))
    ).flatten().collect();
    
    let best_asteroid = asteroids.iter().map(|a| {
        let mut all_dirs = asteroids.iter().map(|b| (b.0 - a.0, b.1 - a.1))
                                           .filter(|b| b.0 != 0 || b.1 != 0)
                                           .collect::<Vec<_>>();
        all_dirs.sort_unstable_by(|a, b|
            // (0, +) < (+, _) < (0, -) < (-, _)
            // a.0 * b.1 - b.0 * a.1 is the cross product a*b
            // which works for anything less than a half-circle
            (*a < (0, 0)).cmp(&(*b < (0, 0)))
            .then((a.0 * b.1).cmp(&(b.0 * a.1)))
            .then(a.0.abs().cmp(&b.0.abs()))
            .then(a.1.abs().cmp(&b.1.abs()))
        );
        let mut dirs = all_dirs.clone();
        dirs.dedup_by(|a, b|
            a.1.signum() == b.1.signum() &&
            a.0.signum() == b.0.signum() &&
            a.0 * b.1 == b.0 * a.1
        );
        (dirs.len(), a, all_dirs)
    }).max().unwrap();

    if part == 'a' {return best_asteroid.0.to_string()}
    
    let mut n_gone = 0;
    let mut prev_asteroid = None;
    let dir_zero = best_asteroid.1;
    let mut asteroids_rem = best_asteroid.2;
    let mut r = None;
    loop {
        asteroids_rem.retain(|a| {
            let b = match prev_asteroid.replace(*a) {
                Some(b) => b, None => {n_gone += 1; return false}
            };
            if a.0 * b.1 != a.1 * b.0 ||
                a.0.signum() != b.0.signum() ||
                a.1.signum() != b.1.signum()
            {
                n_gone += 1;
                if n_gone == 200 {
                    r = Some(100 * (dir_zero.0 + a.0) - (dir_zero.1 + a.1));
                }
                false
            } else {true}
        });
        if let Some(rv) = r {return rv.to_string();}
    }
}

fn day11(part: char, s: &str) -> String {
    use intcode::*;
    let mut cpu = Cpu::new(parse_intcode(s));
    let mut tiles = HashMap::<(i32, i32), bool>::new();
    if part == 'b' {tiles.insert((0,0), true);}
    let mut pos = (0i32, 0i32);
    let mut dir = (0i32, 1i32);
    loop {
        cpu.send(tiles.get(&pos).copied().unwrap_or(false) as isize);
        cpu.run().unwrap();
        match (cpu.recv(), cpu.recv()) {
            (Some(c), Some(d)) if (c == 0 || c == 1) && (d == 0 || d == 1) => {
                tiles.insert(pos, c != 0);
                dir = if d != 0 {(dir.1, -dir.0)} else {(-dir.1, dir.0)};
                pos.0 += dir.0;
                pos.1 += dir.1;
            },
            (None, _) => break,
            e => panic!("unknown response {:?} from CPU", e)
        }
    }
    if part == 'a' {
        tiles.len().to_string()
    } else {
        let mut minx = 0;
        let mut maxx = 0;
        let mut miny = 0;
        let mut maxy = 0;
        for (x, y) in tiles.keys() {
            if minx > *x {minx = *x;}
            if maxx < *x {maxx = *x;}
            if miny > *y {miny = *y;}
            if maxy < *y {maxy = *y;}
        }
        for y in (miny ..= maxy).rev() {
            for x in minx ..= maxx {
                print!("{}", match tiles.get(&(x, y)) {
                    None => ' ', Some(false) => '.', Some(true) => '#'
                });
            }
            println!();
        }
        "".to_string()
    }
}

fn day12(part: char, s: &str) -> String {
    let mut poses = s.lines().map(|line|
        line.split(|c: char| !c.is_ascii_digit() && c != '-')
            .filter(|s| !s.is_empty())
            .map(|s| s.parse::<i32>().unwrap())
            .collect::<Vec<_>>()
    ).collect::<Vec<_>>();
    let poses_init = poses.clone();
    let mut vels = vec![vec![0; poses[0].len()]; poses.len()];
    let mut periods = vec![None; poses[0].len()];
    
    for t in 1 .. {
        for i in 1 .. poses.len() {
            for j in 0 .. i {
                for c in 0 .. poses[i].len() {
                    if poses[i][c] > poses[j][c] {
                        vels[i][c] -= 1;
                        vels[j][c] += 1;
                    } else if poses [i][c] < poses[j][c]{
                        vels[i][c] += 1;
                        vels[j][c] -= 1;
                    }
                }
            }
        }
        for i in 0 .. poses.len() {
            for c in 0 .. poses[i].len() {
                poses[i][c] += vels[i][c];
            }
        }
        if part == 'a' && t == 1000 {
            return (0 .. poses.len()).map(|i|
                poses[i].iter().map(|c| c.abs()).sum::<i32>() * 
                vels[i].iter().map(|c| c.abs()).sum::<i32>()
            ).sum::<i32>().to_string()
        }
        if part == 'b' {
            for c in 0 .. poses[0].len() {
                if periods[c] == None && (0 .. poses.len()).all(|i|
                    poses[i][c] == poses_init[i][c] && vels[i][c] == 0
                ) {
                    periods[c] = Some(t);
                    if let Some(ps) = periods.iter().copied().collect::<Option<Vec<u64>>>() {
                        return ps.iter().fold(1, |a, b| lcm(a, *b)).to_string();
                    }
                }
            }
        }
    }
    unreachable!();
}

fn day13(part: char, s: &str) -> String {
    use intcode::*;
    let mut cpu = Cpu::new(parse_intcode(s));
    if part == 'b' {cpu.mem[0] = 2};
    let mut screen: Vec<Vec<isize>> = vec![];
    let mut score = 0;
    
    let mut ball_x = 0;
    let mut paddle_x = 0;
    cpu.run().unwrap();
    loop {
        match (cpu.recv(), cpu.recv(), cpu.recv()) {
            (Some(x), Some(y), Some(id @ 0 ..= 4)) if x >= 0 && y >= 0 => {
                let x = x as usize;
                let y = y as usize;
                if y >= screen.len() {screen.resize_with(y + 1, || vec![])};
                if x >= screen[y].len() {screen[y].resize(x + 1, 0)};
                screen[y][x] = id;
                if id == 3 {paddle_x = x};
                if id == 4 {ball_x = x};
            },
            (Some(-1), Some(0), Some(s)) => {score = s},

            (None, _, _) => {
                cpu.send((ball_x as isize - paddle_x as isize).signum());
                cpu.run().unwrap();
                if matches!(cpu.state, RunState::Success) {break;}
            }
            e => panic!("unknown response {:?} from CPU", e)
        }
    }
    if part == 'a' {
        screen.iter().flatten().filter(|c| **c == 2).count().to_string()
    } else {
        score.to_string()
    }
}

fn day14(part: char, s: &str) -> String {
    let mut rules_unsorted = s.lines().map(|line| {
        let (reagents_str, product_str) = str_split_once(line, " => ").unwrap();
        let (product_n_str, product_name) = str_split_once(product_str, " ").unwrap();
        let reagents = reagents_str.split(", ").map(|reagent_str| {
            let (reagent_n_str, reagent_name) = str_split_once(reagent_str, " ").unwrap();
            (reagent_name, reagent_n_str.parse::<u64>().unwrap())
        }).collect::<Vec<_>>();
        (product_name, product_n_str.parse::<u64>().unwrap(), reagents)
    }).collect::<Vec<_>>();
    
    let mut rules: Vec<(&str, u64, Vec<(&str, u64)>)> = vec![];
    while !rules_unsorted.is_empty() {
        rules_unsorted.retain(|(pn, pc, rs)| {
            if rs.iter().all(|(rn, _)|
                rn == &"ORE" || rules.iter().any(|(opn, _, _)| opn == rn)
            ) {
                rules.push((*pn, *pc, rs.clone()));
                false
            } else {true}
        })
    }
    
    let fuel_to_ore = |fuel| {
        let mut products_needed = HashMap::with_capacity(rules.len());
        products_needed.insert ("FUEL", fuel);
        for (pn, pc, rs) in rules.iter().rev() {
            let batches = (products_needed[pn] + pc - 1) / pc;
            for (rn, rc) in rs {
                *products_needed.entry(rn).or_insert(0) += rc * batches;
            }
        }
        products_needed["ORE"]
    };
    
    if part == 'a' {
        fuel_to_ore(1).to_string()
    } else {
        let ore_goal = 1e12 as u64;
        let mut min = 0;
        let mut max = 1;
        while fuel_to_ore(max) < ore_goal {max *= 2;}
        while max - min > 1 {
            let mid = (min + max) / 2;
            if fuel_to_ore(mid) < ore_goal {min = mid} else {max = mid}
        }
        min.to_string()
    }
}

fn day15(part: char, s: &str) -> String {
    use intcode::*;
    let mut cpu = Cpu::new(parse_intcode(s));
    let mut map = vec![vec![b' ', b' '], vec![b' ', b'<', b' '], vec![b' ', b' ']];
    let mut bot_y = 1;
    let mut bot_x = 1;
    let mut start = (1, 1);
    let mut path_rem = vec![];
    
    loop {
        if path_rem.is_empty() {
            match bfs(&map, (bot_x, bot_y), |c| c != b'#', |c| c == b' ') {
                Some(p) => {path_rem = p; path_rem.reverse();},
                None => break
            }
        }
        let path_next = path_rem.pop().unwrap();
        let bot_x_next = bot_x + (path_next == b'E') as usize - (path_next == b'W') as usize;
        let bot_y_next = bot_y + (path_next == b'S') as usize - (path_next == b'N') as usize;
        cpu.send(match path_next {b'N' => 1, b'S' => 2, b'W' => 3, b'E' => 4, _ => unreachable!()});
        cpu.run().unwrap();
        let reply = cpu.recv();
        match reply {
            Some(0) => map[bot_y_next][bot_x_next] = b'#',
            Some(1) | Some(2) => {
                if bot_x_next == 0 {
                    for row in &mut map {row.insert(0, b' ');}
                    start.0 += 1;
                } else {bot_x = bot_x_next;}
                if bot_y_next == 0 {
                    map.insert(0, vec![]);
                    start.1 += 1;
                } else {bot_y = bot_y_next;}
                if bot_y + 2 > map.len() {map.resize(bot_y + 2, vec![]);}
                if bot_x + 1 > map[bot_y - 1].len() {map[bot_y - 1].resize(bot_x + 1, b' ');}
                if bot_x + 2 > map[bot_y    ].len() {map[bot_y    ].resize(bot_x + 2, b' ');}
                if bot_x + 1 > map[bot_y + 1].len() {map[bot_y + 1].resize(bot_x + 1, b' ');}
                map[bot_y][bot_x] = if reply == Some(1) {b'.'} else {b'>'}
            },
            e => panic!("unknown response {:?} from CPU", e)
        }
    }
    
    if part == 'a' {
        return bfs(&map, start, |c| c != b'#', |c| c == b'>').unwrap().len().to_string()
    }
    
    for t in 0 .. {
        let mut done = true;
        for y in 0 .. map.len() {
            for x in 0 .. map[y].len() {
                if map[y][x] == b'o' || map[y][x] == b'>' {map[y][x] = b'O';}
            }
        }
        for y in 0 .. map.len() {
            for x in 0 .. map[y].len() {
                if map[y][x] == b'O' {
                    for (nx, ny) in &[(x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)] {
                        if map[*ny][*nx] == b'.' || map[*ny][*nx] == b'<' {
                            map[*ny][*nx] = b'o'; done = false;
                        }
                    }
                } 
            }
        }
        if done {return t.to_string();}
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