use std::collections::{HashSet, HashMap};
use std::ops::RangeInclusive;
use std::io;

pub struct Space {
    active: HashSet<(i32, i32, i32)>,
}

impl Space {
    pub fn new(initial: HashSet<(i32, i32, i32)>) -> Self {
        Self { active: initial }
    }

    pub fn bounds(&self) -> Option<(RangeInclusive<i32>, RangeInclusive<i32>, RangeInclusive<i32>)> {
        let mut iter = self.active.iter();
        if let Some((first_x, first_y, first_z)) = iter.next() {
            let mut min_x = first_x;
            let mut max_x = first_x;
            let mut min_y = first_y;
            let mut max_y = first_y;
            let mut min_z = first_z;
            let mut max_z = first_z;
            while let Some((x, y, z)) = iter.next() {
                if x < min_x { min_x = x; }
                if x > max_x { max_x = x; }
                if y < min_y { min_y = y; }
                if y > max_y { max_y = y; }
                if z < min_z { min_z = z; }
                if z > max_z { max_z = z; }
            }
            Some((*min_x ..= *max_x, *min_y ..= *max_y, *min_z ..= *max_z)) // {min,max}_{x,y,z} are deduced as &i32, not i32, so deref to clone
        } else { // empty
            None
        }
    }

    pub fn iterate(self) -> Self {
        if let Some((x_bounds, y_bounds, z_bounds)) = self.bounds() {
            let min_x = x_bounds.start() - 1;
            let max_x = x_bounds.end()   + 1;
            let min_y = y_bounds.start() - 1;
            let max_y = y_bounds.end()   + 1;
            let min_z = z_bounds.start() - 1;
            let max_z = z_bounds.end()   + 1;
            let mut neighbor_counts = HashMap::new();
            for pos in self.active.iter() {
                if self.active.contains(&pos) {
                    let (x, y, z) = pos;
                    for (dx, dy, dz) in &[
                        (-1, -1, -1),
                        (-1, -1,  0),
                        (-1, -1,  1),
                        (-1,  0, -1),
                        (-1,  0,  0),
                        (-1,  0,  1),
                        (-1,  1, -1),
                        (-1,  1,  0),
                        (-1,  1,  1),

                        ( 0, -1, -1),
                        ( 0, -1,  0),
                        ( 0, -1,  1),
                        ( 0,  0, -1),
                        //( 0,  0,  0), // a cell is not a neighbor to itself
                        ( 0,  0,  1),
                        ( 0,  1, -1),
                        ( 0,  1,  0),
                        ( 0,  1,  1),

                        ( 1, -1, -1),
                        ( 1, -1,  0),
                        ( 1, -1,  1),
                        ( 1,  0, -1),
                        ( 1,  0,  0),
                        ( 1,  0,  1),
                        ( 1,  1, -1),
                        ( 1,  1,  0),
                        ( 1,  1,  1),
                        
                    ] {
                        let neighbor = (x+dx, y+dy, z+dz);
                        let neighbor_count =
                            neighbor_counts
                            .entry(neighbor)
                            .or_insert(0);
                        *neighbor_count += 1;
                    }
                }
            }
            let neighbor_counts = neighbor_counts;
            let mut new_active = HashSet::new();
            for x in min_x ..= max_x {
                for y in min_y ..= max_y {
                    for z in min_z ..= max_z {
                        let pos = (x, y, z);
                        if self.active.contains(&pos) {
                            // currently active
                            // must have exactly 2 or 3 active neighbors to stay active
                            match neighbor_counts.get(&pos) {
                                Some(2) | Some(3) => { new_active.insert(pos); }
                                _ => {}
                            }
                        } else {
                            // currently inactive
                            // must have exactly 3 active neighbors to become active
                            match neighbor_counts.get(&pos) {
                                Some(3) => { new_active.insert(pos); }
                                _ => {}
                            }
                        }
                    }
                }
            }
            Self { active: new_active }
        } else { // empty space
            self
        }
    }

    pub fn active_count(&self) -> usize {
        self.active.len()
    }
}

fn main() -> io::Result<()> {
    let mut lines = Vec::new();
    'read: loop {
        let mut line = String::new();
        if let 0 = io::stdin().read_line(&mut line)? {
            break 'read;
        }
        lines.push(line);
    }
    let mut initial: HashSet<(i32, i32, i32)> = HashSet::new();
    for (y, line) in lines.iter().enumerate() {
        for (x, chr) in line.chars().enumerate() {
            if chr == '#' {
                initial.insert((x as i32, y as i32, 0));
            }
        }
    }
    let mut space = Space::new(initial);
    for _ in 1..=6 {
        space = space.iterate();
    }
    println!("{} active cells after 6 cyles.", space.active_count());
    Ok(())
}
