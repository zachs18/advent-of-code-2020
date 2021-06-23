use std::io;

fn solve(prices: &Vec<u32>) -> Option<u32> {
    for i in 0..prices.len() {
        for j in 0..i {
            if prices[i] + prices[j] == 2020 {
                return Some(prices[i] * prices[j])
            }
        }
    }
    None
}

fn main() -> io::Result<()> {
    let mut line = String::new();
    let mut prices: Vec<u32> = Vec::new();

    'read: loop {
        match io::stdin().read_line(&mut line) {
            Ok(0) => { break 'read; }
            _ => {
                match line.trim().parse() {
                    Ok(price) => { prices.push(price); }
                    Err(err) => { println!("{}", line); return Err(io::Error::new(io::ErrorKind::InvalidData, err)); }
                };
            }
        }
        line.clear();
    }
    println!("{}", solve(&prices).unwrap());
    Ok(())
}
