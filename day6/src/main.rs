use std::collections::{VecDeque, HashSet};



fn main() {
    let input = std::fs::read_to_string("input.txt").unwrap();
    println!("Part1: {}", part1(&input));
    println!("Part2: {}", part2(&input));
}

fn part1(input: &str) -> usize {
    let mut packet: VecDeque<char> = input.chars().take(4).collect();

    for (i,c) in input.chars().enumerate().skip(4){
        packet.pop_front();
        packet.push_back(c);
        let uniq = packet.iter().collect::<HashSet<_>>().len();
        if uniq == 4{
            return i+1;
        }
    }
    0
}

fn part2(input: &str) -> usize{
    let mut packet: VecDeque<char> = input.chars().take(14).collect();

    for (i,c) in input.chars().enumerate().skip(14){
        packet.pop_front();
        packet.push_back(c);
        let uniq = packet.iter().collect::<HashSet<_>>().len();
        if uniq == 14{
            return i+1;
        }
    }
    0
}

