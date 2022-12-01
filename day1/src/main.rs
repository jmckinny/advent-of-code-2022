use std::path::Path;

fn main() {
    let data = parse_data();
    println!("Part1: {}", part1(&data));
    println!("Part2: {}", part2(&data));
}

fn part2(data: &[Vec<i32>]) -> i32 {
    let mut sums: Vec<i32> = data.iter().map(|x| x.iter().sum()).collect::<Vec<i32>>();
    sums.sort();
    sums.iter().rev().take(3).sum()
}

fn part1(data: &[Vec<i32>]) -> i32 {
    data.iter().map(|x| x.iter().sum()).max().unwrap()
}

fn parse_data() -> Vec<Vec<i32>> {
    let data = read_file(Path::new("input.txt"));
    let split = data.split("\n\n");
    let mut calories: Vec<Vec<i32>> = Vec::new();
    for (i, elf) in split.enumerate() {
        calories.push(Vec::new());
        for num in elf.split('\n') {
            calories[i].push(num.parse::<i32>().unwrap());
        }
    }
    calories
}

fn read_file(filename: &Path) -> String {
    std::fs::read_to_string(filename).unwrap()
}
