use std::{collections::VecDeque, path::Path};

use command::Command;
use lazy_static::lazy_static;
use regex::Regex;
mod command;

type Stacks = Vec<VecDeque<char>>;

fn main() {
    let input = read_input(Path::new("input.txt"));
    let mut stacks = parse_stacks(&input);
    let mut stacks2 = parse_stacks(&input);
    let commands: Vec<Command> = input.lines().skip(10).map(parse_command).collect();
    println!("Part1: {}", part1(&mut stacks, &commands));
    println!("Part2: {}", part2(&mut stacks2, &commands));
}

fn part1(stacks: &mut Stacks, commands: &Vec<Command>) -> String {
    let mut result = String::new();
    for command in commands {
        let amount = command.amount;
        let from = command.from;
        let to = command.to;
        let removed = stacks[from - 1].drain(0..amount).collect::<Vec<char>>();
        for item in removed {
            stacks[to - 1].push_front(item);
        }
    }

    for stack in stacks {
        result.push(*stack.get(0).unwrap());
    }
    result
}

fn part2(stacks: &mut Stacks, commands: &Vec<Command>) -> String {
    let mut result = String::new();
    for command in commands {
        let amount = command.amount;
        let from = command.from;
        let to = command.to;
        let removed = stacks[from - 1].drain(0..amount).collect::<Vec<char>>();
        for item in removed.iter().rev() {
            stacks[to - 1].push_front(*item);
        }
    }

    for stack in stacks {
        result.push(*stack.get(0).unwrap());
    }
    result
}

fn parse_stacks(input: &str) -> Stacks {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"^(\s{3}|\[[A-Z]\]) (\s{3}|\[[A-Z]\]) (\s{3}|\[[A-Z]\]) (\s{3}|\[[A-Z]\]) (\s{3}|\[[A-Z]\]) (\s{3}|\[[A-Z]\]) (\s{3}|\[[A-Z]\]) (\s{3}|\[[A-Z]\]) (\s{3}|\[[A-Z]\])").unwrap();
    }
    let mut result = vec![VecDeque::new(); 9];
    let mut stacks = Vec::new();
    let lines = input.lines().take(8);
    for line in lines {
        let caps = RE.captures(line).unwrap();
        let row: Vec<&str> = caps.iter().skip(1).map(|x| x.unwrap().as_str()).collect();
        stacks.push(row);
    }
    let cleaned: Vec<String> = stacks
        .iter()
        .map(|row| {
            row.iter()
                .map(|s| s.replace("   ", " ").replace(['[', ']'], ""))
                .collect()
        })
        .collect();

    for row in cleaned {
        for (i, c) in row.chars().enumerate() {
            if c != ' ' {
                result[i].push_back(c);
            }
        }
    }
    result
}

fn parse_command(line: &str) -> Command {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"^move (\d+) from (\d+) to (\d+)").unwrap();
    }
    let captures = RE.captures(line).unwrap();
    let amount = captures.get(1).unwrap().as_str().parse().unwrap();
    let from = captures.get(2).unwrap().as_str().parse().unwrap();
    let to = captures.get(3).unwrap().as_str().parse().unwrap();
    command::Command::new(amount, from, to)
}

fn read_input(filepath: &Path) -> String {
    std::fs::read_to_string(filepath).unwrap()
}
