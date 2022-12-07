use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};
mod terminal;

const COMMAND_PREFIX: &str = "$";
const DIR_PREFIX: &str = "dir ";
fn main() {
    let commands = parse_commands(Path::new("input.txt"));
    println!("Part1: {}", part1(&commands));
}

fn part1(commands: &Vec<terminal::Command>) -> usize {
    let mut cur_path = PathBuf::new();

    for command in commands {
        match command {
            terminal::Command::Cd(dir_name) => {
                if dir_name == ".." {
                    cur_path.pop();
                } else {
                    cur_path.push(dir_name);
                }
            }
            terminal::Command::Ls(outputs) => {
                todo!()
            }
        }
    }
    0
}

fn parse_commands(input: &Path) -> Vec<terminal::Command> {
    let input = std::fs::read_to_string(input).unwrap();
    let mut result = Vec::new();

    let lines: Vec<&str> = input.lines().collect();
    let mut i = 0;
    while i < lines.len() {
        let cur_line = lines.get(i).unwrap();

        if cur_line.starts_with(COMMAND_PREFIX) {
            match &cur_line[2..4] {
                "cd" => {
                    result.push(terminal::Command::Cd(cur_line[5..].to_string()));
                    i += 1;
                }
                "ls" => {
                    let mut entries = Vec::new();
                    i += 1;
                    while let Some(x) = lines.get(i) {
                        if x.starts_with(COMMAND_PREFIX) {
                            break;
                        } else if let Some(stripped) = x.strip_prefix(DIR_PREFIX) {
                            entries.push(terminal::Output::Directory(stripped.to_string()));
                        } else {
                            let mut split = x.split_ascii_whitespace();
                            let size: usize = split.next().unwrap().parse().unwrap();
                            let name = split.next().unwrap();
                            entries.push(terminal::Output::FileEntry(terminal::File {
                                name: name.to_string(),
                                size,
                            }));
                        }
                        i += 1;
                    }
                    result.push(terminal::Command::Ls(entries))
                }
                _ => {
                    panic!("Invalid Command!")
                }
            };
        }
    }

    result
}
