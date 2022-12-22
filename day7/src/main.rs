use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};
mod file_system;
mod terminal;

const COMMAND_PREFIX: &str = "$";
const DIR_PREFIX: &str = "dir ";
fn main() {
    let commands = parse_commands(Path::new("input.txt"));
    println!("Part1: {:?}", part1(&commands));
    println!("Part2: {:?}", part2(&commands));
}

fn part2(commands: &Vec<terminal::Command>) -> usize {
    let total = 70000000;
    let needed = 30000000;
    let sizes = compute_file_sizes(commands);
    let cur_used = *sizes.values().max().unwrap();
    let mut best = usize::MAX;
    for (_, size) in sizes {
        let possible_free = total - cur_used + size;
        if possible_free >= needed && size < best {
            best = size;
        }
    }
    best
}

fn part1(commands: &Vec<terminal::Command>) -> usize {
    let sizes = compute_file_sizes(commands);
    sizes.values().filter(|x| **x <= 100000).sum()
}

fn compute_file_sizes(commands: &Vec<terminal::Command>) -> HashMap<String, usize> {
    let mut cur_path = PathBuf::new();
    let mut dirs: HashMap<String, file_system::Directory> = HashMap::new();
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
                let mut local_size = 0;
                let mut sub_dir = Vec::new();
                for x in outputs {
                    match x {
                        terminal::Output::Directory(dir_name) => {
                            let mut name = cur_path.to_str().unwrap().to_string();
                            if name != "/" {
                                name.push('/')
                            }
                            name.push_str(dir_name);
                            sub_dir.push(name);
                        }
                        terminal::Output::FileEntry(f) => {
                            local_size += f.size;
                        }
                    }
                }
                dirs.insert(
                    cur_path.to_str().unwrap().to_string(),
                    file_system::Directory {
                        name: cur_path.to_str().unwrap().to_string(),
                        local_size,
                        sub_dirs: sub_dir,
                    },
                );
            }
        }
    }
    let mut result = HashMap::new();
    for d in dirs.clone().keys() {
        result.insert(d.to_string(), calc_dir_size(d, &mut dirs));
    }
    result
}

fn calc_dir_size(name: &str, dirs: &mut HashMap<String, file_system::Directory>) -> usize {
    if dirs
        .get(name)
        .unwrap_or_else(|| panic!("{} not present", name))
        .sub_dirs
        .is_empty()
    {
        return dirs.get(name).unwrap().local_size;
    } else {
        let mut result = 0;
        let sub_dirs = dirs.get(name).unwrap().sub_dirs.clone();
        for sub_dir in sub_dirs {
            result += calc_dir_size(&sub_dir, dirs);
        }
        result + dirs.get(name).unwrap().local_size
    }
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
