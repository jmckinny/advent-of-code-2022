use std::path::Path;

fn main() {
    let data = parse_input(Path::new("input.txt"));
    println!("Part1 {}", part1(&data));
    println!("Part2 {}", part2(&data));
}

fn part2(data: &[Vec<u8>]) -> u32 {
    let mut best = 0;
    for row in 0..data.len() {
        for col in 0..data[row].len() {
            let score = calc_scenic_score(data, row, col);
            best = best.max(score);
        }
    }
    best
}

fn part1(data: &[Vec<u8>]) -> u32 {
    let mut result = 0;
    for row in 0..data.len() {
        for col in 0..data[row].len() {
            if is_visable(data, row, col) {
                result += 1;
            }
        }
    }
    result
}

fn calc_scenic_score(data: &[Vec<u8>], row: usize, col: usize) -> u32 {
    let value = data[row][col];
    top_score(data, value, row, col)
        * bot_score(data, value, row, col)
        * left_score(data, value, row, col)
        * right_score(data, value, row, col)
}

fn top_score(data: &[Vec<u8>], value: u8, row: usize, col: usize) -> u32 {
    for i in (0..row).rev() {
        if data[i][col] >= value {
            return (row - i) as u32;
        }
    }
    row as u32
}

fn bot_score(data: &[Vec<u8>], value: u8, row: usize, col: usize) -> u32 {
    for (i, cur_row) in data.iter().enumerate().skip(row + 1) {
        if cur_row[col] >= value {
            return (i - row) as u32;
        }
    }
    (data.len() - 1 - row) as u32
}

fn left_score(data: &[Vec<u8>], value: u8, row: usize, col: usize) -> u32 {
    for i in (0..col).rev() {
        if data[row][i] >= value {
            return (col - i) as u32;
        }
    }
    col as u32
}

fn right_score(data: &[Vec<u8>], value: u8, row: usize, col: usize) -> u32 {
    for i in (col + 1)..data[0].len() {
        if data[row][i] >= value {
            return (i - col) as u32;
        }
    }
    (data[0].len() - 1 - col) as u32
}

fn is_visable(data: &[Vec<u8>], row: usize, col: usize) -> bool {
    if row == 0 || row == data.len() - 1 || col == 0 || col == data[0].len() - 1 {
        return true;
    }

    let cur_height = data[row][col];
    is_top_visable(data, cur_height, row, col)
        || is_bot_visable(data, cur_height, row, col)
        || is_left_visable(data, cur_height, row, col)
        || is_right_visable(data, cur_height, row, col)
}

fn is_top_visable(data: &[Vec<u8>], value: u8, row: usize, col: usize) -> bool {
    for cur_row in data.iter().take(row) {
        if cur_row[col] >= value {
            return false;
        }
    }
    true
}

fn is_bot_visable(data: &[Vec<u8>], value: u8, row: usize, col: usize) -> bool {
    for cur_row in data.iter().skip(row + 1) {
        if cur_row[col] >= value {
            return false;
        }
    }
    true
}

fn is_left_visable(data: &[Vec<u8>], value: u8, row: usize, col: usize) -> bool {
    for i in 0..col {
        if data[row][i] >= value {
            return false;
        }
    }
    true
}

fn is_right_visable(data: &[Vec<u8>], value: u8, row: usize, col: usize) -> bool {
    for i in col + 1..data[0].len() {
        if data[row][i] >= value {
            return false;
        }
    }
    true
}

fn parse_input(name: &Path) -> Vec<Vec<u8>> {
    let data = std::fs::read_to_string(name).unwrap();
    data.lines()
        .map(|line| {
            line.chars()
                .map(|c| c.to_digit(10).unwrap() as u8)
                .collect::<Vec<u8>>()
        })
        .collect()
}
