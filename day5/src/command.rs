#[derive(Debug)]
pub struct Command {
    pub amount: usize,
    pub from: usize,
    pub to: usize,
}

impl Command {
    pub fn new(amount: usize, from: usize, to: usize) -> Self {
        Command { amount, from, to }
    }
}
