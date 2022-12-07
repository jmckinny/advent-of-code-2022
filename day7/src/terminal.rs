#[derive(Debug)]
pub enum Command {
    Cd(String),
    Ls(Vec<Output>),
}

#[derive(Debug)]
pub enum Output {
    Directory(String),
    FileEntry(File),
}

#[derive(Debug)]
pub struct File {
    pub name: String,
    pub size: usize,
}
