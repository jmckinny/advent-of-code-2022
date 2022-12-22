#[derive(Debug, Clone)]
pub struct Directory {
    pub name: String,
    pub local_size: usize,
    pub sub_dirs: Vec<String>,
}
