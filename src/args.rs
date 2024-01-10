use clap::Parser;

/// Compile halex into an object executable
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
pub struct HalexArgs {
    /// Path of the file you want to compile
    #[arg(short, long, default_value = "main.hlx")]
    pub path: std::path::PathBuf,
    /// Object name
    #[arg(short, long, default_value = "main.o")]
    pub object: String,
    /// Opt level
    #[arg(long, default_value = "3")]
    pub opt: usize,
}
