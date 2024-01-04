use clap::Parser;

/// Compile halex into an object executable
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
pub struct HalexArgs {
    /// Path of the file you want to compile
    #[arg(short, long, default_value = "main.hlx")]
    pub path: std::path::PathBuf,
    /// Object name
    #[arg(short, long, default_value = "a")]
    pub object: String,
}
