use anyhow::Result;
use stlua::StScriptWriter;

fn main() -> Result<()> {
    let lua = std::fs::read_to_string(
        std::env::args()
            .nth(1)
            .expect("Please provide a filename as an argument"),
    )?;
    println!("{}", StScriptWriter::run_code(lua));
    Ok(())
}
