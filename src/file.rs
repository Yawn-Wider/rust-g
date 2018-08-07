use std::fs::File;
use std::io::{Read, Write};
use std::path::Path;
use std::fs;

use error::Result;

byond_fn! { file_read(path) {
    read(path).ok()
} }

byond_fn! { file_write(data, path) {
    write(data, path).err()
} }

fn read(path: &str) -> Result<String> {
    let mut file = File::open(path)?;
    let metadata = file.metadata()?;

    let mut content = String::with_capacity(metadata.len() as usize);
    file.read_to_string(&mut content)?;

    Ok(content)
}

fn write(data: &str, path_str: &str) -> Result<usize> {
    let path = Path::new(path_str);
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)?
    }

    let mut file = File::create(path)?;

    Ok(file.write(data.as_bytes())?)
}
