use crate::error::Result;
use chrono::Local;
use std::{
    cell::RefCell,
    collections::hash_map::{Entry, HashMap},
    ffi::OsString,
    fs,
    fs::{File, OpenOptions},
    io::Write,
    path::Path,
};

thread_local! {
    static FILE_MAP: RefCell<HashMap<OsString, File>> = RefCell::new(HashMap::new());
}

byond_fn! { log_write(path, data) {
    data.split('\n')
        .map(|line| format(line))
        .map(|line| write(path, line))
        .collect::<Result<Vec<_>>>()
        .err()
} }

byond_fn! { log_close_all() {
    FILE_MAP.with(|cell| {
        let mut map = cell.borrow_mut();
        map.clear();
    });
    Some("")
} }

fn format(data: &str) -> String {
    format!("[{}] {}\n", Local::now().format("%FT%T"), data)
}

fn write(path: &str, data: String) -> Result<usize> {
    FILE_MAP.with(|cell| {
        let mut map = cell.borrow_mut();
        let path = Path::new(path);
        let file = match map.entry(path.into()) {
            Entry::Occupied(elem) => elem.into_mut(),
            Entry::Vacant(elem) => elem.insert(open(path)?),
        };

        Ok(file.write(&data.into_bytes())?)
    })
}

fn open(path: &Path) -> Result<File> {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)?
    }

    Ok(OpenOptions::new().append(true).create(true).open(path)?)
}
