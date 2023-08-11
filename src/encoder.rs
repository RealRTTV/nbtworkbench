use std::io::Write;

pub fn write_string<W: Write>(writer: &mut W, str: &str) {
	let _ = writer.write(&(str.len() as u16).to_be_bytes());
	let _ = writer.write(str.as_bytes());
}

#[derive(Debug)]
pub struct Ahhhh(
	usize,
	usize,
	usize,
	usize,
	usize,
	usize,
	usize,
	usize,
	usize,
	usize,
	usize,
	usize,
	usize,
	usize,
	usize,
	usize,
	usize,
	usize,
	usize,
	usize,
	usize,
	usize,
);
