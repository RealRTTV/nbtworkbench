use crate::serialization::UncheckedBufWriter;

pub struct PrettyFormatter {
	buf: UncheckedBufWriter,
	current_depth: usize,
}

impl PrettyFormatter {
	const INDENT: u8 = b' ';
	const INDENT_QUANTITY: usize = 4;

	pub fn new() -> Self { Self { buf: UncheckedBufWriter::new(), current_depth: 0 } }

	pub fn write_str(&mut self, s: &str) { let _ = self.buf.write(s.as_bytes()); }

	pub fn increase(&mut self) { self.current_depth += 1; }

	pub fn indent(&mut self) {
		self.buf.write_bytes(Self::INDENT, self.current_depth * Self::INDENT_QUANTITY);
	}

	pub fn decrease(&mut self) { self.current_depth = self.current_depth.saturating_sub(1); }

	pub fn finish(self) -> String {
		// SAFETY: all bytes written were passed as &str
		unsafe { String::from_utf8_unchecked(self.buf.finish()) }
	}
}

pub trait PrettyDisplay {
	fn pretty_fmt(&self, f: &mut PrettyFormatter);
}
