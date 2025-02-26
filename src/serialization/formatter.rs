use std::fmt::Write;

pub struct PrettyFormatter {
    buf: String,
    current_depth: usize,
}

impl PrettyFormatter {
    const INDENT: u8 = b' ';
    const INDENT_QUANTITY: usize = 4;

    pub const fn new() -> Self {
        Self {
            buf: String::new(),
            current_depth: 0,
        }
    }

    pub fn write_str(&mut self, s: &str) {
        let _ = self.buf.write_str(s);
    }

    pub fn increase(&mut self) {
        self.current_depth += 1;
    }

    pub fn indent(&mut self) {
        unsafe {
            let vec = self.buf.as_mut_vec();
            let len = vec.len();
            vec.reserve(self.current_depth * Self::INDENT_QUANTITY);
            vec.spare_capacity_mut().as_mut_ptr().write_bytes(Self::INDENT, self.current_depth * Self::INDENT_QUANTITY);
            vec.set_len(len + self.current_depth * Self::INDENT_QUANTITY);
        }
    }

    pub fn decrease(&mut self) {
        self.current_depth = self.current_depth.saturating_sub(1);
    }

    pub fn finish(self) -> String { self.buf }
}