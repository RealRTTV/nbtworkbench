#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum TextColor {
	Black,
	DarkBlue,
	DarkGreen,
	DarkAqua,
	DarkRed,
	DarkPurple,
	Gold,
	Gray,
	DarkGray,
	Blue,
	Green,
	Aqua,
	Red,
	LightPurple,
	Yellow,
	White,

	TreeString,
	TreeKey,
	TreePrimitive,

	Custom(u32),
}

impl TextColor {
	pub fn to_raw(self) -> u32 {
		match self {
			Self::Black => 0x000000,
			Self::DarkBlue => 0x0000AA,
			Self::DarkGreen => 0x00AA00,
			Self::DarkAqua => 0x00AAAA,
			Self::DarkRed => 0xAA0000,
			Self::DarkPurple => 0xAA00AA,
			Self::Gold => 0xFFAA00,
			Self::Gray => 0xAAAAAA,
			Self::DarkGray => 0x555555,
			Self::Blue => 0x5555FF,
			Self::Green => 0x55FF55,
			Self::Aqua => 0x55FFFF,
			Self::Red => 0xFF5555,
			Self::LightPurple => 0xFF55FF,
			Self::Yellow => 0xFFFF55,
			Self::White => 0xFFFFFF,

			Self::TreeString => 0xDB6AC0,
			Self::TreeKey => 0x8BC3F3,
			Self::TreePrimitive => 0xF1B073,

			Self::Custom(value) => value & 0xFFFFFF,
		}
	}
}

impl Into<u32> for TextColor {
	fn into(self) -> u32 {
		self.to_raw()
	}
}
