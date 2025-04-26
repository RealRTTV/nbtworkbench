mod array;
mod chunk;
mod compound;
mod element;
mod list;
mod primitive;
mod string;
mod null;

pub use array::*;
pub use chunk::*;
pub use compound::*;
pub use element::*;
pub use list::*;
pub use null::*;
pub use primitive::*;
pub use string::*;
pub type NbtElementAndKey = (Option<compact_str::CompactString>, NbtElement);