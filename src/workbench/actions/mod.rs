mod add;
mod remove;
mod replace;
mod swap;
mod rename;

pub use add::add_element;
pub use remove::{remove_element, RemoveElementResult};
pub use replace::{replace_element, ReplaceElementResult};
pub use swap::{same_depth_swap_element, SwapSameDepthElementResult};
pub use rename::{rename_element, RenameElementResult};