use crate::{Compound, NbtElement};

pub enum WorkbenchAction {
    Toggle {
        y: u32,
    },
    Remove {
        y: u32, // y of parent
        element: NbtElement,
        key: Option<String>, // name
        index: u32
    },
    Add {
        y: u32,
        index: u32
    },
    Rename {
        y: u32,
        before: String,
        key: bool
    }
}

impl WorkbenchAction {
    pub fn undo(self, root: &mut NbtElement) {
        match self {
            WorkbenchAction::Toggle { mut y } => {
                let mut change = 0;
                let change_mut = &mut change as *mut i32;
                root.stack(&mut y, &mut 0, 0, &mut |parent, _| {
                    if change < 0 {
                        parent.decrement(-change as u32);
                    } else {
                        parent.increment(change as u32);
                    }
                }, |tail, _, _| unsafe {
                    let before = tail.height();
                    tail.toggle();
                    *change_mut = tail.height() as i32 - before as i32;
                });
            }
            WorkbenchAction::Remove { y, element, key, index } => {
                let height = element.height();
                root.stack(&mut y.clone(), &mut 0, 0, &mut |parent, _| {
                    parent.increment(height);
                }, |tail, _, _| {
                    if let Compound(compound) = tail {
                        let key = key.expect("was in a compound, must have an index and stuff");
                        compound.drop_index(index, key, element);
                    } else {
                        tail.drop_index(index, element);
                    }
                });
            }
            WorkbenchAction::Add { mut y, index } => {
                let mut height = 0;
                let height_mut = &mut height as *mut u32;
                root.stack(&mut y, &mut 0, 0, &mut |parent, _| {
                    parent.decrement(height);
                }, |tail, _, _| unsafe {
                    *height_mut = tail.child_height(index);
                    tail.decrement(height);
                    tail.delete(index);
                });
            }
            WorkbenchAction::Rename { mut y, before, key } => {
                if key {
                    let mut index = 0;
                    let index_mut = &mut index as *mut u32;
                    let mut first_parent = true;
                    root.stack(&mut y, &mut 0, 0, &mut |parent, _| if first_parent {
                        first_parent = false;
                        if let Compound(compound) = parent {
                            compound.update_key(index, before.clone());
                        }
                    }, |_, _, index| unsafe {
                        *index_mut = index;
                    });
                } else {
                    root.stack(&mut y, &mut 0, 0, &mut |_, _| {}, |tail, _, _| {
                        tail.set_value(&before);
                    });
                }
            }
        }
    }
    
    #[inline]
    pub fn mutation(&self) -> bool {
        match self {
            WorkbenchAction::Toggle { .. } => false,
            WorkbenchAction::Remove { .. } => true,
            WorkbenchAction::Add { .. } => true,
            WorkbenchAction::Rename { .. } => true
        }
    }
}
