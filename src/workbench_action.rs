use crate::{NbtElement, sum_indices};

pub enum WorkbenchAction {
    // Toggle {
    //     indices: Box<[usize]>,
    // },
    Remove {
        element: Result<(Box<str>, NbtElement), NbtElement>,
        indices: Box<[usize]>,
    },
    Add {
        indices: Box<[usize]>,
    },
    Rename {
        indices: Box<[usize]>,
        previous: Box<str>,
        key: bool
    },
    Move {
        from: Box<[usize]>,
        to: Box<[usize]>,
        original_key: Option<Box<str>>,
    }
}

impl WorkbenchAction {
    pub fn undo(self, root: &mut NbtElement) -> WorkbenchAction {
        unsafe { self.undo0(root).unwrap_unchecked() }
    }

    #[inline(always)]
    fn undo0(self, root: &mut NbtElement) -> Option<WorkbenchAction> {
        Some(match self {
         // WorkbenchAction::Toggle { indices } => {
         //     let mut total = sum_indices(indices.iter().copied(), &*root)?;
         //     let mut unused = Vec::with_capacity(indices.len());
         //     // SAFETY: will be valid since our input is valid
         //     let _ = unsafe { root.toggle_fn(&mut total, indices.len(), 0, &mut unused).unwrap_unchecked() };
         //     drop(unused);
         //     WorkbenchAction::Toggle { indices }
         // },
            WorkbenchAction::Remove { element, indices } => {
                let mut total = sum_indices(indices.iter().copied().take(indices.len() - 1), &*root)?;
                let (key, value) = match element {
                    Ok((key, value)) => (Some(key), value),
                    Err(value) => (None, value),
                };

                let mut indices2 = Vec::with_capacity(indices.len());
                // SAFETY: valid inputs
                unsafe { root.drop_simple(&mut total, key, value, indices.last().copied().unwrap_unchecked(), &mut indices2) };

                WorkbenchAction::Add { indices: indices2.into_boxed_slice() }
            }
            WorkbenchAction::Add { indices } => {
                let mut total = sum_indices(indices.iter().copied(), &*root)?;

                let mut unused = Vec::with_capacity(indices.len());
                // SAFETY: valid inputs
                let (key, value, _) = unsafe { root.delete(&mut total, &mut unused) };
                drop(unused);

                WorkbenchAction::Remove { element: match key { Some(key) => Ok((key, value)), None => Err(value) }, indices }
            }
            WorkbenchAction::Rename { indices, previous, key } => {
                if key {
                    let mut element = root;
                    for &idx in indices.iter().take(indices.len() - 1) {
                        element = match element {
                            NbtElement::List(list) => list.get_mut(idx),
                            NbtElement::Compound(compound) => compound.get_mut(idx).map(|(_, x)| x),
                            _ => unsafe { core::hint::unreachable_unchecked() }
                        }.expect("Valid index in indices chain");
                    }
                    let previous = if let NbtElement::Compound(compound) = element {
                        // SAFETY: valid input
                        unsafe { compound.update_key(*indices.last().unwrap(), previous).unwrap_unchecked() }
                    } else {
                        // SAFETY: valid input
                        unsafe { core::hint::unreachable_unchecked() }
                    };
                    WorkbenchAction::Rename { indices, previous, key }
                } else {
                    let mut element = root;
                    let mut prev = None;
                    'a: for &idx in indices.iter() {
                        // SAFETY: valid input
                        element = unsafe {
                            match element {
                                NbtElement::List(list) => list.get_mut(idx).unwrap_unchecked(),
                                NbtElement::Compound(compound) => compound.get_mut(idx).unwrap_unchecked().1,
                                NbtElement::ByteArray(array) => {
                                    let parse = previous.parse().unwrap_unchecked();
                                    let element = array.get_mut(idx).unwrap_unchecked();
                                    prev = Some(element.to_string().into_boxed_str());
                                    *element = parse;
                                    break 'a
                                }
                                NbtElement::IntArray(array) => {
                                    let parse = previous.parse().unwrap_unchecked();
                                    let element = array.get_mut(idx).unwrap_unchecked();
                                    prev = Some(element.to_string().into_boxed_str());
                                    *element = parse;
                                    break 'a
                                }
                                NbtElement::LongArray(array) => {
                                    let parse = previous.parse().unwrap_unchecked();
                                    let element = array.get_mut(idx).unwrap_unchecked();
                                    prev = Some(element.to_string().into_boxed_str());
                                    *element = parse;
                                    break 'a
                                }
                                _ => core::hint::unreachable_unchecked()
                            }
                        };
                    }
                    WorkbenchAction::Rename { indices, previous: match prev {
	                    Some(x) => x,
	                    None => unsafe { element.set_value(previous).unwrap_unchecked() },
                    }, key }
                }
            }
            WorkbenchAction::Move { from, to, original_key } => {
                let to_total = sum_indices(to.iter().copied(), &*root)?;
                let mut from_total = sum_indices(from.iter().copied().take(from.len() - 1), &*root)?;
                let mut unused = Vec::with_capacity(from.len());
                // SAFETY: valid inputs
	            let (key, value, (decrement, _)) = unsafe { root.delete(&mut to_total.clone(), &mut unused) };
                let mut unused = Vec::with_capacity(from.len());

                if from_total > to_total {
                    from_total -= decrement;
                }

                dbg!((from_total, unsafe { from.last().copied().unwrap_unchecked() }, to_total));

                // SAFETY: valid inputs
                unsafe { root.drop_simple(&mut from_total.clone(), original_key, value, from.last().copied().unwrap_unchecked(), &mut unused) };

                WorkbenchAction::Move {
                    from: to,
                    to: from,
                    original_key: key,
                }
            }
        })
    }
}
