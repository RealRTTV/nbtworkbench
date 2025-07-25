use std::ops::{ControlFlow, FromResidual, Try};

// todo: add `FailureOnlyActionResult` which has `Try` that only breaks on failure
/// This should be used for "actions" and in-place of result when there are three distinict return values.
///
/// Note: unlike [`Option`]s and [`Result`]s. These two examples have different sematics:
/// # Example 1a
/// ```rs
/// if should_do_specific_action() {
///     let action: ActionResult = do_specific_action();
///     action?;
///     ActionResult::Success(())
/// }
/// ```
/// # Example 2a
/// ```rs
/// if should_do_specific_action() {
///     let action: ActionResult = do_specific_action();
///     return action;
/// }
/// ```
/// See the expanded version:
/// # Example 1b
/// ```rs
/// if should_do_specific_action() {
///     let action: ActionResult = do_specific_action();
///     match action {
///         ActionResult::Success(()) => return ActionResult::Success(()),
///         ActionResult::Pass => {}, // returns ActionResult::Success(())
///         ActionResult::Failure(()) => return ActionResult::Failure(()),
///     }
///     ActionResult::Success(())
/// }
/// ```
/// # Example 2b
/// ```rs
/// if should_do_specific_action() {
///     let action: ActionResult = do_specific_action();
///     match action {
///         ActionResult::Success(()) => return ActionResult::Success(()),
///         ActionResult::Pass => return ActionResult::Pass,
///         ActionResult::Failure(()) => return ActionResult::Failure(()),
///     }
/// }
/// ```
/// The (most likely) correct way of handling this is:
/// # Example 3
/// ```rs
/// if should_do_specific_action() {
///     let action: ActionResult = do_specific_action();
///     action?
/// }
/// ```
/// Or more realistically:
/// ```rs
/// if should_do_specific_action() {
///     do_specific_action()?;
/// }
/// ```
/// In the case that the borrow checker requires that this branch be terminating, [`ActionResult::Failure`] is likely the branch wanted.
#[must_use]
#[derive(Copy, Clone, Eq, PartialEq)]
pub enum ActionResult<S = (), E = ()> {
	/// When the action finishes successfully.
	Success(S),
	/// When the action does nothing.
	Pass,
	/// This action was attempted, and failed - all other fallthrough actions should not be attempted and an error should be emitted.
	Failure(E),
}

impl<S, E> ActionResult<S, E> {
	#[must_use]
	pub fn passed(&self) -> bool { matches!(self, Self::Pass) }

	pub fn map_success<S2>(self, f: impl FnOnce(S) -> S2) -> ActionResult<S2, E> {
		match self {
			Self::Success(s) => ActionResult::<S2, E>::Success(f(s)),
			Self::Pass => ActionResult::<S2, E>::Pass,
			Self::Failure(e) => ActionResult::<S2, E>::Failure(e),
		}
	}

	pub fn flatten_pass(self, pass: Result<S, E>) -> Result<S, E> {
		match self {
			Self::Success(s) => Ok(s),
			Self::Pass => pass,
			Self::Failure(e) => Err(e),
		}
	}

	pub fn map_failure<E2>(self, f: impl FnOnce(E) -> E2) -> ActionResult<S, E2> {
		match self {
			Self::Success(s) => ActionResult::<S, E2>::Success(s),
			Self::Pass => ActionResult::<S, E2>::Pass,
			Self::Failure(e) => ActionResult::<S, E2>::Failure(f(e)),
		}
	}
}

/// [`ActionResult`]-esc type for ? syntax. Returns [`ActionResult::Failure`] on failure.
/// Examples:
/// ```rs
/// fn apply_action() -> AnyhowActionResult {
///     let result: Result<i32, std::io::Error> = { Ok(4) };
///     let guard: FailingActionResult<i32, std::io::Error> = result.failure_on_err();
///     let value: i32 = guard?;
///     // ... do stuff
///     ActionResult::Success(())
/// }
/// ```
#[must_use]
pub enum FailingActionResult<S, E> {
	Success(S),
	Failure(E),
}

impl<S, E, E2: From<E>> FromResidual<ActionResult<!, E>> for ActionResult<S, E2> {
	fn from_residual(residual: ActionResult<!, E>) -> Self {
		match residual {
			ActionResult::Success(never) => never,
			ActionResult::Pass => ActionResult::Pass,
			ActionResult::Failure(e) => ActionResult::Failure(From::from(e)),
		}
	}
}

impl<S, E> FromResidual<ActionResult<!, E>> for FailingActionResult<S, E> {
	fn from_residual(residual: ActionResult<!, E>) -> Self {
		match residual {
			ActionResult::Success(never) => never,
			ActionResult::Pass => unsafe { core::hint::unreachable_unchecked() },
			ActionResult::Failure(e) => FailingActionResult::Failure(e),
		}
	}
}

impl<S, E> Try for FailingActionResult<S, E> {
	type Output = S;
	type Residual = ActionResult<!, E>;

	fn from_output(output: Self::Output) -> Self { Self::Success(output) }

	fn branch(self) -> ControlFlow<Self::Residual, Self::Output> {
		match self {
			Self::Success(t) => ControlFlow::Continue(t),
			Self::Failure(e) => ControlFlow::Break(ActionResult::Failure(e)),
		}
	}
}

pub trait IntoFailingActionResult<S, E> {
	fn failure_on_err(self) -> FailingActionResult<S, E>;
}

impl<S, E> IntoFailingActionResult<S, E> for Result<S, E> {
	fn failure_on_err(self) -> FailingActionResult<S, E> {
		match self {
			Ok(t) => FailingActionResult::Success(t),
			Err(e) => FailingActionResult::Failure(e),
		}
	}
}

impl<S> IntoFailingActionResult<S, ()> for Option<S> {
	fn failure_on_err(self) -> FailingActionResult<S, ()> {
		match self {
			Some(t) => FailingActionResult::Success(t),
			None => FailingActionResult::Failure(()),
		}
	}
}

pub trait PassOrFail<S, E> {
	fn pass_or_fail(self, pass_predicate: impl FnOnce(&E) -> bool) -> ActionResult<S, E>;
}

impl<S, E> PassOrFail<S, E> for Result<S, E> {
	fn pass_or_fail(self, pass_predicate: impl FnOnce(&E) -> bool) -> ActionResult<S, E> {
		match self {
			Ok(x) => ActionResult::Success(x),
			Err(e) if pass_predicate(&e) => ActionResult::Pass,
			Err(e) => ActionResult::Failure(e),
		}
	}
}

impl<S> PassOrFail<S, ()> for Option<S> {
	fn pass_or_fail(self, pass_predicate: impl FnOnce(&()) -> bool) -> ActionResult<S, ()> {
		match self {
			Some(x) => ActionResult::Success(x),
			None if pass_predicate(&()) => ActionResult::Pass,
			None => ActionResult::Failure(()),
		}
	}
}

impl<E, E2: From<E>> FromResidual<ActionResult<(), E>> for ActionResult<(), E2> {
	fn from_residual(residual: ActionResult<(), E>) -> Self {
		match residual {
			ActionResult::Success(s) => ActionResult::Success(s),
			ActionResult::Pass => ActionResult::Pass,
			ActionResult::Failure(e) => ActionResult::Failure(From::from(e)),
		}
	}
}

impl<E> Try for ActionResult<(), E> {
	type Output = ();
	type Residual = ActionResult<(), E>;

	fn from_output(_output: ()) -> Self { Self::Pass }

	fn branch(self) -> ControlFlow<Self, ()> {
		match self {
			ActionResult::Pass => ControlFlow::Continue(()),
			action => ControlFlow::Break(action),
		}
	}
}
