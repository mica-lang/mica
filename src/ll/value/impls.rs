//! Implementations of dynamically typed values.

#[cfg(target_arch = "x86_64")]
mod nanbox;
#[cfg(not(target_arch = "x86_64"))]
mod portable;

#[cfg(target_arch = "x86_64")]
pub use nanbox::ValueImpl;
#[cfg(not(target_arch = "x86_64"))]
pub use portable::ValueImpl;
