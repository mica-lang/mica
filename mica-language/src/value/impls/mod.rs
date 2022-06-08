//! Implementations of dynamically typed values.

#[cfg(mica_enable_nan_boxing)]
mod nanbox;
mod portable;

#[cfg(mica_enable_nan_boxing)]
pub use nanbox::ValueImpl;
#[cfg(not(mica_enable_nan_boxing))]
pub use portable::ValueImpl;
