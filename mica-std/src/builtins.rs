use std::rc::Rc;

use mica_hl::{StandardLibrary, TypeBuilder};

fn ref_self1<A, R>(mut f: impl FnMut(A) -> R) -> impl FnMut(&A) -> R
where
   A: Copy,
{
   move |x| f(*x)
}

fn ref_self2<A, B, R>(mut f: impl FnMut(A, B) -> R) -> impl FnMut(&A, B) -> R
where
   A: Copy,
{
   move |x, y| f(*x, y)
}

#[derive(Debug)]
struct ShiftOverflow;

impl std::fmt::Display for ShiftOverflow {
   fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      f.write_str("left or right shift overflow")
   }
}

impl std::error::Error for ShiftOverflow {}

struct Lib;

impl StandardLibrary for Lib {
   fn define_nil(&mut self, builder: TypeBuilder<()>) -> TypeBuilder<()> {
      builder
   }

   fn define_boolean(&mut self, builder: TypeBuilder<bool>) -> TypeBuilder<bool> {
      builder
   }

   fn define_number(&mut self, builder: TypeBuilder<f64>) -> TypeBuilder<f64> {
      builder
         // Constants
         .add_static("nan", || f64::NAN)
         .add_static("infinity", || f64::INFINITY)
         .add_static("epsilon", || f64::EPSILON)
         .add_static("e", || std::f64::consts::E)
         .add_static("pi", || std::f64::consts::PI)
         // Static methods
         .add_static("parse", |s: Rc<str>| -> Result<f64, _> { s.parse() })
         // Math stuff
         .add_function("floor", ref_self1(f64::floor))
         .add_function("ceil", ref_self1(f64::ceil))
         .add_function("round", ref_self1(f64::round))
         .add_function("trunc", ref_self1(f64::trunc))
         .add_function("fract", ref_self1(f64::fract))
         .add_function("abs", ref_self1(f64::abs))
         .add_function("signum", ref_self1(f64::signum))
         .add_function("div", ref_self2(f64::div_euclid))
         .add_function("rem", ref_self2(f64::rem_euclid))
         .add_function("pow", ref_self2(f64::powf))
         .add_function("sqrt", ref_self1(f64::sqrt))
         .add_function("exp", ref_self1(f64::exp))
         .add_function("exp2", ref_self1(f64::exp2))
         .add_function("ln", ref_self1(f64::ln))
         .add_function("log", ref_self2(f64::log))
         .add_function("log2", ref_self1(f64::log2))
         .add_function("log10", ref_self1(f64::log10))
         .add_function("cbrt", ref_self1(f64::cbrt))
         .add_function("hypot", ref_self2(f64::hypot))
         .add_function("sin", ref_self1(f64::sin))
         .add_function("cos", ref_self1(f64::cos))
         .add_function("tan", ref_self1(f64::tan))
         .add_function("asin", ref_self1(f64::asin))
         .add_function("acos", ref_self1(f64::acos))
         .add_function("atan", ref_self1(f64::atan))
         .add_function("atan", ref_self2(f64::atan2))
         .add_function("exp_m1", ref_self1(f64::exp_m1))
         .add_function("ln_1p", ref_self1(f64::ln_1p))
         .add_function("sinh", ref_self1(f64::sinh))
         .add_function("cosh", ref_self1(f64::cosh))
         .add_function("tanh", ref_self1(f64::tanh))
         .add_function("asinh", ref_self1(f64::asinh))
         .add_function("acosh", ref_self1(f64::acosh))
         .add_function("atanh", ref_self1(f64::atanh))
         .add_function("atanh", ref_self1(f64::atanh))
         .add_function("recip", ref_self1(f64::recip))
         .add_function("to_degrees", ref_self1(f64::to_degrees))
         .add_function("to_radians", ref_self1(f64::to_radians))
         .add_function("min", ref_self2(f64::min))
         .add_function("max", ref_self2(f64::max))
         // Float properties
         .add_function("is_nan", ref_self1(f64::is_nan))
         .add_function("is_finite", ref_self1(f64::is_finite))
         .add_function("is_infinite", ref_self1(f64::is_infinite))
         .add_function("is_subnormal", ref_self1(f64::is_subnormal))
         .add_function("is_sign_positive", ref_self1(f64::is_sign_positive))
         .add_function("is_sign_negative", ref_self1(f64::is_sign_negative))
         // Bit arithmetic
         .add_function("bnot", |x: &f64| !(*x as u32) as f64)
         .add_function("band", |x: &f64, y: f64| (*x as u32 & y as u32) as f64)
         .add_function("bor", |x: &f64, y: f64| (*x as u32 | y as u32) as f64)
         .add_function("bxor", |x: &f64, y: f64| (*x as u32 ^ y as u32) as f64)
         .add_function("shl", |x: &f64, y: f64| -> Result<_, ShiftOverflow> {
            Ok(((*x as u32).checked_shl(y as u32).ok_or(ShiftOverflow)?) as f64)
         })
         .add_function("shr", |x: &f64, y: f64| -> Result<_, ShiftOverflow> {
            Ok(((*x as u32).checked_shr(y as u32).ok_or(ShiftOverflow)?) as f64)
         })
   }

   fn define_string(&mut self, builder: TypeBuilder<Rc<str>>) -> TypeBuilder<Rc<str>> {
      builder.add_function("cat", |s: &Rc<str>, t: Rc<str>| format!("{}{}", s, t))
   }
}

pub fn lib() -> impl StandardLibrary {
   Lib
}
