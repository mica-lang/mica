use std::ops::Deref;
use std::rc::Rc;

use mica_hl::TypeBuilder;

pub(super) fn define(builder: TypeBuilder<Rc<str>>) -> TypeBuilder<Rc<str>> {
   builder
      .add_function("cat", |s: &Rc<str>, t: Rc<str>| format!("{}{}", s, t))
      .add_function("contains", |s: &Rc<str>, sub: Rc<str>| {
         s.contains(sub.deref())
      })
      .add_function("starts_with", |s: &Rc<str>, prefix: Rc<str>| {
         s.starts_with(prefix.deref())
      })
      .add_function("ends_with", |s: &Rc<str>, suffix: Rc<str>| {
         s.ends_with(suffix.deref())
      })
      .add_function("strip_prefix", |s: &Rc<str>, prefix: Rc<str>| {
         s.strip_prefix(prefix.deref()).map(|x| x.to_owned())
      })
      .add_function("strip_suffix", |s: &Rc<str>, suffix: Rc<str>| {
         s.strip_suffix(suffix.deref()).map(|x| x.to_owned())
      })
      .add_function("find", |s: &Rc<str>, substr: Rc<str>| {
         s.find(substr.deref())
      })
      .add_function("rfind", |s: &Rc<str>, substr: Rc<str>| {
         s.rfind(substr.deref())
      })
      .add_function("byte_at", |s: &Rc<str>, position: usize| {
         s.as_bytes().get(position).copied()
      })
      .add_function("byte_len", |s: &Rc<str>| s.len())
      .add_function("nth_char", |s: &Rc<str>, position: usize| {
         s.chars().nth(position)
      })
      .add_function("char_len", |s: &Rc<str>| s.chars().count())
      .add_function("is_empty", |s: &Rc<str>| s.is_empty())
      .add_function("to_lowercase", |s: &Rc<str>| s.to_lowercase())
      .add_function("to_uppercase", |s: &Rc<str>| s.to_uppercase())
      .add_function("repeat", |s: &Rc<str>, n: usize| s.repeat(n))
      .add_function("replace", |s: &Rc<str>, pat: Rc<str>, with: Rc<str>| {
         s.replace(pat.deref(), &with)
      })
      .add_function(
         "replace",
         |s: &Rc<str>, pat: Rc<str>, with: Rc<str>, n: usize| s.replacen(pat.deref(), &with, n),
      )
      .add_function("trim", |s: &Rc<str>| s.trim().to_owned())
}
