use std::ops::Deref;
use std::rc::Rc;

use mica_hl::TypeBuilder;

pub(super) fn define(builder: TypeBuilder<Rc<String>>) -> TypeBuilder<Rc<String>> {
   builder
      .add_function("cat", |s: &Rc<String>, t: Rc<String>| format!("{}{}", s, t))
      .add_function("contains", |s: &Rc<String>, sub: Rc<String>| {
         s.contains(sub.deref().deref())
      })
      .add_function("starts_with", |s: &Rc<String>, prefix: Rc<String>| {
         s.starts_with(prefix.deref().deref())
      })
      .add_function("ends_with", |s: &Rc<String>, suffix: Rc<String>| {
         s.ends_with(suffix.deref().deref())
      })
      .add_function("strip_prefix", |s: &Rc<String>, prefix: Rc<String>| {
         s.strip_prefix(prefix.deref().deref()).map(|x| x.to_owned())
      })
      .add_function("strip_suffix", |s: &Rc<String>, suffix: Rc<String>| {
         s.strip_suffix(suffix.deref().deref()).map(|x| x.to_owned())
      })
      .add_function("find", |s: &Rc<String>, substr: Rc<String>| {
         s.find(substr.deref().deref())
      })
      .add_function("rfind", |s: &Rc<String>, substr: Rc<String>| {
         s.rfind(substr.deref().deref())
      })
      .add_function("byte_at", |s: &Rc<String>, position: usize| {
         s.as_bytes().get(position).copied()
      })
      .add_function("byte_len", |s: &Rc<String>| s.len())
      .add_function("nth_char", |s: &Rc<String>, position: usize| {
         s.chars().nth(position)
      })
      .add_function("char_len", |s: &Rc<String>| s.chars().count())
      .add_function("is_empty", |s: &Rc<String>| s.is_empty())
      .add_function("to_lowercase", |s: &Rc<String>| s.to_lowercase())
      .add_function("to_uppercase", |s: &Rc<String>| s.to_uppercase())
      .add_function("repeat", |s: &Rc<String>, n: usize| s.repeat(n))
      .add_function(
         "replace",
         |s: &Rc<String>, pat: Rc<String>, with: Rc<String>| s.replace(pat.deref().deref(), &with),
      )
      .add_function(
         "replace",
         |s: &Rc<String>, pat: Rc<String>, with: Rc<String>, n: usize| {
            s.replacen(pat.deref().deref(), &with, n)
         },
      )
      .add_function("trim", |s: &Rc<String>| s.trim().to_owned())
}
