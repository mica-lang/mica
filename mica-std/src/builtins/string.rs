use std::ops::Deref;

use mica_hl::language::gc::Gc;
use mica_hl::TypeBuilder;

pub(super) fn define(builder: TypeBuilder<String>) -> TypeBuilder<String> {
   builder
      .add_function("cat", |s: &String, t: Gc<String>| format!("{}{}", s, t))
      .add_function("contains", |s: &String, sub: Gc<String>| {
         s.contains(sub.deref().deref())
      })
      .add_function("starts_with", |s: &String, prefix: Gc<String>| {
         s.starts_with(prefix.deref().deref())
      })
      .add_function("ends_with", |s: &String, suffix: Gc<String>| {
         s.ends_with(suffix.deref().deref())
      })
      .add_function("strip_prefix", |s: &String, prefix: Gc<String>| {
         s.strip_prefix(prefix.deref().deref()).map(|x| x.to_owned())
      })
      .add_function("strip_suffix", |s: &String, suffix: Gc<String>| {
         s.strip_suffix(suffix.deref().deref()).map(|x| x.to_owned())
      })
      .add_function("find", |s: &String, substr: Gc<String>| {
         s.find(substr.deref().deref())
      })
      .add_function("rfind", |s: &String, substr: Gc<String>| {
         s.rfind(substr.deref().deref())
      })
      .add_function("byte_at", |s: &String, position: usize| {
         s.as_bytes().get(position).copied()
      })
      .add_function("byte_len", |s: &String| s.len())
      .add_function("nth_char", |s: &String, position: usize| {
         s.chars().nth(position)
      })
      .add_function("char_len", |s: &String| s.chars().count())
      .add_function("is_empty", |s: &String| s.is_empty())
      .add_function("to_lowercase", |s: &String| s.to_lowercase())
      .add_function("to_uppercase", |s: &String| s.to_uppercase())
      .add_function("repeat", |s: &String, n: usize| s.repeat(n))
      .add_function(
         "replace",
         |s: &String, pat: Gc<String>, with: Gc<String>| s.replace(pat.deref().deref(), &with),
      )
      .add_function(
         "replace",
         |s: &String, pat: Gc<String>, with: Gc<String>, n: usize| {
            s.replacen(pat.deref().deref(), &with, n)
         },
      )
      .add_function("trim", |s: &String| s.trim().to_owned())
}
