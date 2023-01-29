use std::ops::Deref;

use crate::{
    corelib::iterators::string::{
        bytes::StringBytes, chars::StringChars, code_points::StringCodePoints, lines::StringLines,
        rsplit::StringRSplit, split::StringSplit,
    },
    ll::gc::Gc,
    Arguments, IntoValue, MethodParameterCount, MicaLanguageResultExt, RawFunctionKind,
    TypeBuilder, Value,
};

pub(crate) fn define(builder: TypeBuilder<String>) -> TypeBuilder<String> {
    builder
        .add_static("debug", |x: Value| format!("{x:?}"))
        .add_function("cat", |s: &String, t: Gc<String>| format!("{s}{t}"))
        .add_function("contains", |s: &String, sub: Gc<String>| s.contains(sub.deref().deref()))
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
        .add_function("find", |s: &String, substr: Gc<String>| s.find(substr.deref().deref()))
        .add_function("rfind", |s: &String, substr: Gc<String>| s.rfind(substr.deref().deref()))
        .add_function("byte_at", |s: &String, position: usize| s.as_bytes().get(position).copied())
        .add_function("byte_len", |s: &String| s.len())
        .add_function("nth_char", |s: &String, position: usize| s.chars().nth(position))
        .add_function("nth_code_point", |s: &String, position: usize| {
            s.chars().nth(position).map(u32::from)
        })
        .add_function("char_len", |s: &String| s.chars().count())
        .add_function("is_empty", |s: &String| s.is_empty())
        .add_function("to_lowercase", |s: &String| s.to_lowercase())
        .add_function("to_uppercase", |s: &String| s.to_uppercase())
        .add_function("repeat", |s: &String, n: usize| s.repeat(n))
        .add_function("replace", |s: &String, pat: Gc<String>, with: Gc<String>| {
            s.replace(pat.deref().deref(), &with)
        })
        .add_function("replace", |s: &String, pat: Gc<String>, with: Gc<String>, n: usize| {
            s.replacen(pat.deref().deref(), &with, n)
        })
        .add_function("trim", |s: &String| s.trim().to_owned())
        // TODO: It should be possible to implement these without raw functions in the future.
        .add_raw_function(
            "bytes",
            MethodParameterCount::from_count_with_self(1),
            RawFunctionKind::Foreign(Box::new(|env, gc, args| {
                let arguments = Arguments::new(args, env);
                let iter = unsafe { StringBytes::new(*arguments.raw_self()) };
                Ok(iter.into_value_with_environment(env).to_raw(gc))
            })),
        )
        .add_raw_function(
            "chars",
            MethodParameterCount::from_count_with_self(1),
            RawFunctionKind::Foreign(Box::new(|env, gc, args| {
                let arguments = Arguments::new(args, env);
                let iter = unsafe { StringChars::new(*arguments.raw_self()) };
                Ok(iter.into_value_with_environment(env).to_raw(gc))
            })),
        )
        .add_raw_function(
            "code_points",
            MethodParameterCount::from_count_with_self(1),
            RawFunctionKind::Foreign(Box::new(|env, gc, args| {
                let arguments = Arguments::new(args, env);
                let iter = unsafe { StringCodePoints::new(*arguments.raw_self()) };
                Ok(iter.into_value_with_environment(env).to_raw(gc))
            })),
        )
        .add_raw_function(
            "lines",
            MethodParameterCount::from_count_with_self(1),
            RawFunctionKind::Foreign(Box::new(|env, gc, args| {
                let arguments = Arguments::new(args, env);
                let iter = unsafe { StringLines::new(*arguments.raw_self()) };
                Ok(iter.into_value_with_environment(env).to_raw(gc))
            })),
        )
        .add_raw_function(
            "split",
            MethodParameterCount::from_count_with_self(2),
            RawFunctionKind::Foreign(Box::new(|env, gc, args| {
                let arguments = Arguments::new(args, env);
                let sep: Gc<String> = arguments.get(0).to_language_error()?;
                let iter = unsafe { StringSplit::new(*arguments.raw_self(), sep) };
                Ok(iter.into_value_with_environment(env).to_raw(gc))
            })),
        )
        .add_raw_function(
            "rsplit",
            MethodParameterCount::from_count_with_self(2),
            RawFunctionKind::Foreign(Box::new(|env, gc, args| {
                let arguments = Arguments::new(args, env);
                let sep: Gc<String> = arguments.get(0).to_language_error()?;
                let iter = unsafe { StringRSplit::new(*arguments.raw_self(), sep) };
                Ok(iter.into_value_with_environment(env).to_raw(gc))
            })),
        )
}
