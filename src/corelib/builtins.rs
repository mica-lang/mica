pub(crate) mod dict;
pub(crate) mod list;
pub(crate) mod number;
pub(crate) mod record;
pub(crate) mod string;
pub(crate) mod tuple;

/// Converts a function that takes a `self` parameter to one that takes a `&self` parameter.
fn ref_self1<A, R>(f: impl Fn(A) -> R) -> impl Fn(&A) -> R
where
    A: Copy,
{
    move |x| f(*x)
}

/// Converts a function that takes a `self` and one arbitrary parameter to one that takes a `&self`
/// and one arbitrary parameter.
fn ref_self2<A, B, R>(f: impl Fn(A, B) -> R) -> impl Fn(&A, B) -> R
where
    A: Copy,
{
    move |x, y| f(*x, y)
}
