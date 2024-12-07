#![warn(missing_docs)]

//! This crate contains code that helps with parsing [Advent of Code](https://adventofcode.com)
//! problems using Rust.

use combine::eof;
use combine::parser::char::{alpha_num, spaces};
use combine::stream::{easy, position};
use prelude::*;
use std::convert::Infallible;
use std::marker::PhantomData;
use std::{
    fmt, io, iter, num,
    ops::{Deref, DerefMut},
    slice, str, vec,
};

extern crate self as parse;

/// This module can be imported like
/// ```rust
/// use parse::prelude::*;
/// ```
/// to import a bunch of useful things.
pub mod prelude {
    pub use super::*;
    pub use combine::parser::char::*;
    pub use combine::*;
    pub use combine::{Parser, Stream};
    pub use parse_macro::into_parser;
    pub use parse_macro::HasParser;
    pub use std::str::FromStr;
}

/// Implementing this trait means this type has the ability to construct a parser which yields
/// itself on success.
pub trait HasParser: Sized {
    /// Returns a parser which parses `Self`
    fn parser<Input>() -> impl Parser<Input, Output = Self>
    where
        Input: combine::Stream<Token = char>;
}

impl HasParser for char {
    #[into_parser]
    fn parser() -> _ {
        alpha_num()
    }
}

impl<A, B> HasParser for (A, B)
where
    A: HasParser,
    B: HasParser,
{
    #[into_parser]
    fn parser() -> _ {
        (A::parser(), B::parser())
    }
}

/// Represents the various things that could go wrong when parsing.
#[derive(Debug)]
pub enum Error {
    /// There was an error parsing an integer.
    ParseInt(num::ParseIntError),
    /// There was some kind of IO error.
    Io(io::Error),
    /// There was some error combing from a general parser.
    ParseError(String),
}

impl From<Infallible> for Error {
    fn from(_: Infallible) -> Self {
        unreachable!()
    }
}

impl From<io::Error> for Error {
    fn from(e: io::Error) -> Self {
        Self::Io(e)
    }
}

impl From<num::ParseIntError> for Error {
    fn from(e: num::ParseIntError) -> Self {
        Self::ParseInt(e)
    }
}

impl From<easy::Errors<char, &str, position::SourcePosition>> for Error {
    fn from(e: easy::Errors<char, &str, position::SourcePosition>) -> Self {
        Self::ParseError(e.to_string())
    }
}

/// `std::result::Result` but the error is [`Error`]
pub type Result<T> = std::result::Result<T, Error>;

macro_rules! unsigned_number_parser {
    ($($id:ty),*) => {
        $(impl HasParser for $id {
            #[into_parser]
            fn parser() -> _ {
                many1(digit()).map(|s: String| s.parse::<Self>().unwrap())
            }
        })*
    }
}

unsigned_number_parser!(u8, u16, u32, u64, u128, usize);

macro_rules! signed_number_parser {
    ($($id:ty),*) => {
        $(impl HasParser for $id {
            #[into_parser]
            fn parser() -> _ {
                choice((
                    token('-').with(many1(digit()))
                        .map(|s: String| format!("-{s}").parse::<Self>().unwrap()),
                    u32::parser().map(|v| v.try_into().unwrap())
                ))
            }
        })*
    }
}

signed_number_parser!(i8, i16, i32, i64, i128, isize);

impl HasParser for String {
    #[into_parser]
    fn parser() -> _ {
        many1(any())
    }
}

macro_rules! matching_parser {
    ($name:ident, $c:expr) => {
        /// Parses as `$c`
        #[derive(Debug, Clone, Copy)]
        pub struct $name;

        impl HasParser for $name {
            #[into_parser]
            fn parser() -> _ {
                $c.map(|_| Self)
            }
        }
    };
}

macro_rules! token {
    ($name:ident, $c:expr) => {
        matching_parser!($name, token($c));
    };
}

macro_rules! string {
    ($name:ident, $c:expr) => {
        matching_parser!($name, string($c));
    };
}

macro_rules! many1_token {
    ($name:ident, $c:expr) => {
        matching_parser!($name, many1::<String, _, _>(token($c)));
    };
}

token!(Comma, ',');
string!(CommaSpace, ", ");
string!(SemiSpace, "; ");
token!(Dash, '-');
token!(NewLine, '\n');
token!(Space, ' ');
string!(SpaceBar, " |");
many1_token!(Spaces, ' ');

/// Parse control type which causes collections to parse the given type between elements
#[derive(Debug, Clone, Copy)]
pub struct SepBy<T>(PhantomData<T>);

/// Parse control type which causes collections to parse the given type after elements
#[derive(Debug, Clone, Copy)]
pub struct TermWith<T>(PhantomData<T>);

/// Parse control type which causes collections to parse the given type before elements
#[derive(Debug, Clone, Copy)]
pub struct StartsWith<T>(PhantomData<T>);

/// A vector which implements [`HasParser`] in a way controllable via the second generic parameter.
#[derive(Clone)]
pub struct List<T, Sep>(Vec<T>, PhantomData<Sep>);

impl<T, Sep> From<List<T, Sep>> for Vec<T> {
    fn from(l: List<T, Sep>) -> Self {
        l.0
    }
}

impl<T, Sep> fmt::Debug for List<T, Sep>
where
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<T, Sep> PartialEq<Self> for List<T, Sep>
where
    T: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0)
    }
}

impl<T, Sep> std::hash::Hash for List<T, Sep>
where
    T: std::hash::Hash,
{
    fn hash<H>(&self, state: &mut H)
    where
        H: std::hash::Hasher,
    {
        self.0.hash(state)
    }
}

impl<T, Sep> Eq for List<T, Sep> where T: Eq {}

/// Parses as nothing
#[derive(Clone, Debug)]
pub struct Nil;

/// Parses as anything which isn't `char::is_whitespace`
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NotWhitespace(pub String);

impl HasParser for NotWhitespace {
    #[into_parser]
    fn parser() -> _ {
        many1(satisfy(|c: char| !c.is_whitespace())).map(Self)
    }
}

impl<T, Sep> From<Vec<T>> for List<T, Sep> {
    fn from(v: Vec<T>) -> Self {
        Self(v, PhantomData)
    }
}

impl<T: HasParser> HasParser for List<T, Nil> {
    #[into_parser]
    fn parser() -> _ {
        many1(T::parser()).map(|v: Vec<_>| v.into())
    }
}

impl<T: HasParser> HasParser for Vec<T> {
    #[into_parser]
    fn parser() -> _ {
        many1(T::parser()).map(|v: Vec<_>| v)
    }
}

impl<T: HasParser, Sep: HasParser> HasParser for List<T, SepBy<Sep>> {
    #[into_parser]
    fn parser() -> _ {
        sep_by1(T::parser(), Sep::parser()).map(|v: Vec<_>| v.into())
    }
}

impl<T: HasParser, Sep: HasParser> HasParser for List<T, TermWith<Sep>> {
    #[into_parser]
    fn parser() -> _ {
        many1(T::parser().skip(Sep::parser())).map(|v: Vec<_>| v.into())
    }
}

impl<T: HasParser, Sep: HasParser> HasParser for List<T, StartsWith<Sep>> {
    #[into_parser]
    fn parser() -> _ {
        many1(attempt(Sep::parser().with(T::parser()))).map(|v: Vec<_>| v.into())
    }
}

impl<T, Sep> Default for List<T, Sep> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T, Sep> List<T, Sep> {
    /// Construct an empty list
    pub fn new() -> Self {
        Self(vec![], PhantomData)
    }

    /// Add an element to the end of the list
    pub fn push(&mut self, t: T) {
        self.0.push(t);
    }

    /// Return an iterator over the elements of the list.
    pub fn iter(&self) -> slice::Iter<'_, T> {
        self.0.iter()
    }

    /// Return a mutable iterator over the elements of the list.
    pub fn iter_mut(&mut self) -> slice::IterMut<'_, T> {
        self.0.iter_mut()
    }

    /// Shortens the list keeping the first `size` elements. Does nothing if the given size is
    /// longer than the current length of the list.
    pub fn truncate(&mut self, size: usize) {
        self.0.truncate(size);
    }

    /// Increase the underlying capacity of the list.
    pub fn reserve(&mut self, additional: usize) {
        self.0.reserve(additional)
    }

    /// Remove the last element of the list.
    pub fn pop(&mut self) -> Option<T> {
        self.0.pop()
    }

    /// Remove the given element of the list. Panics if the given index is out of bounds.
    pub fn remove(&mut self, index: usize) -> T {
        self.0.remove(index)
    }

    /// Keep only the elements specified by the given predicate.
    pub fn retain<F>(&mut self, f: F)
    where
        F: FnMut(&T) -> bool,
    {
        self.0.retain(f)
    }

    /// Keep only the elements specified by the given predicate.
    pub fn retain_mut<F>(&mut self, f: F)
    where
        F: FnMut(&mut T) -> bool,
    {
        self.0.retain_mut(f)
    }

    /// Remove and return the tail of the vector starting at given index. Panics if the given index
    /// is invalid.
    pub fn split_off(&mut self, at: usize) -> Self {
        List(self.0.split_off(at), PhantomData)
    }

    /// Re-size the list to the new given length. If the new length is longer, the given value is
    /// used to fill the void.
    pub fn resize(&mut self, new_len: usize, value: T)
    where
        T: Clone,
    {
        self.0.resize(new_len, value)
    }

    /// Delete all the elements of the list.
    pub fn clear(&mut self) {
        self.0.clear()
    }

    /// Insert the given element at the given index. Panics if the given index is invalid.
    pub fn insert(&mut self, index: usize, element: T) {
        self.0.insert(index, element)
    }
}

impl<A, Sep> std::iter::Extend<A> for List<A, Sep> {
    fn extend<T>(&mut self, iter: T)
    where
        T: IntoIterator<Item = A>,
    {
        self.0.extend(iter)
    }
}

impl<'a, T, Sep> IntoIterator for &'a List<T, Sep> {
    type Item = &'a T;
    type IntoIter = slice::Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl<'a, T, Sep> IntoIterator for &'a mut List<T, Sep> {
    type Item = &'a mut T;
    type IntoIter = slice::IterMut<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter_mut()
    }
}

impl<T, Sep> IntoIterator for List<T, Sep> {
    type Item = T;
    type IntoIter = vec::IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<T, Sep> iter::FromIterator<T> for List<T, Sep> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        Self(iter::FromIterator::from_iter(iter), PhantomData)
    }
}

impl<T, Sep> AsRef<[T]> for List<T, Sep> {
    fn as_ref(&self) -> &[T] {
        self.0.as_ref()
    }
}

impl<T, Sep> AsMut<[T]> for List<T, Sep> {
    fn as_mut(&mut self) -> &mut [T] {
        self.0.as_mut()
    }
}

impl<T, Sep> Deref for List<T, Sep> {
    type Target = [T];

    fn deref(&self) -> &[T] {
        self.0.deref()
    }
}

impl<T, Sep> DerefMut for List<T, Sep> {
    fn deref_mut(&mut self) -> &mut [T] {
        self.0.deref_mut()
    }
}

/// Parses a type using [`HasParser::parse`]
pub fn parse_str<T: HasParser>(
    input: &str,
) -> std::result::Result<T, easy::Errors<char, &str, position::SourcePosition>> {
    let (t, _): (T, _) = T::parser()
        .skip(spaces())
        .skip(eof())
        .easy_parse(position::Stream::new(input))?;
    Ok(t)
}
