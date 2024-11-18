use parse::prelude::*;
use std::fmt;

fn test_parse<E: HasParser + PartialEq + fmt::Debug>(expected: E, input: &str) {
    let actual: E = parse::parse_str(input).unwrap();
    assert_eq!(actual, expected);
}

#[derive(Debug, PartialEq, HasParser)]
#[repr(u32)]
enum ManyThings {
    #[parse(string = "Z")]
    Win = 6,
    Salad = 3,
    Salsa = 0,
}

#[test]
fn many_things_parse() {
    test_parse(ManyThings::Win, "Z");
    test_parse(ManyThings::Salad, "salad");
    test_parse(ManyThings::Salsa, "salsa");
}

#[derive(Debug, PartialEq, HasParser)]
enum MixedThings {
    Hello,
    #[parse(before = "(", after = ")", sep_by = ", ")]
    Tuple(u32, u32),
    #[parse(after = "u")]
    UNum(u32),
    #[parse(after = "i")]
    SNum {
        i: i32,
    },
}

#[test]
fn mixed_things_parse() {
    test_parse(MixedThings::Hello, "hello");
    test_parse(MixedThings::Tuple(99, 77), "(99, 77)");
    test_parse(MixedThings::UNum(99), "99u");
    test_parse(MixedThings::SNum { i: -100 }, "-100i");
}

#[derive(Debug, PartialEq, HasParser)]
struct CombineParse {
    c: char,
    i: u32,
    o: ManyThings,
}

#[test]
fn combine_parse() {
    test_parse(
        CombineParse {
            c: 'a',
            i: 33,
            o: ManyThings::Salsa,
        },
        "a 33 salsa",
    );
}

#[derive(Debug, PartialEq, HasParser)]
#[parse(sep_by = ", ")]
struct CustomCombineParse {
    #[parse(before = "{ c: ")]
    c: char,
    #[parse(before = "i: ")]
    i: u32,
    #[parse(before = "o: ", after = " }")]
    o: ManyThings,
}

#[test]
fn custom_combine_parse() {
    test_parse(
        CustomCombineParse {
            c: 'a',
            i: 33,
            o: ManyThings::Salsa,
        },
        "{ c: a, i: 33, o: salsa }",
    );
}

#[derive(Debug, PartialEq, HasParser)]
struct NewType(u32);

#[test]
fn new_type_test() {
    test_parse(NewType(99), "99");
}

#[derive(Debug, PartialEq, HasParser)]
#[parse(sep_by = ", ")]
struct Tuple(u32, u64);

#[test]
fn tuple_test() {
    test_parse(Tuple(99, 12), "99, 12");
}

#[derive(HasParser, PartialEq, Debug)]
struct Foo;

#[derive(HasParser, PartialEq, Debug)]
#[parse(string = "qux")]
struct Baz;

#[test]
fn unit_struct() {
    test_parse(Foo, "foo");
    test_parse(Baz, "qux");
}
