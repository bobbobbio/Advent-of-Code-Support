extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::parse::Parse;
use syn::parse::ParseStream;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned as _;
use syn::token::*;
use syn::*;

fn part_inner(input: TokenStream, part_number: usize) -> Result<File> {
    let func: ItemFn = parse(input)?;

    let func_name = func.sig.ident.clone();
    let args = func.sig.inputs.clone();
    let ret = if let ReturnType::Type(_, ty) = func.sig.output.clone() {
        *ty
    } else {
        parse_quote!(())
    };

    if args.len() != 1 {
        return Err(Error::new(args.span(), "too many inputs"));
    }
    let parsed_type = if let FnArg::Typed(pat_type) = args.first().unwrap() {
        (*pat_type.ty).clone()
    } else {
        return Err(Error::new(args.span(), "invalid input type"));
    };

    let tramp = Ident::new(&format!("_run_part_{}", part_number), Span::call_site());
    let test_tramp = Ident::new(&format!("_test_part_{}", part_number), Span::call_site());

    Ok(parse_quote! {
        #func
        fn #tramp(input: &str, json: bool) -> ::advent::parse::Result<()> {
            let p: #parsed_type = ::advent::parse::parse_str(input)?;
            let result = #func_name(p);
            if json {
                println!("{{\"part\": {}, \"answer\": \"{}\"}}", #part_number, result);
            } else {
                println!("Part {}: {}", #part_number, result);
            }
            Ok(())
        }

        #[cfg(test)]
        fn #test_tramp(input: &str) -> ::advent::parse::Result<#ret> {
            let p: #parsed_type = ::advent::parse::parse_str(input)?;
            Ok(#func_name(p))
        }
    })
}

#[proc_macro_attribute]
pub fn part_one(_attr: TokenStream, input: TokenStream) -> TokenStream {
    match part_inner(input, 1) {
        Ok(v) => quote!(#v).into(),
        Err(e) => e.into_compile_error().into(),
    }
}

#[proc_macro_attribute]
pub fn part_two(_attr: TokenStream, input: TokenStream) -> TokenStream {
    match part_inner(input, 2) {
        Ok(v) => quote!(#v).into(),
        Err(e) => e.into_compile_error().into(),
    }
}

fn test_for_input(expected: FieldValue) -> Result<ItemFn> {
    let expected_expr = expected.expr;
    if let Member::Named(test_name) = expected.member {
        let sut = Ident::new(&format!("_test_{test_name}"), Span::call_site());
        Ok(parse_quote! {
            #[test]
            fn #test_name() {
                use ::std::io::Read as _;
                let root_dir = ::std::env!("CARGO_MANIFEST_DIR");
                let input_path = ::std::path::Path::new(root_dir).join("input.txt");
                let mut input = ::std::fs::File::open(input_path).unwrap();
                let mut input_str = ::std::string::String::new();
                input.read_to_string(&mut input_str).unwrap();
                assert_eq!(#sut(&input_str).unwrap(), #expected_expr);
            }
        })
    } else {
        Err(Error::new(expected.member.span(), "invalid argument"))
    }
}

fn main_func() -> ItemFn {
    parse_quote! {
        fn main() -> ::advent::parse::Result<()> {
            use ::std::io::Read as _;
            let mut input = ::std::string::String::new();
            ::std::io::stdin().lock().read_to_string(&mut input)?;
            let json = ::std::env::args().any(|a| a == "--json");

            _run_part_1(&input, json)?;
            _run_part_2(&input, json)?;

            Ok(())
        }
    }
}

struct HarnessInput {
    input: Punctuated<FieldValue, Comma>,
}

impl Parse for HarnessInput {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Self {
            input: Punctuated::parse_terminated(input)?,
        })
    }
}

fn harness_inner(input: HarnessInput) -> Result<Vec<ItemFn>> {
    let mut funcs: Vec<_> = input
        .input
        .into_iter()
        .map(test_for_input)
        .collect::<Result<_>>()?;
    funcs.push(main_func());
    Ok(funcs)
}

#[proc_macro]
pub fn harness(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as HarnessInput);
    match harness_inner(input) {
        Ok(ts) => quote!(#(#ts)*),
        Err(e) => e.into_compile_error(),
    }
    .into()
}
