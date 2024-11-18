extern crate proc_macro;

use heck::ToSnakeCase as _;
use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use std::collections::BTreeMap;
use std::matches;
use syn::*;

mod attrs;

fn verify_signature(sig: &Signature) -> Result<()> {
    let as_expected = matches!(sig, Signature {
        constness: None,
        asyncness: None,
        unsafety: None,
        abi: None,
        generics: Generics {
            lt_token: None,
            gt_token: None,
            where_clause: None,
            ..
        },
        inputs,
        variadic: None,
        output: ReturnType::Type(_, ret_type),
        ..
    } if inputs.is_empty() && matches!(&**ret_type, Type::Infer(_)));

    if !as_expected {
        Err(Error::new(
            sig.ident.span(),
            "function signature wrong for into_parser",
        ))
    } else {
        Ok(())
    }
}

fn into_parser_inner(input: TokenStream) -> Result<TokenStream> {
    let input: ItemFn = parse(input)?;
    verify_signature(&input.sig)?;

    let name = input.sig.ident;
    let block = input.block;
    Ok(quote! {
        fn #name<Input>() -> impl ::parse::prelude::Parser<Input, Output = Self>
        where
            Input: ::combine::Stream<Token = char>,
        #block
    }
    .into())
}

#[proc_macro_attribute]
pub fn into_parser(_attr: TokenStream, input: TokenStream) -> TokenStream {
    match into_parser_inner(input) {
        Ok(v) => v,
        Err(e) => e.into_compile_error().into(),
    }
}

fn derive_has_parser_struct(
    name: Ident,
    attrs: Vec<Attribute>,
    data: DataStruct,
) -> Result<ItemImpl> {
    let parser_expr = parse_expr_for_struct(parse_quote!(Self), name.clone(), attrs, data.fields)?;

    Ok(parse_quote! {
        impl ::parse::HasParser for #name {
            #[::parse::prelude::into_parser]
            fn parser() -> _ {
                #parser_expr
            }
        }
    })
}

fn parse_expr_for_struct(
    self_expr: Expr,
    name: Ident,
    attrs: Vec<Attribute>,
    fields: Fields,
) -> Result<Expr> {
    let fields: Vec<&Field> = fields.iter().collect();

    if fields.is_empty() {
        let parser = get_unit_parser_from_attrs(name_parser(&name), attrs)?;
        Ok(parse_quote!(::parse::prelude::Parser::map(#parser, |_| #self_expr)))
    } else {
        let mut patterns: Vec<Pat> = vec![];
        let mut field_names: Vec<Ident> = vec![];
        let mut parsers: Vec<Expr> = vec![];
        let mut fields_iter = fields.iter().peekable();
        let mut unique = (1..).map(|n| Ident::new(&format!("f{n}"), Span::call_site()));

        let attr_map = attrs::parse_attr_map::<attrs::ContainerKeyword>(attrs)?;
        let sep_parser: Expr = get_separator_parser_from_attrs(parse_quote!(char(' ')), &attr_map);

        while let Some(f) = fields_iter.next() {
            let ty = &f.ty;

            let default_parser_expr = parse_quote!(<#ty as ::parse::HasParser>::parser());
            let parser_expr = get_field_parser_from_attrs(default_parser_expr, f.attrs.clone())?;
            if fields_iter.peek().is_some() {
                parsers.push(parse_quote! {
                    ::parse::prelude::Parser::skip(#parser_expr, #sep_parser)
                });
            } else {
                parsers.push(parser_expr);
            }

            if let Some(field_name) = f.ident.clone() {
                patterns.push(parse_quote!(#field_name));
                field_names.push(field_name);
            } else {
                let ident = unique.next().unwrap();
                patterns.push(parse_quote!(#ident));
            }
        }

        let parser_expr = get_struct_parser_from_attrs(parse_quote!((#(#parsers),*)), &attr_map);

        let map_closure: Expr = if field_names.is_empty() {
            if patterns.len() == 1 {
                parse_quote!(#self_expr)
            } else {
                parse_quote!(|(#(#patterns),*)| #self_expr(#(#patterns),*))
            }
        } else {
            parse_quote!(|(#(#patterns),*)| #self_expr { #(#field_names),* })
        };

        Ok(parse_quote! {
            ::parse::prelude::Parser::map(#parser_expr, #map_closure)
        })
    }
}

fn get_unit_parser_from_attrs(default_parser: Expr, attrs: Vec<syn::Attribute>) -> Result<Expr> {
    let attr_map = attrs::parse_attr_map::<attrs::VariantKeyword>(attrs)?;

    if let Some(value) = attr_map.get(&attrs::VariantKeyword::String) {
        Ok(parse_quote!(::parse::prelude::string(#value)))
    } else {
        Ok(default_parser)
    }
}

fn get_separator_parser_from_attrs(
    default_parser: Expr,
    attr_map: &BTreeMap<attrs::ContainerKeyword, LitStr>,
) -> Expr {
    if let Some(value) = attr_map.get(&attrs::ContainerKeyword::SepBy) {
        parse_quote!(::parse::prelude::string(#value))
    } else {
        default_parser
    }
}

fn get_struct_parser_from_attrs(
    default_parser: Expr,
    attr_map: &BTreeMap<attrs::ContainerKeyword, LitStr>,
) -> Expr {
    let mut parser = default_parser;

    if let Some(value) = attr_map.get(&attrs::ContainerKeyword::Before) {
        parser = parse_quote!(::parse::prelude::string(#value).with(#parser));
    }

    if let Some(value) = attr_map.get(&attrs::ContainerKeyword::After) {
        parser = parse_quote! {
            ::parse::prelude::Parser::skip(#parser, ::parse::prelude::string(#value))
        };
    }

    parser
}

fn get_field_parser_from_attrs(default_parser: Expr, attrs: Vec<syn::Attribute>) -> Result<Expr> {
    let attr_map = attrs::parse_attr_map::<attrs::FieldKeyword>(attrs)?;

    let mut parser = default_parser;

    if let Some(value) = attr_map.get(&attrs::FieldKeyword::Before) {
        parser = parse_quote!(::parse::prelude::string(#value).with(#parser));
    }

    if let Some(value) = attr_map.get(&attrs::FieldKeyword::After) {
        parser = parse_quote! {
            ::parse::prelude::Parser::skip(#parser, ::parse::prelude::string(#value))
        };
    }

    Ok(parser)
}

fn name_parser(name: &Ident) -> Expr {
    let name = name.to_string().to_snake_case();
    parse_quote!(::parse::prelude::string(#name))
}

fn derive_has_parser_enum(name: Ident, data: DataEnum) -> Result<ItemImpl> {
    let mut parsers: Vec<Expr> = vec![];
    for v in data.variants {
        let name = v.ident;
        let parser = parse_expr_for_struct(parse_quote!(Self::#name), name, v.attrs, v.fields)?;
        parsers.push(parse_quote!(::parse::prelude::attempt(#parser)));
    }
    Ok(parse_quote! {
        impl ::parse::HasParser for #name {
            #[::parse::prelude::into_parser]
            fn parser() -> _ {
                ::parse::prelude::choice((#(#parsers),*))
            }
        }
    })
}

fn derive_has_parser_inner(input: DeriveInput) -> Result<ItemImpl> {
    match input.data {
        Data::Struct(ds) => derive_has_parser_struct(input.ident, input.attrs, ds),
        Data::Enum(de) => derive_has_parser_enum(input.ident, de),
        _ => Err(Error::new(Span::call_site(), "Unsupported type")),
    }
}

#[proc_macro_derive(HasParser, attributes(parse))]
pub fn derive_has_parser(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match derive_has_parser_inner(input) {
        Ok(v) => quote!(#v).into(),
        Err(e) => e.into_compile_error().into(),
    }
}
