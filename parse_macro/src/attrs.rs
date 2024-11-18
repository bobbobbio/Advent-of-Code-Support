use proc_macro2::Span;
use std::collections::BTreeMap;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::*;

pub trait AttrKeywordKind: TryFrom<Ident, Error = Error> + PartialOrd + Ord {}

struct ParseAttrs<Kind> {
    _parens: token::Paren,
    attrs: Punctuated<ParseAttr<Kind>, Token![,]>,
}

impl<Kind: AttrKeywordKind> Parse for ParseAttrs<Kind> {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        Ok(Self {
            _parens: parenthesized!(content in input),
            attrs: content.parse_terminated(ParseAttr::parse)?,
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, PartialOrd, Ord)]
pub enum FieldKeyword {
    Before,
    After,
}

impl AttrKeywordKind for FieldKeyword {}

impl TryFrom<Ident> for FieldKeyword {
    type Error = Error;

    fn try_from(id: Ident) -> Result<Self> {
        Ok(match &id.to_string()[..] {
            "before" => Self::Before,
            "after" => Self::After,
            _ => return Err(Error::new(id.span(), "unknown keyword")),
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, PartialOrd, Ord)]
pub enum VariantKeyword {
    String,
}

impl AttrKeywordKind for VariantKeyword {}

impl TryFrom<Ident> for VariantKeyword {
    type Error = Error;

    fn try_from(id: Ident) -> Result<Self> {
        Ok(match &id.to_string()[..] {
            "string" => Self::String,
            _ => return Err(Error::new(id.span(), "unknown keyword")),
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, PartialOrd, Ord)]
pub enum ContainerKeyword {
    SepBy,
    Before,
    After,
}

impl AttrKeywordKind for ContainerKeyword {}

impl TryFrom<Ident> for ContainerKeyword {
    type Error = Error;

    fn try_from(id: Ident) -> Result<Self> {
        Ok(match &id.to_string()[..] {
            "sep_by" => Self::SepBy,
            "before" => Self::Before,
            "after" => Self::After,
            _ => return Err(Error::new(id.span(), "unknown keyword")),
        })
    }
}

#[derive(Debug, Clone)]
struct AttrKeyword<Kind> {
    kind: Kind,
    span: Span,
}

impl<Kind> Spanned for AttrKeyword<Kind> {
    fn span(&self) -> Span {
        self.span
    }
}

impl<Kind: AttrKeywordKind> Parse for AttrKeyword<Kind> {
    fn parse(input: ParseStream) -> Result<Self> {
        let id: Ident = input.parse()?;
        Ok(Self {
            span: id.span(),
            kind: id.try_into()?,
        })
    }
}

struct ParseAttr<Kind> {
    kw: AttrKeyword<Kind>,
    _equal_token: Token![=],
    value: LitStr,
}

impl<Kind: AttrKeywordKind> Parse for ParseAttr<Kind> {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Self {
            kw: input.parse()?,
            _equal_token: input.parse()?,
            value: input.parse()?,
        })
    }
}

pub fn parse_attr_map<Kind: AttrKeywordKind>(
    attrs: Vec<syn::Attribute>,
) -> Result<BTreeMap<Kind, LitStr>> {
    let parsed_attrs: Vec<ParseAttrs<Kind>> = attrs
        .into_iter()
        .filter(|a| a.path.get_ident() == Some(&Ident::new("parse", Span::call_site())))
        .map(|a| parse2(a.tokens))
        .collect::<Result<_>>()?;
    let attrs: Vec<_> = parsed_attrs
        .into_iter()
        .flat_map(|a| a.attrs.into_iter())
        .collect();

    let mut attr_map = BTreeMap::new();
    for attr in attrs {
        if attr_map.contains_key(&attr.kw.kind) {
            return Err(Error::new(attr.kw.span(), "Duplicate attribute"));
        }
        attr_map.insert(attr.kw.kind, attr.value);
    }
    Ok(attr_map)
}
