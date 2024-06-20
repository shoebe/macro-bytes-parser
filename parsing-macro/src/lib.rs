use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{
    braced, bracketed, parse::Parse, parse_macro_input, spanned::Spanned, token::Comma, Attribute,
    Expr, Field, Generics, Ident, Token, Visibility, WhereClause,
};

enum ParsingDirective {
    Magic { typ: syn::Type, val: syn::Expr },
    Ignore { typ: syn::Type },
    Padding { num_bytes: syn::Expr },
    Param { typ: syn::Type, name: syn::Ident },
    LimitBuffer { size: syn::Expr },
}

/*

pub struct Header {
    [[magic: u32 = 0x0401]]
    pub field1: u16,
    [[padding_bytes = 4]]
    pub field2: u8,
    [[padding_bytes = 40]]
    [[param: u8 = string_len]]
}
*/

struct DoubleBracketedInput {
    bracket1: syn::token::Bracket,
    bracket2: syn::token::Bracket,
    ident: syn::Ident,
}

impl DoubleBracketedInput {
    fn parse_nested(
        input: syn::parse::ParseStream,
        f: impl FnOnce(Self, syn::parse::ParseStream),
    ) -> syn::Result<()> {
        let bracketed_input;
        let bracket1 = bracketed!(bracketed_input in input);
        let input = bracketed_input;
        let bracketed_input;
        let bracket2 = bracketed!(bracketed_input in input);
        let input = bracketed_input;

        let ident = input.parse::<Ident>()?;

        let s = Self {
            bracket1,
            bracket2,
            ident,
        };
        f(s, &input);
        Ok(())
    }
}

impl Parse for ParsingDirective {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut out = None;
        DoubleBracketedInput::parse_nested(input, |br, input| {
            out = Some(Self::parse_from_double_brackets(&br, input));
        })?;
        out.unwrap()
    }
}

impl ParsingDirective {
    fn parse_from_double_brackets(
        br: &DoubleBracketedInput,
        input: syn::parse::ParseStream,
    ) -> syn::Result<Self> {
        match br.ident.to_string().as_str() {
            "magic" => {
                input.parse::<Token![:]>()?;
                let typ: syn::Type = input.parse()?;
                input.parse::<Token![=]>()?;
                let val: syn::Expr = input.parse()?;
                Ok(Self::Magic { typ, val })
            }
            "padding_bytes" => {
                input.parse::<Token![=]>()?;
                let num_bytes: syn::Expr = input.parse()?;
                Ok(Self::Padding { num_bytes })
            }
            "ignore" => {
                input.parse::<Token![:]>()?;
                let typ: syn::Type = input.parse()?;
                Ok(Self::Ignore { typ })
            }
            "param" => {
                input.parse::<Token![:]>()?;
                let typ: syn::Type = input.parse()?;
                input.parse::<Token![=]>()?;
                let name: syn::Ident = input.parse()?;
                Ok(Self::Param { typ, name })
            }
            "limit_buffer" => {
                input.parse::<Token![=]>()?;
                let size: syn::Expr = input.parse()?;
                Ok(Self::LimitBuffer { size })
            }
            _ => Err(syn::Error::new(br.ident.span(), "invalid")),
        }
    }
    fn as_tokens(&self, endianess: &proc_macro2::TokenStream) -> proc_macro2::TokenStream {
        match self {
            ParsingDirective::Magic { typ, val } => {
                quote! {
                    {
                        let magic = input.read_type::<#endianess, #typ>()?;
                        if magic != #val {
                            return Err(::parsing::Error::MagicCheckFailed);
                        }
                    }
                }
            }

            ParsingDirective::Padding { num_bytes } => {
                quote! {
                    input.read_bytes(#num_bytes)?;
                }
            }
            ParsingDirective::Param { typ, name } => {
                quote! {
                    let #name = input.read_type::<#endianess, #typ>()?;
                }
            }
            ParsingDirective::Ignore { typ } => {
                quote! {
                    input.read_type::<#endianess, #typ>()?;
                }
            }
            ParsingDirective::LimitBuffer { size } => {
                quote! {
                    use ::parsing::ReadBytes;
                    let mut input = input.read_bytes(#size as usize)?;
                }
            }
        }
    }
}

struct FieldStruct {
    name: Ident,
    read_type: syn::Type,
    option: Option<syn::Expr>,
    e: FieldEnum,
}

enum FieldEnum {
    Normal,
    SizedUtf8String(syn::Expr),
    SizedBuf(syn::Expr),
    RestOfBuf,
    Collection {
        field_ty: syn::Type,
        num_elems: syn::Expr,
    },
}

impl FieldStruct {
    fn as_tokens(&self, endianess: &proc_macro2::TokenStream) -> proc_macro2::TokenStream {
        let read = match &self.e {
            FieldEnum::Normal => {
                let ty = &self.read_type;
                quote! {input.read_type::<#endianess, #ty>()?}
            }
            FieldEnum::SizedUtf8String(size) => {
                quote! {
                    {
                        let bytes = input.read_bytes(#size as usize)?;
                        std::str::from_utf8(bytes)
                            .map_err(|err| ::parsing::Error::InterpretStrFailed(err))?
                            .into()
                    }
                }
            }
            FieldEnum::SizedBuf(size) => {
                quote! {input.read_bytes(#size as usize)?.into()}
            }
            FieldEnum::RestOfBuf => {
                quote! {input.read_rest().into()}
            }
            FieldEnum::Collection {
                field_ty,
                num_elems,
            } => {
                let item_ty = &self.read_type;
                quote! {
                    {
                        let tmp: ::parsing::Result<#field_ty> = (0..#num_elems)
                            .map(|_| input.read_type::<#endianess, #item_ty>())
                            .collect();
                        tmp?
                    }
                }
            }
        };
        let name = &self.name;
        if let Some(e) = &self.option {
            quote! {
                let #name = if #e {
                    Some(#read)
                } else {
                    None
                };
            }
        } else {
            quote! {
                let #name = #read;
            }
        }
    }
}

enum FieldOrDirective {
    Field(FieldStruct),
    Directive(ParsingDirective),
}

impl FieldOrDirective {
    fn as_tokens(&self, endianess: &proc_macro2::TokenStream) -> proc_macro2::TokenStream {
        match self {
            FieldOrDirective::Directive(thing) => thing.as_tokens(endianess),
            FieldOrDirective::Field(field) => field.as_tokens(endianess),
        }
    }
}

struct ParsedStruct {
    s: syn::ItemStruct,
    things: Vec<FieldOrDirective>,
}

impl Parse for ParsedStruct {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let struct_attrs = input.call(Attribute::parse_outer)?;
        let vis = input.parse::<Visibility>()?;
        let struct_token = input.parse::<Token![struct]>()?;
        let name = input.parse::<Ident>()?;
        let mut generics = input.parse::<Generics>()?;
        if input.lookahead1().peek(Token![where]) {
            generics.where_clause = Some(input.parse::<syn::WhereClause>()?)
        };

        let braced_input;
        let brace_token = braced!(braced_input in input);
        let input = braced_input;

        let mut fields = syn::punctuated::Punctuated::<Field, Comma>::new();
        let mut parsing_things = Vec::new();

        while !input.is_empty() {
            let mut inner = None;
            let r = DoubleBracketedInput::parse_nested(&input, |br, input| {
                let mut o = || -> syn::Result<()> {
                    let thing = ParsingDirective::parse_from_double_brackets(&br, input)?;
                    parsing_things.push(FieldOrDirective::Directive(thing));

                    Ok(())
                };
                inner = Some(o());
            });
            if r.is_ok() {
                inner.unwrap()?; // if the double brackets got parsed but it errored within, bubble up
            } else {
                let mut field = input.call(syn::Field::parse_named)?;
                let mut field_thing = FieldStruct {
                    name: field.ident.clone().unwrap(),
                    option: None,
                    read_type: field.ty.clone(),
                    e: FieldEnum::Normal,
                };
                let mut err = Ok(());
                field.attrs.retain(|attr| {
                    /*
                    #[parse(sized_utf8_string = param_name)]
                     */
                    if attr.path().is_ident("parse") {
                        let res = attr.parse_nested_meta(|meta| {
                            if meta.path.is_ident("sized_utf8_string") {
                                meta.input.parse::<Token![=]>()?;
                                let size = meta.input.parse::<Expr>()?;
                                field_thing.e = FieldEnum::SizedUtf8String(size);
                            } else if meta.path.is_ident("sized_buf") {
                                meta.input.parse::<Token![=]>()?;
                                let size = meta.input.parse::<Expr>()?;
                                field_thing.e = FieldEnum::SizedBuf(size);
                            } else if meta.path.is_ident("collection") {
                                meta.input.parse::<Token![:]>()?;
                                let ty = meta.input.parse::<syn::Type>()?;
                                meta.input.parse::<Token![=]>()?;
                                let num_elems = meta.input.parse::<Expr>()?;
                                field_thing.read_type = ty;
                                field_thing.e = FieldEnum::Collection {
                                    field_ty: field.ty.clone(),
                                    num_elems,
                                };
                            } else if meta.path.is_ident("option_if") {
                                meta.input.parse::<Token![:]>()?;
                                let ty = meta.input.parse::<syn::Type>()?;
                                meta.input.parse::<Token![=]>()?;
                                let if_expr = meta.input.parse::<Expr>()?;
                                field_thing = FieldOrDirective::OptionIf {
                                    name: field.ident.clone().unwrap(),
                                    ty,
                                    if_expr,
                                }
                            } else if meta.path.is_ident("rest_of_buf") {
                                field_thing = FieldOrDirective::RestOfBuf {
                                    name: field.ident.clone().unwrap(),
                                }
                            }
                            Ok(())
                        });
                        if res.is_err() {
                            err = res;
                        }
                        false
                    } else {
                        true
                    }
                });
                err?;
                parsing_things.push(field_thing);
                fields.push_value(field);
                if let Ok(comma) = input.parse() {
                    fields.push_punct(comma);
                }
            }
        }

        let fields = syn::FieldsNamed {
            brace_token,
            named: fields,
        };

        let s = syn::ItemStruct {
            attrs: struct_attrs,
            vis,
            struct_token,
            ident: name,
            generics,
            fields: syn::Fields::Named(fields),
            semi_token: None,
        };

        Ok(Self {
            s,
            things: parsing_things,
        })
    }
}

fn add_extra_lifetime_and_bound_generics(
    generics: &syn::Generics,
    lifetime: &syn::Lifetime,
    f: impl FnOnce(syn::ImplGenerics, syn::TypeGenerics, Option<&syn::WhereClause>),
) {
    let mut generics_mod = generics.clone();

    generics_mod.params.insert(
        0,
        syn::GenericParam::Lifetime(syn::LifetimeParam::new(lifetime.clone())),
    );

    let mut bounds = syn::punctuated::Punctuated::new();
    for l in generics.lifetimes() {
        bounds.push(l.lifetime.clone());
    }

    if !bounds.is_empty() {
        generics_mod
            .make_where_clause()
            .predicates
            .push(syn::WherePredicate::Lifetime(syn::PredicateLifetime {
                lifetime: lifetime.clone(),
                colon_token: syn::token::Colon::default(),
                bounds,
            }));
    }

    let (impl_generics, _ty_generics, where_clause) = generics_mod.split_for_impl();
    let (_impl_generics, ty_generics, _where_clause) = generics.split_for_impl();
    f(impl_generics, ty_generics, where_clause);
}

fn generate_parse_impl(
    things: impl Iterator<Item = FieldOrDirective>,
    struct_def: &syn::ItemStruct,
) -> proc_macro2::TokenStream {
    let le = quote! {::parsing::LE};
    let be = quote! {::parsing::BE};
    let (parsing_le, parsing_be): (Vec<_>, Vec<_>) =
        things.map(|t| (t.as_tokens(&le), t.as_tokens(&be))).unzip();

    let struct_name = &struct_def.ident;
    let field_names_1 = struct_def.fields.iter().map(|f| f.ident.as_ref().unwrap());
    let field_names_2 = struct_def.fields.iter().map(|f| f.ident.as_ref().unwrap());

    let parse_lifetime = syn::Lifetime::new("'parse", Span::call_site());

    let mut out = None;

    add_extra_lifetime_and_bound_generics(
        &struct_def.generics,
        &parse_lifetime,
        |impl_generics, ty_generics, where_clause| {
            let tmp = quote! {
                impl #impl_generics ::parsing::Parse<#parse_lifetime, #le> for #struct_name #ty_generics #where_clause {
                    fn parse(input: &mut impl ::parsing::ReadBytes<#parse_lifetime>) -> ::parsing::Result<Self> {
                        #(
                            #parsing_le
                        )*
                        Ok(Self {
                            #(
                                #field_names_1
                            ),*
                        })
                    }
                }

                impl #impl_generics ::parsing::Parse<#parse_lifetime, #be> for #struct_name #ty_generics #where_clause {
                    fn parse(input: &mut impl ::parsing::ReadBytes<#parse_lifetime>) -> ::parsing::Result<Self> {
                        #(
                            #parsing_be
                        )*
                        Ok(Self {
                            #(
                                #field_names_2
                            ),*
                        })
                    }
                }
            };
            out = Some(tmp);
        },
    );
    out.unwrap()
}

/// Derives the Parse trait for a struct
/// allows adding padding and magic numbers
/// ```ignore
/// parsable_struct! {
///     pub struct Header {
///         [[magic: u32 = 0x0401]]
///         pub field1: u16,
///         [[padding_bytes = 4]]
///         pub field2: u8,
///         [[param: u32 = size_of_something]]
///         [[padding_bytes = 40]]
///     }
/// }
/// ```
/// `padding_bytes` will be taken out from the buffer during the parse,
/// but are not part of the struct definition.
/// `magic` is treated the same way as `padding_bytes`, but its value is verified
/// and the parse will fail if it does not match the expected value
/// `[[param: <int type> = <name>]]` will parse an int from the buffer,
/// making it available to future fields, but will not add it to the struct definition
/// mostly intended to be used to hide sizes of buffers and stringsfrom the struct definition
///
/// Parsing sized buffers and strings
///  ```ignore
/// parsable_struct! {
///     pub struct Header<'a> {
///         pub field1: u16,
///         [[param: u8 = buf_size]]
///         pub some_other_field: u32,
///         #[parse(sized_buf = buf_size)]
///         s: &'a [u8],
///     }
/// }
/// ```
/// `.into()` is called on the byte slice, so anything that implements `From<&[u8]>`
/// can be used with `#[parse(sized_buf = <size_param>)]`
///
/// ```ignore
/// parsable_struct! {
///     pub struct Header<'a> {
///         pub field1: u16,
///         [[param: u8 = string_size]]
///         pub some_other_field: u32,
///         #[parse(sized_utf8_string = string_size)]
///         s: &'a str,
///     }
/// }
/// ```
/// `.into()` is called on the string slice, so anything that implements `From<&str>`
/// can be used with `#[parse(sized_utf8_string = <size_param>)]`
///
/// `[[param: <int type> = <name>]]` does not necessarily need to be used
/// as the size parameter
/// Any field in the struct can be used
/// ```ignore
/// parsable_struct! {
///     pub struct Header<'a> {
///         pub field1: u16,
///         pub string_size: u8,
///         pub some_other_field: u32,
///         #[parse(sized_utf8_string = string_size)]
///         s: &'a str,
///     }
/// }
/// ```
///
/// Collections
/// ```
/// parsing::parsable_struct! {
///     #[derive(Debug)]
///     pub struct Item<'a> {
///         [[param: u8 = buf_size]]
///         #[parse(sized_buf = buf_size)]
///         buf: &'a [u8],
///     }
/// }
/// parsing::parsable_struct! {
///     #[derive(Debug)]
///     pub struct Header<'a> {
///         [[param: u8 = num_items]]
///         something: u32,
///         #[parse(collection: Item = num_items)]
///         items: Vec<Item<'a>>,
///     }
/// }
/// ```
/// Parse a number of variably-sized elements into a collection
/// Does not have to be a Vec, .collect() is called on an iterator.
#[proc_macro]
pub fn parsable_struct(input: TokenStream) -> TokenStream {
    let parsed = parse_macro_input!(input as ParsedStruct);
    let parse_impl = generate_parse_impl(parsed.things.into_iter(), &parsed.s);
    let struct_def = &parsed.s;
    let expanded = quote! {
        #struct_def
        #parse_impl
    };
    TokenStream::from(expanded)
}

/// A simpler version of parsable_struct! that can be derived
#[proc_macro_derive(Parse)]
pub fn parse_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as syn::ItemStruct);
    let things = input.fields.iter().map(|field| FieldOrDirective::Field {
        name: field.ident.clone().unwrap(),
        ty: field.ty.clone(),
    });
    TokenStream::from(generate_parse_impl(things, &input))
}

struct ParsedEnum {
    directives: Vec<ParsingDirective>,
    type_field_name: syn::Expr,
    e: syn::ItemEnum,
}

/*
#[derive(Clone, PartialEq, Eq, Debug)]
#[repr(u16)]
pub enum Chunk<'a> {
    OldPalette1 { packets: Vec<OldPalettePacket> } = 0x0004,
    OldPalette2 { packets: Vec<OldPalettePacket> } = 0x0011,
    Layer(LayerChunk<'a>) = 0x2004,
    //Cel(CelChunk) = 0x2005,
}
*/

impl Parse for ParsedEnum {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let attrs = input.call(Attribute::parse_outer)?;
        let vis = input.parse::<Visibility>()?;
        let enum_token = input.parse::<Token![enum]>()?;
        let ident = input.parse::<Ident>()?;
        let mut generics = input.parse::<Generics>()?;
        generics.where_clause = input.parse()?;

        let content;
        let brace_token = braced!(content in input);
        let input = content;

        let mut type_field_name = None;
        let mut directives = Vec::new();

        loop {
            let mut inner = None;
            let Ok(()) = DoubleBracketedInput::parse_nested(&input, |br, input| {
                let mut o = || -> syn::Result<()> {
                    if br.ident == "enum_type" {
                        input.parse::<Token![=]>()?;
                        let expr = input.parse::<syn::Expr>()?;
                        type_field_name = Some(expr);
                    } else {
                        directives.push(ParsingDirective::parse_from_double_brackets(&br, input)?);
                    }
                    Ok(())
                };
                inner = Some(o());
            }) else {
                // not a double bracket, must have reached enum variants
                break;
            };
            inner.expect("unwrap inner in loop")?; // if the double brackets got parsed but it errored within, bubble up
        }

        let Some(type_field_name) = type_field_name else {
            return Err(input.error("missing enum type directive `[[enum_size: <int-type>]]`"));
        };

        let variants = input.parse_terminated(syn::Variant::parse, Token![,])?;

        for var in variants.iter() {
            if var.discriminant.is_none() {
                return Err(syn::Error::new(
                    var.ident.span(),
                    "variant missing a discriminant ( ex: = 0x021 )",
                ));
            }
            match &var.fields {
                syn::Fields::Unnamed(fields) => {
                    if fields.unnamed.len() != 1 {
                        return Err(syn::Error::new(
                            fields.span(),
                            "Only 1 unnamed field is allowed",
                        ));
                    }
                }
                syn::Fields::Named(_) | syn::Fields::Unit => {
                    return Err(syn::Error::new(
                        var.fields.span(),
                        "Only 1 unnamed field is allowed",
                    ));
                }
            }
        }

        let e = syn::ItemEnum {
            attrs,
            vis,
            enum_token,
            ident,
            generics,
            brace_token,
            variants,
        };

        Ok(Self {
            directives,
            type_field_name,
            e,
        })
    }
}

#[proc_macro]
pub fn parsable_enum(input: TokenStream) -> TokenStream {
    let parsed = parse_macro_input!(input as ParsedEnum);

    let le = quote! {::parsing::LE};
    let be = quote! {::parsing::BE};

    let parsing_le = parsed.directives.iter().map(|dir| dir.as_tokens(&le));
    let parsing_be = parsed.directives.iter().map(|dir| dir.as_tokens(&be));

    let enum_def = &parsed.e;
    let enum_name = &enum_def.ident;
    let type_name = &parsed.type_field_name;

    let enum_values: Vec<_> = enum_def
        .variants
        .iter()
        .map(|var| &var.discriminant.as_ref().unwrap().1)
        .collect();

    let enum_idents: Vec<_> = enum_def.variants.iter().map(|var| &var.ident).collect();

    let parse_lifetime = syn::Lifetime::new("'parse", Span::call_site());
    let mut out = None;

    add_extra_lifetime_and_bound_generics(
        &enum_def.generics,
        &parse_lifetime,
        |impl_generics, ty_generics, where_clause| {
            let tmp = quote! {
                impl #impl_generics ::parsing::Parse<#parse_lifetime, #le> for #enum_name #ty_generics #where_clause {
                    fn parse(input: &mut impl ::parsing::ReadBytes<#parse_lifetime>) -> ::parsing::Result<Self> {
                        #(
                            #parsing_le
                        )*

                        Ok(match #type_name {
                            #(
                                #enum_values => #enum_name :: #enum_idents(input.read_type::<#le,_>()?),
                            )*
                            _ => {
                                return Err(::parsing::Error::EnumTypeValueMatchFailed);
                            }
                        })
                    }
                }

                impl #impl_generics ::parsing::Parse<#parse_lifetime, #be> for #enum_name #ty_generics #where_clause {
                    fn parse(input: &mut impl ::parsing::ReadBytes<#parse_lifetime>) -> ::parsing::Result<Self> {
                        #(
                            #parsing_be
                        )*

                        Ok(match #type_name {
                            #(
                                #enum_values => #enum_name :: #enum_idents(input.read_type::<#be,_>()?),
                            )*
                            _ => {
                                return Err(::parsing::Error::EnumTypeValueMatchFailed);
                            }
                        })
                    }
                }
            };
            out = Some(tmp);
        },
    );
    let parse_impl = out.expect("unwrap on token stream");
    let expanded = quote! {
        #enum_def
        #parse_impl
    };
    TokenStream::from(expanded)
}
