use core::panic;
use proc_macro2::{Literal, Ident, Span};
use litrs::{StringLit};
use quote::{quote, ToTokens, TokenStreamExt};
use std::{collections::HashMap, mem::swap};
extern crate proc_macro;

enum TokenData {
    STRING(String),
    IDENTIFIER(Ident),
    NONE,
}

impl ToTokens for TokenData {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            TokenData::STRING(string) => {
                tokens.append(Literal::string(string));
            },
            TokenData::IDENTIFIER(ident) => {
                
                tokens.append_all(ident.into_token_stream());
            },
            TokenData::NONE => {
            }
        }
    }
}

#[derive(Debug)]
struct TokenIdent {
    ident: String,
}

impl ToTokens for TokenIdent {
    fn to_tokens(&self, tokens: &mut quote::__private::TokenStream) {
        tokens.append(Ident::new(&self.ident, Span::call_site()));
    }
}

#[proc_macro]
pub fn token_lookup(input: proc_macro::TokenStream) ->  proc_macro::TokenStream {
    let mut token_data = vec![];
    let mut token_idents = vec![];
    let mut max_string_size = 0;
    for token in input {
        match token {
            proc_macro::TokenTree::Group(group) => {
                let mut token = group.stream().into_iter();

                let data = token.next().expect("Expected token string");
                if let Ok(token_string) = StringLit::try_from(data.clone()) {
                    let token_string = token_string.value();
                    max_string_size = if max_string_size < token_string.len() {token_string.len()} else {max_string_size};
                    token_data.push(TokenData::STRING(String::from(token_string)));
                } else if let proc_macro::TokenTree::Ident(ident) = data {
                    if ident.to_string() == "_" {
                        token_data.push(TokenData::NONE);
                    } else {
                        token_data.push(TokenData::IDENTIFIER(Ident::new(ident.to_string().as_str(), Span::call_site())));
                    }
                } else {
                    panic!("Could't parse Token data");
                }
                
                match token.next().expect("Expected Delimiter") {
                    proc_macro::TokenTree::Punct(punc) => {
                        if punc.to_string() != "," {
                            panic!("Expected delimiter ','");
                        }
                    },
                    _ => {
                        panic!("Expected delimiter ','");
                    }
                }
                
                let token_ident = match token.next().expect("Expected Delimiter") {
                    proc_macro::TokenTree::Ident(ident) => {
                        ident.to_string()
                    },
                    _ => {
                        panic!("Expected token identifier");
                    }
                };
                
                token_idents.push(TokenIdent{ident: token_ident});
            },
            proc_macro::TokenTree::Punct(_) => {

            }
            _ => {
                panic!("Expected token data or delimiter");
            }
        }
    }
    
    let mut stream = proc_macro::TokenStream::new();
    stream.extend::<proc_macro::TokenStream>(gen_enum(&token_data, &token_idents));
    stream.extend::<proc_macro::TokenStream>(gen_table(&token_data, &token_idents, max_string_size));



    eprintln!("Tokens: {}", stream);
    stream
}


fn gen_enum(token_data: &Vec<TokenData>, token_idents: &Vec<TokenIdent>) -> proc_macro::TokenStream {
    let tokens = token_idents.iter().zip(token_data).map(|(ident, data)| 
        match data {
            TokenData::STRING(_) => {
                quote!(#ident)
            },
            TokenData::IDENTIFIER(_) => {
                quote!(#ident (#data))
            },
            TokenData::NONE => {
                quote!(#ident)
            }
        }
    );
    quote!(
        #[derive(Debug, PartialEq, Clone)]
        pub enum Token {
            #(#tokens),*
        }
    ).into()
}

fn gen_table(token_data: &Vec<TokenData>, token_idents: &Vec<TokenIdent>, max_string_size: usize) -> proc_macro::TokenStream {
    let mut stream = quote!(
        #[derive(Clone)]
        pub enum Lookup_Data {
            INDEX(usize, usize),
            TOKEN(Token),
            INDEX_TOKEN(usize, usize, Token)
        }
        
        pub struct Token_Lookup_Column<'a> {
            pub column: &'a [(char, Lookup_Data)],
        }
    );

    let mut table_map = Vec::<HashMap<char, HashMap<char, Option<&TokenIdent>>>>::new();
    table_map.resize(max_string_size, HashMap::new());

    for (i, token_data) in token_data.iter().enumerate() {
        if let TokenData::STRING(token_string) = token_data {
            let mut prev_char = '\0';
            for (j, char) in token_string.chars().enumerate() {
                if !table_map[j].contains_key(&prev_char) {
                    table_map[j].insert(prev_char, HashMap::new());
                }
                
                let group = table_map[j].get_mut(&prev_char).unwrap();
                let token = if j == token_string.len() - 1 { Some(&token_idents[i]) } else { None};
                if !group.contains_key(&char) {
                    group.insert(char, None);
                }
                
                if let Some(_) = token {
                    match group.get_mut(&char).unwrap() {
                        Some(_) => {
                            panic!("Duplicate token found");
                        },
                        None => {
                            *(group.get_mut(&char).unwrap()) = token;
                        }
                    }
                }
                prev_char = char;
            }
        }
    }

    eprintln!("table map {:?}", table_map);
    
    let mut prev_column_meta: HashMap<char, (usize, usize)> = HashMap::new();    
    let mut prev_column_meta_swap: HashMap<char, (usize, usize)> = HashMap::new();
    let mut table_entries = vec![];  
    for i in (0..table_map.len()).rev() {
        let mut column_entries = proc_macro2::TokenStream::new();
        let mut index = 0;
        let mut column_count = 0;
        for prev_key in table_map[i].keys() {
            for group in table_map[i].get(prev_key) {
                let mut group_count = 0;
                for (char, ident) in group {
                    if let Some((index, count)) = prev_column_meta.get(char) {
                        match ident {
                            Some(ident) => {
                                column_entries.append_all(quote!((#char, Lookup_Data::INDEX_TOKEN(#index, #count, Token::#ident)),));
                            },
                            None => {
                                column_entries.append_all(quote!((#char, Lookup_Data::INDEX(#index, #count)),));
                            }
                        }
                    } else {
                        match ident {
                            Some(ident) => {
                                column_entries.append_all(quote!((#char, Lookup_Data::TOKEN(Token::#ident)),));
                            },
                            None => {
                            }
                        }
                    }
                    group_count += 1;
                }
                prev_column_meta_swap.insert(*prev_key, (index, group.len()));
                index += group_count;
                column_count += group_count;
            }
        }
        let column_ident = Ident::new(format!("TOKEN_LOOKUP_COLUMN_{}", i).as_str(), Span::call_site());
        stream.append_all(quote!(
            pub const #column_ident: [(char, Lookup_Data); #column_count] = [
                #column_entries
            ];
        ));
        table_entries.push(quote!(Token_Lookup_Column{column: &#column_ident},));
        swap(&mut prev_column_meta_swap, &mut prev_column_meta);
    }

    let table_entries = table_entries.iter().rev();
    stream.append_all(quote!(
        pub const TOKEN_LOOKUP_TABLE: [Token_Lookup_Column; #max_string_size] = [
            #(#table_entries)*
        ];
    ));

    stream.into()
}
