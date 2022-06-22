use token_lookup_macro::token_lookup;

token_lookup!(
    ("+", ADD),
    ("-", SUB),
    ("*", MUL),
    ("/", DIV),

    ("=", EQU),
    ("+=", ADD_EQU),
    ("-=", SUB_EQU),
    ("*=", MUL_EQU),
    ("/=", DIV_EQU),

    ("->", EXEC),
    ("-*", REDUCE_EXEC),

    (String, STRING),
    (f64, NUMBER),
    (String, IDENTIFIER),
    (_, EOF),
    (_, UNDEFINED)
);

pub const token_string_start: &str = "\"[";
pub const token_string_end: &str = "]";
pub const token_string_escape: [(char, char); 5] = [
    ('n', '\n'),
    ('r', '\r'),
    ('t', '\t'),
    (']', ']'),
    ('\\', '\\'),
];