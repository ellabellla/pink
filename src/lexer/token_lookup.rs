use token_lookup_macro::token_lookup;

token_lookup!(
    ("true", True),
    ("false", False),

    ("not", Not),
    ("and", And),
    ("or", Or),
    ("xor", Xor),
    
    ("@@", Peek),
    ("@", Pop),
    ("#", Matrix),
    ("|", Call)
    ("!", Return),

    ("?", If),
    ("<-", Into),
    ("->", Exec),
    ("*>", Reduce),
    ("<*", ForEach)

    (":", Set),

    ("+", Add),
    ("-", Subtract),
    ("*", Multiply),
    ("/", Divide),
    ("<", LessThan),
    (">", GreaterThan),
    ("=", Equals),
    ("<=", LessThanOrEqual),
    (">=", GreaterThanOrEqual),
    
    ("+:", AddAndSet),
    ("-:", SubtractAndSet),
    ("*:", MultiplyAndSet),
    ("/:", DivideAndSet),


    (",", SeparateAndPush),
    (";", Separate),
    
    ("[", OpenBracket),
    ("]", CloseBracket),
    ("(", OpenParentheses),
    (")", CloseParentheses),
    ("{", OpenBrace),
    ("}", CloseBrace),

    (String, ListString),
    (f64, Number),
    (String, Identifier),
    (_, EOF),
    (_, Undefined)
);

pub const TOKEN_STRING_START: &str = "\"";
pub const TOKEN_STRING_END: &str = "\"";
pub const TOKEN_STRING_ESCAPE: [(char, char); 4] = [
    ('n', '\n'),
    ('r', '\r'),
    ('t', '\t'),
    ('\\', '\\'),
];