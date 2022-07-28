use token_lookup_macro::token_lookup;

token_lookup!(
    ("true", True),
    ("false", False),
    
    ("and", And),
    ("or", Or),
    ("xor", Xor),
    
    ("@@", Peek),
    ("@", Pop),
    ("#", Matrix),
    ("|", Call)

    ("?", If),
    ("<-", Into),
    ("->", Exec),
    ("*>", Reduce),
    ("<*", ForEach)

    (":", Set),

    ("!", Not),
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

pub const TOKEN_STRING_START: &str = "\"[";
pub const TOKEN_STRING_END: &str = "]";
pub const TOKEN_STRING_ESCAPE: [(char, char); 5] = [
    ('n', '\n'),
    ('r', '\r'),
    ('t', '\t'),
    (']', ']'),
    ('\\', '\\'),
];