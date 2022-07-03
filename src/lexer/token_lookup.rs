use token_lookup_macro::token_lookup;

token_lookup!(
    ("true", TRUE),
    ("false", FALSE),
    
    ("and", AND),
    ("or", OR),
    ("xor", XOR),
    
    ("@@", PEEK),
    ("@", POP),
    ("^", OUTER),

    ("?", IF),
    ("'", NTH),
    ("<-", INTO),
    ("->", EXEC),
    ("*>", REDUCE_EXEC),

    ("&", CONCAT),

    (":", SET),
    (";", RANGE),

    ("!", NOT),
    ("+", ADD),
    ("-", SUB),
    ("*", MUL),
    ("/", DIV),
    ("<", LESS),
    (">", GREAT),
    ("=", EQU),
    ("<=", LESS_EQU),
    (">=", GREAT_EQU),
    
    ("+:", ADD_SET),
    ("-:", SUB_SET),
    ("*:", MUL_SET),
    ("/:", DIV_SET),

    (".", CHAIN),

    ("_", NOTHING),

    ("|", FROM).
    (",", COMMA),
    
    ("[", OPEN_BRACKET),
    ("]", CLOSE_BRACKET),
    ("(", OPEN_PARENTHESES),
    (")", CLOSE_PARENTHESES),
    ("{", OPEN_BRACE),
    ("}", CLOSE_BRACE),

    (String, STRING),
    (f64, NUMBER),
    (String, IDENTIFIER),
    (_, EOF),
    (_, UNDEFINED)
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