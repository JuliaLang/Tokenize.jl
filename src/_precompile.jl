isdefined(Base, :GenericIOBuffer) ? (import Base.GenericIOBuffer) : (@compat GenericIOBuffer{T} = Base.AbstractIOBuffer{T})

function _precompile_()
    ccall(:jl_generating_output, Cint, ()) == 1 || return nothing
    precompile(Tokenize.Lexers.is_cat_id_start, (Char, Int32,))
    precompile(Tokenize.Lexers.peekchar, (GenericIOBuffer{Array{UInt8, 1}},))
    precompile(Tokenize.Lexers.is_identifier_char, (Char,))
    precompile(Tokenize.Lexers.tryread, (Tokenize.Lexers.Lexer{GenericIOBuffer{Array{UInt8, 1}}}, Tuple{Char, Char}, Tokenize.Tokens.Kind,))
    precompile(Tokenize.Lexers.lex_greater, (Tokenize.Lexers.Lexer{GenericIOBuffer{Array{UInt8, 1}}},))
    precompile(Tokenize.Lexers.lex_prime, (Tokenize.Lexers.Lexer{GenericIOBuffer{Array{UInt8, 1}}},))
    precompile(Tokenize.Lexers.tryread, (Tokenize.Lexers.Lexer{GenericIOBuffer{Array{UInt8, 1}}}, Tuple{Char, Char, Char, Char, Char, Char}, Tokenize.Tokens.Kind,))
    precompile(Tokenize.Lexers.tryread, (Tokenize.Lexers.Lexer{GenericIOBuffer{Array{UInt8, 1}}}, Tuple{Char, Char, Char, Char, Char}, Tokenize.Tokens.Kind,))
    precompile(Tokenize.Lexers.lex_digit, (Tokenize.Lexers.Lexer{GenericIOBuffer{Array{UInt8, 1}}},))
    precompile(Tokenize.Lexers.lex_identifier, (Tokenize.Lexers.Lexer{GenericIOBuffer{Array{UInt8, 1}}}, Char,))
    precompile(Tokenize.Lexers.lex_less, (Tokenize.Lexers.Lexer{GenericIOBuffer{Array{UInt8, 1}}},))
    precompile(Tokenize.Lexers.tryread, (Tokenize.Lexers.Lexer{GenericIOBuffer{Array{UInt8, 1}}}, Tuple{Char, Char, Char}, Tokenize.Tokens.Kind,))
    precompile(Tokenize.Lexers.tryread, (Tokenize.Lexers.Lexer{GenericIOBuffer{Array{UInt8, 1}}}, Tuple{Char, Char, Char, Char, Char, Char, Char, Char}, Tokenize.Tokens.Kind,))
    precompile(Tokenize.Lexers.lex_forwardslash, (Tokenize.Lexers.Lexer{GenericIOBuffer{Array{UInt8, 1}}},))
    precompile(Tokenize.Lexers.lex_minus, (Tokenize.Lexers.Lexer{GenericIOBuffer{Array{UInt8, 1}}},))
    precompile(Tokenize.Lexers.tryread, (Tokenize.Lexers.Lexer{GenericIOBuffer{Array{UInt8, 1}}}, Char, Tokenize.Tokens.Kind,))
    precompile(Tokenize.Lexers.tryread, (Tokenize.Lexers.Lexer{GenericIOBuffer{Array{UInt8, 1}}}, Tuple{Char, Char, Char, Char}, Tokenize.Tokens.Kind,))
    precompile(Tokenize.Lexers.start, (Tokenize.Lexers.Lexer{GenericIOBuffer{Array{UInt8, 1}}},))
    precompile(Tokenize.Lexers.lex_xor, (Tokenize.Lexers.Lexer{GenericIOBuffer{Array{UInt8, 1}}},))
    precompile(Tokenize.Lexers.Type, (Type{Tokenize.Lexers.Lexer}, String,))
    precompile(Tokenize.Lexers.tryread, (Tokenize.Lexers.Lexer{GenericIOBuffer{Array{UInt8, 1}}}, Tuple{Char}, Tokenize.Tokens.Kind,))
    precompile(Tokenize.Lexers.lex_equal, (Tokenize.Lexers.Lexer{GenericIOBuffer{Array{UInt8, 1}}},))
    precompile(Tokenize.Lexers.readchar, (Tokenize.Lexers.Lexer{GenericIOBuffer{Array{UInt8, 1}}},))
    precompile(Tokenize.Lexers.lex_bar, (Tokenize.Lexers.Lexer{GenericIOBuffer{Array{UInt8, 1}}},))
    precompile(Tokenize.Lexers.readrest, (Tokenize.Lexers.Lexer{GenericIOBuffer{Array{UInt8, 1}}},))
    precompile(Tokenize.Lexers.backup!, (Tokenize.Lexers.Lexer{GenericIOBuffer{Array{UInt8, 1}}},))
    precompile(Tokenize.Lexers.ishex, (Char,))
    precompile(Tokenize.Lexers.next_token, (Tokenize.Lexers.Lexer{GenericIOBuffer{Array{UInt8, 1}}},))
    precompile(Tokenize.Tokens.untokenize, (Array{Tokenize.Tokens.Token, 1},))
    precompile(Tokenize.Lexers.lex_quote, (Tokenize.Lexers.Lexer{GenericIOBuffer{Array{UInt8, 1}}},))
    precompile(Tokenize.Tokens.kind, (Tokenize.Tokens.Token,))
    precompile(Tokenize.Lexers.accept, (Tokenize.Lexers.Lexer{GenericIOBuffer{Array{UInt8, 1}}}, String,))
    precompile(Tokenize.Lexers.lex_plus, (Tokenize.Lexers.Lexer{GenericIOBuffer{Array{UInt8, 1}}},))
    precompile(Tokenize.Lexers.tryread, (Tokenize.Lexers.Lexer{GenericIOBuffer{Array{UInt8, 1}}}, Tuple{Char, Char, Char, Char, Char, Char, Char}, Tokenize.Tokens.Kind,))
    precompile(Tokenize.Lexers.lex_dot, (Tokenize.Lexers.Lexer{GenericIOBuffer{Array{UInt8, 1}}},))
    precompile(Tokenize.Lexers.lex_exclaim, (Tokenize.Lexers.Lexer{GenericIOBuffer{Array{UInt8, 1}}},))
    precompile(Tokenize.Lexers.lex_colon, (Tokenize.Lexers.Lexer{GenericIOBuffer{Array{UInt8, 1}}},))
    precompile(Tokenize.Tokens.untokenize, (Tokenize.Lexers.Lexer{GenericIOBuffer{Array{UInt8, 1}}},))
    precompile(Tokenize.Lexers.lex_percent, (Tokenize.Lexers.Lexer{GenericIOBuffer{Array{UInt8, 1}}},))
    precompile(Tokenize.Lexers.lex_comment, (Tokenize.Lexers.Lexer{GenericIOBuffer{Array{UInt8, 1}}},))
    precompile(Tokenize.Lexers.lex_cmd, (Tokenize.Lexers.Lexer{GenericIOBuffer{Array{UInt8, 1}}},))
    precompile(Tokenize.Lexers.lex_division, (Tokenize.Lexers.Lexer{GenericIOBuffer{Array{UInt8, 1}}},))
    precompile(Tokenize.Lexers.lex_circumflex, (Tokenize.Lexers.Lexer{GenericIOBuffer{Array{UInt8, 1}}},))
    precompile(Tokenize.Lexers.lex_backslash, (Tokenize.Lexers.Lexer{GenericIOBuffer{Array{UInt8, 1}}},))
    precompile(Tokenize.Lexers.read_string, (Tokenize.Lexers.Lexer{GenericIOBuffer{Array{UInt8, 1}}}, Tokenize.Tokens.Kind,))
    precompile(Tokenize.Lexers.lex_star, (Tokenize.Lexers.Lexer{GenericIOBuffer{Array{UInt8, 1}}},))
    precompile(Tokenize.Lexers.accept_batch, (Tokenize.Lexers.Lexer{GenericIOBuffer{Array{UInt8, 1}}}, Function,))
    precompile(Tokenize.Lexers.extract_tokenstring, (Tokenize.Lexers.Lexer{GenericIOBuffer{Array{UInt8, 1}}},))
    precompile(Tokenize.Lexers.lex_amper, (Tokenize.Lexers.Lexer{GenericIOBuffer{Array{UInt8, 1}}},))
    precompile(Tokenize.Lexers.accept, (Tokenize.Lexers.Lexer{GenericIOBuffer{Array{UInt8, 1}}},typeof( Base.UTF8proc.isdigit),))
    precompile(Tokenize.Lexers.lex_whitespace, (Tokenize.Lexers.Lexer{GenericIOBuffer{Array{UInt8, 1}}},))
    precompile(Tokenize.Lexers.iswhitespace, (Char,))
    precompile(Tokenize.Lexers.next, (Tokenize.Lexers.Lexer{GenericIOBuffer{Array{UInt8, 1}}}, Bool,))
    precompile(Tokenize.Lexers.start_token!, (Tokenize.Lexers.Lexer{GenericIOBuffer{Array{UInt8, 1}}},))
    precompile(Tokenize.Lexers.accept, (Tokenize.Lexers.Lexer{GenericIOBuffer{Array{UInt8, 1}}},typeof( Tokenize.Lexers.iswhitespace),))
    precompile(Tokenize.Lexers.accept, (Tokenize.Lexers.Lexer{GenericIOBuffer{Array{UInt8, 1}}},typeof( Tokenize.Lexers.is_identifier_char),))
    precompile(Tokenize.Tokens.endpos, (Tokenize.Tokens.Token,))
    precompile(Tokenize.Lexers.done, (Tokenize.Lexers.Lexer{GenericIOBuffer{Array{UInt8, 1}}}, Bool,))
    precompile(Tokenize.Lexers.Type, (Type{Tokenize.Lexers.Lexer}, GenericIOBuffer{Array{UInt8, 1}},))
    precompile(Tokenize.Tokens.startpos, (Tokenize.Tokens.Token,))
    precompile(Tokenize.Tokens.untokenize, (Tokenize.Tokens.Token,))
    precompile(Tokenize.Lexers.accept, (Tokenize.Lexers.Lexer{GenericIOBuffer{Array{UInt8, 1}}},typeof( Tokenize.Lexers.ishex),))
    precompile(Tokenize.Lexers.accept, (Tokenize.Lexers.Lexer{GenericIOBuffer{Array{UInt8, 1}}}, Char,))
    precompile(Tokenize.Lexers._doret, (Char, Tokenize.Lexers.Lexer{GenericIOBuffer{Array{UInt8, 1}}},))
    precompile(Tokenize.Lexers.is_identifier_start_char, (Char,))
end
