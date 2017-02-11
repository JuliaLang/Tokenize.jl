 __precompile__()

module Tokenize

const USE_STRING_LOOKUP = true

include("token.jl")
include("lexer.jl")

import .Lexers: tokenize
import .Tokens: untokenize

export tokenize, untokenize, Tokens

end # module
