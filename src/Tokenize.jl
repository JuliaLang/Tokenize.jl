__precompile__()

module Tokenize

using Compat

include("token.jl")
include("lexer.jl")

import .Lexers: tokenize
import .Tokens: untokenize

export tokenize, untokenize, Tokens

include("_precompile.jl")
_precompile_()

end # module
