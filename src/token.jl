module Tokens

using Compat
import Compat.String
import Base.eof

import .. USE_STRING_LOOKUP

export Token

include("token_kinds.jl")


iskeyword(k::Kind) = begin_keywords < k < end_keywords
isliteral(k::Kind) = begin_literal < k < end_literal
isoperator(k::Kind) = begin_ops < k < end_ops

# Create string => keyword kind
const KEYWORDS = Dict{String, Kind}()

function _add_kws()
    for k in instances(Kind)
        if iskeyword(k)
            KEYWORDS[lowercase(string(k))] = k
        end
    end
end
_add_kws()

# TODO: more
@enum(TokenError,
  NO_ERR,
  EOF_MULTICOMMENT,
  EOF_STRING,
  EOF_CHAR,
  EOF_CMD,
  UNKNOWN,
)

# Error kind => description
TOKEN_ERROR_DESCRIPTION = Dict{TokenError, String}(
  EOF_MULTICOMMENT => "unterminated multi-line comment #= ... =#",
  EOF_STRING => "unterminated string literal",
  EOF_CHAR => "unterminated character literal",
  EOF_CMD => "unterminated cmd literal",
  UNKNOWN => "unknown",
)

abstract AbstractToken

immutable RawToken <: AbstractToken
    kind::Kind
    # Offsets into a string or buffer
    startpos::Tuple{Int, Int} # row, col where token starts /end, col is a string index
    endpos::Tuple{Int, Int}
    startbyte::Int64 # The byte where the token start in the buffer
    endbyte::Int64 # The byte where the token ended in the buffer
    token_error::TokenError
end

immutable Token <: AbstractToken
    kind::Kind
    # Offsets into a string or buffer
    startpos::Tuple{Int, Int} # row, col where token starts /end, col is a string index
    endpos::Tuple{Int, Int}
    startbyte::Int64 # The byte where the token start in the buffer
    endbyte::Int64 # The byte where the token ended in the buffer
    val::Compat.UTF8String # The actual string of the token
    token_error::TokenError
end

function Token(kind::Kind, startposition::Tuple{Int, Int}, endposition::Tuple{Int, Int},
               startbyte::Int64, endbyte::Int64, val::String)
    Token(kind, startposition, endposition, startbyte, endbyte, val, NO_ERR)
end
Token() = Token(ERROR, (0,0), (0,0), 0, 0, "", UNKNOWN)

function kind(t::AbstractToken)
    isoperator(t.kind) && return OP
    iskeyword(t.kind) && return KEYWORD
    return t.kind
end
exactkind(t::AbstractToken) = t.kind
startpos(t::AbstractToken) = t.startpos
endpos(t::AbstractToken) = t.endpos
untokenize(t::Token) = t.val


function untokenize(ts)
    if eltype(ts) != Token
        throw(ArgumentError("element type of iterator has to be Token"))
    end
    io = IOBuffer()
    for tok in ts
        write(io, untokenize(tok))
    end
    return String(take!(io))
end

function untokenize(t::RawToken, str::String)
    if _need_extract(exactkind(t)) || !USE_STRING_LOOKUP
      str[t.startbyte + 1: t.endbyte + 1]
    else
      return STRINGS[Int(exactkind(t))]
    end
end

function untokenize(t::RawToken, io::IO)
    if _need_extract(exactkind(t)) || !USE_STRING_LOOKUP
        p = position(io)
        seek(io, t.startbyte)
        str = String(read(io, t.endbyte - t.startbyte + 1))
        seek(io, p)
        return str
    else
        return STRINGS[Int(exactkind(t))]
    end
end

function untokenize(ts, source::Union{String, IO})
    if eltype(ts) != RawToken
        throw(ArgumentError("element type of iterator has to be RawToken"))
    end
    io = IOBuffer()
    for tok in ts
        write(io, untokenize(tok, source))
    end
    return String(take!(io))
end


function Base.show(io::IO, t::Token)
  start_r, start_c = startpos(t)
  end_r, end_c = endpos(t)
  str = kind(t) == ENDMARKER ? "" : untokenize(t)
    print(io, start_r, ",", start_c, "-",
            end_r,   ",", end_c,   ":",
            "   ", kind(t), "\t")
    show(io, str)
end

function Base.show(io::IO, t::RawToken)
  start_r, start_c = startpos(t)
  end_r, end_c = endpos(t)
    print(io, start_r, ",", start_c, "-",
            end_r,   ",", end_c,   ":",
            "   ", kind(t), "\t")
end

Base.print(io::IO, t::Token) = print(io, untokenize(t))

eof(t::AbstractToken) = t.kind == Eof

end # module
