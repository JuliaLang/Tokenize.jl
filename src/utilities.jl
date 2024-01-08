#=
The code in here has been extracted from the JuliaParser.jl package
with license:

The JuliaParser.jl package is licensed under the MIT "Expat" License:

> Copyright (c) 2014: Jake Bolewski.
>
> Permission is hereby granted, free of charge, to any person obtaining
> a copy of this software and associated documentation files (the
> "Software"), to deal in the Software without restriction, including
> without limitation the rights to use, copy, modify, merge, publish,
> distribute, sublicense, and/or sell copies of the Software, and to
> permit persons to whom the Software is furnished to do so, subject to
> the following conditions:
>
> The above copyright notice and this permission notice shall be
> included in all copies or substantial portions of the Software.
>
> THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
> EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
> MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
> IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
> CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
> TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
> SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
=#

import Base.Unicode

const EOF_CHAR = typemax(Char)

eof(io::IO) = Base.eof(io)
eof(c::Char) = c === EOF_CHAR

readchar(io::IO) = eof(io) ? EOF_CHAR : read(io, Char)

# Checks whether a Char is an operator, which can not be juxtaposed with another
# Char to be an operator (i.e <=), and can be prefixed by a dot (.)
# magic number list created by filtering ops by those that successfully parse
# `a .(op) b` or `.(op)a` and where `length(string(op)) == 1`
@inline function dotop1(c1::Char)
    c1 == EOF_CHAR && return false
    c = UInt32(c1)
    c == 0x00000021 ||
    c == 0x000000a6 ||
    c == 0x0000002e ||
    c == 0x0000007e ||
    c == 0x000000ac ||
    c == 0x000000b1 ||
    c == 0x000000d7 ||
    c == 0x00002026 ||
    c == 0x0000205d ||
    c == 0x0000214b ||
    0x00002190 <= c <= 0x00002194 ||
    0x0000219a <= c <= 0x0000219e ||
    c == 0x000021a0 ||
    0x000021a2 <= c <= 0x000021a4 ||
    0x000021aa <= c <= 0x000021ac ||
    c == 0x000021a6 ||
    c == 0x000021a9 ||
    c == 0x000021ae ||
    c == 0x000021c0 ||
    c == 0x000021c1 ||
    c == 0x000021c4 ||
    c == 0x000021c6 ||
    c == 0x000021c7 ||
    c == 0x000021c9 ||
    0x000021cb <= c <= 0x000021cf ||
    c == 0x000021d2 ||
    c == 0x000021d4 ||
    c == 0x000021b6 ||
    c == 0x000021b7 ||
    0x000021ba <= c <= 0x000021bd ||
    c == 0x000021d0 ||
    0x000021da <= c <= 0x000021dd ||
    c == 0x000021e0 ||
    c == 0x000021e2 ||
    0x000021f4 <= c <= 0x000021ff ||
    0x00002208 <= c <= 0x0000220d ||
    0x00002213 <= c <= 0x00002214 ||
    0x00002217 <= c <= 0x00002219 ||
    0x0000221a <= c <= 0x0000221d ||
    0x00002224 <= c <= 0x0000222a ||
    0x00002237 <= c <= 0x00002238 ||
    0x0000223a <= c <= 0x0000223b ||
    0x0000223d <= c <= 0x0000223e ||
    0x00002240 <= c <= 0x0000228b ||
    0x0000228d <= c <= 0x0000229c ||
    0x0000229e <= c <= 0x000022a3 ||
    c == 0x000022a9 ||
    c == 0x000022ac ||
    c == 0x000022ae ||
    0x000022b0 <= c <= 0x000022b7 ||
    0x000022bc <= c <= 0x000022bd ||
    0x000022c4 <= c <= 0x000022c7 ||
    0x000022c9 <= c <= 0x000022d3 ||
    0x000022d5 <= c <= 0x000022ff ||
    c == 0x0000233f ||
    c == 0x000025b7 ||
    c == 0x000027c2 ||
    0x000027c8 <= c <= 0x000027c9 ||
    0x000027d1 <= c <= 0x000027d2 ||
    0x000027d5 <= c <= 0x000027d7 ||
    0x000027f0 <= c <= 0x000027f1 ||
    0x000027f5 <= c <= 0x000027f7 ||
    0x000027f9 <= c <= 0x000027ff ||
    0x00002900 <= c <= 0x00002918 ||
    0x0000291d <= c <= 0x00002920 ||
    0x00002944 <= c <= 0x00002970 ||
    0x000029b7 <= c <= 0x000029b8 ||
    c == 0x000029bc ||
    0x000029be <= c <= 0x000029c1 ||
    c == 0x000029e1 ||
    0x000029e3 <= c <= 0x000029e5 ||
    c == 0x000029f4 ||
    0x000029f6 <= c <= 0x000029f7 ||
    0x000029fa <= c <= 0x000029fb ||
    0x00002a07 <= c <= 0x00002a08 ||
    c == 0x00002a1d ||
    c == 0x00002a1f ||
    0x00002a22 <= c <= 0x00002a2e ||
    0x00002a30 <= c <= 0x00002a3d ||
    0x00002a40 <= c <= 0x00002a45 ||
    0x00002a4a <= c <= 0x00002a58 ||
    0x00002a5a <= c <= 0x00002a63 ||
    0x00002a66 <= c <= 0x00002a67 ||
    0x00002a6a <= c <= 0x00002ad9 ||
    c == 0x00002adb ||
    0x00002af7 <= c <= 0x00002afa ||
    0x00002b30 <= c <= 0x00002b44 ||
    0x00002b47 <= c <= 0x00002b4c ||
    0x0000ffe9 <= c <= 0x0000ffec ||
    0x00002aea <= c <= 0x00002aeb ||
    c == 0x00000387 ||
    c == 0x000000b7 ||
    c == 0x0000297A ||
    c == 0x00002977
end

function dotop2(pc, dpc)
    dotop1(pc) ||
    pc =='+' ||
    pc =='-' ||
    pc =='*' ||
    pc =='/' ||
    pc =='\\' ||
    pc =='^' ||
    pc =='<' ||
    pc =='>' ||
    pc =='&' && dpc === '=' ||
    pc =='&' ||
    pc =='%' ||
    pc == '=' && dpc != '>' ||
    pc == '|' && dpc != '|' ||
    pc == '!' && dpc == '=' ||
    pc == '⊻' ||
    pc == '÷' ||
    pc == '=' && dpc == '>'
end

# suffix operators
# https://github.com/JuliaLang/julia/blob/d7d2b0c692eb6ad409d7193ba8d9d42972cbf182/src/flisp/julia_extensions.c#L156-L174
#
# ₀₁₂₃₄₅₆₇₈₉₊₋₌₍₎²³¹ʰʲʳʷʸˡˢˣᴬᴮᴰᴱᴳᴴᴵᴶᴷᴸᴹᴺᴼᴾᴿᵀᵁᵂᵃᵇᵈᵉᵍᵏᵐᵒᵖᵗᵘᵛᵝᵞᵟᵠᵡᵢᵣᵤᵥᵦᵧᵨᵩᵪᶜᶠᶥᶦᶫᶰᶸᶻᶿ ⁰ⁱ⁴⁵⁶⁷⁸⁹⁺⁻⁼⁽⁾ⁿₐₑₒₓₕₖₗₘₙₚₛₜⱼⱽ′″‴‵‶‷⁗

@inline function isopsuffix(c1::Char)
    c1 == EOF_CHAR && return false
    c = UInt32(c1)
    if (c < 0xa1 || c > 0x10ffff)
        return false
    end
    cat = Base.Unicode.category_code(c)
    if (cat == Base.Unicode.UTF8PROC_CATEGORY_MN ||
        cat == Base.Unicode.UTF8PROC_CATEGORY_MC ||
        cat == Base.Unicode.UTF8PROC_CATEGORY_ME)
        return true
    end
    return 0x000000b2 <= c <= 0x000000b3 ||
    c == 0x000000b9 ||
    c == 0x000002b0 ||
    0x000002b2 <= c <= 0x000002b3 ||
    0x000002b7 <= c <= 0x000002b8 ||
    0x000002e1 <= c <= 0x000002e3 ||
    c == 0x00000302 ||
    c == 0x00001d2c ||
    c == 0x00001d2e ||
    0x00001d30 <= c <= 0x00001d31 ||
    0x00001d33 <= c <= 0x00001d3a ||
    c == 0x00001d3c ||
    0x00001d3e <= c <= 0x00001d43 ||
    0x00001d47 <= c <= 0x00001d49 ||
    c == 0x00001d4d ||
    0x00001d4f <= c <= 0x00001d50 ||
    c == 0x00001d52 ||
    0x00001d56 <= c <= 0x00001d58 ||
    c == 0x00001d5b ||
    0x00001d5d <= c <= 0x00001d6a ||
    c == 0x00001d9c ||
    c == 0x00001da0 ||
    0x00001da5 <= c <= 0x00001da6 ||
    c == 0x00001dab ||
    c == 0x00001db0 ||
    c == 0x00001db8 ||
    c == 0x00001dbb ||
    c == 0x00001dbf ||
    c == 0x00002009 ||
    0x00002032 <= c <= 0x00002037 ||
    c == 0x00002057 ||
    0x00002070 <= c <= 0x00002071 ||
    0x00002074 <= c <= 0x0000208e ||
    0x00002090 <= c <= 0x00002093 ||
    0x00002095 <= c <= 0x0000209c ||
    0x00002c7c <= c <= 0x00002c7d ||
    0x0000a71b <= c <= 0x0000a71d
end


function optakessuffix(k)
    (Tokens.begin_ops < k < Tokens.end_ops) &&
    !(
        k == Tokens.DDDOT ||
        Tokens.begin_assignments <= k <= Tokens.end_assignments ||
        k == Tokens.CONDITIONAL ||
        @static(if !CAN_DOT_LAZY_AND_OR
            k == Tokens.LAZY_OR ||
            k == Tokens.LAZY_AND
        else
            false
        end) ||
        k == Tokens.ISSUBTYPE ||
        k == Tokens.ISSUPERTYPE ||
        k == Tokens.IN ||
        k == Tokens.ISA ||
        k == Tokens.COLON_EQUALS ||
        k == Tokens.DOUBLE_COLON_EQUAL ||
        k == Tokens.COLON ||
        k == Tokens.DDOT ||
        k == Tokens.EX_OR ||
        k == Tokens.DECLARATION ||
        k == Tokens.WHERE ||
        k == Tokens.DOT ||
        k == Tokens.NOT ||
        k == Tokens.TRANSPOSE ||
        k == Tokens.ANON_FUNC ||
        Tokens.NOT_SIGN <= k <= Tokens.QUAD_ROOT
    )
end

# It is unknown where this array comes from...
const startchars = ['!', '$', '%', '&', '\'', '*', '+', '-', '.', '/', ':', '<', '=', '>', '?', '\\', '^', 'i', 'w', '|', '~', '¬', '±', '×', '÷', '…', '⁝', '⅋', '←', '↑', '→', '↓', '↔', '↚', '↛', '↠', '↣', '↦', '↮', '⇎', '⇏', '⇒', '⇔', '⇴', '⇵', '⇶', '⇷', '⇸', '⇹', '⇺', '⇻', '⇼', '⇽', '⇾', '⇿', '∈', '∉', '∊', '∋', '∌', '∍', '∓', '∔', '∗', '∘', '∙', '√', '∛', '∜', '∝', '∤', '∥', '∦', '∧', '∨', '∩', '∪', '∷', '∸', '∺', '∻', '∽', '∾', '≀', '≁', '≂', '≃', '≄', '≅', '≆', '≇', '≈', '≉', '≊', '≋', '≌', '≍', '≎', '≏', '≐', '≑', '≒', '≓', '≔', '≕', '≖', '≗', '≘', '≙', '≚', '≛', '≜', '≝', '≞', '≟', '≠', '≡', '≢', '≣', '≤', '≥', '≦', '≧', '≨', '≩', '≪', '≫', '≬', '≭', '≮', '≯', '≰', '≱', '≲', '≳', '≴', '≵', '≶', '≷', '≸', '≹', '≺', '≻', '≼', '≽', '≾', '≿', '⊀', '⊁', '⊂', '⊃', '⊄', '⊅', '⊆', '⊇', '⊈', '⊉', '⊊', '⊋', '⊍', '⊎', '⊏', '⊐', '⊑', '⊒', '⊓', '⊔', '⊕', '⊖', '⊗', '⊘', '⊙', '⊚', '⊛', '⊜', '⊞', '⊟', '⊠', '⊡', '⊢', '⊣', '⊩', '⊬', '⊮', '⊰', '⊱', '⊲', '⊳', '⊴', '⊵', '⊶', '⊷', '⊻', '⊼', '⊽', '⋄', '⋅', '⋆', '⋇', '⋉', '⋊', '⋋', '⋌', '⋍', '⋎', '⋏', '⋐', '⋑', '⋒', '⋓', '⋕', '⋖', '⋗', '⋘', '⋙', '⋚', '⋛', '⋜', '⋝', '⋞', '⋟', '⋠', '⋡', '⋢', '⋣', '⋤', '⋥', '⋦', '⋧', '⋨', '⋩', '⋪', '⋫', '⋬', '⋭', '⋮', '⋯', '⋰', '⋱', '⋲', '⋳', '⋴', '⋵', '⋶', '⋷', '⋸', '⋹', '⋺', '⋻', '⋼', '⋽', '⋾', '⋿', '▷', '⟂', '⟈', '⟉', '⟑', '⟒', '⟕', '⟖', '⟗', '⟰', '⟱', '⟵', '⟶', '⟷', '⟹', '⟺', '⟻', '⟼', '⟽', '⟾', '⟿', '⤀', '⤁', '⤂', '⤃', '⤄', '⤅', '⤆', '⤇', '⤈', '⤉', '⤊', '⤋', '⤌', '⤍', '⤎', '⤏', '⤐', '⤑', '⤒', '⤓', '⤔', '⤕', '⤖', '⤗', '⤘', '⤝', '⤞', '⤟', '⤠', '⥄', '⥅', '⥆', '⥇', '⥈', '⥉', '⥊', '⥋', '⥌', '⥍', '⥎', '⥏', '⥐', '⥑', '⥒', '⥓', '⥔', '⥕', '⥖', '⥗', '⥘', '⥙', '⥚', '⥛', '⥜', '⥝', '⥞', '⥟', '⥠', '⥡', '⥢', '⥣', '⥤', '⥥', '⥦', '⥧', '⥨', '⥩', '⥪', '⥫', '⥬', '⥭', '⥮', '⥯', '⥰', '⥷', '⥺', '⦷', '⦸', '⦼', '⦾', '⦿', '⧀', '⧁', '⧡', '⧣', '⧤', '⧥', '⧴', '⧶', '⧷', '⧺', '⧻', '⨇', '⨈', '⨝', '⨢', '⨣', '⨤', '⨥', '⨦', '⨧', '⨨', '⨩', '⨪', '⨫', '⨬', '⨭', '⨮', '⨰', '⨱', '⨲', '⨳', '⨴', '⨵', '⨶', '⨷', '⨸', '⨹', '⨺', '⨻', '⨼', '⨽', '⩀', '⩁', '⩂', '⩃', '⩄', '⩅', '⩊', '⩋', '⩌', '⩍', '⩎', '⩏', '⩐', '⩑', '⩒', '⩓', '⩔', '⩕', '⩖', '⩗', '⩘', '⩚', '⩛', '⩜', '⩝', '⩞', '⩟', '⩠', '⩡', '⩢', '⩣', '⩦', '⩧', '⩪', '⩫', '⩬', '⩭', '⩮', '⩯', '⩰', '⩱', '⩲', '⩳', '⩴', '⩵', '⩶', '⩷', '⩸', '⩹', '⩺', '⩻', '⩼', '⩽', '⩾', '⩿', '⪀', '⪁', '⪂', '⪃', '⪄', '⪅', '⪆', '⪇', '⪈', '⪉', '⪊', '⪋', '⪌', '⪍', '⪎', '⪏', '⪐', '⪑', '⪒', '⪓', '⪔', '⪕', '⪖', '⪗', '⪘', '⪙', '⪚', '⪛', '⪜', '⪝', '⪞', '⪟', '⪠', '⪡', '⪢', '⪣', '⪤', '⪥', '⪦', '⪧', '⪨', '⪩', '⪪', '⪫', '⪬', '⪭', '⪮', '⪯', '⪰', '⪱', '⪲', '⪳', '⪴', '⪵', '⪶', '⪷', '⪸', '⪹', '⪺', '⪻', '⪼', '⪽', '⪾', '⪿', '⫀', '⫁', '⫂', '⫃', '⫄', '⫅', '⫆', '⫇', '⫈', '⫉', '⫊', '⫋', '⫌', '⫍', '⫎', '⫏', '⫐', '⫑', '⫒', '⫓', '⫔', '⫕', '⫖', '⫗', '⫘', '⫙', '⫛', '⫷', '⫸', '⫹', '⫺', '⬰', '⬱', '⬲', '⬳', '⬴', '⬵', '⬶', '⬷', '⬸', '⬹', '⬺', '⬻', '⬼', '⬽', '⬾', '⬿', '⭀', '⭁', '⭂', '⭃', '⭄', '⭇', '⭈', '⭉', '⭊', '⭋', '⭌', '￩', '￪', '￫', '￬']

@assert issorted(startchars)

function is_operator_start_char(c::Char)
    eof(c) && return false
    return !isempty(searchsorted(startchars, c))
end
