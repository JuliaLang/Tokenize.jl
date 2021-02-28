using Documenter
using Tokenize

makedocs(
    sitename = "Tokenize",
    format = Documenter.HTML(),
    modules = [Tokenize],
    pages=[
        "Home" => "index.md",
    ],
)

deploydocs(
    repo = "github.com/JuliaLang/Tokenize.jl.git",
    devbranch = "master",
    push_preview = true,
)
