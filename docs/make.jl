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

# Documenter can also automatically deploy documentation to gh-pages.
# See "Hosting Documentation" and deploydocs() in the Documenter manual
# for more information.
#=deploydocs(
    repo = "<repository url>"
)=#
