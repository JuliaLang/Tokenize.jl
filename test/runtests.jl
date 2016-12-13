using Base.Test

import Tokenize

const PKGPATH = joinpath(dirname(@__FILE__), "..")

tot_files = 0
tot_time = 0.0
tot_tokens = 0
tot_errors = 0
function testall(srcdir::AbstractString)
    global tot_files, tot_time, tot_tokens, tot_errors
    dirs, files = [], []

    for fname in sort(readdir(srcdir))
        path = joinpath(srcdir, fname)
        if isdir(path)
            push!(dirs, path)
            continue
        end
        _, ext = splitext(fname)
        if ext == ".jl"
            push!(files, path)
        end
    end

    if !isempty(files)
        for jlpath in files

            fname = splitdir(jlpath)[end]

            buf = IOBuffer()
            write(buf, open(readstring, jlpath))
            tot_files += 1
            tot_time += @elapsed tokens = collect(Tokenize.tokenize(buf))
            tot_tokens += length(tokens)

            seek(buf, 0)
            str = String(take!(buf))
            collect(Tokenize.tokenize(str))

            for token in tokens
                if Tokenize.Tokens.kind(token) == Tokenize.Tokens.ERROR
                    tot_errors += 1
                end
            end
        end
    end
    for dir in dirs
        testall(dir)
    end
end


testall(joinpath(PKGPATH, "benchmark"))
testall(joinpath(PKGPATH, "src"))
testall(joinpath(PKGPATH, "test"))


print("Lexed ", tot_files, " files in ", @sprintf("%3.4f", tot_time),
      " seconds with a total of ", tot_tokens, " tokens with ", tot_errors, " errors")

@test tot_errors == 0
