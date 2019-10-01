include("../generalized-chart-parser/chartparser.jl")
using .ChartParser: run_chartparser, MetaRule, CFGrammar, CFRule, score, Grammar

function read_treebank_grammar(number_sentences::Int, Score=LogProb)
    @assert number_sentences in (1, 5, 50, 500, 972)
    grammarfile = joinpath(dirname(@__FILE__), "WSJ_data", "WSJ.$number_sentences.grammar.txt")
    io = open(grammarfile, "r")
    grammarstring = read(io, String)
    Grammar(grammarstring, ["START"])
end

function compute_treebank_scores(number_sentences::Int, Score=LogProb)
    @assert number_sentences in (1, 5, 50, 500, 972)
    grammar=read_treebank_grammar(number_sentences, Score)
    sentences = map(split, readlines(joinpath(dirname(@__FILE__), "WSJ_data", "WSJ.$number_sentences.sentences.txt")))
    scores = zeros(Score, length(sentences))
    for (i,s) in enumerate(sentences)
        scores[i] = score(run_chartparser(s, grammar))
    end
    scores
end

function treebank_goldstandard_scores(number_sentences::Int, Score=LogProb)
    lines = readlines(joinpath(dirname(@__FILE__), "WSJ_data", "WSJ.$number_sentences.scores.txt"))
    scores = zeros(Score, length(lines))
    for (i,ln) in enumerate(lines)
        scores[i] = Score(parse(Float64, ln), islog=true)
    end
    scores
end
