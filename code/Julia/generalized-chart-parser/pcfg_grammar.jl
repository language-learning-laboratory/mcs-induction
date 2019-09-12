module PCFGrammar
export CFRules, CFGrammar, is_possible_transition, completions, transition, isfinal, startstate, startsymbols, types

using LogProbs
using StatsFuns.RFunctions: gammarand
using SpecialFunctions: logbeta
import Base: +, -, *, /, zero, one, <, ==



#############################
### Dirichlet Multinomial ###
#############################

function categorical_sample(tokens, weights)
    T = eltype(weights)
    x = rand(T) * sum(weights)
    cum_weights = zero(T)
    for (t, w) in zip(tokens, weights)
        cum_weights += w
        if cum_weights > x
            return t
        end
    end
end

categorical_sample(d::Dict) = categorical_sample(keys(d), values(d))
categorical_sample(v::Vector) = categorical_sample(1:length(v), v)

abstract type Distribution{T} end

mutable struct DirCat{T, C} <: Distribution{T}
    counts :: Dict{T, C}
end

DirCat(support, priors) = DirCat(Dict(x => p for (x,p) in zip(support, priors)))
support(dc::DirCat) = keys(dc.counts)

function sample(dc::DirCat)
    weights = [gammarand(c, 1) for c in values(dc.counts)]
    categorical_sample(keys(dc.counts), weights)
end

function logscore(dc::DirCat, obs)
    LogProb(logbeta(sum(values(dc.counts)), 1) - logbeta(dc.counts[obs], 1); islog=true)
end

function add_obs!(dc::DirCat, obs)
    dc.counts[obs] += 1
end

function rm_obs!(dc::DirCat, obs)
    dc.counts[obs] -= 1
end


################################
### Conditional Distribution ###
################################

struct SimpleCond{C, D, S} # context, distribution, support
    dists   :: Dict{C, D}
    support :: S
    SimpleCond(dists::Dict{C, D}, support::S) where {C, D, S} =
        new{C, D, S}(dists, unique(support))
end

function SimpleCond(dists::AbstractDict)
    SimpleCond(
        dists,
        vcat([collect(support(dist)) for dist in values(dists)]...)
    )
end

sample(sc::SimpleCond, context, args...) = sample(sc.dists[context], args...)
logscore(sc::SimpleCond, obs, context) = logscore(sc.dists[context], obs)
rm_obs!(sc::SimpleCond, obs, context) = rm_obs!(sc.dists[context], obs)

score_type(::SimpleCond) = LogProb

function add_obs!(cond::SimpleCond{C,D,S}, obs, context) where {C,D,S}
    if !haskey(cond.dists, context)
        cond.dists[context] = D(cond.support)
    end
    add_obs!(cond.dists[context], obs)
end

###############
### CFRules ###
###############

mutable struct RunningCounter
    n :: Int
end

RunningCounter() = RunningCounter(0)
count!(c::RunningCounter) = c.n += 1

rule_counter = RunningCounter()

struct CFRules{LHS, RHS} # left hand side and right hand side of the rule
    mappings ::Dict{LHS, Vector{RHS}}
    name :: Symbol
end

==(r1::CFRules, r2::CFRules) = r1.name == r2.name
hash(r::CFRules, h::UInt) = hash(hash(CFRules, hash(r.name)), h)

Base.show(io::IO, r::CFRules) = print(io, "CFRules($(r.name))")

CFRules(pairs::Pair...) =
    CFRules(Dict(pairs...), Symbol("rules", count!(rule_counter)))
CFRules(g::Base.Generator) =
    CFRules(Dict(g), Symbol("rules", count!(rule_counter)))
CFRules(f::Function, lhss, name) =
    CFRules(Dict(lhs => f(lhs) for lhs in lhss), name)
CFRules(f::Function, lhss) =
    CFRules(Dict(lhs => f(lhs) for lhs in lhss), Symbol("rules", count!(rule_counter)))

lhss(r::CFRules) = keys(r.mappings) # aka domain
isapplicable(r::CFRules, lhs) = haskey(r.mappings, lhs)
(r::CFRules)(lhs) = r.mappings[lhs]

###############
### CFState ###
###############


mutable struct CompletionAutomaton{Cat,Comp} # category, completion
    transitions :: Vector{Dict{Cat, Int}}
    completions :: Vector{Vector{Comp}}
end

CompletionAutomaton(Cat::Type, Comp::Type) =
    CompletionAutomaton([Dict{Cat, Int}()], [Vector{Comp}()])

number_of_states(ca::CompletionAutomaton) = length(ca.transitions)
isfinal(ca::CompletionAutomaton, s) = isempty(ca.transitions[s])
is_possible_transition(ca::CompletionAutomaton, s, c) = haskey(ca.transitions[s], c)
transition(ca::CompletionAutomaton, s, c) = ca.transitions[s][c]
completions(ca::CompletionAutomaton, s) = ca.completions[s]

#Not sure what is going on here
function add_completion!(ca::CompletionAutomaton{Cat,Comp}, comp, categories) where {Cat,Comp}
    s = 1
    for c in categories
        if is_possible_transition(ca, s, c)
            s = transition(ca, s, c)
        else
            push!(ca.transitions, Dict{Cat,Int}())
            push!(ca.completions, Vector{Comp}())
            s = ca.transitions[s][c] = number_of_states(ca)
        end
    end
    push!(ca.completions[s], comp)
end

function add_rule!(ca::CompletionAutomaton, r::CFRules)
    for lhs in lhss(r)
        add_completion!(ca, (lhs, r), r(lhs))
    end
end

#################
### CFGrammar ###
#################

struct CFGrammar{C, T, Cond, F}
    comp_automtn  :: CompletionAutomaton{C, Tuple{C, CFRules{C, C}}}
    startsymbols  :: Vector{C}
    terminal_dict :: Dict{T, Vector{Tuple{C, CFRules{C, T}}}}
    cond          :: Cond # conditional scoring
    dependent_components::F
end

function CFGrammar(
        category_rules::Vector{CFRules{C, C}},
        terminal_rules::Vector{CFRules{C, T}},
        startsymbols  ::Vector{C},
        dependent_components=identity::Function
        ) where {C, T}
    comp_automtn = CompletionAutomaton(C, Tuple{C, CFRules{C, C}})
    for r in category_rules
        add_rule!(comp_automtn, r)
    end

    terminal_dict = Dict{T, Vector{Tuple{C, CFRules{C, T}}}}()
    for r in terminal_rules
        for lhs in lhss(r)
        t = r(lhs)[1]
            if haskey(terminal_dict, t)
                push!(terminal_dict[t], (lhs, r))
            else
                terminal_dict[t] = [(lhs, r)]
            end
        end
    end

    applicable_rules = Dict{C, Vector{CFRules}}()
    for r in CFRules[category_rules; terminal_rules]
        for c in lhss(r)
            if haskey(applicable_rules, c)
                push!(applicable_rules[c], r)
            else
                applicable_rules[c] = CFRules[r]
            end
        end
    end

    cond = SimpleCond(
        Dict(
            dependent_components(c) => let rules = applicable_rules[c]
                n = length(rules)
                k = count(isa.(rules, CFRules{C, T})) # number terminal rules
                DirCat(rules, [fill(1.0, n-k); fill(1/k, k)])
            end
            for c in keys(applicable_rules)
        )
    )

    CFGrammar(comp_automtn, startsymbols, terminal_dict, cond, dependent_components)
end

dependent_components(g::CFGrammar, c) = g.dependent_components(c)

startstate(g::CFGrammar) = 1
startsymbols(g::CFGrammar) = g.startsymbols

isfinal(g::CFGrammar, s) = isfinal(g.comp_automtn, s)
is_possible_transition(g::CFGrammar, s, c) = is_possible_transition(g.comp_automtn, s, c)
transition(g::CFGrammar, s, c) = transition(g.comp_automtn, s, c)

completions(g::CFGrammar, s::Int) =
    ((c, r, score(g, c, r)) for (c, r) in completions(g.comp_automtn, s))
completions(g::CFGrammar, t) =
    ((c, r, score(g, c, r)) for (c, r) in g.terminal_dict[t])

score(g::CFGrammar, c, r) = logscore(g.cond, r, dependent_components(g, c))

@inline function types(grammar::CFGrammar{C, T, Cond, F}) where {C, T, Cond, F}
    C, T, CFRules{C, C}, CFRules{C, T}, Int, LogProb
end

end
