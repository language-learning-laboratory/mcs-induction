module PCFGrammar
export CFRule, MetaRule, CFGrammar, is_possible_transition, completions, transition, isfinal, startstate, startsymbols, types, Grammar, CompletionAutomaton, DiffGrammar
using LogProbs
import Base: +, -, *, /, zero, one, <, ==

include("./distributions.jl")
import .Distributions: categorical_sample, Distribution, CatDist, DirCat, DiffCatDist, SimpleCond, support, logscore, sample, add_obs!, rm_obs!


###############
### CFRule  ###
###############

struct CFRule{LHS,RHS}
    lhs :: LHS                # left hand side
    rhs :: Vector{RHS}        # right hand side
end

lhs(r::CFRule) = r.lhs
rhs(r::CFRule) = r.rhs

# function can_create_as_prefix(r::CFRule, rhss)
#     @assert length(rhss) <= length(r.rhs)
#     for (i,c) in enumerate(rhss)
#         if c != r.rhs[i]
#             return false
#         end
#     end
#     return true
# end
#
# function can_create(r::CFRule, rhss)
#     length(r.rhs) == length(rhss) && can_create_as_prefix(r,rhss)
# end
# can_create(r::CFRule, rhss...) = can_create(r, rhss)

function completion(r::CFRule, rhss)
    @assert rhss == r.rhs
    r.lhs
end

isapplicable(r::CFRule, cat) = lhs(r) == cat

(r::CFRule)(lhs) = rhs(r)

domain(r::CFRule) = r.lhs




###############
### MetaRule ##
###############

mutable struct RunningCounter
    n :: Int
end

RunningCounter() = RunningCounter(0)
count!(c::RunningCounter) = c.n += 1

rule_counter = RunningCounter()

struct MetaRule{LHS, RHS} # left hand side and right hand side of the rule
    mappings ::Dict{LHS,CFRule{LHS, RHS}}
    name :: Symbol
end

==(r1::MetaRule, r2::MetaRule) = r1.name == r2.name
hash(r::MetaRule, h::UInt) = hash(hash(MetaRule, hash(r.name)), h)

Base.show(io::IO, mr::MetaRule) = print(io, "MetaRule($(mr.name))")

lhss(mr::MetaRule) = keys(mr.mappings) # aka domain

isapplicable(mr::MetaRule, lhs) = haskey(mr.mappings, lhs)
(mr::MetaRule)(lhs) = mr.mappings[lhs].rhs

# function can_create_as_prefix(mr::MetaRule, rhss)
#     for r in mr.rules
#         if can_create_as_prefix(r, rhss)
#             return true
#         end
#     end
#     return false
# end
#
# function can_create(mr::MetaRule, rhss)
#     for r in mr.rules
#         if can_create(r, rhss)
#             return true
#         end
#     end
#     return false
# end
#
# can_create(r::MetaRule, rhss...) = can_create(r, rhss)
#
# function completion(mr::MetaRule, rhss)
#     for r in mr.rules
#         if can_create(r,rhss)
#             return completion(r, rhss)
#         end
#     end
#     error("No completion possible.")
# end

MetaRule(rules::Vector{CFRule{LHS,RHS}}) where {LHS,RHS} =
    MetaRule(Dict(lhs(r) => r for r in rules), Symbol("rules", count!(rule_counter)))
MetaRule(pairs::Pair...) =
    MetaRule(Dict(p.first => CFRule(p.first, p.second) for p in pairs), Symbol("rules", count!(rule_counter)))
 MetaRule(g::Base.Generator) =
     MetaRule(g, Symbol("rules", count!(rule_counter)))
MetaRule(f::Function, lhss, name) =
    MetaRule(Dict(lhs => CFRule(lhs,f(lhs)) for lhs in lhss), name)
MetaRule(f::Function, lhss) =
    MetaRule(Dict(lhs => CFRule(lhs,f(lhs)) for lhs in lhss), Symbol("rules", count!(rule_counter)))


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

function add_rule!(ca::CompletionAutomaton, mr::MetaRule)
    for lhs in lhss(mr)
        add_completion!(ca, (lhs, mr), mr(lhs))
    end
end

#################
### CFGrammar ###
#################

struct CFGrammar{C, T, Cond, F}
    comp_automtn  :: CompletionAutomaton{C, Tuple{C, MetaRule{C, C}}}
    startsymbols  :: Vector{C}
    terminal_dict :: Dict{T, Vector{Tuple{C, MetaRule{C, T}}}}
    cond          :: Cond # conditional scoring
    dependent_components::F
end

function CFGrammar(
        category_rules::Vector{MetaRule{C, C}},
        terminal_rules::Vector{MetaRule{C, T}},
        startsymbols  ::Vector{C},
        dependent_components=identity::Function
        ) where {C, T}
    comp_automtn = CompletionAutomaton(C, Tuple{C, MetaRule{C, C}})
    for mr in category_rules
        add_rule!(comp_automtn, mr)
    end

    terminal_dict = Dict{T, Vector{Tuple{C, MetaRule{C, T}}}}()
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

    applicable_rules = Dict{C, Vector{MetaRule}}()
    for r in MetaRule[category_rules; terminal_rules]
        for c in lhss(r)
            if haskey(applicable_rules, c)
                push!(applicable_rules[c], r)
            else
                applicable_rules[c] = MetaRule[r]
            end
        end
    end

    cond = SimpleCond(
        Dict(
            dependent_components(c) => let rules = applicable_rules[c]
                n = length(rules)
                k = count(isa.(rules, MetaRule{C, T})) # number terminal rules
                DirCat(rules, [fill(1.0, n-k); fill(1/k, k)])
            end
            for c in keys(applicable_rules)
        )
    )

    CFGrammar(comp_automtn, startsymbols, terminal_dict, cond, dependent_components)
end

function CFGrammar(
        category_rules_strings::Vector{Vector{C}},
        terminal_rules_strings::Vector{Vector{T}},
        startsymbols,
        Score,
        dependent_components=identity::Function) where {C,T}

    if length(terminal_rules_strings[1]) > 2 #if probability included in strings

        category_rules_with_probs = [
            (MetaRule([CFRule(s[2],[s[3:end]...])]), Score(parse(Float64, s[1]), islog=true))
            for s in category_rules_strings]
        terminal_rules_with_probs = [
            (MetaRule([CFRule(s[2],[s[3:end]...])]), Score(parse(Float64, s[1]), islog=true))
            for s in terminal_rules_strings]

        category_rules = [mr for (mr,p) in category_rules_with_probs]
        terminal_rules = [mr for (mr,p) in terminal_rules_with_probs]

        comp_automtn = CompletionAutomaton(C, Tuple{C, MetaRule{C, C}})
        for mr in category_rules
            add_rule!(comp_automtn, mr)
        end

        terminal_dict = Dict{T, Vector{Tuple{C, MetaRule{C, T}}}}()
        for mr in terminal_rules
            for lhs in lhss(mr)
                t = mr(lhs)[1]
                if haskey(terminal_dict, t)
                    push!(terminal_dict[t], (lhs, mr))
                else
                    terminal_dict[t] = [(lhs, mr)]
                end
            end
        end

        all_rules_with_probs = [category_rules_with_probs;terminal_rules_with_probs]
        # cond = SimpleCond(Dict(
        #     cat => CatDist(Dict(
        #         r => p for (r,p) in all_rules_with_probs if r.lhs == cat
        #     ))
        #     for cat in unique(lhss for (r,p) in all_rules_with_probs))
        # )
        cond = SimpleCond(Dict(
            cat => CatDist(Dict(
                mr => p for (mr,p) in all_rules_with_probs if isapplicable(mr, cat)
            ))
            for cat in unique(Base.reduce(append!, [lhss(mr) for (mr,p) in all_rules_with_probs], init=T[])))
        )

        CFGrammar(comp_automtn, startsymbols, terminal_dict, cond, dependent_components)

    else
        category_rules = [MetaRule([CFRule(s[1],[s[2:end]...])]) for s in category_rules_strings]
        terminal_rules = [MetaRule([CFRule(s[1],[s[2:end]...])]) for s in terminal_rules_strings]

        CFGrammar(category_rules, terminal_rules, startsymbols, dependent_components)
    end
end



dependent_components(g::CFGrammar, c) = g.dependent_components(c)

startstate(g::CFGrammar) = 1
startsymbols(g::CFGrammar) = g.startsymbols

isfinal(g::CFGrammar, s) = isfinal(g.comp_automtn, s)
is_possible_transition(g::CFGrammar, s, c) = is_possible_transition(g.comp_automtn, s, c)
transition(g::CFGrammar, s, c) = transition(g.comp_automtn, s, c)

completions(g::CFGrammar, s::Int) =
    ((c, r, score(g, c, r)) for (c, r) in completions(g.comp_automtn, s))
completions(g::CFGrammar, t::T) where T =
    ((c, r, score(g, c, r)) for (c, r) in g.terminal_dict[t])

score(g::CFGrammar, c, r) = logscore(g.cond, r, dependent_components(g, c))

@inline function types(grammar::CFGrammar{C, T, Cond, F}) where {C, T, Cond, F}
    C, T, MetaRule{C, C}, MetaRule{C, T}, Int, LogProb
end

function split_category_from_terminal_rules(rules_string::AbstractString)
    rule_string_lists = map(split, split(rules_string, "\n"))
    terminal_rule_stringlists = Vector{String}[]
    category_rule_stringlists = Vector{String}[]
    for lst in rule_string_lists
        if !isempty(lst)
            if lst[end][1] == '_'
                corrected_lst = [s[1] == '_' ? s[2:end] : s for s in lst]
                push!(terminal_rule_stringlists, corrected_lst)
            else
                push!(category_rule_stringlists, lst)
            end
        end
    end
    category_rule_stringlists, terminal_rule_stringlists
end

function Grammar(rules_string::AbstractString, startsymbols, Score=LogProb)
    category_rules, terminal_rules = split_category_from_terminal_rules(rules_string)
    CFGrammar(category_rules, terminal_rules, startsymbols, Score)
end

################### SCRATCH ################

function DiffCFGrammar(
        category_rules_strings::Vector{Vector{C}},
        terminal_rules_strings::Vector{Vector{T}},
        startsymbols::Vector{C},
        Score,
        dependent_components=identity::Function) where {C,T}

    if length(terminal_rules_strings[1]) > 2 #if probability included in strings

        category_rules_with_probs = [
            (MetaRule([CFRule(s[2],[s[3:end]...])]), Score(parse(Float64, s[1]), islog=true))
            for s in category_rules_strings]
        terminal_rules_with_probs = [
            (MetaRule([CFRule(s[2],[s[3:end]...])]), Score(parse(Float64, s[1]), islog=true))
            for s in terminal_rules_strings]

        category_rules = [mr for (mr,p) in category_rules_with_probs]
        terminal_rules = [mr for (mr,p) in terminal_rules_with_probs]

        comp_automtn = CompletionAutomaton(C, Tuple{C, MetaRule{C, C}})
        for mr in category_rules
            add_rule!(comp_automtn, mr)
        end

        terminal_dict = Dict{T, Vector{Tuple{C, MetaRule{C, T}}}}()
        for mr in terminal_rules
            for lhs in lhss(mr)
                t = mr(lhs)[1]
                if haskey(terminal_dict, t)
                    push!(terminal_dict[t], (lhs, mr))
                else
                    terminal_dict[t] = [(lhs, mr)]
                end
            end
        end

        all_rules_with_probs = [category_rules_with_probs;terminal_rules_with_probs]

        cond = SimpleCond(Dict(
            cat => DiffCatDist(
                (mr,p) for (mr,p) in all_rules_with_probs if isapplicable(mr, cat)
            ))
            for cat in unique(Base.reduce(append!, [lhss(mr) for (mr,p) in all_rules_with_probs], init=T[])))


    else
        category_rules = [MetaRule([CFRule(s[1],[s[2:end]...])]) for s in category_rules_strings]
        terminal_rules = [MetaRule([CFRule(s[1],[s[2:end]...])]) for s in terminal_rules_strings]

            comp_automtn = CompletionAutomaton(C, Tuple{C, MetaRule{C, C}})
        for mr in category_rules
            add_rule!(comp_automtn, mr)
        end

        terminal_dict = Dict{T, Vector{Tuple{C, MetaRule{C, T}}}}()
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

        applicable_rules = Dict{C, Vector{MetaRule}}()
        for r in MetaRule[category_rules; terminal_rules]
            for c in lhss(r)
                if haskey(applicable_rules, c)
                    push!(applicable_rules[c], r)
                else
                    applicable_rules[c] = MetaRule[r]
                end
            end
        end

        cond = SimpleCond(
            Dict(
                dependent_components(c) => let rules = applicable_rules[c]
                    n = length(rules)
                    k = count(isa.(rules, MetaRule{C, T})) # number terminal rules
                    probs = [Score(p) for p in [fill(1.0, n-k); fill(1/k, k)]]
                    DiffCatDist(rules, probs)

                end
                for c in keys(applicable_rules))
            )
    end
    CFGrammar(comp_automtn, startsymbols, terminal_dict, cond, dependent_components)
end


function DiffGrammar(rules_string::AbstractString, startsymbols, Score=LogProb)
    category_rules, terminal_rules = split_category_from_terminal_rules(rules_string)
    category_rules = [ s[2:end] for s in category_rules ]
    terminal_rules = [ s[2:end] for s in terminal_rules ]
    DiffCFGrammar(category_rules, terminal_rules, startsymbols, Score)
end



end
