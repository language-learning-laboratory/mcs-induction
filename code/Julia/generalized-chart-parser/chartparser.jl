module ChartParser
export run_chartparser, CFRules, CFGrammar

include("./pcfg_grammar.jl")
import .PCFGrammar: CFRules, CFGrammar, is_possible_transition, completions, transition, isfinal, startstate, types, startsymbols
#include("./Trees.jl")
# using .Trees: Tree, EmptyTree, TreeNode, isterminal, insert_child!, tree

using DataStructures: PriorityQueue
import Base: +, -, *, /, zero, one, <, ==
import Base: length, insert!, isempty, convert, getindex, promote_rule, range
import DataStructures: enqueue!, dequeue!, peek



###################
### Completions ###
###################

struct TerminalCompletion{T,TR,S}
    terminal :: T
    rule     :: TR
    score    :: S
end
terminal(comp::TerminalCompletion) = comp.terminal
rule(comp::TerminalCompletion) = comp.rule
score(comp::TerminalCompletion) = comp.score

struct EdgeCompletion{E,CR,S}
    edge   :: E
    rule   :: CR
    score  :: S
    inloop :: Bool
end
edge(comp::EdgeCompletion) = comp.edge
rule(comp::EdgeCompletion) = comp.rule
score(comp::EdgeCompletion) = comp.score
inloop(comp::EdgeCompletion) = comp.inloop
EdgeCompletion(edge, rule, score) = EdgeCompletion(edge, rule, score, false)
@inline function ==(c1::EdgeCompletion, c2::EdgeCompletion)
    c1.edge == c2.edge && c1.rule == c2.rule
end


#################
### Traversal ###
#################

struct Traversal{E,CO,S}
    edge   :: Union{E,Nothing}
    cont   :: CO
    score  :: S
    inloop :: Bool
end
Traversal(edge, cont, score) = Traversal(edge, cont, score, false)
Traversal(edge, cont) = Traversal(edge, cont, score(edge) * score(cont), false)
hasedge(trav::Traversal) = !isnothing(trav.edge)
#edge(trav::Traversal) = get(trav.edge)
edge(trav::Traversal) = trav.edge
cont(trav::Traversal) = trav.cont
score(trav::Traversal) = trav.score
inloop(trav::Traversal) = trav.inloop

@inline function ==(t1::Traversal, t2::Traversal)
    if hasedge(t1)
        if hasedge(t2)
            t1.edge == t2.edge && t1.cont == t2.cont
        else
            false
        end
    else
        if hasedge(t2)
            false
        else
            t1.cont == t2.cont
        end
    end
end


##############
### ModInt ###
##############

struct ModInt{n} <: Number
  val::Int
  ModInt{n}(val) where {n} = new(mod(val,n))
end

show(io::IO, a::ModInt{n}) where n = print(io, "$(a.val) mod $n")

+(a::ModInt{n}, b::ModInt{n}) where n = ModInt{n}(a.val + b.val)
-(a::ModInt{n}) where n = - a.val
-(a::ModInt{n}, b::ModInt{n}) where n = ModInt{n}(a.val - b.val)
*(a::ModInt{n}, b::ModInt{n}) where n = ModInt{n}(a.val * b.val)
/(a::ModInt{n}, b::ModInt{n}) where n = a * invmod(b, n)

<(a::ModInt{n}, b::ModInt{n}) where n = a.val < b.val

one(a::ModInt{n}) where n = ModInt{n}(1)
zero(a::ModInt{n}) where n = ModInt{n}(0)

convert(::Type{ModInt{n}}, x::Int) where n = ModInt{n}(x)
convert(::Type{Int}, x::ModInt) = x.val

getindex(t::Union{Tuple, Array}, i::ModInt) = getindex(t, i.val + 1)

promote_rule(::Type{ModInt{n}}, ::Type{Int}) where n = ModInt{n}


#############
### Range ###
#############

abstract type ItemRange end

ItemRange(s::Int, e::Int, n::Int, cyclic::Bool) =
    cyclic ? CyclicRange(s, e, n) : IntervalRange(s, e)

start(r::ItemRange) = r.start
_end(r::ItemRange)  = r._end

struct IntervalRange <: ItemRange
    start :: Int
    _end  :: Int
end

length(r::IntervalRange) = _end(r) - start(r)
concatenable(r1::IntervalRange, r2::IntervalRange) = _end(r1) == start(r2)

function *(r1::IntervalRange, r2::IntervalRange)
    @assert concatenable(r1, r2)
    IntervalRange(start(r1), _end(r2))
end

struct CyclicRange{n} <: ItemRange
    start  :: ModInt{n}
    _end   :: ModInt{n}
    length :: Int
end
CyclicRange(s::ModInt, e::ModInt) = CyclicRange(s, e, Int(e-s))
CyclicRange(s::Int, e::Int, n::Int) = CyclicRange(ModInt{n}(s), ModInt{n}(e))

length(r::CyclicRange) = r.length

@inline function concatenable(r1::CyclicRange{n}, r2::CyclicRange{n}) where n
    _end(r1) == start(r2) && length(r1) + length(r2) <= n
end

function *(r1::CyclicRange, r2::CyclicRange)
    @assert concatenable(r1, r2)
    CyclicRange(start(r1), _end(r2), length(r1) + length(r2))
end

###############
### ItemKey ###
###############

abstract type ItemKey{R} end

range(k::ItemKey)  =          k.range
start(k::ItemKey)  =  start(range(k))
_end(k::ItemKey)   =   _end(range(k))
length(k::ItemKey) = length(range(k))

struct EdgeKey{R,ST} <: ItemKey{R}
    range :: R
    state :: ST
end
state(k::EdgeKey) = k.state

struct ConstituentKey{R,C} <: ItemKey{R}
    range    :: R
    category :: C
end
category(k::ConstituentKey) = k.category

############
### Item ###
############

abstract type Item end

Item(key, trav::Traversal) = Edge(key, trav)
Item(key, comp::EdgeCompletion) = Constituent(key, comp)

key(item::Item) = item.key
range(item::Item) = range(key(item))
start(item::Item) = start(range(item))
_end(item::Item) = _end(range(item))
length(item::Item) = length(range(item))
isfinished(item::Item) = !(isnothing(item.score))
lastpopscore(item::Item) = item.lastpopscore
insidepopnumber(item::Item) = item.insidepopnumber

############
### Edge ###
############

mutable struct Edge{R,ST,S,CO} <: Item
    key             :: EdgeKey{R,ST}
    score           :: Union{S,Nothing}
    lastpopscore    :: S
    insidepopnumber :: Int
    traversals      :: Vector{Traversal{Edge{R,ST,S,CO},CO,S}}
end

@inline function Edge(key, trav::Traversal{E,CO,S}) where {E,CO,S}
    Edge(key, nothing, zero(S), 0, [trav])
end

state(edge::Edge) = state(key(edge))
traversals(edge::Edge) = edge.traversals

@inline function score(edge::Edge)
    if isfinished(edge)
        get(edge.score)
    else
        sum(score(trav) for trav in edge.traversals)
    end
end

function add!(edge::Edge, trav)
    found = false
    for (i, t) in enumerate(edge.traversals)
        if t==trav
            edge.traversals[i] = trav
            found = true
            break
        end
    end
    if !found
        push!(edge.traversals, trav)
    end
    nothing
end

###################
### Constituent ###
###################

mutable struct Constituent{R,C,T,CR,TR,ST,S} <: Item
    key                 :: ConstituentKey{R,C}
    score               :: Union{S,Nothing}
    lastpopscore        :: S
    insidepopnumber     :: Int
    terminal_completion :: Union{TerminalCompletion{T,TR,S}, Nothing}
    completions         :: Vector{EdgeCompletion{Edge{R,ST,S,Constituent{R,C,T,CR,TR,ST,S}},CR,S}}
end

@inline function Constituent(
        key::ConstituentKey{R,C}, comp::TerminalCompletion, grammar
    ) where {R,C}
    C_, T, CR, TR, ST, S = types(grammar)
    Constituent(
        key, nothing, zero(S), 0, comp,
        Vector{EdgeCompletion{Edge{R,ST,S,Constituent{R,C,T,CR,TR,ST,S}},CR,S}}()
    )
end

@inline function Constituent(
        key,
        comp :: EdgeCompletion{Edge{R,ST,S,Constituent{R,C,T,CR,TR,ST,S}},CR,S}
    ) where {R,C,T,CR,TR,ST,S}
    Constituent(
        key, nothing, zero(S), 0,
        nothing,
        [comp]
    )
end

category(cont::Constituent) = category(key(cont))
completions(cont::Constituent) = cont.completions

hasterminal(cont::Constituent) = !(isnothing(cont.terminal_completion))
terminal_completion(cont::Constituent) = cont.terminal_completion
terminal(cont::Constituent) = terminal(cont.terminal_completion)
#terminal_completion(cont::Constituent) = get(cont.terminal_completion)
#terminal(cont::Constituent) = get(cont.terminal_completion).terminal

@inline function score(cont::Constituent)
    if isfinished(cont)
        get(cont.score)
    else
        if hasterminal(cont)
            if isempty(completions(cont))
                score(terminal_completion(cont))
            else
                +(
                    score(terminal_completion(cont)),
                    sum(score(comp) for comp in completions(cont))
                )
            end
        else
            sum(score(comp) for comp in completions(cont))
        end
    end
end

function add!(cont::Constituent, comp)
    found = false
    for (i, c) in enumerate(cont.completions)
        if c==comp
            cont.completions[i] = comp
            found = true
            break
        end
    end
    if !found
        push!(cont.completions, comp)
    end
    nothing
end

@inline function Traversal(cont::Constituent{R,C,T,CR,TR,ST,S}) where {R,C,T,CR,TR,ST,S}
    Traversal{Edge{R,ST,S,Constituent{R,C,T,CR,TR,ST,S}}, Constituent{R,C,T,CR,TR,ST,S}, S}(nothing, cont, score(cont), false)
end

#####################
### ParserLogbook ###
#####################

struct ParserLogbook{R,C,T,CR,TR,ST,S}
    edges :: Dict{EdgeKey{R,ST}, Edge{R,ST,S,Constituent{R,C,T,CR,TR,ST,S}}}
    conts :: Dict{ConstituentKey{R,C}, Constituent{R,C,T,CR,TR,ST,S}}
end

@inline function ParserLogbook(grammar, n::Int, cyclic::Bool)
    R = cyclic ? CyclicRange{n} : IntervalRange
    C,T,CR,TR,ST,S = types(grammar)
    ParserLogbook(
        Dict{EdgeKey{R,ST}, Edge{R,ST,S,Constituent{R,C,T,CR,TR,ST,S}}}(),
        Dict{ConstituentKey{R,C}, Constituent{R,C,T,CR,TR,ST,S}}()
    )
end
discover!(logbook::ParserLogbook, edge::Edge) =
    logbook.edges[key(edge)] = edge
discover!(logbook::ParserLogbook, cont::Constituent) =
    logbook.conts[key(cont)] = cont
isdiscovered(logbook, key::EdgeKey) = haskey(logbook.edges, key)
isdiscovered(logbook, key::ConstituentKey) = haskey(logbook.conts, key)
getitem(logbook, key::EdgeKey) = logbook.edges[key]
getitem(logbook, key::ConstituentKey) = logbook.conts[key]

##################
### ParseChart ###
##################

struct ChartCell{R,C,T,CR,TR,ST,S}
    edges :: Dict{ST, Vector{Edge{R,ST,S,Constituent{R,C,T,CR,TR,ST,S}}}}
    conts :: Dict{C, Vector{Constituent{R,C,T,CR,TR,ST,S}}}
end
@inline function ChartCell(grammar, n::Int, cyclic::Bool)
    R = cyclic ? CyclicRange{n} : IntervalRange
    C,T,CR,TR,ST,S = types(grammar)
    ChartCell(
        Dict{ST, Vector{Edge{R,ST,S,Constituent{R,C,T,CR,TR,ST,S}}}}(),
        Dict{C, Vector{Constituent{R,C,T,CR,TR,ST,S}}}()
    )
end

struct ParseChart{R,C,T,CR,TR,ST,S}
    cells :: Vector{ChartCell{R,C,T,CR,TR,ST,S}}
end
# vector indices begin with 1
# item   indices begin with 0

edges(chart::ParseChart, edge::Edge)        = chart.cells[ _end(edge)+1].edges
edges(chart::ParseChart, cont::Constituent) = chart.cells[start(cont)+1].edges
conts(chart::ParseChart, edge::Edge)        = chart.cells[ _end(edge)+1].conts
conts(chart::ParseChart, cont::Constituent) = chart.cells[start(cont)+1].conts

@inline function push_or_init!(d::Dict, k, v)
    if haskey(d, k)
        push!(d[k], v)
    else
        d[k] = [v]
    end
end
insert!(chart::ParseChart, edge::Edge) =
    push_or_init!(edges(chart, edge), state(edge), edge)
insert!(chart::ParseChart, cont::Constituent) =
    push_or_init!(conts(chart, cont), category(cont), cont)

####################
### InsideAgenda ###
####################

struct InsideAgenda{R,C,T,CR,TR,ST,S}
    edge_queue :: PriorityQueue{Edge{R,ST,S,Constituent{R,C,T,CR,TR,ST,S}}, Int, Base.Order.ForwardOrdering}
    cont_queue :: PriorityQueue{Constituent{R,C,T,CR,TR,ST,S}, Int, Base.Order.ForwardOrdering}
end
function InsideAgenda(grammar, n::Int, cyclic)
    R = cyclic ? CyclicRange{n} : IntervalRange
    C,T,CR,TR,ST,S = types(grammar)
    InsideAgenda(
        PriorityQueue{Edge{R,ST,S,Constituent{R,C,T,CR,TR,ST,S}}, Int}(),
        PriorityQueue{Constituent{R,C,T,CR,TR,ST,S}, Int}()
    )
end
@inline function enqueue!(agenda::InsideAgenda, edge::Edge, just_used)
    agenda.edge_queue[edge] = priority(edge, just_used)
end
@inline function enqueue!(agenda::InsideAgenda, cont::Constituent, just_used)
    agenda.cont_queue[cont] = priority(cont, just_used)
end
@inline function next_is_edge(agenda::InsideAgenda)
    isempty(agenda.cont_queue) || !isempty(agenda.edge_queue) && peek(agenda.edge_queue)[2] < peek(agenda.cont_queue)[2]
end
dequeue_edge!(agenda::InsideAgenda)    = dequeue!(agenda.edge_queue)
dequeue_cont!(agenda::InsideAgenda)    = dequeue!(agenda.cont_queue)
isempty(agenda::InsideAgenda)          = isempty(agenda.edge_queue) && isempty(agenda.cont_queue)
priority(edge::Edge, just_used)        = 4 * length(edge) - 2*!(just_used) - 1
priority(cont::Constituent, just_used) = 4 * length(cont) - 2*!(just_used)


###################
### ParseForest ###
###################

#prob(p::LogProb) = p

struct ParseForest{R,C,T,CR,TR,ST,S}
    heads     :: Vector{Constituent{R,C,T,CR,TR,ST,S}}
    terminals :: Vector{T}
end

function ParseForest(chart::ParseChart, terminals, grammar, cyclic)
    if cyclic
        ParseForest(
            vcat(
                [
                    vcat(
                        map(collect(keys(cell.conts))) do category
                            filter(cell.conts[category]) do cont
                                length(cont) == length(terminals) &&
                                    category in startsymbols(grammar)
                            end
                        end...
                    )
                    for cell in chart.cells
                ]...
            )
            ,
            terminals
        )
    else
        ParseForest(
            vcat(
                map(collect(keys(chart.cells[1].conts))) do category
                    filter(chart.cells[1].conts[category]) do cont
                        length(cont) == length(terminals) &&
                            category in startsymbols(grammar)
                    end
                end...
            ),
            terminals
        )
    end
end

heads(forest::ParseForest) = forest.heads
iscomplete(forest::ParseForest) = !isempty(forest.heads)
score(forest::ParseForest) = sum(score(h) for h in forest.heads)


######################
### Parser Methods ###
######################

function create_or_update!(key, trav_or_comp, agenda, logbook)
    if isdiscovered(logbook, key)
        item = getitem(logbook, key)
        add!(item, trav_or_comp)
    else
        item = Item(key, trav_or_comp)
        discover!(logbook, item)
    end
    enqueue!(agenda, item, false)
    nothing
end

@noinline function initialize(terminals, grammar, epsilon, cyclic)
    n       = length(terminals)
    chart   = ParseChart([ChartCell(grammar, n, cyclic) for i in 0:(cyclic ? n-1 : n)])
    agenda  = InsideAgenda(grammar, n, cyclic)
    logbook = ParserLogbook(grammar, n, cyclic)

    for (i, terminal) in enumerate(terminals)
        for (category, rule, score) in completions(grammar, terminal)
            cont = Constituent(
                ConstituentKey(ItemRange(i-1, i, n, cyclic), category),
                TerminalCompletion(terminal, rule, score),
                grammar
            )
            discover!(logbook, cont)
            enqueue!(agenda, cont, false)
        end
        if !ismissing(epsilon)
            for (category, rule, score) in completions(grammar, epsilon)
                cont = Constituent(
                    ConstituentKey(ItemRange(i-1, i-1, n, cyclic), category),
                    TerminalCompletion(epsilon, rule, score),
                    grammar
                )
                discover!(logbook, cont)
                enqueue!(agenda, cont, false)
            end
        end
    end
    if !ismissing(epsilon) && !cyclic
        for (category, rule, score) in completions(grammar, epsilon)
            cont = Constituent(
                ConstituentKey(ItemRange(n, n, n, cyclic), category),
                TerminalCompletion(epsilon, rule, score),
                grammar
            )
            discover!(logbook, cont)
            enqueue!(agenda, cont, false)
        end
    end
    chart, agenda, logbook
end

@inline function do_fundamental_rule!(
        edge::Edge, chart, agenda, logbook, grammar, cyclic
    )
    for category in keys(conts(chart, edge))
        if is_possible_transition(grammar, state(edge), category)
            for cont in conts(chart, edge)[category]
                if !cyclic || concatenable(range(edge), range(cont))
                    trav      = Traversal(edge, cont)
                    new_state = transition(grammar, state(edge), category)
                    key       = EdgeKey(range(edge) * range(cont), new_state)
                    create_or_update!(key, trav, agenda, logbook)
                end
            end
        end
    end
    nothing
end

@inline function do_fundamental_rule!(
        cont::Constituent, chart, agenda, logbook, grammar, cyclic
    )
    for state in keys(edges(chart, cont))
        if is_possible_transition(grammar, state, category(cont))
            for edge in edges(chart, cont)[state]
                if !cyclic || concatenable(range(edge), range(cont))
                    trav      = Traversal(edge, cont)
                    new_state = transition(grammar, state, category(cont))
                    key       = EdgeKey(range(edge) * range(cont), new_state)
                    create_or_update!(key, trav, agenda, logbook)
                end
            end
        end
    end
    nothing
end

@inline function introduce_edge!(cont, agenda, logbook, grammar)
    if is_possible_transition(grammar, startstate(grammar), category(cont))
        state = transition(grammar, startstate(grammar), category(cont))
        key   = EdgeKey(range(cont), state)
        create_or_update!(key, Traversal(cont), agenda, logbook)
    end
    nothing
end

@noinline function complete_edge!(edge, agenda, logbook::ParserLogbook, grammar)
    C, T, CR, TR, ST, S = types(grammar) #added
    for (category::C, rule::CR, s::S) in completions(grammar, state(edge))
        key  = ConstituentKey(range(edge), category)
        comp = EdgeCompletion(edge, rule, score(edge) * s)
        create_or_update!(key, comp, agenda, logbook)
    end
    nothing
end

@noinline function process_edge!(
        edge, chart, agenda, logbook, grammar, max_pop_num, cyclic
    )
    s = score(edge)
    edge.insidepopnumber += 1
    if s ≈ lastpopscore(edge) || insidepopnumber(edge) == max_pop_num
        if !isfinal(grammar, state(edge))
            insert!(chart, edge)
        end
        edge.score = nothing
        #edge.score = Nullable(s) # finish the edge
        do_fundamental_rule!(edge, chart, agenda, logbook, grammar, cyclic)
    else
        complete_edge!(edge, agenda, logbook, grammar)
        edge.lastpopscore = s
        enqueue!(agenda, edge, true)
    end
    nothing
end

@noinline function process_cont!(
        cont, chart, agenda, logbook, grammar, max_pop_num, cyclic
    )
    s = score(cont)
    cont.insidepopnumber += 1
    if s ≈ lastpopscore(cont) || insidepopnumber(cont) == max_pop_num
        insert!(chart, cont)
        cont.score = nothing
        #cont.score = Nullable(s) # finish the constituent
        do_fundamental_rule!(cont, chart, agenda, logbook, grammar, cyclic)
    else
        introduce_edge!(cont, agenda, logbook, grammar)
        cont.lastpopscore = s
        enqueue!(agenda, cont, true)
    end
    nothing
end

@noinline function loop!(chart, agenda, args...)
    while !isempty(agenda)
        if next_is_edge(agenda)
            process_edge!(dequeue_edge!(agenda), chart, agenda, args...)
        else
            process_cont!(dequeue_cont!(agenda), chart, agenda, args...)
        end
    end
end

@noinline function run_chartparser(
        terminals, grammar; epsilon=missing, max_pop_num=4, cyclic=false
    )
    C, T, CR, TR, ST, S = types(grammar)
    chart, agenda, logbook = initialize(
        T.(terminals), grammar, epsilon, cyclic)
    loop!(chart, agenda, logbook, grammar, max_pop_num, cyclic)
    ParseForest(chart, T.(terminals), grammar, cyclic)
end

end
