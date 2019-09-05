module ChartParser

include("./parser_types.jl")
include("./pcfg_grammar.jl")
using .ParserTypes: Edge, Constituent, ParserLogbook
#using .PCFGGrammar: CFGrammar, is_possible_transition, completions, transition, isfinal

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
        edge.score = Nullable(s) # finish the edge
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
        cont.score = Nullable(s) # finish the constituent
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

@noinline function run_chartparser(terminals, grammar; epsilon=missing, max_pop_num=4, cyclic=false)
    C, T, CR, TR, ST, S = types(grammar)
    chart, agenda, logbook = initialize(
        T.(terminals), grammar, epsilon, cyclic)
    loop!(chart, agenda, logbook, grammar, max_pop_num, cyclic)
    #ParseForest(chart, T.(terminals), grammar, cyclic)
    chart
end

end
