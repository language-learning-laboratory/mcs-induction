from parser_types import Agenda, Chart, ParserLogBook
from pcfg_grammar import GrammarInterface, PCFG


def initialize(grammar, input):
    n = len(input)
    chart = Chart()
    agenda = Agenda()
    logbook = ParserLogBook()

    for i in range(1, n):
        for category, rule, score in grammar.completions(input[i]):
            key = CompleteItemKey(i, i+1, category)
            completion = TerminalCompletion(input[i], rule, score)
            key.update(completion, logbook, agenda)
        if grammar.has_epsilon:
            for category, rule, score in grammar.completions(epsilon):
                key = CompleteItemKey(i, i, category)
                completion = TerminalCompletion(epsilon, rule, score)
                key.update(completion, logbook, agenda)
    return chart, agenda, logbook


def process_item(item, chart, agenda, logbook, grammar):
    if item.no_noteworthy_inside_score_change_since_its_last_dequeue():
        chart.insert(item)
        item.isfinished = True
        item.fundamental_rule(chart, agenda, logbook, grammar)
    else:
        item.inference_rule(agenda, logbook, grammar)
        item.lastpopscore = item.score
        agenda.enqueue(item)

def run_chartparser(grammar, input):
    chart, agenda, logbook = initialize(grammar,input)
    while agenda:
        id = agenda.dequeue
        process_item(logbook[id], chart, agenda, logbook, grammar)
    return (chart, logbook)
