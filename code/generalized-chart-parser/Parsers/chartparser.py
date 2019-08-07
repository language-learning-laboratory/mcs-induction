from parser_types import Agenda, Chart, ParserLogBook, Traversal, Completion, TerminalCompletion, NonterminalCompletion
from pcfg_grammar import PCFG

# TODO
def initialize(grammar, input):
    chart = Chart([ChartCell()])
    return chart, agenda, logbook


def process_item(item, chart, agenda, logbook, grammar):
    if no_noteworthy_inside_score_change_since_its_last_dequeue(item):
        chart.insert(item)
        item.fundamental_rule(chart, agenda, logbook, grammar)
    else:
        item.inference_rule(agenda, logbook, grammar)
        item.update_inside_score()
        agenda.enqueue(item, True)


def run_chartparser(grammar, input):
    chart, agenda, logbook = initialize(grammar,input)
    while agenda:
        itemID = agenda.dequeue
        process_item(logbook.getitem_fromID(itemID), chart, agenda, logbook, grammar)
    return (chart, logbook)
