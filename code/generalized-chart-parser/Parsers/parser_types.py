import math
from queue import PriorityQueue
from pcfg_grammar import GrammarInterface, PCFG

### Interface Types ###

class Traversal:
    def __init__(self, completeitemID, partialitemID, score, inloop=False):
        self.completeitemID = completeitemID
        self.partialitemID = partialitemID
        self.score = score
        self.inloop = inloop

class Completion:
    def __init__(self, score):
        pass

class NonterminalCompletion(Completion):
    def __init__(self, completeitemID, rule, score, inloop=False):
        self.score = score
        self.completeitemID = completeitemID
        self.rule = rule
        self.inloop = inloop

class TerminalCompletion(Completion):
    def __init__(self, terminal, rule, score):
        self.score = score
        self.terminal = terminal
        self.rule = rule


### LogBook ###

class ParserLogBook:
    def __init__(self):
        self.partialitems = list()
        self.completeitems = list()
        self.partialitemkeys = dict()
        self.completeitemkeys = dict()

    def __contains__(self, key):
        return key.is_in_logbook(self)

    def get_atkey(self, key):
        return key[self]

    def get_partialitem(self, id):
        return self.partialitems[(id-1)]

    def get_completeitem(self, id):
        return self.completeitems[((-id)-1)]

    def __getitem__(self, id):
        if id <= 0:
            return self.get_partialitem(id)
        else:
            return self.get_completeitem(id)

    def discover_item(self, item):
        item.discover_item(self)


### Chart ###

class ChartCell:
    def __init__(self):
        # keys = state
        self.completeitems = dict()
        # keys = category
        self.partialitems = dict()

class Chart:
    def __init__(self):
        self.cells = list()


    def insert(self, item):
        item.insert(self)


### Agenda ###

class Agenda:
    def __init__(self):
        pqueue = PriorityQueue()

    def enqueue(self, item):
        self.pqueue.put(item.priority())

    def dequeue(self):
        return self.pqueue.get()[1]

    def __bool__(self):
        return (not self.pqueue.empty())


### Type Item Key ###

class PartialItemKey:
    def __init__(self, start, end, state):
        self.start = start
        self.end = end
        self.state = state

    def __hash__(self):
        return hash((self.start, self.end, self.state))

    def __eq__(self, other):
        return (self.start == other.start and self.end == other.end and self.state == other.state)

    def is_in_logbook(self, logbook:ParserLogBook):
        return (self in logbook.partialitemkeys)

    def __getitem__(self, logbook:ParserLogBook):
            return logbook.get_partialitem[logbook.partialitemkeys[self]]

    def update(self, traversal: Traversal, logbook:ParserLogBook, agenda: Agenda):
        if self in logbook:
            partialitem = logbook.get_atkey(self)
            partialitem.add(traversal)
        else:
            partialitem = PartialItem(self, [traversal], 0, traversal.score, (traversal.score - traversal.score))
            logbook.discover_item(partialitem)
        agenda.enqueue(partialitem)



class CompleteItemKey:
    def __init__(self, start, end, category):
        self.start = start
        self.end = end
        self.category = category

    def __hash__(self):
        return hash((self.start, self.end, self.category))

    def __eq__(self, other):
        return (self.start == other.start and self.end == other.end and self.category == other.category)

    def is_in_logbook(self, logbook:ParserLogBook):
        return (self in logbook.completeitemkeys)

    def __getitem__(self, logbook:ParserLogBook):
            return logbook.get_completeitem[logbook.completeitemkeys[self]]

    def update(self, completion: NonterminalCompletion, logbook:ParserLogBook, agenda: Agenda):
        if self in logbook:
            completeitem = logbook.get_atkey(self)
            completeitem.add(completion)
        else:
            completeitem = CompleteItem(self, [completion], 0, completion.score, (completion.score - completion.score))
            logbook.discover_item(completeitem)
        agenda.enqueue(completeitem)

### Type Item ###

class Item:
    def __init__(self, id: int, score, lastpopscore):
        self.id = id
        self.score = score
        self.lastpopscore = lastpopscore

    def no_noteworthy_inside_score_change_since_its_last_dequeue(self):
        return math.isclose(self.score, self.lastpopscore)


# PARTIAL ITEM #

class PartialItem(Item):
    def __init__(self, key: PartialItemKey, traversals, id, score, lastpopscore):
        Item.__init__(self, id, score, lastpopscore)
        self.key = key
        self.start = key.start
        self.end = key.end
        self.state = key.state
        #List of Traversal type
        self.traversals = traversals
        self.isfinished = False

    def __len__(self):
        return (self.end - self.start)

    def add(self, traversal: Traversal):
        for trav in self.traversals:
            if trav.partialitemID == traversal.partialitemID and trav.completelitemID == traversal.completeitemID:
                trav.inloop = True
                return
        self.traversals.append(traversal)
        self.score += traversal.score

# for logbook
    def discover_item(self, logbook: ParserLogBook):
        logbook.partialitems.append(self)
        id = len(logbook.partialitems)
        logbook.partialitemkeys[self.key] = id
        self.id = id

# for chart
    def partialitem_ids(self, chart:Chart):
        return chart.cells[self.end].partialitems

    def completeitem_ids(self, chart: Chart):
        return chart.cells[self.end].completeitems

    def insert(self, chart: Chart):
        d = self.partialitem_ids(chart)
        if self.state in d :
            if not d.contains(self.id):
                chart.cells[self.end].partialitems[self.state].append(self.id)
        else:
            chart.cells[self.end].partialitems[self.state] = [self.id]

# for agenda
    def priority(self):
        p = 2 * self.len() - 1
        return (p, self.id)

# for fundamental rule
    def fundamental_rule(self, chart: Chart, agenda: Agenda, logbook: ParserLogBook, grammar: GrammarInterface):
        for category in self.completeitem_ids(chart):
            if grammar.is_possible_transition(self.state, category):
                for id in self.completeitem_ids(chart)[category]:
                    completeitem = logbook[id]
                    nstate = grammar.transition(self.state, category)
                    nkey = PartialItemKey(self.start, completeitem.end, nstate)
                    traversal = Traversal(self.id, id, self.score * completeitem.score)
                    nkey.update(traversal, logbook, agenda)


# for inference rule
    def complete_partialitem(self, agenda, logbook, grammar):
        for category, rule, score in grammar.completions(self.state):
            key = CompleteItemKey(self.start, self.end, category)
            completion = NonterminalCompletion(self.id, rule, self.score * score)
            key.update(completion, logbook, agenda)

    def inference_rule(self, agenda: Agenda, logbook: ParserLogBook, grammar: GrammarInterface):
        self.complete_partialitem(agenda, logbook, grammar)


# COMPLETE ITEM #

class CompleteItem(Item):
    def __init__(self, key:CompleteItemKey, completions, id, score, lastpopscore):
        Item.__init__(self, id, score, lastpopscore)
        self.start = key.start
        self.end = key.end
        self.category = key.category
        self.key = key
        self.completions = completions
        self.isfinished = False

    def __len__(self):
        return (self.end - self.start)

    def add(self, completion: NonterminalCompletion):
        for comp in self.completions:
            if comp.completeitemID == completion.completeitemID:
                comp.inloop = True
                return completion
        self.completions.append(completion)
        self.score += completion.score
        return completion

# for logbook
    def discover_item(self, logbook: ParserLogBook):
        logbook.completeitems.append(self)
        id = -(len(logbook.completeitems))
        logbook.completeitemkeys[self.key] = id
        self.id = id

# for chart
    def completeitem_ids(self, chart:Chart):
        return chart.cells[self.start].completeitems

    def partialitem_ids(self, chart:Chart):
        return chart.cells[self.start].partialitems

    def insert(self, chart: Chart):
        d = self.completeitem_ids(chart)
        if self.category in d :
            if not d.contains(self.id):
                chart.cells[self.start].completeitems[self.category].append(self.id)
        else:
            chart.cells[self.start].completeitems[self.category] = [self.id]

# for agenda
    def priority(self):
        p = 2 * self.len()
        return (p, self.id)

# for fundamental rule
    def fundamental_rule(self, chart: Chart, agenda: Agenda, logbook: ParserLogBook, grammar: GrammarInterface):
        for state in self.partialitem_ids(chart):
            if grammar.is_possible_transition(state, self.category):
                for id in self.partialitem_ids(chart)[state]:
                    partialitem = logbook[id]
                    nstate = grammar.transition(state, self.category)
                    nkey = PartialItemKey(partialitem.start, self.end, nstate)
                    traversal = Traversal(id, self.id, partialitem.score * self.score)
                    nkey.update(traversal, logbook, agenda)

# for inference rule
    def introduce_partialitem(self, agenda, logbook, grammar):
        if grammar.is_possible_transition(grammar.startstate, self.category):
            state = grammar.transition(grammar.startstate, self.category)
            key = PartialItemKey(self.start, self.end, state)
            traversal = Traversal(0, self.id, self.score)
            key.update(traversal, logbook, agenda)

    def inference_rule(self, agenda: Agenda, logbook: ParserLogBook, grammar: GrammarInterface):
        self.introduce_partialitem(agenda, logbook, grammar)


### ParseForest ###

# class ParseForest:
#     def __init__(chart: Chart, logbook: ParserLogBook, input, grammar):
