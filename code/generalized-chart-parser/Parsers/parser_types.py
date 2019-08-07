
from queue import PriorityQueue
from pcfg_grammar import PCFG:
Score = Integer
Terminal = String


### LogBook ###

class ParserLogBook:
    def __init__(self):
        self.partialitems = list()
        self.completeitems = list()
        self.partialitemkeys = dict()
        self.completeitemkeys = dict()

    def __contains__(self, key):
        return key.is_in_logbook(self)

    def __getitem__(self, key):
        return key.__getitem__(self)

    def get_partialitem(self, id):
        return self.partialitems[id]

    def get_completeitem(self, id):
        return self.completeitems[((-id)-1)]

    def getitem_fromID(self, id):
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


### Interface Types ###

class Traversal:
    def __init__(self, completeitemID: int, partialitemID: int, score: Score, inloop=False):
...     self.completeitemID = completeitemID
...     self.partialitemID = partialitemID
        self.score = score
        self.inloop = inloop

class Completion:
    def __init__(self, score: Score):
        self.score = score

class NonterminalCompletion(Completion):
    def __init__(self, completeitemID: int, rule, score: Score, inloop=False):
        Completion.__init__(self, score)
        self.completeitemID = completeitemID
        self.rule = rule
        self.inloop = inloop

class TerminalCompletion(Completion):
    def __init__(self, terminal: Terminal, rule, score: Score):
        Completion.__init__(self, score)
        self.terminal = terminal
        self.rule = rule



### Type Item Key ###

class PartialItemKey:
    def __init__(self, start, end, state):
        self.start = start
        self.end = end
        self.state = state

    # def __hash__(self):
    #     return hash((self.start, hash(self.end, hash(self.category)))

    def is_in_logbook(self, logbook:ParserLogBook):
        return (self in logbook.partialitemkeys)

    def __getitem__(self, logbook:ParserLogBook):
            return logbook.get_partialitem[logbook.partialitemkeys[self]]



class CompleteItemKey(ItemKey):
    def __init__(self, start, end, category):
        self.start = start
        self.end = end
        self.category = category

    def __eq__(self, other):
        return (self.start == other.start and self.end == other.end and self.category == other.category)

    def is_in_logbook(self, logbook:ParserLogBook):
        return (self in logbook.completeitemkeys)

    def __getitem__(self, logbook:ParserLogBook):
            return logbook.get_completeitem[logbook.completeitemkeys[self]]

### Type Item ###

class Item:
    def __init__(self, id: int, score: Score, lastpopscore):
        self.id = id
        self.score = score
        self.lastpopscore = lastpopscore

# PARTIAL ITEM #

class PartialItem(Item):
    def __init__(self, key: PartialItemKey, traversals, id, score: Score, lastpopscore):
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
                return self
        self.traversals.append(traversal)
        self.score += traversal.score
        return self

# for logbook
    def discover_item(self, logbook: ParserLogBook):
        logbook.partialitems.append(self)
        id = len(logbook.partialitems)-1
        logbook.partialitemkeys[self.key] = id
        self.id = id

# for chart
    def partialitem_ids(self, chart:Chart):
        return chart.cells[self.end].partialitems

    def insert(self, chart: Chart):
        d = self.partialitem_ids(chart)
        if self.state in d and not d.contains(self.id):
            chart.cells.[self.end].partialitems[self.state].append(self.id)
        else:
            chart.cells.[self.end].partialitems[self.state] = [self.id]

# for agenda
    def priority(self):
        p = 2 * self.len() - 1
        return (p, self.id)

# for fundamental rule
    def fundamental_rule(self, chart, agenda, logbook, grammar):
        continue

# for inference rule
    def complete_partialitem(self, agenda, logbook, grammar):
        continue

    def inference_rule(self, agenda, logbook, grammar):
        self.complete_partialitem(agenda, logbook, grammar)

# for inside score update
    def update_inside_score(self):
        continue

# COMPLETE ITEM #

class CompleteItem(Item):
    def __init__(self, key:CompleteItemKey, completions, id, score: Score, lastpopscore):
        Item.__init__(self, id, score, lastpopscore)
        self.start = key.start
        self.end = key.end
        self.category = key.category
        self.key = key
        self.completions = completions
        self.isfinished = False

    def __len__(self):
        return (self.end - self.start)

    def add(self, completion):
        assert isinstance(completion, NonterminalCompletion):
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

    def insert(self, chart: Chart):
        d = self.completeitem_ids(chart)
        if self.category in d and not d.contains(self.id):
            chart.cells.[self.start].completeitems[self.category].append(self.id)
        else:
            chart.cells.[self.start].completeitems[self.category] = [self.id]

# for agenda
    def priority(self):
        p = 2 * self.len()
        return (p, self.id)

# for fundamental rule
    def fundamental_rule(self, chart, agenda, logbook, grammar):
        continue

# for inference rule
    def introduce_partialitem(self, agenda, logbook, grammar):
        continue

    def inference_rule(self, agenda, logbook, grammar):
        self.introduce_partialitem(agenda, logbook, grammar)

# for inside score update
    def update_inside_score(self):
        continue

### ParseForest ###

# class ParseForest:
#     def __init__(chart: Chart, logbook: ParserLogBook, input, grammar):
