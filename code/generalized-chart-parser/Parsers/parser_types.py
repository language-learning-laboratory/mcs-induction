
from queue import PriorityQueue

Score = Integer
Terminal = String


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
    def __init__(self, ranges_seen, state):
        self.ranges_seen = ranges_seen
        self.state = state

    def is_in_logbook(self, logbook:ParserLogBook):
        return (self in logbook.partialitemkeys)

    def __getitem__(self, logbook:ParserLogBook):
            return logbook.partialitems[logbook.partialitemkeys[self]]



class CompleteItemKey(ItemKey):
    def __init__(self, ranges, category):
        self.ranges = ranges
        self.category = category

    def is_in_logbook(self, logbook:ParserLogBook):
        return (self in logbook.completeitemkeys)

    def __getitem__(self, logbook:ParserLogBook):
            return logbook.completeitems[logbook.completeitemkeys[self]]

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
        self.ranges_seen = key.ranges_seen
        self.state = key.state
        #List of Traversal type
        self.traversals = traversals
        self.isfinished = False


    def add(self, traversal: Traversal):
        for trav in self.traversals:
            if trav.partialitemID == traversal.partialitemID and trav.completelitemID == traversal.completeitemID:
                trav.inloop == True
                return traversal
        self.traversals.append(traversal)
        return traversal

    def discover_item(self, logbook: ParserLogBook):
        logbook.partialitems.append(self)
        id = length(logbook.partialitems)-1
        logbook.partialitemkeys[self.key] =
        self.id = id

    def insert(self, chart: Chart):
            if self.state in chart.cells.partialitems :
                chart.cells.partialitems[self.state].append(self.id)
            else:
                chart.cells.partialitems[self.state] = [self.id]



# COMPLETE ITEM #

class CompleteItem(Item):
    def __init__(self, key:CompleteItemKey, completions, id, score: Score, lastpopscore):
        Item.__init__(self, id, score, lastpopscore)
        self.ranges = key.ranges
        self.category = key.category
        self.key = key
        self.completions = completions
        self.isfinished = False

    def add(self, completion):
        assert issubclass(completion, Completion)
        if isinstance(completion, NonterminalCompletion):
            for comp in self.completions:
                if isinstance(comp, NonterminalCompletion) and comp.completeitemID == completion.completeitemID:
                    completion.inloop = True
                    return completion
        self.completions.append(completion)
        self.score += completion.score
        return completion

    def discover_item(self, logbook: ParserLogBook):
        logbook.completeitems.append(self)
        id = length(logbook.completeitems)-1
        logbook.completeitemkeys[self.key] =
        self.id = id

    def insert(self, chart: Chart):
            if self.category in chart.cells.completeitems :
                chart.cells.completeitems[self.category].append(self.id)
            else:
                chart.cells.completeitems[self.category] = [self.id]



### LogBook ###

class ParserLogBook:
    def __init__(self):
        self.partialitems = list()
        self.completeitems = list()
        self.partialitemkeys = dict()
        self.completeitemkeys = dict()

    def __contains__(self, key):
        return key.is_in_logbook(self)

    # def get_partialitem(id):
    #     if isinstance(id, int):
    #         return self.partialitems[id]
    #     else:
    #         return self.partialitems[self.partialitemkeys[id]]
    #
    # def get_completeitem(id):
    #     if isinstance(id, int):
    #         return self.completeitems[id]
    #     else:
    #         return self.completeitems[self.completeitemkeys[id]]

    def __getitem__(key):
        return key.__getitem__(self)



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

    def insert_item(self, item):
        item.insert(self)

### Agenda ###

class Agenda:
    def __init__(self):
        pqueue = PriorityQueue()

    def enqueue(self, item, just_used):
        pqueue.put(item.priority(self))

    def dequeue(self):
