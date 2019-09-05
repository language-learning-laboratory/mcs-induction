

class GrammarInterface:
    def __init__(self, state, category, epsilon, has_epsilon = False):
        self.startstate = state
        self.startcategory = category
        self.epsilon = epsilon
        self.has_epsilon = has_epsilon

    def is_possible_transition(self, state, category):
        #should return bool representing whether a transition is possible
        pass

    def transition(self, state, category):
        #should return new state incorporating the previous state + the category
        pass

    def completions(self, state):
        # should return a a list of all (category, rule, score) that result in the completion of state
        pass

class PCFG(GrammarInterface):
    def __init__(self):
        pass
