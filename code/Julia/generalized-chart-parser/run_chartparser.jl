
include("./chartparser.jl")
using .ChartParser: run_chartparser, CFRules, CFGrammar

## Test 1

ascend = CFRules(1:9) do i
    [i, i+1]
end
double = CFRules(1:10) do i
    [i, i]
end
terminate = CFRules(1:10) do i
    [string(i)]
end

grammar = CFGrammar([ascend, double], [terminate], [1])

#returns a ParseForest obj that contains all the completed parses from chart
forest = run_chartparser(["1" for i in 1:3], grammar)
print(forest)
