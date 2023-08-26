#a)
library(bnlearn)
#1. Making DAG
dag <- empty.graph(c('A', 'S', 'E', 'R', 'O', 'T'))
#2. Adding directed edges
dag <- set.arc(dag, from = "A", to = "E")
dag <- set.arc(dag, from = "S", to = "E")
dag <- set.arc(dag, from = "E", to = "R")
dag <- set.arc(dag, from = "E", to = "O")
dag <- set.arc(dag, from = "R", to = "T")
dag <- set.arc(dag, from = "O", to = "T")
#3. We are given the Factorized Probability Distribition
# of the Bayesian Network: 
#model:
#  [A][S][E|A:S][R|E][O|E][T|R:O]
# Based on theorem 2 as P factorizes according
# to original BN G, G is an I-map of P
# hence our created DAG is correct

#b)
#if R->S is introduced, the graph will have a cycle and no longer will remain a DAG
# It will no longer be a Bayesian Network
nodes(dag) # gives us all nodes in DAG
arcs(dag) # gives us all the arcs in DAG
parents(dag, 'E') # to obtain parents of given node
children(dag, 'E') # to obtain children of given node
modelstring(dag) #returns string containing all local dependencies
#c)
otherDag <- empty.graph(nodes = c('A', 'S', 'E', 'R', 'O', 'T'))
arcs <- matrix(c("A", "E",
                 "S", "E",
                 "E", "R",
                 "E", "O",
                 "R", "T",
                 "O", "T"), byrow=TRUE, ncol = 2)
arcs(otherDag) <- arcs
all.equal(dag, otherDag)

#d
lastDag <- model2network("[A][S][E|A:S][R|E][O|E][T|R:O]")
all.equal(dag, lastDag)