nodes <- c("A", "S", "E", "O", "R", "T")
levels <- list(A=c("young", "adult", "old"),
               S=c("M", "F"),
               E=c("high", "uni"),
               O=c("emp", "self"),
               R=c("small", "large"),
               T=c("car", "train", "other"))
arcs <- matrix(c("A", "E",
                 "S", "E",
                 "E", "O",
                 "E", "R",
                 "O", "T",
                 "R", "T"), byrow=TRUE, ncol = 2)

#Test on BulgularyDag as well
bulgularyDag <- model2network("[B][E][A|B:E][J|A][M|A]")
bulgularyLevels <- list(B=c("true", "false"),
                        E=c("true", "false"),
                        A=c("true", "false"),
                        J=c("true", "false"),
                        M=c("true", "false"))

#Making variable to store required parameters for each feature
eq_6_func <- function(nodes, levels, arcs){
  #making a dag
  dag <- empty.graph(nodes = nodes)
  arcs(dag) <- arcs
  
  numberParam <- 0
  for (node in nodes) {
    parents <- parents(dag, node)
    num_levels <- length(levels[[node]])
    
    num_parameters <- (num_levels - 1)
    for (parent in parents){
      num_parameters <- num_parameters*length(levels[[parent]])
    }
    
    numberParam <- numberParam + num_parameters
    
    # Print the number of parameters in local distribution
    cat("Node", node, ":", num_parameters, "parameters\n")
  }
  print(paste("Parameters of the local distributions of the BN: ", numberParam))
}

#write on console: eq_6_func(nodes, levels, arcs) to print sum of local parameters in this distribution