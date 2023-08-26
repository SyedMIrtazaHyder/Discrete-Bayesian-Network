#a)
#In order to install the Rgraphviz package.
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
library(Rgraphviz)
graphviz.plot(dag) #regular plot
graphviz.plot(dag, layout = "neato", shape = "rectangle", main = "DAG with layout = neato and shape = rectangle")
graphviz.plot(dag, layout = "circo", shape = "rectangle", main = "DAG with layout = circo and shape = rectangle")
graphviz.plot(dag, layout = "circo", shape = "ellipse", main = "DAG with layout = circo and shape = ellipse")
graphviz.plot(dag, layout = "fdp", shape = "ellipse", main = "DAG with layout = fdp and shape = ellipse")
graphviz.plot(dag, layout = "twopi", shape = "circle", main = "DAG with layout = twopi and shape = circle")

#b)
aesthetics <- list(nodes = c('A', 'S', 'E', 'R', 'O', 'T'),
                   arcs=newDag$arcs,
                   col = "deepskyblue2",
                   fill = "darkolivegreen2",
                   textCol = "firebrick1",
                   lwd = 2,
                   lty = 5)
#Plot the DAG.
graphviz.plot(newDag, layout = "neato", shape="circle",
              main = "Burglary Network (parents of A)",
              highlight = aesthetics)

#c)
plotDag <- graphviz.plot(newDag)

#1st path
edgeRenderInfo(plotDag) <- list(col = c("A~E" = "orange2",
                                        "E~R" = "orange2",
                                        "R~T" = "orange2"),
                                lwd = c("A~E" = 3, "E~R" = 3, "R~T" = 3),
                                lty = c("A~E" = 2, "E~R" = 2, "R~T" = 2))
#2nd path.
edgeRenderInfo(plotDag) <- list(col = c("S~E" = "dodgerblue2",
                                        "E~O" = "dodgerblue2",
                                        "O~T" = "dodgerblue2"),
                                lwd = c("S~E" = 2, "E~O" = 2, "O~T" = 2),
                                lty = c("S~E" = 1, "E~O" = 1, "O~T" = 1))
#Modify the aesthetics of nodes B, E and A.
nodeRenderInfo(plotDag) <- list(col = c("A" = "yellow",
                                        "S" = "yellow",
                                        "E" = "goldenrod1"),
                                textCol = c("A" = "black",
                                            "S" = "black",
                                            "E" = "bisque"),
                                fill = c("A" = "grey",
                                         "S" = "grey",
                                         "E" = "darkorchid"),
                                fontsize = 9)
#Continue modifying the aesthetics of the nodes.
nodeRenderInfo(plotDag) <- list(col = c("O" = "gold",
                                        "R" = "gold",
                                        "T" = "cyan4"),
                                textCol = c("O" = "darksalmon",
                                            "R" = "darksalmon",
                                            "T" = "darkgrey"),
                                fill = c("O" = "darkseagreen1",
                                         "R" = "darkseagreen1",
                                         "T" = "coral"),
                                lty="solid",
                                fontsize = 7)
#Display the final result in the "Plots" tab of RStudio.
renderGraph(plotDag)

#d)
#Representation of the CPT of Age with bar chart
#and dot plot.
bn.fit.barchart(bn$A, main = "CPT of Age (A)",
                xlab = "P(A)", ylab = "")
bn.fit.dotplot(bn$A, main = "CPT of Age (A)",
               xlab = "P(A)", ylab = "")

#Representation of the CPT of Sex with bar chart
#and dot plot.
bn.fit.barchart(bn$S, main = "CPT of Sex (S)",
                xlab = "P(S)", ylab = "")
bn.fit.dotplot(bn$S, main = "CPT of Sex (S)",
               xlab = "P(S)", ylab = "")

#Representation of the CPT of Education given Age and Sex with bar chart
#and dot plot.
bn.fit.barchart(bn$E, main = "CPT of Education given Age and E|A,Sex (E|A,S)",
                xlab = "P(E|A,S)", ylab = "")
bn.fit.dotplot(bn$E, main = "CPT of Education given Age and E|A,Sex (E|A,S)",
               xlab = "P(E|A,S)", ylab = "")

#Representation of the CPT of Residence given Education with bar chart
#and dot plot.
bn.fit.barchart(bn$R, main = "CPT of Residence given Education (R|E)",
                xlab = "P(R|E)", ylab = "")
bn.fit.dotplot(bn$R, main = "CPT of Residence given Education (R|E)",
               xlab = "P(R|E)", ylab = "")

#Representation of the CPT of Occupation given Education with bar chart
#and dot plot.
bn.fit.barchart(bn$O, main = "CPT of Occupation given Education (O|E)",
                xlab = "P(O|E)", ylab = "")
bn.fit.dotplot(bn$O, main = "CPT of Occupation given Education (O|E)",
               xlab = "P(O|E)", ylab = "")

#Representation of the CPT of Transport given Residence and Occupation with bar chart
#and dot plot.
bn.fit.barchart(bn$T, main = "CPT of Transport given Residence and Occupation (T|R,O)",
                xlab = "P(T|R,O)", ylab = "")
bn.fit.dotplot(bn$T, main = "CPT of Transport given Residence and Occupation (T|R,O)",
               xlab = "P(T|R,O)", ylab = "")