knitr::opts_chunk$set(echo = FALSE)
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(comment = NA)
knitr::opts_chunk$set(warning = FALSE)
options(knitr.table.format = "latex")

### Behavioral Network Science
### Language, Mind, and Society
# By Thomas T. Hills

# Code to reproduce figures and analyses for

#### Chapter 2: Network Metrics ####

library(igraph)
library(tidyverse)
library(latex2exp)
library(igraphdata)

#### Table 2-1 ####

# make list of variable names
labels = c("$N$", "$E$", "$\\rho$", "$L$", "$D$", "$C$", "$k$", "$b$", "$c$", "$x$", "$r$", "$Q$")
# make list of definitions
Definitions =c("Number of nodes",
               "Number of edges",
               "Density",
               "Average shortest path length",
               "Diameter",
               "Clustering Coefficient",
               "Degree",
               "Betweenness centrality",
               "Closeness centrality",
               "Eigenvector centrality",
               "Assortativity",
               "Modularity"
               ) 
# make a data frame from lists
dt = data.frame("Variable"=labels, "Definition" = Definitions)

# make table for data frame
knitr::kable(dt, booktabs = F, escape = FALSE, caption = "Basic network measures.") 


#### Figure 2-1 ####

# set seed
set.seed(5)
# make network
gp <- igraph::sample_gnm(10, 15)
# get nodes on longest shortest path 
diam <- igraph::get_diameter(gp, directed=T)
# make color list for vertices
vcol <- rep("gray60", vcount(gp))
# color vertices on path
vcol[diam] <- "white"
#  make color list for edges
ecol <- rep("gray80", ecount(gp))
# color edges on path
ecol[E(gp, path=diam)] <- "black" 
# make list for edge widths
ewid <- rep(2, ecount(gp))
# change width for edges on path
ewid[E(gp, path=diam)] <- 3
# set laytou
l = igraph::layout_with_fr(gp)

#pdf("Figure2-1.pdf")
# 2 panels
par(mfrow=c(1,2))
# plot network
plot(gp, layout = l, vertex.color=vcol, edge.color=ecol, edge.arrow.mode=0, vertex.label=V(gp)$name, edge.width=ewid, vertex.size=24)
# add text for path length
text(0, -1.2, TeX(r"($d_{1,5}=3$)"))

# make random network with isolate  (trial and error)
gp2 <- igraph::sample_gnm(10, 12)

# plot network
plot(gp2, vertex.color="white", vertex.size = 24)
# add text
text(0, -1.2, TeX(r"($\d_{4,2}=\infty$)"))
#dev.off()



#### Table 2-2 ####

set.seed(2)
# make random graph 
gp <- igraph::sample_gnm(10, 10)
# get matrix of all shortest path lengths between pairs of nodes
d <- igraph::distances(gp)
# add matrix names 
rownames(d) <- c(1:nrow(d))
colnames(d) <- c(1:nrow(d))
# print table to pdf
knitr::kable(d, row.names = TRUE, caption = "The distance table for the graph shown below. Each cell indicates the shortest path between the nodes indicated by the row and column.", format = 'pandoc')

#### Figure 2-2 ####

# set diagonal to NA
diag(d) <- NA
# get average distance for each node and round so it looks nice
dd <- round(rowMeans(d, na.rm=T), 1)
# plot network with labels == <l>
lo = layout_nicely(gp)
lo[,1] = (lo[,1] - min(lo[,1])) / (max(lo[,1]) - min(lo[,1]))
lo[,2] = (lo[,2] - min(lo[,2])) / (max(lo[,2]) - min(lo[,2]))
lo = 2*lo - 1

#pdf("Figure2-2.pdf", width = 5, height = 5)
par(mfrow=c(1,1))
plot(gp, layout = lo, vertex.color="white", vertex.label = dd, vertex.label.cex = .8, vertex.size = 24)
# IDs
text(lo, labels=1:length(V(gp)), pos=4, offset=1, cex = .8)
# get average of average shortest path length

aD <- round(mean_distance(gp, directed = FALSE),2)
# add text label
text(1, -.5, TeX(sprintf(r'($<L> = %f$)', aD)))
#dev.off()

#### Figure 2-3 ####

# set seed
set.seed(2)
# one panel
par(mfrow=c(1,1))
# random network with 28 nodes
gp <- igraph::sample_gnp(28, .1)

# plot network
#pdf("Figure2-3.pdf")
plot(gp, layout=layout_with_fr(gp), vertex.color="white", vertex.label = NA, edge.width=2, edge.color="black", vertex.size=7)
#dev.off()

#### Figure 2-4 ####

# set seed
set.seed(1)
# generate list of network sizes for n
n = seq(10,60, 10) 
# generate list of probabilities 
p = seq(0,.1, .025)

#pdf("Figure2-4.pdf", width = 5, height = 5)
# makes panels
par(mfrow=c(length(n),length(p)))
# across all network sizes
for(i in n){
# margins 
par(mar=c(rep(1.5, 4)))
  # across all probabilities
  for(j in p){
    # make random graph with i nodes and j probability
    gt <-igraph::sample_gnp(i, j)
    # plot graph
    plot(gt, vertex.label=NA, vertex.color="white",edge.color="black")
    # make text labels in appropriate places
    if(j==0) {text(-1.8,0, paste("", i))}
    if(i==10){text(0,1.8,paste("", j), xpd=NA)}
  }
}
#dev.off()

#### Figure 2-5 ####

rm(list=ls())
set.seed(1)
par(mfrow=c(1,1))
# make random graph -- preferential attachment
g <- igraph::ba.game(15, m=1, directed=FALSE)
# label nodes with degree
V(g)$name = igraph::degree(g)

#pdf("Figure2-5.pdf")
# plot graph
plot(g, vertex.size=20, vertex.color="white")
#dev.off()

#### Figure 2-6 ####

# usual stuff
#pdf("Figure2-6.pdf")
set.seed(3)
par(mfrow=c(1,2))
par(mar=c(3,3,3,3))
# make two graph with 3 nomes and 1 or 3 edges
gc1 <- igraph::sample_gnm(3,1)
gc3 <- igraph::sample_gnm(3,3)
# set layout
l = igraph::layout_with_kk(gc3)
# plot graphs
plot(gc3, vertex.color = "white", vertex.label=NA, layout=l, edge.lty=2,edge.color="gray40" )
# add text
text(0, -1.4, "Possible")

plot(gc1, vertex.color = "white", vertex.label=NA, layout=l)
text(0, -1.4, "Observed")
#dev.off()


#### Figure 2-7 ####

# 3 panels
#pdf("Figure2-7.pdf")
par(mfrow=c(1,3))
# make random directed graph
g <- igraph::erdos.renyi.game(10, .25, directed=TRUE)
# layout
l = igraph::layout_with_fr(g)

#plot and label vertices accordingly
plot(g, vertex.size=20, vertex.color="white", vertex.label=igraph::degree(g), layout=l, edge.arrow.size = .5)
# add label
text(0,-2, "Total", cex=2)
plot(g, vertex.size=20, vertex.color="white", vertex.label=igraph::degree(g, mode="out"), layout=l, edge.arrow.size = .5)
text(0,-2, "Outdegree", cex=2)
plot(g, vertex.size=20, vertex.color="white", vertex.label=igraph::degree(g, mode="in"), layout=l, edge.arrow.size = .5)
text(0,-2, "Indegree", cex=2)
#dev.off()


#### Table 2.3 ####

# get adjacency matrix, not sparse
am <- igraph::as_adjacency_matrix(g, sparse=F)
# name matrix rows and columns
rownames(am) <- c(1:10)
colnames(am) <- c(1:10)
# this is used in the next code chunk
am2 <- am
# add column sums 
# add row sums
am <- rbind(am, "Column sums" = colSums(am))
am <- cbind(am, "Row sums" = c(rowSums(am)))
# print to pdf
knitr::kable(am, row.names = TRUE, format = 'pandoc', caption ="The adjacency matrix for a directed network showing the row sums (outdegree) and column sums (indegree), which when summed over all nodes are equivalent.")


#### Figure 2-8 ####

# set seed
set.seed(3)
# set number of nodes
n = 10
# make random graph
gw <- igraph::erdos.renyi.game(n, .2, directed=TRUE)
# make integer weights between 1 and 10
E(gw)$weight <- round(runif(length(E(gw)),1,10))
# assign weights to labels
E(gw)$label <- E(gw)$weight

#pdf("Figure2-8.pdf", width = 6, height = 6)
# 2 x 2 panels
par(mfrow=c(2,2))
# margins
par(mar=c(rep(1,4)))
# x and y coordinates for text and vertex size for plots below
x = .9; y = 1; c =24 
# layout
l = igraph::layout_with_fr(gw)
# plot
plot(gw, vertex.color="white", vertex.size = c, vertex.label=igraph::degree(gw), layout=l, edge.arrow.size = .5)
text(x,y, "Network \n degree", cex=.9)
plot(gw, vertex.color="white",vertex.size= c, vertex.label=igraph::strength(gw), layout=l, edge.arrow.size = .5)
text(x,y, "Weighted total \n strength", cex=.9, xpd = NA)
plot(gw, vertex.color="white", vertex.size = c,vertex.label=igraph::strength(gw, mode="in"), layout=l, edge.arrow.size = .5)
text(x,y, "Weighted \n indegree strength", cex=.9, xpd = NA)
plot(gw, vertex.color="white",vertex.size = c, vertex.label=igraph::strength(gw, mode="out"), layout=l, edge.arrow.size = .5)
text(x,y, "Weighted \n outdegree strength", cex=.9, xpd = NA)
#dev.off()

#### Figure 2-9 ####


# set seed
set.seed(4)
# make graph by stating each edge explicitly
gcc1 <- igraph::graph_from_literal(1-2,
                          1-3,
                          1-4,
                          1-5,
                          1-6)
# make graph by stating each edge explicitly
gcc2 <- igraph::graph_from_literal(1-2,
                          1-3,
                          1-4,
                          1-5,
                          1-6,
                          2-3,
                          2-4)
# make graph by stating each edge explicitly
gcc3 <- igraph::graph_from_literal(1-2,
                          1-3,
                          1-4,
                          1-5,
                          1-6,
                          2-3,
                          2-4,2-5,2-6, 3-4,3-5,3-6,4-6,4-6,5-4,5-6)


#pdf("Figure2-9.pdf")
# make 3 panels
par(mfrow=c(1,3))
# layout
l = igraph::layout_with_fr(gcc1)
# plot and add labels
plot(gcc1, vertex.color = "white", vertex.size=24, layout = l)
text(0, -1.5, "C=0", cex = 1)
plot(gcc2, vertex.color = "white", vertex.size=24, layout = l)
text(0, -1.5, "C=.2", cex = 1)
plot(gcc3, vertex.color = "white", vertex.size=24, layout = l)
text(0, -1.5, "C=1", cex = 1)
#dev.off()

#### Figure 2-10

# set seed
set.seed(2)
# make graph from literal
gc1 <- igraph::graph_from_literal(1-2,2-3)
gc2 <- igraph::graph_from_literal(1-2,2-3, 3-1)
# plot
l = igraph::layout_with_dh(gc2)

#pdf("Figure2-10.pdf")
par(mfrow=c(1,2))
plot(gc1, layout=l, vertex.color = "black")
text(0.3, -1.25, "Intransitive triplet")
plot(gc2, layout=l, vertex.color = "black")
text(0.3, -1.25, "Transitive triplet")
#dev.off()

set.seed(9)
#### Figure 2-11 ####

set.seed(9)
n = 10
mn <- matrix(0, nrow = n, ncol = n)
mn[1:2, 1:n] <- 1
diag(mn) <- 0
gc3 <- igraph::graph_from_adjacency_matrix(mn, mode="undirected")
l=igraph::layout_with_dh(gc3)


#pdf("Figure2-11.pdf")
par(mfrow=c(1,1))
cc <- c("white","white", rep("gray40",n-2))
plot(gc3, layout=l, vertex.color = cc, vertex.label=NA)
tgc3 <- igraph::transitivity(gc3)
text(-.9, -.7, paste("Transitivity = ", toString(tgc3)), cex = .8)
ccgc3 <- round(mean(igraph::transitivity(gc3, type="localundirected")),2)
text(-.9, -1, paste("Average local \n clustering \n coefficient = ", toString(ccgc3)), xpd=NA, cex=.8)
#dev.off()

#### Figure 2-12 ####

# set seed
set.seed(1)

#pdf("Figure2-12.pdf")
# 2 panels
par(mfrow=c(1,3))
# number of nodes
n =3 
# vertex size
c=41
# make random graph
gcl <- igraph::sample_gnp(n, .7)
# compute closeness
V(gcl)$name <- round(igraph::closeness(gcl, normalized = FALSE), 2)
# plot network
plot(gcl, vertex.color = "white", vertex.size = c)
# repeat for a network of size 10
n =10
gcl <- igraph::sample_gnp(n, .22)
l = layout_nicely(gcl)
V(gcl)$name <- round(igraph::closeness(gcl), 2)
plot(gcl, vertex.color = "white", vertex.size = c, layout = l)
V(gcl)$name <- round(igraph::closeness(gcl, normalized = TRUE), 2)
V(gcl)$color = "white"
V(gcl)$color[4] <- "gray80"
plot(gcl, vertex.size = c, layout = l)
V(gcl)$name <- round(igraph::closeness(gcl, normalized = TRUE), 2)
#dev.off()
# #cor.test( dd, V(gp)$name)

#### Figure 2-13 #####

# make literal graph
gbet <- igraph::graph_from_literal(1-2,2-3,
                           4-2,5-2)
# assign betweenness scores
V(gbet)$name <- igraph::betweenness(gbet, normalized = FALSE)

# plot 
#pdf("Figure2-13.pdf")
plot(gbet, vertex.size = c, vertex.color = "white")
#dev.off()


#### Figure 2-14 ####

# vertex size
c=30
#pdf("Figure2-14.pdf")
# 3 panels
par(mfrow=c(1,3))
# margins
par(mar=c(1,1,1,1))
# graph from literal
gbet <- igraph::graph_from_literal(1-2)
# name nodes with eigenvector centrality measures
V(gbet)$name <- round(igraph::eigen_centrality(gbet)$vector, 1)
# plot
plot(gbet, vertex.size = c, vertex.color = "white")
# graph from literal
gbet <- igraph::graph_from_literal(1-2,2-3)
# name nodes with eigenvector centrality measures
V(gbet)$name <- round(igraph::eigen_centrality(gbet)$vector, 1)
# plot
plot(gbet, vertex.size = c, vertex.color = "white")

# as above
gbet <- igraph::graph_from_literal(1-2,2-3, 2-4, 4-5,4-6,4-7)
V(gbet)$name <- round(igraph::eigen_centrality(gbet)$vector, 1)
plot(gbet, vertex.size = c, vertex.color = "white")
#dev.off()




#### Figure 2-15 ####

#pdf("Figure2-15.pdf", width = 8, height=8)
# set seed
set.seed(6)
# 6 panels
par(mfrow=c(3,2))
# margins
par(mar=c(1,1,1,1))
# make body and isolates 
gccomp <- igraph::make_ring(10) +  igraph::vertices(c(11, 12, 13, 14, 15,16,17,18,19))
# connect the parts
gccomp <- gccomp+ igraph::edges(1,5,5,11,1,12,8,13, 14,13,15,13,16,13,17,13, 1,4,5,2, 7,18,9,19 ,2, 4 )
gccomp <- gccomp+ igraph::edges(14,15,16,17, 15,16)

scale_node_size <- function(x){
maxi = 16
mini = 4
 return((x - min(x))/(max(x)-min(x))*(maxi-mini)+mini)
}
# layout
l = igraph::layout_with_fr(gccomp)
# color nodes white
V(gccomp)$color = "white"
# remove names
V(gccomp)$name = NA
# plot basic network 
plot(gccomp, layout = l, vertex.size = 10, main = "The Mouse")
# add degree as size
V(gccomp)$size <- scale_node_size(igraph::degree(gccomp))
# plot
plot(gccomp, vertex.label=NA, main = "Degree", layout = l )
# repeat for each measure below
# cc
V(gccomp)$size <-scale_node_size(igraph::transitivity(gccomp, type = "localundirected" , isolates = "zero"))
plot(gccomp, vertex.label=NA , main = "Clustering coef.", layout=l)
# closeness 
V(gccomp)$size <- scale_node_size(igraph::closeness(gccomp, normalized=TRUE))
plot(gccomp, vertex.label=NA , main = "Closeness", layout=l)
# betweenness 
V(gccomp)$size <- scale_node_size(igraph::betweenness(gccomp))
plot(gccomp, vertex.label=NA , main = "Betweenness", layout=l)
# eigenvector 
V(gccomp)$size <- scale_node_size(igraph::eigen_centrality(gccomp)$vector)
plot(gccomp, vertex.label=NA ,main = "Eigenvector", layout=l)
#dev.off()




#### Figure 2-16 ####
#pdf("Figure2-16.pdf")
par(mfrow=c(2,3))
par(mar=c(1,1,1,1))
set.seed(2)
data(kite)
V(kite)$color = "white"
plot(kite, main = "Kite Network",vertex.label=NA)
plot(kite, vertex.size = scale_node_size(igraph::degree(kite)),vertex.label=NA, main = "Degree")
plot(kite, vertex.size = scale_node_size(igraph::transitivity(kite, type = "localundirected" , isolates = "zero")),vertex.label=NA, main = "Clustering")
plot(kite, vertex.size = scale_node_size(igraph::closeness(kite, normalized=TRUE)),vertex.label=NA, main = "Closeness")

plot(kite, vertex.size = scale_node_size(igraph::betweenness(kite)),vertex.label=NA, main ="Betweenness")
plot(kite, vertex.size = scale_node_size(igraph::eigen_centrality(kite)$vector^4),vertex.label=NA, main = "Eigenvector")
#dev.off()


#### Figure 2-17 ####

# set seed
set.seed(2)
# 20 nodes
n=20
# here I create a simulation to find high and low assortative networks
# number of simulations
sims = 10000
# holder for + and - networks and assortativities 
bestnet <- NA
worstnet <- NA
bestac <- 0
worstac <- 0
# run simulation sims times
for(i in 1:sims){
  # make network
 gass <- igraph::sample_gnp(n,.2)
 # compute assortativity
 assc <-  igraph::assortativity_degree(gass, directed = F)
 # keep the highest or lowest yet
 if (assc < worstac){
   worstnet <- gass
   worstac <- assc
 } 
 if (assc > bestac){
   bestnet <- gass
   bestac <- assc
 } 
}
# 2 panels
#pdf("Figure2-17.pdf")
par(mfrow=c(1,2))
# plot both networks
plot(bestnet, vertex.size=scale_node_size(igraph::degree(bestnet)), vertex.color = "white", vertex.label=NA)
plot(worstnet, vertex.size=scale_node_size(igraph::degree(worstnet)), vertex.color="white", vertex.label=NA)
#dev.off()


#### Figure 2-18 ####

# set seed
#pdf("Figure2-18.pdf")
set.seed(1)
# 20 nodes
n=20
# number of simulations
sims = 10000
# holders for values and networks
bestnet <- NA
worstnet <- NA
bestac <- 0
worstac <- 0
# run simulations
for(i in 1:sims){
  # generate random network
 gass <- igraph::sample_gnp(n,.2)
 # assign random color category to each node
 V(gass)$Group <-   sample(c(1,2), 20, replace = TRUE)
 # compute assortativity with color category
 assc <- igraph::assortativity_nominal(gass, V(gass)$Group, directed =F) 
 # keep best or worst yet 
 if (assc < worstac){
   worstnet <- gass
   worstac <- assc
 } 
 if (assc > bestac){
   bestnet <- gass
   bestac <- assc
 } 
}
# assign colors
colmap <- c("white", "gray50")
V(bestnet)$color <- colmap[V(bestnet)$Group] 
V(worstnet)$color <- colmap[V(worstnet)$Group] 
# plot
par(mfrow=c(1,2))
plot(bestnet, vertex.label=NA)
plot(worstnet, vertex.label=NA)
#dev.off()

#### Figure 2-19 ####

#pdf("Figure2-19.pdf")
# set seed
set.seed(2)
# make graph from literal
gmodex <- igraph::graph_from_literal(1-2,3-4)
# remove labels
V(gmodex)$name = NA
# margins
par(mar=c(2,2,2,2))
# 2 panels
par(mfrow=c(1,2))
# layout
l = igraph::layout_with_fr(gmodex)
# assign colors
colmap <- c("black", "white")
V(gmodex)$Group1 <- c(1,1,2,2)
# plot
plot(gmodex, vertex.color=colmap[V(gmodex)$Group1], layout=l)
# reassign colors
V(gmodex)$Group2<- c(1,2,1,2)
plot(gmodex, vertex.color=colmap[V(gmodex)$Group2], layout=l)
#dev.off()
# to check modularities include groupings
#modularity(gmodex, V(gmodex)$Group1)
#modularity(gmodex, V(gmodex)$Group2)



#### Figure 2-20 ####

#pdf("Figure2-20.pdf")
# set seed
set.seed(1)
# 3 panels
par(mfrow=c(1,3))
# make random graph
gbetex <- igraph::sample_gnp(20,.12)
# remove 3 isolates
gbetex = igraph::delete.vertices(gbetex, which(igraph::degree(gbetex)==0))
# which edge has highest edge betweenness
xx <- which(igraph::edge_betweenness(gbetex)== max(igraph::edge_betweenness(gbetex)))
# set line type for all edge
E(gbetex)$lty <- rep(1, 17)
# set line type to dotted for xx
E(gbetex)$lty[xx] <- 4
# layout 
l = igraph::layout_with_fr(gbetex)
# plot 
plot(gbetex, edge.width=2, vertex.label = NA, vertex.color ="white", edge.color = "black", layout = l)
# delete edge xx 
gbetex2 <- igraph::delete_edges(gbetex, xx)
# find next edge with highest betweenness
xx <- which(igraph::edge_betweenness(gbetex2)== max(igraph::edge_betweenness(gbetex2)))
# repeat above
E(gbetex2)$lty <- rep(1, 16)
E(gbetex2)$lty[xx] <- 4
plot(gbetex2, edge.width=2, vertex.label = NA, vertex.color ="white", edge.color = "black")
gbetex3 <- igraph::delete_edges(gbetex2, xx)
plot(gbetex3, edge.width=2, vertex.label = NA, vertex.color ="white", edge.color = "black")
#dev.off()


#### Figure 2-21 ####
#pdf("Figure2-21.pdf")
# 2 panels
par(mfrow=c(1,2))
# margins
par(mar=c(2,2,2,2))
# run Girvan-Newman algorithm on graph object gbetex
cebetex <- igraph::cluster_edge_betweenness(gbetex) 
# plot dendrogram for result of Girvan Newman
dendPlot(cebetex, mode="hclust", cex=.8, col = "gray80")
# assign line types
E(gbetex)$lty <- 1
# set node colors by community membership
new_cols <- c("white", "gray40", "gray80", "black")[membership(cebetex)]
# plot hypergraph network with cbetex as communities
plot(cebetex, gbetex, col="white", mark.border="gray90", 
     mark.col=c("gray90"), 
    vertex.label.cex=.7, layout = l, vertex.size = 15)
#dev.off()



#### Figure 2-22 ####
#pdf("Figure2-22.pdf")
# set seed
set.seed(1)
# make random graph
gbetex <- igraph::sample_gnp(20,.13)
# fix layout
l = igraph::layout_with_fr(gbetex)
# Girvan-Newman
cebetex <- igraph::cluster_edge_betweenness(gbetex) 
# Louvain
ceblouv <- igraph::cluster_louvain(gbetex,weights=NA) 
# Walktrap
cebwalkt <- igraph::cluster_walktrap(gbetex) 
#plot
par(mar=c(2,2,2,2))
par(mfrow=c(1,4))
c =20 
plot(cebetex, gbetex, col="white", mark.border="gray90", 
     mark.col=c("gray90"), 
    vertex.label.cex=.6, layout = l, vertex.size = c)
cl = -10
title("Girvan-Newman", line = cl)
plot(ceblouv, gbetex, col="white", mark.border="gray90", 
     mark.col=c("gray90"), 
    vertex.label.cex=.6, layout = l, vertex.size = c)
title("Louvain", line = cl)
plot(cebwalkt, gbetex, col="white", mark.border="gray90", 
     mark.col=c("gray90"), 
    vertex.label.cex=.6, layout = l, vertex.size = c)
title("Walktrap", line = cl)
V(gbetex)$color = "white"
V(gbetex)$color[c(4,15,2)] = "black"
plot(gbetex, vertex.label.cex=.6, layout = l, vertex.size = c)
title("Clique percolation", line = cl)
#dev.off()

