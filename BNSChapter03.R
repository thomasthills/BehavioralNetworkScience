knitr::opts_chunk$set(echo = FALSE)
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(comment = NA)
knitr::opts_chunk$set(warning = FALSE)
options(knitr.table.format = "latex")

### Behavioral Network Science
### Language, Mind, and Society
# By Thomas T. Hills

# Code to reproduce figures and analyses for

#### Chapter 3 ####

library(igraph)
library(tidyverse)
library(igraphdata)
library(kableExtra)


#### Figure 3-1 ####

# set seed
set.seed(1)
# set probability
p = .2
# make fully connected graph
endg <- igraph::make_full_graph(5)
l = igraph::layout_with_fr(endg)
# color all nodes gray
E(endg)$color <- "gray50" 
# make dotted edges
E(endg)$lty  <- 2 
# name nodes
E(endg)$name <- 1:10
# set up a multi-panel layout of different sizes
layout(matrix(c(1,1,2,3,4,5,6,1,1,7, 8, 9, 10, 11), 2, 7, byrow = TRUE))
# plot the full graph with dotted edges
plot(igraph::make_full_graph(5), vertex.color="white", vertex.label = NA, edge.lty = 2, layout = l, edge.label = E(endg)$name, edge.label.cex = 1.2, ledge.label.dist=20)
# set margins
par(mar=c(1,1,1,1))
# for each edge
for(i in 1:length(E(endg))){
  # color the ith node white
  E(endg)$color[i] <- "white"
  # set the ith edge to solid
  E(endg)$lty[i]  <- 1
  # flip a coint
  if(rbinom(1,1,p)==1)  {
    # if heads, make the edge black
    E(endg)$color[i]  <- "black" 
  }
# plot network
plot(endg, vertex.color="white", vertex.label = NA, layout = l)
# add label
text(0,-1.6, i)
}




#### Figure 3-2 ####

# set seed
set.seed(2)
# margins
par(mar=c(2,2,2,2))
# nodes
n = 20
# edge probabilities
p = .2
# simulations
sims = 1000
# keep track of density, cc, and aspl
dens <- rep(0, sims)
cc <- rep(0, sims)
aspl <- rep(0, sims)
# run simulations
for(i in 1:sims){
  # make network
  gfs <- igraph::sample_gnp(n,p)
  # compute statistics
  dens[i] <- igraph::edge_density(gfs)
  cc[i] <- igraph::transitivity(gfs, type="undirected")
  aspl[i] <- igraph::mean_distance(gfs, directed = FALSE)
}
# make table for each parameter
dst <- table(dens)
cct <- table(cc)
asplt <- table(aspl)
# round numbers to make pretty
names(dst) <- round(as.numeric(names(dst)),2)
# normalize
dst <- dst/sum(dst)
cct <- cct/sum(cct)
asplt <- asplt/sum(asplt)
# make names prettier
names(cct) <- round(as.numeric(names(cct)),2)
names(asplt) <- round(as.numeric(names(asplt)),2)
# multi-panel layout
layout(matrix(c(1,2,3,4,5,6), 3, 2, byrow = FALSE))
# plot three random graphs
plot(igraph::sample_gnp(n,p), vertex.size = 20, vertex.color = "white", vertex.label=NA)
plot(igraph::sample_gnp(n,p), vertex.size = 20, vertex.color = "white", vertex.label=NA)
plot(igraph::sample_gnp(n,p), vertex.size = 20, vertex.color = "white", vertex.label=NA)
# margins
par(mar=c(5,5,1,2))
# plot histograms from tables
plot(dst, ylab = "Probability", xlab="Density")
plot(cct, ylab = "Probability", xlab="Transitivity")
plot(asplt, ylab = "Probability", xlab="ASPL")




#### Figure 3-3 ####

# set seed
set.seed(4)
# assign probabilities of rewiring
pv <- c(0,.05, .1, 1)
# make panels
par(mfrow=c(1,length(pv)))
# nodes
n=20
# for each probability
for(i in pv){
  # make small world network
  gsw <- igraph::sample_smallworld(1,n,2, p=i)
  # layout
  l = igraph::layout_in_circle(gsw)
  # plot
  plot(gsw, layout = l, vertex.color = "white", vertex.label = NA)
  # compute local cc
  mc <- round(mean(igraph::transitivity(gsw, type="localundirected", isolates = "Zero")),1)
  # compute aspl
  md <- round(igraph::mean_distance(gsw, directed = FALSE),1)
  # print label
  text(0,-1.8, paste("p=", i," \n C=", mc, "\n ASPL = ",md ))
}




#### Figure 3-4 ####

# set seed
set.seed(1)
# 2 panels
par(mfrow=c(1,2))
# nodes
n = 100
# number of links to add for each node added
m = 1
# make preferential attachment network with 100 nodes
gpa <- igraph::sample_pa(n, m=m, power=1, out.pref=TRUE, zero.appeal=1, directed=FALSE, algorithm="psumtree")
# plot network
plot(gpa, vertex.label=NA, vertex.size = igraph::degree(gpa)/2, vertex.color ="black")

# number links to add for each node added
m = 2
# make preferential attachment network with 100 nodes
gpa <- igraph::sample_pa(100, m=m, power=1, out.pref=TRUE, zero.appeal=1, directed=FALSE, algorithm="psumtree")
# plot networks
plot(gpa, vertex.label=NA, vertex.size = igraph::degree(gpa)/2, vertex.color ="black")


#### Figure 3-5 ####

# set seed
set.seed(1)
# 20 nodes
n = 20
# figure parameters
par(mfrow=c(4,5))
par(mar=c(2,2,2,2))
# make empty graph with 1 node
gpa2 <- igraph::make_empty_graph(1)
# make list for graphs
glist <- list()
# assign first element of list
glist[[1]] <- gpa2
# for each added node
for(i in 2:n){
  # make bag of existing node
  # one for each degree plus a = 1
   nodesamplelist <- rep(1:length(V(gpa2)),igraph::degree(gpa2)+1)
   # sample a node from the list
   newnode <- sample(nodesamplelist, 1)
   # add the node
   gpa2 <- gpa2 %>% igraph::add_vertices(1)
   # add the edge from new node to sampled newnode
   gpa2 <- gpa2 %>% igraph::add_edges(c(i,newnode))
   # save graph
   glist[[i]] <- gpa2
}
# layout
l = igraph::layout_with_fr(gpa2)
# set i = 1 for iterator
i = 1
# plot first graph
  plot(glist[[1]], vertex.label=NA, vertex.color = "white", layout = t(as.matrix(l[1,])), edge.arrow.size = 0, vertex.size = rep(c(15,0),times=c(i,n-i)))  
# for remaining nodes
for(i in 2:n){
  # plot ith graph
  plot(glist[[i]], vertex.label=NA, vertex.color = rep(c("black","white"),times=c(i-1,n-(i-1))), layout = as.matrix(l[1:i,]),
       edge.arrow.size = 0, vertex.size = rep(c(15,0),times=c(i,n-i)))  
}


#### Figure 3-6 ####

# set seed
set.seed(1)
par(mfrow=c(1,2))

# based on Goh, Kahng, & Kim, 2001
# nodes
n = 20
# edges
m = 60
# power on rank of fitness distribution: if this is 1, then fitness increases linearly with respect to rank
alpha <- 1
# function to return fitness
fita <- function(i,a){
 return(i^{-a}) 
}
# vector of nodes
x <- 1:n
# assign fitness to nodes
fitness.scores <- fita(x, alpha)
# make fitness network with edges proportional to fitness scores
gerf <- igraph::sample_fitness(m, fitness.scores)
# plot it
plot(gerf, vertex.label = NA, vertex.size = igraph::degree(gerf), vertex.color = "white")
# add lable
text(0, -1.2, "ER fitness graph")
# margins for figure
par(mar=c(4.5,4,6,3))
# plot fitness against degree
plot(fitness.scores, igraph::degree(gerf), xlab = "Node fitness", ylab="Node degree", pch = 16, cex = .5)



#### Figure 3-7 ####

# set seed
set.seed(1)
# nodes
n =20 
# figure parameters
par(mfrow=c(4,5))
par(mar=c(2,2,2,2))
# make empty graph
gpaf <- igraph::make_empty_graph(1)
# assign fitness to first node, uniform random number between 0 and 1
V(gpaf)$fitness[1] <- runif(1)
# make graph list
glist <- list()
# add first graph
glist[[1]] <- gpaf
# add node
gpaf <- gpaf %>% igraph::add_vertices(1)
# add edge
gpaf <- gpaf %>% igraph::add_edges(c(2,1))
# add fitness
V(gpaf)$fitness[2] <- runif(1)
# add graph
glist[[2]] <- gpaf
# for remaining nodes
for(i in 3:n){
   # make node list
   x <- 1:(i-1)
   # sample one based on combination of fitness and degree
   edge_to <- sample(x,1, prob=(igraph::degree(gpaf))*V(gpaf)$fitness)
   # add new node
   gpaf <- gpaf %>% igraph::add_vertices(1)
   # make edge to sampled node
   gpaf <- gpaf %>% igraph::add_edges(c(i,edge_to))
   # add fitness value to new node
   V(gpaf)$fitness[i] <- runif(1)
   # save graph
   glist[[i]] <- gpaf
}
# layout
l = igraph::layout_with_fr(gpaf)
# iterator
i = 1
# plot first
  plot(glist[[1]], vertex.label=NA, vertex.color = "white", layout = t(as.matrix(l[1,])), edge.arrow.size = 0, vertex.size = rep(c(15,0),times=c(i,n-i)))  
# for remainder of nodes 
for(i in 2:length(glist)){
  #plot each network in turn
  plot(glist[[i]], vertex.label=NA, vertex.color = rep(c("black","white"),times=c(i-1,n-(i-1))), layout = as.matrix(l[1:i,]), edge.arrow.size = 0, vertex.size = rep(c(25,0),times=c(i,n-i))*V(gpaf)$fitness)  
  
}



#### Figure 3-8 ####

# set seed
set.seed(5)
# plot parameters
par(mfrow=c(1,3))
par(mar=c(1,1,1,1))
# make the Mouse
gccomp <- igraph::make_ring(10) +  igraph::vertices(c(11, 12, 13, 14, 15,16,17,18,19))
                        # , 20, 21)) # add hind legs
gccomp <- gccomp+ igraph::edges(1,5,5,11,1,12,8,13, 14,13,15,13,16,13,17,13,
                        #14,15, 15,16,16,17,# remove to remove pompom
                        #18,20,19,21, # add long hind legs
                        1,4,5,2, 7,18,9,19 ,2, 4 )
gccomp <- gccomp+ igraph::edges(14,15,16,17, 15,16)

# set layout
l = igraph::layout_with_fr(gccomp)
# node color
V(gccomp)$color = "white"
# name nodes
V(gccomp)$name = c(1:19)
# plot the mouse
plot(gccomp, layout=l)
# plot mouse with dotted edges
plot(gccomp, edge.lty=2, layout=l)
# rewire the network keeping degree sequence
gconfig <- gccomp %>%
  rewire(igraph::keeping_degseq(niter = 20))
# plot network
plot(gconfig, edge.lty=1)




#### Figure 3-9 ####

# set seed
set.seed(1)
# nodes
n=20
# probabilities of edges
po =.1 
pc =.5 
# 2 panels
par(mfrow=c(1,2))
# start with a full network to create all edges for evaluation
ggnc <- igraph::make_full_graph(n) 
# assign group membership
groupcol <- c(rep("black",n/2), rep("white",n/2)) 
# assign groups to nodes -- also colors at the same time
V(ggnc)$color <- sample(groupcol,n)
# list of edges created by edge-wise evaluation  
list_of_edges_to_delete <- c()
# for all edges in full network
for(i in 1:length(E(ggnc))){
  # if same or different group, see if edge survives
  # for same group
  if(V(ggnc)$color[ends(ggnc,i)[,1]] == V(ggnc)$color[ends(ggnc,i)[,2]]){
   if(runif(1) <= 1-pc) list_of_edges_to_delete <- c(list_of_edges_to_delete, i)
  }
  # for different group
  if(V(ggnc)$color[ends(ggnc,i)[,1]] != V(ggnc)$color[ends(ggnc,i)[,2]]){
   if(runif(1) <= 1-po) list_of_edges_to_delete <- c(list_of_edges_to_delete, i)
  }
}
# delete all edges to plot isolate group members
ggnc_empty <- ggnc %>% igraph::delete_edges(1:length(E(ggnc)))
# delete edges that didn't survive to preserve final network
ggnc_final <- ggnc %>% igraph::delete_edges(list_of_edges_to_delete)
# plot networks and labels
plot(ggnc_empty, vertex.label=NA)
text(0,-1.4, "Step 1: Assign membership")
plot(ggnc_final, vertex.label=NA )
text(0,-1.4, "Step 2: Form edges")



#### Figure 3-10 ####

# set seed
set.seed(8) # change or remove to allow for random generations with same parameters
# nodes -- play with this and probability to get a sense of how this works
n = 50
# probability
p = .94
# make preferential attachment network
gff <- igraph::sample_pa(n,directed = FALSE) # make a starting BA graph with n nodes
# color nodes
V(gff)$color = "white"
# count edges
starting.edge.count <- length(E(gff))
# find an ambassador -- first node
ambassadors <- sample(1:n, 1)
# add nodes that are connected, starting with last node
connected <- c(n+1)
# make empty vector for new ambassadors
new_ambassadors <- c()
# add new vertex
gff1 <- gff %>% igraph::add_vertices(1, color = "gray50" )
# layout
l = igraph::layout_with_fr(gff1)
# if there are ambassadors, continue
while(length(ambassadors) > 0){
  # make adjacency matrix
  gff1am <- igraph::as_adjacency_matrix(gff1, sparse=F)
  # for each ambasaddor
  for(i in ambassadors){
    # add edge between new node and ambassador
    gff1 <- gff1 %>% igraph::add_edges(c(n+1, i)) 
    # set color of ambassador to black
    V(gff1)$color[i] <- "black"
    # find neighbors of ambassador
    neighlist <- which(gff1am[i,]==1)
    # determine which neighbors to add
    neigh.to.add <- sapply(neighlist,  function(x) ifelse(runif(1) <= p, x, 0))
    # add ambassadors to list
    new_ambassadors <- c(new_ambassadors,neigh.to.add[neigh.to.add != 0] )
    # add the ambassador to the connected list
    connected <- c(connected, i)
  }
  # add new ambassadors to list if they are not in connected already
  ambassadors <- subset(new_ambassadors, !(new_ambassadors %in% connected) )
  # empty new ambassadors list
  new_ambassadors <- c()
}
# center image in one frame
par(mfrow=c(1,1))
# plot network, assign edge colors and line type
plot(gff1, layout= l, edge.color = rep(c("gray80", "black", "gray60"), times = c(starting.edge.count, 1, length(E(gff1))-starting.edge.count-1)), edge.lty = rep(c(1,1,3), times=c(starting.edge.count, 1,length(E(gff1))-starting.edge.count-1) ), vertex.label = NA, vertex.size = 10)





#### Figure 3-11 ####

# set seed
set.seed(1)
# print parameters
par(mar=c(1,1,1,1))
# multi-panel polot
layout(matrix(c(1,2,3,3,3,3,3,3), 4, 2, byrow = TRUE))
# make graph from literal
gddl <- igraph::graph_from_literal(1-2-3, 1-4-3)
# assign edge line types
E(gddl)$lty <- c(1,1,2,2)
# assign node colors
V(gddl)$color <- c("gray50", "white", "white", "white")
# assign node frame colors
V(gddl)$frame.color <- c("black","black","white","black")
# assign edge colors
E(gddl)$color <- c("black", "black", "white", "white")
# remove labels
V(gddl)$name <- NA
# set layout
l = igraph::layout_with_fr(gddl)
# plot first network
plot(gddl, layout = l)
# label it
text(1, -.8, "Original \n network")
text(-1.3,.80, "A", cex = 3)
# assign colors
V(gddl)$color <- c("gray50", "white", "black", "white")
V(gddl)$frame.color <- c("black","black","black","black")
E(gddl)$color <- c("black", "black", "black", "black")
# plot
plot(gddl, layout = l)
# label
text(1, -.8, "Node \n duplication")
text(-1.3, .80, "B", cex = 3)
# set seed
set.seed(4)
# nodes
n = 10 
# probability
p = .8 
# make starting network
gdd <- igraph::make_empty_graph(2, directed = FALSE)
# add edges
gdd <- gdd %>% igraph::add_edges(c(1,2))
# add remaining n nodes
for(i in 3:n){
  # make list of nodes to duplicat
 dupli.list <- V(gdd) 
 # sample from list to get duplicated node (.0001 for isolates)
 node.to.duplicate <- sample(dupli.list, 1, prob = igraph::degree(gdd)+.0001)
 # get adjacency matrix
 gddam <- igraph::as_adjacency_matrix(gdd, sparse=F)
 # get neighbors of duplicated node
 model <- gddam[node.to.duplicate,]
 # get node ids
 edges.to.add <- which(model == 1)
 # add new node
 gdd <- gdd %>% igraph::add_vertices(1, color = "white", name = paste(i, "-", node.to.duplicate))
 # randomly add edges from neighbor list
 for(j in 1:length(edges.to.add)){
   # if probability is satisfied
   if(runif(1) <= p){
     #add edge
    gdd <- gdd %>% igraph::add_edges(c(edges.to.add[j],i))
   }
 }
}
# name nodes
V(gdd)$name[c(1,2)] <- c("1", "2-1")
# find isolates
Isolated = which(igraph::degree(gdd)==0)
# delete isolates
gdd = igraph::delete.vertices(gdd, Isolated)
# margins
par(mar=c(1,1,1,1))
# plot
plot(gdd, vertex.size = 30, label.cex = .8 )
# label
text(-1.2,.80, "C", cex = 3)



#### Figure 3-12 ####

# set seed
set.seed(2)
# margins
par(mar=c(1,1,1,1))
# nodes
n=21
# probability
p=.2
# multi-panel layout
layout(matrix(c(1,2,3, 4,4,4, 4,4, 4), 3, 3, byrow = TRUE))
# set preferential attachment network as our base
gpa <- igraph::sample_pa(n,power = 2, m=2,directed=FALSE)
# sample nodes in proportion to degree 
node.acquisition.list <- sample(1:n, n, prob=igraph::degree(gpa))
# layout
l = igraph::layout_with_fr(gpa)
# color nodes according to order of acquisition 
V(gpa)$color[node.acquisition.list] <- rep(c("black", "gray50", "white"), times=c(n/3, n/3, n/3))
# delete vertices from 2nd and 3rd stage
gpa1 <- gpa %>% igraph::delete_vertices(node.acquisition.list[(n/3+1):n])
# delete vertices from 3rd stage
gpa2 <- gpa %>% igraph::delete_vertices(node.acquisition.list[(2*n/3+1):n])
# plot stages
plot(gpa1)
plot(gpa2, vertex.label = NA)
plot(gpa, vertex.label = NA, layout = l)
# plot full network
plot(gpa, vertex.label = NA,layout=l,vertex.color="white" , vertex.size = igraph::degree(gpa)*2)

#### Figure 3-13 ####

set.seed(1)
par(mfrow=c(3,3))
par(mar=c(2,2,2,2))
plot(igraph::make_empty_graph(10), vertex.label = NA, vertex.color="white", main = "Empty graph")
plot(igraph::make_full_graph(10), vertex.label = NA, vertex.color="white", main = "Full graph")
plot(igraph::make_star(10, mode="undirected"), vertex.label = NA, vertex.color="white", main = "Star")
plot(igraph::make_ring(10), vertex.label = NA, vertex.color="white", main = "1D ring lattice")
plot(igraph::make_lattice(dimvector=c(5,5)), vertex.label = NA, vertex.color="white", main = "2D lattice")
plot(igraph::make_tree(21, 2, mode="undirected"), vertex.label = NA, vertex.color="white", main = "Tree graph")
 plot(igraph::sample_gnp(10,.4), vertex.label = NA, vertex.color="white", main = "Erdös-Renyi random graph")
 plot( igraph::sample_pa(10, m=1, power=1.5,  zero.appeal=1, directed=FALSE), vertex.label = NA, vertex.color="white", main = "Preferential attachment")
 plot( igraph::sample_smallworld(1,10,2, p=.1), vertex.label = NA, vertex.color="white", main = "Small-world", layout=layout_in_circle)
