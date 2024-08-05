### Behavioral Network Science
### Language, Mind, and Society
# By Thomas T. Hills

# Code to reproduce figures and analyses for

#### Chapter 1: Making and Recognizing Networks ####

# To install packages remove the #, then put the # back after as this is only needed once. Then you can upload libraries locally below
#install.packages('igraph')
#install.packages('tidyverse')
#install.packages('kableExtra')
#install.packages('IsingFit')
#install.packages('NetworkToolbox')
#install.packages('bootnet')

# igraph package
library(igraph)
# package for some basic data manipulation
library(tidyverse)
# package for tables
library(kableExtra)
# packages for section 1-6 on networks from data
library(IsingFit)
library(NetworkToolbox)
library(bootnet)


rm(list=ls()) # for good health, remove existing data!
# to make graphs reproducible, this sets the seed for the internal random number generator

#### Figure 1-1 #####
set.seed(3)
# make random graph with 6 nodes and probability of edge = .5
g <- igraph::sample_gnp(6, .5) # simple
# control network layout
l <- igraph::layout_with_fr(g)
# plot the graph.  Igraph objects are plotted as a network
# pdf() exports saved figure, dev.off() completes figure save 
pdf(file="Figure1-1.pdf")
plot(g, vertex.label = c(1:6), vertex.label.cex=1, vertex.color="white", layout=l)
dev.off()


# produce edge list from a graph object
edgeList <- igraph::as_edgelist(g)

# write the edge list to a csv file you can open with a text editor on your desktop
write.csv(edgeList, file="edgelist.csv", quote=F, row.names = F)
# inspect the csv file in excel or a text editor

# read the edgelist into R as a new object 
el <- read.csv("edgelist.csv") 

#### Table 1-1 ####

# print the edge list (if this were a knitted pdf file using RMarkdown, it would look very nice in the pdf)
knitr::kable(el, caption="An edgelist for our basic network.", format = 'pandoc')

# turn the edge list into a matrix
el <- as.matrix(el) 

# transform matrix into graph object, gs = simple graph
gs <- igraph::graph_from_edgelist(el, directed=F) # transform from matrix to graph, gs = simple

# plot graph from edgelist (remove # below)
# plot(gs, vertex.label = c(1:6), vertex.label.size=2, vertex.color="white", layout=l)




# get adjacency matrix from graph object.  Assign sparse = F if you want to see the zeros
am <- igraph::as_adjacency_matrix(gs, sparse=F)

# assign rownames so it looks nice
rownames(am) <- c(1,2,3,4,5,6)
colnames(am) <- c(1,2,3,4,5,6)

#### Table 1-2 ####

# print table to console (or pdf)
knitr::kable(am,booktabs=TRUE, caption = "An adjacency matrix for our basic network.",linesep = "", row.names = TRUE, format = 'pandoc') %>%
  column_spec (1, border_right = TRUE) %>% 
  kable_classic_2(full_width = F)

write.csv(am, file="adjacencyMatrix.csv", quote = F) # open this in excel or a text editor

# read in stored adjacency matrix
am <- read.csv("adjacencyMatrix.csv", header = T, row.names=1 )

# assign column names from row names
colnames(am) <- rownames(am)

# transform data frame into matrix
am <- as.matrix(am)

# transform matrix into graph object, assign undirected
gam <- igraph::graph_from_adjacency_matrix(am, mode="undirected")

# plot graph object from adjacency matrix (remove # below)
# plot(gam, vertex.label = c(1:6), vertex.label.size=2, vertex.color="white", layout=l)

# add a self loop
gamloop <- gs %>%  igraph::add_edges(c(2,2))
# make adjacency matrix for display
gamloopmat <- as.matrix(igraph::as_adjacency_matrix(gamloop))
# add row and column names
rownames(gamloopmat) <- c(1,2,3,4,5,6)
colnames(gamloopmat) <- c(1,2,3,4,5,6)
# print table 
knitr::kable(gamloopmat,booktabs=TRUE, caption = "Adjacency matrix for a network with a self-loop.",linesep = "", row.names = TRUE, format = 'pandoc') %>%
  column_spec (1, border_right = TRUE) %>% 
  kable_classic_2(full_width = F)


#### Figure 1-2 #####
pdf(file="Figure1-2.pdf")
plot(gamloop, vertex.label = c(1:6), vertex.label.cex=1, vertex.color="white", layout=l)
dev.off()

# to remove the self loop
which_loop(gamloop)
gamloop <- simplify(gamloop, remove.loops = TRUE)

# Copy the simple graph to a new graph object
gw <- gs
# assign uniform random values between 1 and 10 to each edge
E(gw)$weight <- round(runif(length(E(gw)),1,10))
# label each edge with its edge weight
E(gw)$label <- E(gw)$weight


#### Figure 1-3 #####

# plot the graph making edge width = to edge wieght
pdf("Figure1-3.pdf")
plot(gw,layout=l, edge.width=E(gw)$weight,  vertex.label = c(1:6), vertex.label.cex=1, vertex.color="white")
dev.off()


# get adjacency matrix with weight attribute
am <- igraph::as_adjacency_matrix(gw, attr="weight", sparse = F)
# assign names
rownames(am) <- c(1,2,3,4,5,6)
colnames(am) <- c(1,2,3,4,5,6)


#### Table 1-4 ####

# print table to pdf
knitr::kable(am, booktabs=TRUE, caption = "Weighted adjacency matrix.",linesep = "", row.names = TRUE, format = 'pandoc') %>%
  column_spec (1, border_right = TRUE) %>% 
  kable_classic_2(full_width = F)


# add a weight column to edge list
elw <- data.frame('V1' = as_edgelist(gw)[,1],'V2' = as_edgelist(gw)[,2],weight=E(gw)$weight)

# here is how to turn a data.frame into a graph
# testnet <- graph_from_data_frame(elw, directed = FALSE) 

#### Table 1-5 ####

# print table to pdf
knitr::kable(elw, booktabs=TRUE, caption = "Weighted edge list.",linesep = "", row.names = FALSE, format = 'pandoc') %>%
  kable_classic_2(full_width = F)


# read in edge list
el <- read.csv("edgelist.csv") # read in the file
# assign to matrix class
el <- as.matrix(el) # transform from data.frame to matrix
el <- rbind(el, c(4,1))
# make graph from edge list matrix, make it directed
gdir <- igraph::graph_from_edgelist(el, directed=T) 


#### Figure 1-4 #####

# plot directed network -- arrows are plotted automatically
pdf("Figure1-5.pdf")
plot(gdir,layout=l, vertex.label = c(1:6), vertex.label.cex=1, vertex.color="white")
dev.off()

# assign row names
rownames(el) <- c(1:dim(el)[1]) # label the edges from 1 to 8
# get adjacency matrix from directed network
gdiram <- igraph::as_adjacency_matrix(gdir, sparse=F)
# assign names to matrix
rownames(gdiram) <- c(1,2,3,4,5,6)
colnames(gdiram) <- c(1,2,3,4,5,6)

#### Table 1-6 ####

# print table to pdf
knitr::kable(el, booktabs=TRUE, caption = "Directed edge list.",linesep = "", row.names = FALSE, format = 'pandoc') %>%
  kable_classic_2(full_width = F)

#### Table 1-7 ####

knitr::kable(gdiram, booktabs=TRUE, caption = "Directed adjacency matrix.",linesep = "", row.names = T, format = 'pandoc') %>%
    column_spec (1, border_right = TRUE) %>% 
  kable_classic_2(full_width = F)

# vertex size 
vc = 20
# remove edge labels
E(gw)$label <- NA



#### Figure 1-5 #####

pdf("Figure1-5.pdf")
# set 2 x 2 plotting window
par(mfrow=c(2,2))
# set plotting margins
par(mar=c(2,2,2,2))

# plot each network in turn
plot(g,layout=l, vertex.label = c(1:6), vertex.label.cex=1, vertex.color="white", main="Unweighted, Undirected", vertex.size = vc)
plot(gw,layout=l, vertex.label = c(1:6), vertex.label.cex=1, vertex.color="white", main="Weighted, Undirected",  edge.width=E(gw)$weight*.7, vertex.size = vc)
plot(gdir,layout=l, vertex.label = c(1:6), vertex.label.cex=1, vertex.color="white", main="Unweighted, Directed", vertex.size = vc)
# copy graph object
gwdir <- gdir
# assign weights to new graph object
E(gwdir)$weight <- c(E(gw)$weight, 5)
#
plot(gwdir,layout=l, vertex.label = c(1:6), vertex.label.cex=1, edge.width=E(gw)$weight*.7, vertex.color="white", main="Weighted, Directed", vertex.size = vc, edge.curved=c(0.4,0,0,0,0,0,0,0,.4))
dev.off()

# 20 nodes
n=20
# create an empty adjacency matrix
gwtam <- matrix(0,ncol = n, nrow =n)
# randomly assign each node to one of two groups with p = .5
ntype <- rbinom(n,1,.5)
# for the upper diagonal of the adjacency matrix 
for(i in 1:(n-1)){
  for(j in (i+1):n){
    # choose different edge withs for in and out-group connects
    gwtam[i,j] <- ifelse(ntype[i] ==ntype[j], runif(1, .5, 1), runif(1,0,.5))
  }
}
# make undirected weighted graph from adjacency matrix
gwt <- igraph::graph.adjacency(gwtam, mode=c("undirected"), weighted=TRUE, diag=FALSE)
# assign color to each node based on group
V(gwt)$color <- ifelse(ntype==0, 'gray40', 'gray90')
# function to threshold graph and make all remaining edges 1--input graph and threshold
thresh_graph <- function(g, thresh) {
  # delete edges with weight < thresh
  gt <- igraph::delete_edges(g, E(g)[E(g)$weight < thresh])
  # set remaining edges to 1
  E(gt)$weight <- 1
  # return graph
  return(gt)
}

#  make sequence of thresholds
threshold.sequence <- seq(0,1, .2)

pdf("Figure1-6.pdf")
# makes as many plots as in threshold sequence
par(mfrow=c(1,length(threshold.sequence)))
# set margins
par(mar=c(1,1,1,1))
# for loop that plots each graph after thresholding

#### Figure 1-6 #####

for(i in threshold.sequence){
  plot(thresh_graph(gwt,i),vertex.label=NA, adj = -1)
  title( paste("T = ",i), line = -8)
}
dev.off()


set.seed(2)
# make directed random graph with 20 nodes and edge probability of .2
gdirs <- igraph::erdos.renyi.game(20,.2,directed = TRUE)
# color all nodes
V(gdirs)$color = "gray50"
# make dummy list for all edges
x <- 1:length(E(gdirs))
# make list of non.reciprocal edges, that are *not* mutual
non.reciprocal <- x[!which_mutual(gdirs)]
# make directed graph object undirected
gdirsu <- as.undirected(gdirs)
# copy directed graph object
girsu.rec <- gdirs
# delete non-reciprocal edges and make network undirected 
girsu.rec <- as.undirected(igraph::delete.edges(gdirs, non.reciprocal))
# set layout
l = igraph::layout_with_fr(gdirs)


#### Figure 1-7 #####

pdf("Figure1-7.pdf")
# make three figure panes
par(mfrow=c(1,3))
# set margins
par(mar=c(2,2,2,2))

# plot three networks
plot(gdirs, layout=l, edge.arrow.size=.4, vertex.label = NA)
plot(gdirsu, layout=l, vertex.label = NA)
plot(girsu.rec, layout=l, edge.arrow.size = .4, vertex.label =  NA)
dev.off()


# set seed 
set.seed(4)
# make random network
gna1 <- igraph::sample_gnp(12, .3) # simple
# make a sequence of three color types, one for each political party, repeated four times to cover the number of nodes
colors<-rep(c("cadetblue1","firebrick", "khaki"), 4)
# assign the colors to nodes 
V(gna1)$type <- colors

#### Figure 1-8 #####
pdf("Figure1-8.pdf")
par(mfrow=c(1,2))
# set margins
par(mar=c(2,2,2,2))
# plot political network
plot(gna1, vertex.color=V(gna1)$type, vertex.label=NA, main = "Political party")
# make legend
legend(-1,1.2,legend=c("Party A", "Party B", "Party C"), fill = c("cadetblue1","firebrick", "khaki"), cex = .8, bty="n")

# make random network
gna2 <- igraph::sample_gnp(12, .3) # simple
# assign uniform random ages to each node
V(gna2)$age <- runif(12, 0,100)
V(gna2)$happiness <- runif(12, 0,100)
# make two panels for plotting
# make a color ramp
colram <- colorRampPalette(c("red", "mediumblue"))
# assign 100 values, for ages
colr <- colram(100)
# make raster for legend
legend_image <- as.raster(matrix(colram(20), ncol=1))
# plot network with node size and color scaled to age
E(gna2)$weight <- runif(length(E(gna2)))*5
plot(gna2, vertex.size = V(gna2)$age/3, vertex.label=NA, vertex.color = colr[100-V(gna2)$happiness], main = "Relationship age", edge.width = E(gna2)$weight)
# make legend
text(x=.75, y = c(1.2,.9), labels = c("Happy", "Sad"), cex = .8)
# plot color raster
rasterImage(legend_image, .4, 1.2, .5,.9)
dev.off()


# make bipartite adjacency matrix with different numbers of rows and columns
bimat <- matrix(rbinom(50, 1, .3), nrow=10, ncol = 5)
# give the columns and rows different kinds of names
colnames(bimat) <- c('J', 'K', 'L', 'M', 'N')
rownames(bimat) <- 1:10
# convert matrix to bipartite graph: graph.incidence makes a bipartite graph
gbn<- igraph::graph.incidence(bimat)
# check if its bipartite
checkbipartite <- igraph::is.bipartite(gbn) # sanity check 
# assign colors to nodes (ordered by row type then column type)
V(gbn)$color <- c(rep('gray80', 10), rep('white', 5)) # individuals = black, groups = white 

#### Figure 1-9 #####

pdf("Figure1-9.pdf")
# make two panels
par(mfrow=c(2,2))
# margins
par(mar=c(0,0,0,0)+1)
# plot the bipartite network in two different ways
plot(gbn, layout = layout_with_fr, vertex.size = 20)
plot(gbn, layout=layout_as_bipartite, vertex.size = 20)

# make projections
g.bp <- igraph::bipartite.projection(gbn)
# plot projections
plot(g.bp[[1]], layout = layout_with_fr,vertex.size = 20)
plot(g.bp[[2]], layout = layout_with_fr, vertex.size = 20)
dev.off()

#### Table 1-8 ####

knitr::kable(bimat, booktabs=TRUE, caption = "Bipartite adjacency matrix with two node types.",linesep = "", row.names = T, format = 'pandoc') %>%
    column_spec (1, border_right = TRUE) %>% 
  kable_classic_2(full_width = F)


# make random graph
gmp <- igraph::sample_gnp(20, .1) # simple
# make list of three edge types
x = c('gray30', 'gray50', 'gray80')
# randomly assign edge types to edges in random graph
E(gmp)$type <- sample(x, length(E(gmp)), replace=T)
# color edges appropriately
E(gmp)$color <- E(gmp)$type
# layout
l = igraph::layout_with_fr(gmp)

#### Figure 1-10 #####

pdf("Figure1-10.pdf")
# four panels
par(mfrow=c(1,4))
# margins
par(mar=c(1,1,1,1))
# remove labels
V(gmp)$name <- NA
# plot networks
plot(gmp, edge.width=5, vertex.color="white", layout=l)
cl = -10
title("All edges", line = cl)
x = 1:length(E(gmp))  
# removed edges of different types for each network
gmp30 <- igraph::delete.edges(gmp, x[!(E(gmp)$type=='gray30')])
gmp50 <- igraph::delete.edges(gmp, x[!(E(gmp)$type=='gray50')])
gmp80 <- igraph::delete.edges(gmp, x[!(E(gmp)$type=='gray80')])
# plot networks
plot(gmp30, edge.width=5, edge.color = E(gmp30)$type, vertex.color="white", layout=l)
title("Edge A", line = cl)
plot(gmp50, edge.width=5, edge.color = E(gmp50)$type, vertex.color="white", layout=l)
title("Edge B", line = cl)
plot(gmp80, edge.width=5, edge.color = E(gmp80)$type,vertex.color="white", layout=l)
title("Edge C", line = cl)
dev.off()


#### Section 1-6 ####
# requires uploading data #
# you will need to download binda2.csv and cdata2.csv in the SampleDataFilesBNS folder from the website and place the folder in the same directory as your R session

bida <- read.csv("SampleDataFilesBNS/binda2.csv")[,-1]
bida5 <- bida[1:5,1:5]

#### Table 1-9 ####

knitr::kable(bida5, row.names = TRUE, format = 'pandoc', caption ="A sample of cross-sectional survey data. Participants are rows and survey questions are columns.")

cda <- read.csv("SampleDataFilesBNS/cdata2.csv")[,-1]
names(cda) <- LETTERS[1:7]
cda5 <- cda[1:5,]

#### Table 1-10 ####

knitr::kable(cda5, row.names = TRUE, format = 'pandoc', caption ="An example cross-sectional data file. Participants are rows and columns are survey questions with continuous ratings on a scale from 1 to 7.")


#### Figure 1-11 #####

pdf("Figure1-11.pdf")
par(mfrow=c(1,3))
ising<-IsingFit(bida, plot = TRUE)
text(.8,-1.2, "Ising model", cex =1)

tmfg2<-TMFG(bida)$A
qgraph::qgraph(tmfg2, layout="spring")
text(.8,-1.2, "TMFG", cex = 1)

cdn <- estimateNetwork(cda, default = "EBICglasso", tuning = 0)
plot(cdn, layout = "spring", labels = TRUE)
text(.7,-1.2, "EBICglasso", cex = 1)
dev.off()
