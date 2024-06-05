
### Behavioral Network Science
### Language, Mind, and Society
# By Thomas T. Hills

# Code to reproduce figures and analyses for

#### Chapter 13 Creativity: How Noisy Processes Create Novel Structure ####

knitr::opts_chunk$set(echo = FALSE)
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(comment = NA)
knitr::opts_chunk$set(warning = FALSE)
options(knitr.table.format = "latex")

rm(list=ls())
library(igraph)
library(R.matlab)
library(tidyverse)
library(plot.matrix)


#### Figure 13-1 ####
# Figure parameters
par(mfrow=c(1,1))
# list from 0 to 4
x <- 0:4
# Function for y value high creative
y = 100*exp(-x)
# Function for y value low creative
y2 = 50*exp(-x/4)
# Plot function values
plot(x,y, type = "l", ylab = "Associative strength to 'Monkey'", xaxt="n", yaxt="n", lwd=2, cex.lab = 1.4, xlab="Concept")
lines(x, y2, lty=2, lwd=2)
axis(1, at = seq(0, 4, by = 1), labels = c("Tree", "Primate", "Human", "Evolution", "Zoo") )
axis(2, at = c(0, 100), labels = c("Low", "High"), las =1 )
legend(3,100, legend=c("Higher", "Lower"),title="Creativity", lty=2:1)




#### Figure 13-3 ####

set.seed(1)
# Size of lattice
n=19
# Vertex size
cs = 6 
# Make lattice -- base network
g <- make_lattice(length= n, dim = 2 )
# Set layout
l <- layout_on_grid(g)
# Layout for various images
layout(matrix(c(1,2,3,4,5,5,5,5,5,5,5,5), 3, 4, byrow = TRUE))
# Probability values
perct <- c(.2, .4, .6, .8)
# Figure parameters
par(mar=c(1,1,1,3))
# For loop to handle different probabilities
for(i in 1:length(perct)){
# Remove edges that fail probability check  
g2 <- g %>% delete_edges(which(rbinom(length(E(g)),1,perct[i])==1))
# Identify largest component nodes
largestcomponent = which(igraph::components(g2)$csize ==max(igraph::components(g2)$csize))
whosInIt <- which(igraph::components(g2)$membership == largestcomponent[1])
# Plot network and color nodes
plot(g2, vertex.size = cs,vertex.label=NA, vertex.color=ifelse(V(g2) %in% whosInIt, "black", "white"), layout=l, vertex.frame.width = .1)
  text(-.1, -1.3, paste("prob =", toString(perct[i])), cex = 1)
}

# Sequence for probabilities
perct <- seq(0, 1, .1)
# Simulations per probability
sims = 100
# Data buffers for component count and size of largest
num_components <- matrix(0, nrow=sims, ncol=length(perct))
largest_component_size <- matrix(0, nrow=sims, ncol=length(perct))
# For each probability
for(j in 1:length(perct)){
  # For each simulation 
  for(i in 1:sims){
    # Remove edges according to probability from base network
    g2 <- g %>% delete_edges(which(rbinom(length(E(g)),1,perct[j])==1))
    # Count number of components and identify size of largest
    num_components[i, j] <- igraph::components(g2)$no
    largest_component_size[i,j] <- max(igraph::components(g2)$csize)
  }
}

# Compute means
ms <- colMeans(num_components)
msize <- colMeans(largest_component_size)
# Plot means
par(mar=c(5,5,1,1))
plot(perct, msize, xlab = "Probability of edge removal", ylab = "Largest component size", type="b")




#### Figure 13-4 ####

# Same as Figure 13-1 but add two lines to indicate faster-further and faster-proportional
par(mfrow=c(1,1))
par(mar=c(5,5,2,2))
x <- 0:4
y = 
hc = 60*exp(-x)
hp = 100*exp(-x) 
hf = 50*exp(-x/6)
lc = 40*exp(-x/6)
plot(x,hc, type = "l", ylab = "Associative strength to 'Monkey'", xaxt="n", yaxt="n", lwd=3, cex.lab = 1.4, xlab="Concept", lty=2, ylim=c(0,100), col="gray80")
lines(x, lc, lty=1, lwd=3, col="gray80")
lines(x, hp, lty=3, lwd=3)
lines(x, hf, lty=4, lwd=3)
##lines(x, y4, lty=4, lwd=2)
axis(1, at = seq(0, 4, by = 1), labels = c("Tree", "Primate", "Human", "Evolution", "Zoo") )
axis(2, at = c(0, 100), labels = c("Low", "High"), las =1 )
legend(2.5,90, legend=c("Lower - Mednick","Higher - Mednick", "Faster-but-proportional", "Faster-and-further"),title="Creativity", lty=c(2, 1, 3, 4), lwd=c(3,3,3, 3), col = c("gray80", "gray80", "black", "black"), cex = .75)



## Read in data from Ovando-Tellez et al., 2022
## Data generously provided by Marcela Ovando-Tellez
rm(list=ls())
cpm <- read.csv('SampleDataFilesBNS/data_for_CPMfinal.csv')
partFiles.w <- list.files('SampleDataFilesBNS/Individual_AdjencyMatrices_SemNets_WUN/')
partFiles.u <- list.files('SampleDataFilesBNS/Individual_AdjencyMatrices_SemNets_UUN/')

# Setup data buffers
ccw <- rep(NA, nrow(cpm))
asplw <- rep(NA, nrow(cpm))
gmatiw <- list()
pmatiw <- list()
ccu <- rep(NA, nrow(cpm))
asplu <- rep(NA, nrow(cpm))
gmatiu <- list()
pmatiu <- list()
meanrating <- rep(NA, nrow(cpm))
swu <- rep(NA, nrow(cpm))
sww <- rep(NA, nrow(cpm))
modw <- rep(NA, nrow(cpm))
modu <- rep(NA, nrow(cpm))
denu <- rep(NA, nrow(cpm))
par(mfrow=c(12,8))
par(mar=c(0,0,0,0))
# For each data file
for(i in 1:length(partFiles.w)){
  # Read next file
  thisnamew <- paste("SampleDataFilesBNS/Individual_AdjencyMatrices_SemNets_WUN/", partFiles.w[i], sep="")
  thisnameu <- paste("SampleDataFilesBNS/Individual_AdjencyMatrices_SemNets_UUN/", partFiles.u[i], sep="")
  # Import matrix
  pmatw <- readMat(thisnamew)
  pmatu <- readMat(thisnameu)
  # Add adjacency matrix to list
  pmatiw[[i]] <- pmatw$AdjencyMatrix.WUN
  pmatiu[[i]] <- pmatu$AdjencyMatrix.UUN
  # Create graphs
  gmatw <- graph_from_adjacency_matrix(pmatiw[[i]], mode="undirected", weighted = TRUE)
  gmatu <- graph_from_adjacency_matrix(pmatiu[[i]], mode="undirected")
  # Add to list
  gmatiw[[i]] <- gmatw
  gmatiu[[i]] <- gmatu
  # Compute metrics
  # density for undirected
  denu[i] <- igraph::graph.density(gmatu)
  # clustering coefficient
  ccw[i] <- mean(igraph::transitivity(gmatw, type="weighted"))
  ccu[i] <- mean(igraph::transitivity(gmatu, type="local"))
  # take inverse weights
  gmatwinv <- gmatw
  E(gmatwinv)$weight <- 1/E(gmatwinv)$weight 
  # average shortest path length
  asplw[i] <- mean(igraph::distances(gmatwinv, mode="all"))
  asplu[i] <- mean(igraph::distances(gmatu, mode="all"))
  # mean weight
  pmiw <- pmatiw[[i]]
  # take mean of upper triangle 
  meanrating[i] <- mean(pmiw[upper.tri(pmiw)]) 
  # modularity
  wtw <- cluster_walktrap(gmatw)
  wtu <- cluster_walktrap(gmatu)
  modw[i] <- modularity(wtw)
  modu[i] <- modularity(wtu)
}

names(pmatiw) <- cpm$Subject_ID
## Acts and achievements are correlated.  I'm scaling them here because I want to add them
cacts<-scale(cpm$C.Act)
cachs<-scale(cpm$C.Ach)
##cor.test(cacts,cachs).  r = .66
### strength of rating predicts creativity

# Add metrics as new columns
cpm2 <- cpm %>% add_column(ccw, ccu, asplw, asplu, modw, modu, meanrating, cacts, cachs, denu)
# order by C.Act (creative activities)
cpm2 <- cpm2[order(cpm2$C.Act, decreasing=TRUE),]  

### Are the more creative people producing the same edges  
### Remove quantiles of edges for each participant and recompute correlations for each pair of individuals
cutquant <- function(graph, thresha){
     # Separate into percentiles by thresha and get the lower threshold
     thresh <- quantile(graph, probs=c(0, thresha, 1))[2]
     # Set those below to 0
     graph[graph < thresh] <- 0
     # And above to 1
     graph[graph > 0] <- 1
     # Return the graph
     return(graph)
}

# IDs
nameOrder <- cpm2$Subject_ID
## fix this -- these aren't the same
listorder <- order(cpm$C.Act, decreasing=TRUE) 

matlist <- list()
# Threshold sequence
threshseq <- seq(0,.95, .05)
# Data buffers
densii <- matrix(NA, nrow=length(pmatiw), ncol=length(threshseq))
mrii <- matrix(NA, nrow=length(pmatiw), ncol=length(threshseq)) # keeping myself honest by recalculating the mean ratings--they match ( mrii[,1]==cpm2$meanrating)

# For each threshold value
for(k in 1:length(threshseq)){
  # Setup data buffer for pairwise correlations
  matt <- matrix(0, nrow=length(pmatiw), ncol=length(pmatiw))
  # Name rows and columns with participants
  rownames(matt) <- nameOrder
  colnames(matt) <- nameOrder
  # For each adjacency matrix
  for(i in 1:(length(pmatiw)-1)){
    # Get name
    nx1 <- toString(nameOrder[i])
    # Get matrix
    pm1<-pmatiw[[nx1]]
    # Threshold
    pm1 <- cutquant(pm1, threshseq[k])
    # Compute density
    densii[i,k]<-graph.density(graph_from_adjacency_matrix(pm1, mode="undirected", weighted = TRUE))
    # For each pair of individuals
    # But don't do the last one as it has no pairs
    if(i < 94){
      # Count up from (i+1) to end of list 
      for(j in (i+1):length(pmatiw)){
       # Get name
       nx2 <- toString(nameOrder[j])
       # Get matrix
       pm2<-pmatiw[[nx2]]
       # Threshold
       pm2 <- cutquant(pm2, threshseq[k])
       # Set diagonals
       diag(pm2) <- NA
       diag(pm1) <- NA
       # Compute correlation
       matt[i,j] <- cor.test(as.vector(pm1),as.vector(pm2))$estimate
       # If it's the last one to compute pairs
       if(i == 93){
       # Save density of each network as you go
       densii[j,k]<-graph.density(graph_from_adjacency_matrix(pm2, mode="undirected", weighted = TRUE)) 
       }
      }
    }
  }    
  # Save results to list for each threshold k
  matlist[[k]] <- matt
}


##corcorssmat




#### Figure 13-5 ####

## Plot parameters
par(mfrow=c(1,3))
par(mar=c(5,5,2,2))
# Plot mean similarity rating against creative activities and achievements
with(cpm2, plot(meanrating, C.Act, xlab ="Mean similarity rating", ylab="Creative activities"))
abline(with(cpm2, lm(C.Act~meanrating)))
## summary(at <- with(cpm2, lm(C.Act~meanrating)))
## summary(with(cpm2, lm(meanrating~C.Act)))
with(cpm2, plot(meanrating, C.Ach, xlab ="Mean similarity rating", ylab="Creative achievements"))
abline(with(cpm2, lm(C.Ach~meanrating)))

## summary(with(cpm2, lm(C.Act~denu)))

# Label H and Low creatives (remember we ordered them above)
HorL <- rep(c("H", "L"), each=c(47))
# Add the label column
cpm2 <- cpm2 %>% add_column(HorL)

##with(cpm2, t.test(meanrating~HorL))
##with(cpm2, t.test(denu~HorL))
##summary(with(cpm2, lm(denu~C.Act)))

# Add all the low or high individual ratings together for all pairs
# To get mean pairwise similarities
cn <- cpm2$Subject_ID[1:24]
cn <- paste(cn)
list_sumH <- Reduce("+",pmatiw[cn] )/24

cn <- cpm2$Subject_ID[70:94]
cn <- paste(cn)
list_sumL <- Reduce("+",pmatiw[cn] )/24

## Plot histogram of weights -- High c has more high weights
hist(list_sumL, col="coral",xlim=c(0,100),ylim=c(0,300), xlab="Similarity rating", main="")
hist(list_sumH, col=alpha("cornflowerblue", alpha=.5),add=T)
legend(60, 300, legend=c("Low", "High"), fill = c("coral", "cornflowerblue") ,bty="n")

## Gini(list_sumL, unbiased=FALSE)
## Gini(list_sumH, unbiased=FALSE)

## # permutation test
## N <- 1000 # repeat shuffling for N times
## corPerm <- numeric(length = N) # vector with the results of the test statistic under each permutation
## for(i in 1:N)
## {
##  shufdata <- cpm[sample(nrow(cpm)),]
##  corPerm[i] <- cor(shufdata$C.Ach, cpm$UUN_ASPL)
## }
## corObserved <- cor(cpm$C.Ach, cpm$UUN_ASPL)
## 
## 
## p_value_Cor <- (sum(corPerm>=corObserved))/length(corPerm)
## p_value_Cor
## 
## # a permutation test from lmPerm
## summary(lmp(C.Ach~UUN_ASPL,data=cpm, perm="Prob"))

## # is pmatiu[[1]] the same as pmatiw[[1]] thresholded at 50 or greater [paper says greater than 50, but close enough]
## testpmat1 <- pmatiw[[1]]
## testpmat1[testpmat1 < 50] <- 0
## testpmat1[testpmat1 >= 50] <- 1
## cor.test(pmatiu[[1]],testpmat1) # correlation between network measures for all pairs between SID 1 and 2
## 
## testpmat2 <- pmatiw[[2]]
## testpmat2[testpmat2 < 50] <- 0
## testpmat2[testpmat2 >= 50] <- 1
## cor.test(pmatiu[[2]],testpmat2)




#### Figure 13-6 ####

# Get mean similarities for different pairs of individuals
corcorssmat <- matrix(NA, ncol=length(threshseq), nrow=5)
# Compute correlations for each threshold
for(i in 1:length(matlist)){
  # To avoid the 0s--We are looking at relative values
  matt <- matlist[[i]] + t(matlist[[i]])
  # High Creatives
  corcorssmat[1,i] <- mean(matt[1:24, 1:24])
  # Low Creatives
  corcorssmat[2,i] <- mean(matt[70:94, 70:94])
  corcorssmat[3,i] <- mean(matt[1:24, 70:94])
  corcorssmat[4,i] <- mean(matt[70:94, 1:24])
  corcorssmat[5,i] <- mean(matt)
}


## t.test(matt[1:24, 1:24], matt[70:94, 70:94])
## t.test(matt[1:47, 1:47], matt[48:94, 48:94])
## t.test(matt[1:47, 1:47], matt[1:47, 48:94])

# As x-axis
xl = threshseq
# Figure parameters
par(mfrow=c(1,2))
par(mar=c(5,5,2,2))
# Plot correlations
plot(xl, corcorssmat[1,], xlab = "Top % of edges", ylab = "Mean correlation within group", xaxt="n", lwd=2, cex = .6, ylim = c(0, .45))
axis(1, at = seq(0, .9, by = .1), labels = seq(100, 10, by =-10))
points(xl, corcorssmat[2,],  col= "gray50", lwd = 2, cex = .6, lty=2)

legend(0, .45, legend=c("High creatives", "Low creatives"), lty=c(1,2), col=c("black", "gray50"), lwd=2, bty="n")

## Threshold both and ask if they get more similar as one moves up in weights threshold
threshseq <- seq(0,.95, .05)
cort <- rep(NA, length(threshseq))
for(i in 2:length(threshseq)){
## Are the top X% shared for both groups
  cut1 <- quantile(list_sumH, probs=c(0, threshseq[i], 1))[2]
  cut2 <- quantile(list_sumL, probs=c(0, threshseq[i], 1))[2]
  cm1 <- cutquant(list_sumH, cut1/100)
  cm2 <- cutquant(list_sumL, cut2/100)
  cort[i]<-cor.test(cm1,cm2)$estimate
}

plot(threshseq, cort, type = "b", xlab="Weight threshold", ylab="Correlation (H and L)", cex.lab = .8)


#### Figure 13-7 ####
# Figure parameters
par(mfrow=c(2,2))
par(mar=c(5,5,2,2))
set.seed(1)
# Make the cognitive representation
eg <- sample_gnp(500, .005)
# Plot it
plot(eg, vertex.label = NA, vertex.size = 1)
# Compute distances from node 1
dd <- igraph::distances(eg, 1)
# Order by distance, closest first
dd<-dd[base::order(dd)]
# Remove the infinites
dd <- dd[which(dd != 'Inf')][-1]
# Plot similarity (1/d)
plot(1/dd, xlab = "Rank", ylab = "Association strength (1/distance)", cex = .4 )

# Similarities
x <- 1/dd
# Number of possible targets
targets =  length(x) 

# Function for noisy activation model
word_race_nam <- function(probs,noise = 0,noisy_mean=0, noisy_sd = 0, threshold=5){ #noise curation model
  # Probs are similarities
  xx = 1:length(probs)
  # Set time to 0
  time <- 0
  # Initialize activation values
  activation_values <- rep(0, targets)
  # Until threshold is reached
  while(max(activation_values)<threshold){
    # Sample a target in proportion to similarity
    samp <- sample(1:length(probs), 1, prob=probs)
    # Add activation with variance to sampled value 
    activation_values[samp] <-  activation_values[samp] + 1 + rnorm(1, mean =noisy_mean, sd=noisy_sd)
    # Add atime unit
    time <- time + 1
  }
  # Identify word that exceeded threshold
  chosen_one = xx[activation_values == max(activation_values)]
  # Return the chosen one with the time of retrieval
  return(data.frame(word = chosen_one, time))
}


# Number of retrievals
sims = 10000 # equivalent to retrievals
# Data buffers
results_nn  = c()
results_hc = c()
# Ask for 10000 retrievals for high and low creatives
for(i in 1:sims){
  results_nn <- rbind(results_nn, word_race_nam(x, noisy_mean = 0, noisy_sd = .5))
  results_hc <- rbind(results_hc, word_race_nam(x, noisy_mean = 0, noisy_sd = 2 ) )
}

# Boxplot or RTs
boxplot( data.frame(results_nn[,2], results_hc[,2]), names=c("Low", "High"), ylab = "Reaction Time")

# Get number of retrievals for each word
x1<-with(results_nn, tapply(word, word, length))
x2<-with(results_hc, tapply(word, word, length))

# x1 <- x1[order(x1, decreasing=TRUE)]
# x2 <- x2[order(x2, decreasing=TRUE)]
# Normalize by total number of retrievals
x1 <- x1/sum(x1)
x2 <- x2/sum(x2)


# Plot low creative line
plot(x1, xlim=c(0, targets), pch=16, xlab = "Rank", ylab= "Association strength", log = "y", yaxt="n", cex = .4)
axis(2, at = c(.0001,.001, .01, .1), labels = c(".0001",".001",".01", ".1"), las =1 )
lines(x1)
# Plot high creative line
points(x2, col="grey80", pch = 16, cex = .4)
lines(x2, col="grey80")
legend(200, .1, legend=c("Low", "High"), col=c("black", "gray80"), bty="n", lty=1, lwd=1.6)





# This was commented out in the book, but offers a nice approach to generating the PMFG and other network types

## this is adapted from Alex Christensen, https://github.com/AlexChristensen/PMFG/blob/master/PMFG.R

PMFG <- function (data) # for weighted matrices only
{
 # make sure to install 
  if(!"RBGL" %in% rownames(installed.packages())){
    cat("In order to perform this function, please copy code below to install: RBGL and graph packages",sep="\n")
    cat('install.packages("BiocManager")',sep="\n")
    cat('BiocManager::install(version = "3.14")',sep="\n")
    cat('BiocManager::install("RBGL")',sep="\n")
  }
  
  #create sparse data
  cormat<-data
  i<-as.vector(rep(1:ncol(data),ncol(data)))
  j<-sort(as.vector(rep(1:ncol(data),ncol(data))))
  w<-as.vector(cormat)
  kk<-which(i<j) # upper diagonal
  ijw<-cbind(i[kk],j[kk],w[kk])
  # order edges by edge weight 
  ijw<-ijw[order(ijw[,3],decreasing=TRUE),] 
  # prep matrix
  P<-Matrix::Matrix(0,nrow=ncol(data),ncol=ncol(data)) 
  # make graphnel object -- this can probably be simplified 
  as_graphnel <- function(graph) {
    if (!igraph::is_igraph(graph)) {
      stop("Not an igraph graph")
    }
    if ("name" %in% suppressWarnings(igraph::vertex_attr_names(graph)) &&
        is.character(suppressWarnings(igraph::V(graph)$name))) {
      name <- suppressWarnings(igraph::V(graph)$name)
    } else {
      name <- as.character(seq(igraph::vcount(graph)))    
    }
    edgemode <- "undirected"  
    if ("weight" %in% igraph::edge_attr_names(graph) &&
        is.numeric(igraph::E(graph)$weight)) {
      al <- lapply(igraph::as_adj_edge_list(graph, "out"), as.vector)
      for (i in seq(along=al)) {
        edges <- igraph::ends(graph, al[[i]], names = FALSE)
        edges <- ifelse( edges[,2]==i, edges[,1], edges[,2])
        weights <- igraph::E(graph)$weight[al[[i]]]
        al[[i]] <- list(edges=edges, weights=weights)
      }
    } else {
      al <- igraph::as_adj_list(graph, "out")
      al <- lapply(al, function(x) list(edges=as.vector(x)))
    }  
    names(al) <- name
    res <- graph::graphNEL(nodes=name, edgeL=al, edgemode=edgemode)
    # left in original comments here
    ## Add graph attributes (other than 'directed')
    ## Are this "officially" supported at all?
    g.n <- igraph::graph_attr_names(graph)
    if ("directed" %in% g.n) {
      warning("Cannot add graph attribute `directed'")
      g.n <- g.n[ g.n != "directed" ]
    }
    for (n in g.n) {
      res@graphData[[n]] <- igraph::graph_attr(graph, n)
    }
    ## Add vertex attributes (other than 'name', that is already
    ## added as vertex names)
    v.n <- igraph::vertex_attr_names(graph)
    v.n <- v.n[ v.n != "name" ]
    for (n in v.n) {
      graph::nodeDataDefaults(res, attr=n) <- NA
      graph::nodeData(res, attr=n) <- igraph::vertex_attr(graph, n)
    }
    ## Add edge attributes (other than 'weight')
    
    e.n <- igraph::edge_attr_names(graph)
    e.n <- e.n[ e.n != "weight" ]
    if (length(e.n) > 0) {
      el <- igraph::as_edgelist(graph)
      el <- paste(sep="|", el[,1], el[,2])
      for (n in e.n) {
        graph::edgeDataDefaults(res, attr=n) <- NA
        res@edgeData@data[el] <- mapply(function(x,y) {
          xx <- c(x,y); names(xx)[length(xx)] <- n; xx },
          res@edgeData@data[el],
          igraph::edge_attr(graph, n),
          SIMPLIFY=FALSE)
      }
    }
    
    res
  }
 
  # start adding edges 
  for(ii in 1:pmin(6,nrow(ijw))){ # you can add the first 6 without worry of violating planarity
    P[ijw[ii,1],ijw[ii,2]]<-ijw[ii,3]
    P[ijw[ii,2],ijw[ii,1]]<-ijw[ii,3]
  }
  
  E<-6
  P1<-P
  
  #   if(progBar==TRUE){pb <- txtProgressBar(max=(3*(ncol(data)-2)), style = 3)}
  
  while(E < 3*(ncol(data)-2)){
    ii<-ii+1
    P1[ijw[ii,1],ijw[ii,2]]<-ijw[ii,3] # adds the next edge
    P1[ijw[ii,2],ijw[ii,1]]<-ijw[ii,3]
    
    graphn <-suppressWarnings( # create graph
      graph_from_adjacency_matrix(P1, mode="undirected", weighted=TRUE) 
    )
    
    g<-igraph::as_graphnel(graphn) # make graphNEL for RBGL
    
    if(RBGL::boyerMyrvoldPlanarityTest(g)==TRUE){
      P<-P1
      E<-E+1
      
      # if(progBar==TRUE)
      # {setTxtProgressBar(pb, E)}
    }else{P1<-P}
    
    if(ii>(ncol(data)*(ncol(data)-1)/2)){message("PMFG not found")}
    
  }
  #   if(progBar==TRUE)
  #   {close(pb)}
  
  pmfg<-as.matrix(P)
  
  return(pmfg)
}


## 
## 
## ## Make a random network with 10 nodes
## set.seed(1)
## n=30
## fixed_edge_number = n
## fixed_threshold = .5
## 
## gg <- sample_gnp(n, .2)
## gg2 <- sample_gnp(n, .2)
## E(gg)$weight <- runif(length(E(gg)))
## E(gg2)$weight <- runif(length(E(gg2)))
## ggd <- as_adj(gg, sparse=FALSE, attr="weight")
## ggd2 <- as_adj(gg2, sparse=FALSE, attr="weight")
## 
## outputmat <- PMFG(ggd)
## outputmat2 <- PMFG(ggd2)
## 
## vsize = 10
## par(mfrow=c(4,2))
## par(mar=c(1,1,1,1))
## ## Weighted
## ww <- igraph::as.igraph(qgraph::qgraph(ggd,DoNotPlot=TRUE))
## E(ww)$width <- E(ww)$width/2
## l=layout_with_fr(ww)
## plot(ww, layout = l, vertex.size =vsize, vertex.label = NA, main = "Weighted")
## ww2 <- igraph::as.igraph(qgraph::qgraph(ggd2,DoNotPlot=TRUE))
## E(ww2)$width <- E(ww2)$width/2
## 
## l2=layout_with_fr(ww2)
## plot(ww2, layout = l2, vertex.size =vsize, vertex.label = NA )
## 
## # To make the networks no longer look weighted in plot
## E(ww2)$width <- 1
## E(ww)$width <- 1
## 
## ## Fixed edge number -- these are already ordered from weakest to strongest, but it's worth checking
## topN <- which(E(ww) %in% E(ww)[order(E(ww)$weight, decreasing=TRUE)][1:fixed_edge_number])
## wwfn <- ww %>% delete_edges(which(!(E(ww) %in% topN)))
## plot(wwfn, #layout = l,
##      vertex.size =vsize, vertex.label = NA, main = "Fixed number")
## topN2 <- which(E(ww2) %in% E(ww2)[order(E(ww2)$weight, decreasing=TRUE)][1:fixed_edge_number])
## wwfn2 <- ww2 %>% delete_edges(which(!(E(ww2) %in% topN2)))
## plot(wwfn2, #layout = l2,
##      vertex.size =vsize, vertex.label = NA)
## ## Fixed minimum relatedness
## wwfmr <- ww %>% delete_edges(which(E(ww)$weight < fixed_threshold))
## plot(wwfmr, #layout = l,
##      vertex.size =vsize, vertex.label = NA, main = "Fixed minimum relatedness")
## wwfmr2 <- ww2 %>% delete_edges(which(E(ww)$weight < fixed_threshold))
## plot(wwfmr2, #layout = l2,
##      vertex.size =vsize, vertex.label = NA)
## ## PMFG
## outputmat <- PMFG(ggd)
## 
## E(ww)$width <- 1
## ww <- igraph::as.igraph(qgraph::qgraph(outputmat,DoNotPlot=TRUE))
## E(ww)$width <- 1
## plot(ww, #layout=l,
##      vertex.size =vsize, vertex.label = NA, main = "Planar Maximally Filtered")
## outputmat2 <- PMFG(ggd2)
## ww2 <- igraph::as.igraph(qgraph::qgraph(outputmat2,DoNotPlot=TRUE))
## # To make the networks no longer look weighted in plot
## E(ww2)$width <- 1
## # E(ww2)$width <- E(ww2)$width/2
## plot(ww2, #layout=l2,
##      vertex.size =vsize, vertex.label = NA)
## 
## 


#### Figure 13-8 ####

rm(list=ls())

rd <- read.csv(file = "sample_data/YuvalHart_creativityData/edges.csv")
gr <- graph_from_edgelist(as.matrix(rd), directed = FALSE)
gr <- igraph::simplify(gr)
V(gr)$label = NA
set.seed(2)
cl <- fastgreedy.community(gr)
weights <- ifelse(igraph::crossing(cl, gr), 15, 1.5)
edgecols <- ifelse(igraph::crossing(cl, gr), "gray80", "black")

layout <- layout_with_kk(gr, weights=weights)



## Get Shapes
sd <- scan(file="sample_data/YuvalHart_creativityData/shapes.csv", what = " ")
sdo <- sapply(sd, function(x) strsplit(x, split = " -> "))
shapenames <-  lapply(sdo, function(x) x[1])
shapenames <- unlist(shapenames)
shapes <-  lapply(sdo, function(x) x[2])
shapes <- unlist(shapes)
sdd <- data.frame(shapenames, shapes)
sdd$shapes <- gsub("\\{", "", sdd$shapes)
sdd$shapes <- gsub("\\}", "", sdd$shapes)


## Build shapes
shapmat <- list(NULL)
for(i in 1:length(sdd$shapes)){ # 10 x 10
  numbs <- as.numeric(unlist(strsplit(sdd$shapes[i], split =  ", ")))
  x <- c()
  for(j in 1:length(numbs)){
      x <- rbind(x, intToBits(numbs[j])[1:10])
  }
  shapmat[[i]] <- x
}  
## shape to matrix
plotshape <- function(mv){
  nr <- nrow(mv)
  left <- 10 - nr
  if(left %% 2 == 0){
    upfil <- left / 2
    downfil <- upfil
  } else {
    upfil <- ceiling(left / 2)
    downfil <- upfil - 1
  }
  zb <- rep(0, 12)
  xm <- c()
  for(i in 1:upfil){
    xm <- rbind(xm, zb)
  }
  for(i in 1:nr){
    xm <- rbind(xm, c(0, as.integer(mv[i,]),0))
  }
  for(i in 1:downfil){
    xm <- rbind(xm, zb)
  }
  colfill <- xm[,1] 
  maxfil <- max(which(colSums(xm) != 0))
  minfil <- min(which(colSums(xm) != 0))
  chunk <- xm[,minfil:maxfil]
  filpas <- 12-dim(chunk)[2]
  if(filpas %% 2 == 0){
   matfil1 <-  matrix(0, ncol = filpas/2, nrow=length(colfill))
   matfil2 <-  matrix(0, ncol = filpas/2, nrow=length(colfill))
  } else {
   matfil1 <-  matrix(0, ncol = filpas/2, nrow=length(colfill))
   matfil2 <-  matrix(0, ncol = filpas/2-1, nrow=length(colfill))
  }
  xbb <- cbind(matfil1, chunk)
  xm <- cbind(xbb, matfil2)
  return(xm)
}

## Make cluster dataframe

clust <- scan(file="sample_data/YuvalHart_creativityData/clusters.csv", what = " ")
cdo <- sapply(clust, function(x) strsplit(x, split = " -> "))
clustnames <-  lapply(cdo, function(x) x[1])
clustnames <- unlist(clustnames)
cshapes <-  lapply(cdo, function(x) x[2])
cshapes <- unlist(cshapes)
cdd <- data.frame(clustnames, cshapes)
rownames(cdd) <-1:nrow(cdd)
cdd$cshapes <- gsub("\\{", "", cdd$cshapes)
cdd$cshapes <- gsub("\\}", "", cdd$cshapes)

## Plot all items in a cluster 
mbe <- membership(cl)

plotclusters <- function(x, panes){ # x = supercluster, max x = 14
 #par(mfrow=c(1,panes))
 mlist <- which(mbe == x)
 plist <- sample(mlist, panes)
 for(gg in 1:length(plist)){
   # Get cluster #
   dkd <- gsub("\\D", "", names(plist[gg]) )
   # Get shape number
   shapestr <- cdd[as.numeric(dkd),2]
   shapeles <- unlist(strsplit(shapestr, split = ", "))
   shapeles <- as.numeric(gsub("\\D", "", shapeles)  )
   shapeToPrint <- sample(shapeles, 1)
   plot(plotshape(shapmat[[shapeToPrint]]), axis.col = NULL, axis.row = NULL,  xlab = "", ylab="", main = "", key = NULL, col = c("gray20", "seagreen1"), border = NA, asp =TRUE)
 }
}

# Multi-panel plot

layout(matrix(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,3,4,5,6,7,8,9,10,11,12,13), 4, 7, byrow = FALSE))

plot(cl, gr, vertex.size=3, layout = layout,vertex.color=membership(cl), edge.color=edgecols)

par(mar=c(1.2,2,0,0))
set.seed(5)
plotclusters(5,4)
text(6,-.8, "Group 1", cex = 1)
set.seed(5)
plotclusters(7,4)
text(6,-1, "Group 2", cex = 1)
set.seed(5)
plotclusters(13,4)
text(6,-1, "Group 3", cex = 1)



