
### Behavioral Network Science
### Language, Mind, and Society
# By Thomas T. Hills

# Code to reproduce figures and analyses for

#### Chapter 19 -- Fund People Not Projects: A Universal Basic Income for Research ####


knitr::opts_chunk$set(echo = FALSE)
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(comment = NA)
knitr::opts_chunk$set(warning = FALSE)
options(knitr.table.format = "latex")

rm(list=ls())
library(igraph)
library(tidyverse)



#### Figure 19-2 ####

# Set random seed
set.seed(1)
# How many simulations--stictly, iterations.
sims = 20
# How many nodes
n =500 
# How many targets for contributions
ep = 4 
# Fraction to be given away
F = .5 # Fraction to be given away
# Matrix n x n 
gmat <- matrix(0, nrow = n, ncol = n)
# List of nodes
x <- 1:n
# Set buffer to capture pairs
pairs <- c()
# Each agent choose ep pairs to contribute to 
for(i in 1:n){ 
  pairs <- rbind(pairs, 
                 # Repeat self ep times
                 matrix(c(rep(i,ep),  
                          # Sample but don't choose self
                          sample(x[-i], size = ep,  
                 replace = FALSE)), ncol=2))
}

# Make graph from pairs
g <- graph_from_edgelist(pairs,directed=TRUE) 
# Compute degrees in network
#degree(g, mode="out")
#degree(g, mode="in")
# Get adjacency matrix
gmat <- as.matrix(as_adj(g))
# Starting budget
Budget_Injection <- rep(100000, n)
ID_budgets <- rep(0, n)
# Allocate budgets
ID_budgets <- ID_budgets + Budget_Injection 
# Start the budget keeper
B <- ID_budgets
# For each sim
for(i in 1:sims){
  # Update budgets
  ID_budgets <-  Budget_Injection + 
    colSums( (gmat/(rowSums(gmat))) * (ID_budgets*F)) # 
  # Add vector to budget keeper
  B <- rbind(B, ID_budgets)
}
# Store it
B1 <- B

# Save graph for statistics below
g1 <- g

# Make distribution graph for rank based
F = .5 # fraction to be given away
# Node vector
x <- 1:n
# Slope of preferences
a =1 
# Pairs buffer
pairs <- c()
# For each agent
for(i in 1:n){ 
  # Choose your contributions based on rank
  pairs <- rbind(pairs, 
                 matrix(c(rep(i,ep), 
                 sample(x[-i], size = ep, prob=x[-i]^(-a), 
                 replace = FALSE)), ncol=2))
}
# Make graph
g <- graph_from_edgelist(pairs,directed=TRUE) 
# Save network
g2 <- g
#degree(g, mode="out")
#degree(g, mode="in")
# Get adjacency
gmat <- as.matrix(as_adj(g))
# Budgets (as above)
Budget_Injection <- rep(100000, n)
ID_budgets <- rep(0, n)
ID_budgets <- ID_budgets + Budget_Injection # allocate budgets
numberOfNewNeighbors =20 
B2 <- ID_budgets
for(i in 1:sims){
  ID_budgets <-  Budget_Injection + 
    colSums( (gmat/(rowSums(gmat))) * (ID_budgets*F)) # 
  B2 <- rbind(B2, ID_budgets)
}

par(mfrow=c(1,1))
## Plot image of allocations
for(i in 1:nrow(B)){
  x <- B[i,]
  yy <-  sort(x, decreasing=TRUE)
  xx <- 1:length(yy)
  if(i == 1){
    par(mar=c(5,5,2,2))
    plot(xx, yy[xx], log="xy",cex.lab=1.5, cex.axis=1.2, ylim=c(min(B),max(B2)*1.2), xlim = c(1,n), ylab = "Total funding in dollars", xlab="Rank", type="n")
  }
  # Make grayness a function of order
  colstring <- paste("grey", toString(100-i*round(80/nrow(B))), sep="")
  lines(xx, yy[xx],col=colstring)
  
  x <- B2[i,]
  yy <-  sort(x, decreasing=TRUE)
  xx <- 1:length(yy)
  colstring <- paste("grey", toString(100-i*round(80/nrow(B))), sep="")
  if(i > 1) lines(xx, yy[xx],col=colstring, lty = 2)
}
legend(05, 6000000, legend=c("Lottery graph", "Ranked preference graph"), lty = 1:2, lwd = 1.5)


#### Figure 19-3 ####

# Sequence from 0 to end 
x <- seq(0, nrow(B1)-1)
# For each item in sequence
for(i in 1:(nrow(B1)-1)){
  x[i]<- sum((B1[i+1,]-B1[i,])) / sum(B1[i,])
}

x2 <- seq(0, nrow(B2)-1)
for(i in 1:(nrow(B2)-1)){
  x2[i]<- sum((B2[i+1,]-B2[i,])) / sum(B2[i,])
}
# Plot margins
par(mar=c(5,5,2,2))
# Plot
plot(x[1:10], cex.axis = 1.2, xlim = c(1, 10), cex.lab = 1, xlab="Iterations", ylab="Proportional difference from previous year")
lines(x2[1:10], col = "red")
lines(x[1:10], lwd=2)
lines(1:10,y=rep(.01,10), lty=2)

## eigenvector comparison

# Statistics based on eigenvector centrality
ec1 <- igraph::eigen_centrality(g1)$vector # compute eigenvector centrality
pr1 <- igraph::page_rank(g1, damping=.5)$vector
ac1 <- alpha_centrality(g1, alpha = .1)
# Eigenvector centrality correlation with final budgets
cor.test(B1[nrow(B1),], ec1)
# Pagerank correlation with final budgets
cor.test(B1[nrow(B1),], pr1)
# Alpha centrality with final budgets
cor.test(B1[nrow(B1),], ac1)

# Sambe for B2
ec2 <- igraph::eigen_centrality(g)$vector # compute eigenvector centrality
pr2 <- igraph::page_rank(g, damping = .5)$vector
ac2 <- alpha_centrality(g, alpha = .1)
cor.test(B2[nrow(B2),], ec2)
cor.test(B2[nrow(B2),], pr2)
cor.test(B2[nrow(B2),], ac2)

