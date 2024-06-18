
### Behavioral Network Science
### Language, Mind, and Society
# By Thomas T. Hills

# Code to reproduce figures and analyses for

#### Chapter 18 -- The Kennedy Paradox: Games of Conflict and Escalation ####


knitr::opts_chunk$set(echo = FALSE)
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(comment = NA)
knitr::opts_chunk$set(warning = FALSE)
options(knitr.table.format = "latex")

library(igraph)
library(tidyverse)
library(kableExtra)
library(Rmisc)


#### Table 18-1 ####

# Clear the decks
rm(list=ls())
# Set up payoffs
all_pairs <- c("$R=3$", "$S=0$", "$T=5$", "$P=2$")
# Make matrix from payoffs
payoff.mat <- matrix(all_pairs, nrow=2, byrow=T)
# Label dimensions
dimnames(payoff.mat) <- c(rep(list(c("Cooperate","Defect")), 2))
# Make table
kable(payoff.mat,row.names=TRUE, escape = FALSE, caption = "The payoff matrix for the prisoner's Dilemma. Each player can choose to cooperate or defect. The values in each cell indicate the payoff for the strategy on the left played against the strategy on the top. $R$ is the reward payoff. $T$ is the temptation payoff. $P$ is the punishment payoff. And $S$ is the sucker's payoff. The exact numerical values assigned to these payoffs are less important than their position relative to one another. A prisoner's dilemma is any game with the structure $T > R > P > S$.", digits=2 ) %>% kable_classic(full_width = F, html_font = "Cambria")



#### Figure 18-1 ####

# Set random seed
set.seed(1)

# Set simulation parameters
# Show two rounds of play
rounds = 2
# Provide 9 examples
examples =9 
# Plot in 3 x 9 frames
par(mfrow=c(examples,rounds+1))
par(mar=c(0,0,0,0))

# Run sim
for(ex in 1:examples){
  # Make fully connected network (20 nodes)
  gameNetwork <- make_full_graph(20)
  # Assign strategies at random but with increasing probability of defection (ex*.1-.1)
  V(gameNetwork)$strategy <- rbinom(length(V(gameNetwork)), 1, ex*.1-.1)+1
  # Layout
  l = layout_with_fr(gameNetwork) 
  # Remove labels
  V(gameNetwork)$label = NA
  # Plot with color according to strategy the original networks
  plot(gameNetwork, vertex.color=ifelse(V(gameNetwork)$strategy==1, "darkolivegreen1", "red1"), layout=l, vertex.size = 20, edge.width = .5)
  # Payoff matrix
  payoff_matrixPD = matrix(c(3, 0, 5, 1), ncol = 2, byrow = TRUE)
  # Function to get strategy attributes
  get_strategies <- function(players){
    strats <- V(gameNetwork)$strategy[players]
    return(strats)
  } 
  # Get payoffs based on strategies
  get_payoffs <- function(strats, pm){
   p1 <- pm[strats[1], strats[2]] 
   transposePM <- t(pm)
   p2 <- transposePM[strats[1], strats[2]]
   return(c(p1, p2))
  }
  # For rounds
  for(i in 1:rounds){
   # Each round play over all edges
   # Keep track of each players payoffs
    
   # Reset payoff value to 0 at start of each round
   V(gameNetwork)$value <- 0
   # For each edge
   for(j in 1:length(E(gameNetwork))) { # play over all edges each round
     # Get players/nodes at end of edge j
      players = ends(gameNetwork, j) 
      # Get strategies from players
      strategies =  get_strategies(players)
      # Get payoffs from strategies
      payoffs <- get_payoffs(strategies, payoff_matrixPD)
      # Adjust payoffs for both players
      V(gameNetwork)$value[players[1]] <- V(gameNetwork)$value[players[1]] + payoffs[1]
      V(gameNetwork)$value[players[2]] <- V(gameNetwork)$value[players[2]] + payoffs[2]
   }
   # Plot payoffs
   # Adjust node size according to payoffs
   V(gameNetwork)$size <- V(gameNetwork)$value + 4
   # Plot with color and size
   plot(gameNetwork, vertex.color=ifelse(V(gameNetwork)$strategy==1, "darkolivegreen1", "red1"), layout=l, edge.width = .5)
   # Update strategies
   # Set strategy buffer so we can do everyone based on current
   V(gameNetwork)$strategybuf <- NA
    for(nodek in 1:length(V(gameNetwork))){
     # Different possible strategies 
      # Choose strategy of max neighbor
      
      # Get neighbors
      neebs <- neighbors(gameNetwork, V(gameNetwork)[nodek])
      # Get payoff values after last round
      neebValues <- V(gameNetwork)$value[neebs]
      # Strategy change
      # If there is only one strategy, switch to it, otherwise stay as you are
      
      # First stay as you are
      V(gameNetwork)$strategybuf[nodek] <- V(gameNetwork)$strategy[nodek]
      # Then change...
      # If max of neighbor values is greater than your own value
      if(max(neebValues) > V(gameNetwork)$value[nodek]){
        # Set best strategy as best strategy of neighbors
        bestStrategy <- V(gameNetwork)$strategy[neebs[which(neebValues==max(neebValues))]]
        # If there is only one best strategy, change (otherwise it includes the nodes present strategy)
        if(length(unique(bestStrategy)) == 1){
          V(gameNetwork)$strategybuf[nodek] <- bestStrategy
        }
      }
    }
   # Change all strategies simultaneously from buffer
   V(gameNetwork)$strategy <- V(gameNetwork)$strategybuf
   
  }
} # end of examples



# This figure is not in the book.
# It produces random networks with random strategy assignments.
# It follows the same logic as Figure 18.1 but the network structures are different

# Figure description: "This prisoner's dilemma on Erdös-Renyi random graphs starting with different proportions of defectors in the starting network on the left. Each network has 20 nodes and edge probability of .2. Moving rightward, individuals play against each of the neighbors and then follow the best performing strategy in their neighborhood (including themselves).  Performance is measured as the sum of payoffs, giving higher degree nodes more impact. Moving downward, each row starts with more defectors. The different outcomes reflect the impact of structure."

set.seed(6)
rounds = 4
examples =6 
par(mfrow=c(examples,rounds+2))
par(mar=c(0,0,0,0))
for(ex in 1:examples){
  gameNetwork <- sample_gnp(20, .2)
  V(gameNetwork)$strategy <- rbinom(length(V(gameNetwork)), 1, ex*.1-.1)+1
  V(gameNetwork)$label <- 1:length(V(gameNetwork))
  l = layout_with_fr(gameNetwork) 
  V(gameNetwork)$label = NA
  plot(gameNetwork, vertex.color=ifelse(V(gameNetwork)$strategy==1, "darkolivegreen1", "red1"), layout=l, vertex.size = 20, vertex.label.cex = 1, vertex.label.color = "black")
  payoff_matrixPD = matrix(c(3, 0, 5, 1), ncol = 2, byrow = TRUE)
  get_strategies <- function(players){
    strats <- V(gameNetwork)$strategy[players]
    return(strats)
  } 
  get_payoffs <- function(strats, pm){
   p1 <- pm[strats[1], strats[2]] 
   transposePM <- t(pm)
   p2 <- transposePM[strats[1], strats[2]]
   return(c(p1, p2))
  }
  for(i in 1:rounds){
   V(gameNetwork)$value <- 0
   for(j in 1:length(E(gameNetwork))) { # play over all edges each round
      players = ends(gameNetwork, j) 
      strategies =  get_strategies(players)
      payoffs <- get_payoffs(strategies, payoff_matrixPD)
      V(gameNetwork)$value[players[1]] <- V(gameNetwork)$value[players[1]] + payoffs[1]
      V(gameNetwork)$value[players[2]] <- V(gameNetwork)$value[players[2]] + payoffs[2]
   }
   V(gameNetwork)$size <- V(gameNetwork)$value + 4
   plot(gameNetwork, vertex.color=ifelse(V(gameNetwork)$strategy==1, "darkolivegreen1", "red1"), layout=l)
   V(gameNetwork)$strategybuf <- NA
    for(nodek in 1:length(V(gameNetwork))){
      neebs <- neighbors(gameNetwork, V(gameNetwork)[nodek])
      neebValues <- V(gameNetwork)$value[neebs]
      V(gameNetwork)$strategybuf[nodek] <- V(gameNetwork)$strategy[nodek]
      if(max(neebValues) > V(gameNetwork)$value[nodek]){
        bestStrategy <- V(gameNetwork)$strategy[neebs[which(neebValues==max(neebValues))]]
        if(length(bestStrategy) == 1){
          V(gameNetwork)$strategybuf[nodek] <- bestStrategy
        }
      }
    }
   V(gameNetwork)$strategy <- V(gameNetwork)$strategybuf
  }
  V(gameNetwork)$size <- V(gameNetwork)$value + 4
  plot(gameNetwork, vertex.color=ifelse(V(gameNetwork)$strategy==1, "darkolivegreen1", "firebrick3"), layout=l)
} # end of examples



#### Figure 18-2 ####

# Set random seed
set.seed(3)
# Plot parameters
rounds = 2
par(mfrow=c(1,rounds+1))
par(mar=c(0,0,0,0))
# 20 nodes in each network--Caveman graphs with two caves
N =20 
  # Make networks
  gameNetworkc <- make_full_graph(N)
  # Set strategies
  V(gameNetworkc)$strategy <- 1
  # Make other network
  gameNetworkd <- make_full_graph(N)
  # Set alternative strategy
  V(gameNetworkd)$strategy <- 2
  # Combein networks
  gameU <- gameNetworkc + gameNetworkd
  # Add 1 edge between the two caves
  gameU <- igraph::add.edges(gameU, c(1, N+1))
  # Set layout
  l = layout_with_kk(gameU) 
  # Remove labels
  V(gameU)$label = NA
  # Plot
  plot(gameU, vertex.color=ifelse(V(gameU)$strategy==1, "darkolivegreen1", "red1"), layout=l, vertex.size = 15, edge.width = .5)
  # Rename
  gameNetwork <- gameU
  # Payoff matrix
  payoff_matrixPD = matrix(c(3, 0, 5, 1), ncol = 2, byrow = TRUE)
  
  # Function to get strategy attribute
  get_strategies <- function(players){
    strats <- V(gameNetwork)$strategy[players]
    return(strats)
  } 
  # Get payoffs  
  get_payoffs <- function(strats, pm){
   p1 <- pm[strats[1], strats[2]] 
   transposePM <- t(pm)
   p2 <- transposePM[strats[1], strats[2]]
   return(c(p1, p2))
  }
  # Rounds
  for(i in 1:rounds){
   # Each round play over all edges
   # Keep track of each players payoffs
   # Reset value to 0
   V(gameNetwork)$value <- 0
   for(j in 1:length(E(gameNetwork))) { # Play over all edges each round
      players = ends(gameNetwork, j) 
      strategies =  get_strategies(players)
      payoffs <- get_payoffs(strategies, payoff_matrixPD)
      V(gameNetwork)$value[players[1]] <- V(gameNetwork)$value[players[1]] + payoffs[1]
      V(gameNetwork)$value[players[2]] <- V(gameNetwork)$value[players[2]] + payoffs[2]
   }
   # Plot networks with payoffs and strategy
   V(gameNetwork)$size <- V(gameNetwork)$value/4 + 4 
   plot(gameNetwork, vertex.color=ifelse(V(gameNetwork)$strategy==1, "darkolivegreen1", "red1"), layout=l, edge.width = .5)
   # Update strategies
   # Set strategy buffer so we can do everyone based on current
   V(gameNetwork)$strategybuf <- NA
    for(nodek in 1:length(V(gameNetwork))){
      # Choose strategy of max neighbor
      # Get neighbors
      neebs <- neighbors(gameNetwork, V(gameNetwork)[nodek])
      # Get values
      neebValues <- V(gameNetwork)$value[neebs]
      # Strategy change
      # If there is only one strategy, switch to it, otherwise stay as you are
      # First stay as you are
      V(gameNetwork)$strategybuf[nodek] <- V(gameNetwork)$strategy[nodek]
      # Then change...
      # If max of neighbor values is greater than my value
      if(max(neebValues) > V(gameNetwork)$value[nodek]){
        bestStrategy <- V(gameNetwork)$strategy[neebs[which(neebValues==max(neebValues))]]
        # Then change if necessary
        if(length(unique(bestStrategy)) == 1){
          V(gameNetwork)$strategybuf[nodek] <- bestStrategy
        }
      }
    }
   V(gameNetwork)$strategy <- V(gameNetwork)$strategybuf
  }



#### Preparation for Figure 18.3 ####

# We start with caveman graph with two disconnected caves, then swap edges so they become increasingly disassorative, computing proportion of stable cooperators after repeated interactions.

# Set random seed
set.seed(1)
# Simulation per level of assortativity
simulationsPerAss = 50 
## Making starting networks
# Number of nodes
  N =30 
  # Make the cooperator network
  gameNetworkc <- make_full_graph(N)
  V(gameNetworkc)$strategy <- 1
  # Make the Defector network
  gameNetworkd <- make_full_graph(N)
  V(gameNetworkd)$strategy <- 2
  # Combine them
  gameU <- gameNetworkc + gameNetworkd
  # Get edges
  listofEdgesToSwap <- E(gameU)
  # Get edges between cooperators
  splitpoint = length(E(gameU))/2 # everything below this is edges between cooperators
  # Number of swaps needed to fully integrate networks
  swaps = length(E(gameU))
  # Set up data buffers
  prop_cooperators30 <- matrix(NA, nrow=swaps/2, ncol=simulationsPerAss)
  assoi30 <- matrix(NA, nrow=swaps/2, ncol=simulationsPerAss)
  # For each simulation
for(spa in 1:simulationsPerAss){
  # Get edge lists to swap--this is so we use all edges once
  edgeswaplistD <-sample(1:splitpoint, splitpoint)
  edgeswaplistC <-sample((splitpoint+1):swaps, splitpoint)
  # Get end points for all edges -- they will move around once we start adding and deleting edges, so do it before that
  defers <- ends(gameU, edgeswaplistD)
  cooers <- ends(gameU, edgeswaplistC)
  
  # Payoffs
  Tr = 5
  Pr = 1
  Sr = 0
  Rr = 3
  gameNetwork <- gameU
  # Longkey to protect during matrix payoff assignment
  lkey <- .0103408 # random number
  # For each edge swap
  for(sw in 1:(swaps/2)){
    # Take row, but randomize order
    defers_now <- sample(defers[sw,],2)
    cooers_now <- sample(cooers[sw,],2)
    # Add and delete edges accordingly
    gameNetwork <- igraph::add_edges(gameNetwork, c(defers_now[1], cooers_now[1]))  
    gameNetwork <- igraph::add_edges(gameNetwork, c(defers_now[2], cooers_now[2]))  
    gameNetwork <- igraph::delete_edges(gameNetwork, paste(defers[sw,], collapse = "|"))
    gameNetwork <- igraph::delete_edges(gameNetwork, paste(cooers[sw,], collapse = "|"))
    # Compute assortativity
    assoi30[sw, spa] <- assortativity_nominal(gameNetwork, types=V(gameNetwork)$strategy, directed = FALSE)
    # Rename it
    gplayout <- gameNetwork
    # Run game on network until it stabilizes -- no one changes strategy
    # rounds = 10
    #for(ri in 1:rounds){
    sts <- rep(0, length(V(gplayout)$strategy)) # list of past strategies
    check = TRUE 
    roundsi <- 0
    while(check & roundsi < 50){# stop if stable or 50 rounds 
     # Keep track of each players payoffs
     # Reset values to 0
     V(gplayout)$value <- 0
     # Get adjacency matrix  
     mat1 <- as_adj(gplayout, sparse=FALSE)
     # Get all strategies
     sts <- V(gplayout)$strategy 
     # Assign strategies to matrix
     p1plays_against <- mat1*sts
     # Transpose and assign strategies for player 2
     p2plays_against <- t(t(mat1)*sts)
     # Combine strategies to create a unique identifier
     # e.g.,  C against C = 1*3-1+key = 2 + key below --> Rr
     dmat<-p1plays_against*3-p2plays_against+lkey # add key
     # Compute payoff based on identifier
     dmat[which(dmat==0+lkey)] <- NA
     dmat[which(dmat == 4+lkey)] <- Pr # defectors against defectors
     dmat[which(dmat == 5+lkey)] <- Tr # defectors against cooperator 
     dmat[which(dmat == 2+lkey)] <- Rr # cooperator against cooperator 
     dmat[which(dmat == 1+lkey)] <- Sr # cooperator against cooperator 
     # Assign payoff
     payoffsi <- dmat  
     # Compute values for each player against all their neighbors
     V(gplayout)$value <- rowMeans(payoffsi, na.rm=T)
     # Update strategies
     # Matrix with payoffs for all neighbors
     paymat <- t(t(mat1)* V(gplayout)$value)
     # Add (own) individual value to diagonal
     diag(paymat) <- V(gplayout)$value
     # Find index of best strategy in each row [take first best]
     # First best may fail in case of ties, so check if you care
     indimax <- apply(paymat, 1, 
                      function(x) sts[which(x==max(x,na.rm=T))][1])
     # Old strategies
     V(gplayout)$strategybuf <- sts
     # New strategies
     V(gplayout)$strategy <- indimax
     # Is number of changes greater than 0 -- this stops the for loop if false
     check <- sum(abs(sts-V(gplayout)$strategy)) > 0
     # Iterate rounds
     roundsi <- roundsi + 1
    } 
    # Count cooperators
    prop_cooperators30[sw, spa] <- sum(V(gplayout)$strategy == 1) / length(V(gplayout))
    #
    # if(assoi30[sw,spa] < .4 & prop_cooperators30[sw, spa] > 0){
    # #  plot(gplayout,layout=l, vertex.color=ifelse(V(gplayout)$strategy==1, "pink", "red"), layout=l, vertex.size = 5, vertex.label=NA)
    # }
  }
}  


# Commented as above but smaller network size

# Set random seed
set.seed(1)
# Simulation per level of assortativity
simulationsPerAss = 50 
## Making starting networks
# Number of nodes
  N =10 
  gameNetworkc <- make_full_graph(N)
  V(gameNetworkc)$strategy <- 1
  gameNetworkd <- make_full_graph(N)
  V(gameNetworkd)$strategy <- 2
  gameU <- gameNetworkc + gameNetworkd
  listofEdgesToSwap <- E(gameU)
  splitpoint = length(E(gameU))/2 
  swaps = length(E(gameU))
  # Data for network size 10
  prop_cooperators10 <- matrix(NA, nrow=swaps/2, ncol=simulationsPerAss)
  assoi10 <- matrix(NA, nrow=swaps/2, ncol=simulationsPerAss)
    
for(spa in 1:simulationsPerAss){
  edgeswaplistD <-sample(1:splitpoint, splitpoint)
  edgeswaplistC <-sample((splitpoint+1):swaps, splitpoint)
  l = layout_with_fr(gameU)
  defers <- ends(gameU, edgeswaplistD)
  cooers <- ends(gameU, edgeswaplistC)
  # Payoffs
  Tr = 5
  Pr = 1
  Sr = 0
  Rr = 3
  gameNetwork <- gameU
  lkey <- .0103408 
  for(sw in 1:(swaps/2)){
    defers_now <- sample(defers[sw,],2)
    cooers_now <- sample(cooers[sw,],2)
    gameNetwork <- igraph::add_edges(gameNetwork, c(defers_now[1], cooers_now[1]))  
    gameNetwork <- igraph::add_edges(gameNetwork, c(defers_now[2], cooers_now[2]))  
    gameNetwork <- igraph::delete_edges(gameNetwork, paste(defers[sw,], collapse = "|"))
    gameNetwork <- igraph::delete_edges(gameNetwork, paste(cooers[sw,], collapse = "|"))
    # Assortativity for network size 10
    assoi10[sw, spa] <- assortativity_nominal(gameNetwork, types=V(gameNetwork)$strategy, directed = FALSE)
    
    gplayout <- gameNetwork
    sts <- rep(0, length(V(gplayout)$strategy)) 
    check = TRUE 
    roundsi <- 0
    while(check & roundsi < 30){
       V(gplayout)$value <- 0
       mat1 <- as_adj(gplayout, sparse=FALSE)
       sts <- V(gplayout)$strategy 
       p1plays_against <- mat1*sts
       p2plays_against <- t(t(mat1)*sts)
       dmat<-p1plays_against*3-p2plays_against+lkey 
       dmat[which(dmat==0+lkey)] <- NA
       dmat[which(dmat == 4+lkey)] <- Pr # defectors against defectors
       dmat[which(dmat == 5+lkey)] <- Tr # defectors against cooperator 
       dmat[which(dmat == 2+lkey)] <- Rr # cooperator against cooperator 
       dmat[which(dmat == 1+lkey)] <- Sr # cooperator against cooperator 
       payoffsi <- dmat 
       V(gplayout)$value <- rowMeans(payoffsi, na.rm=T)
       paymat <- t(t(mat1)* V(gplayout)$value)
       diag(paymat) <- V(gplayout)$value
       indimax <- apply(paymat, 1, 
                        function(x) sts[which(x==max(x,na.rm=T))][1])
       V(gplayout)$strategybuf <- sts
       V(gplayout)$strategy <- indimax
      check <- sum(abs(sts-V(gplayout)$strategy)) > 0
      roundsi <- roundsi + 1
    } 
  # Data output for this network size
  prop_cooperators10[sw, spa] <- sum(V(gplayout)$strategy == 1) / length(V(gplayout))
  }
}  





# Commented as above but still smaller network size

# Set random seed
set.seed(1)
# Simulation per level of assortativity
simulationsPerAss = 50 
## Making starting networks
# Number of nodes
  N =5 
  gameNetworkc <- make_full_graph(N)
  V(gameNetworkc)$strategy <- 1
  gameNetworkd <- make_full_graph(N)
  V(gameNetworkd)$strategy <- 2
  gameU <- gameNetworkc + gameNetworkd
  listofEdgesToSwap <- E(gameU)
  splitpoint = length(E(gameU))/2 # everything below this is edges between cooperators
  swaps = length(E(gameU))
  # Data for this network size
  prop_cooperators5 <- matrix(NA, nrow=swaps/2, ncol=simulationsPerAss)
  assoi5 <- matrix(NA, nrow=swaps/2, ncol=simulationsPerAss)
  for(spa in 1:simulationsPerAss){
    edgeswaplistD <-sample(1:splitpoint, splitpoint)
    edgeswaplistC <-sample((splitpoint+1):swaps, splitpoint)
    l = layout_with_fr(gameU)
    defers <- ends(gameU, edgeswaplistD)
    cooers <- ends(gameU, edgeswaplistC)
    
    # Payoffs
    Tr = 5
    Pr = 1
    Sr = 0
    Rr = 3
    gameNetwork <- gameU
    lkey <- .0103408 # random number
  for(sw in 1:(swaps/2)){
    defers_now <- sample(defers[sw,],2)
    cooers_now <- sample(cooers[sw,],2)
    gameNetwork <- igraph::add_edges(gameNetwork, c(defers_now[1], cooers_now[1]))  
    gameNetwork <- igraph::add_edges(gameNetwork, c(defers_now[2], cooers_now[2]))  
    gameNetwork <- igraph::delete_edges(gameNetwork, paste(defers[sw,], collapse = "|"))
    gameNetwork <- igraph::delete_edges(gameNetwork, paste(cooers[sw,], collapse = "|"))
    assoi5[sw, spa] <- assortativity_nominal(gameNetwork, types=V(gameNetwork)$strategy, directed = FALSE)
    gplayout <- gameNetwork
    sts <- rep(0, length(V(gplayout)$strategy)) # list of past strategies
    check = TRUE 
    roundsi <- 0
    while(check & roundsi < 30){# stop if stable
       V(gplayout)$value <- 0
       mat1 <- as_adj(gplayout, sparse=FALSE)
       sts <- V(gplayout)$strategy # list of past strategies
       p1plays_against <- mat1*sts
       p2plays_against <- t(t(mat1)*sts)
       dmat<-p1plays_against*3-p2plays_against+lkey # add key
       dmat[which(dmat==0+lkey)] <- NA
       dmat[which(dmat == 4+lkey)] <- Pr # defectors against defectors
       dmat[which(dmat == 5+lkey)] <- Tr # defectors against cooperator 
       dmat[which(dmat == 2+lkey)] <- Rr # cooperator against cooperator 
       dmat[which(dmat == 1+lkey)] <- Sr # cooperator against cooperator 
       payoffsi <- dmat # (dmat-1)^.3
       V(gplayout)$value <- rowMeans(payoffsi, na.rm=T)
       paymat <- t(t(mat1)* V(gplayout)$value)
       diag(paymat) <- V(gplayout)$value
       indimax <- apply(paymat, 1, 
                        function(x) sts[which(x==max(x,na.rm=T))][1])
       V(gplayout)$strategybuf <- sts
       V(gplayout)$strategy <- indimax
      check <- sum(abs(sts-V(gplayout)$strategy)) > 0
      roundsi <- roundsi + 1
      } 
    prop_cooperators5[sw, spa] <- sum(V(gplayout)$strategy == 1) / length(V(gplayout))
    }
}  







#### Figure 18-3 ####

# plot data outputs from above -- first take the rowmeans
pc30 <- rowMeans(prop_cooperators30)
as30 <- rowMeans(assoi30)
pc10 <- rowMeans(prop_cooperators10)
as10 <- rowMeans(assoi10)
pc5 <- rowMeans(prop_cooperators5)
as5 <- rowMeans(assoi5)
# Plot assortativity against number of cooperators
par(mfrow=c(1,1))
plot(as30, pc30, xlab="Assortativity", ylab="Proportion of cooperators", type="l", lwd = 2, lty=1)
lines(as10, pc10, type="l", lwd = 2, lty=2) # this is n = 10
lines(as5, pc5, type="l", lwd = 2, lty=3)
legend(-1, .6, legend=c("30","10", "5"),title="Group size", lty=c(1,2,3))



## Code adapted from Barkoczi and Galesic, 2016
# Set seed
set.seed(1)
# Number of nodes
Nodes<-40
# Degree
degree=5
#
sims <- 20 # number of starting networks
# Rewiring iterations for each network
its <- 5000 # rewiring iterations for each networks

generate_network<-function(Nodes,degree){
  # Makes a random network with nodes each with same 'degree'
  # Returns edge list
  a<-1:Nodes
  temp<-matrix(NA,Nodes,degree)
  # Generate network
  while(any(is.na(temp)))
  {
    # Create matrices
    temp<-matrix(NA,Nodes,degree)
    collect<-matrix(0,Nodes,1)
    # Make edge list
    network<-matrix(NA,ncol=2,nrow=Nodes*degree)
    # Generate network
    for (i in 1:Nodes){
      if(i==1){
      # Sample nodes that are not node i
      contacts<-sample(setdiff(1:Nodes,a[i]),degree)
      # Set node i to degree
      collect[i,]<-degree
      # Set neighbors of node i to +1 degree
      collect[c(contacts),]<-collect[c(contacts),]+1
      # Add contacts to matrix for node i
      temp[i,]<-contacts
      # Add node 1 to all its of neighbors rows in the matrix
      for(k in 1:length(contacts)){
        temp[contacts[k],which.min(is.na(temp[contacts[k],]))]<-i
      }
      # Do same for rest of nodes 
      } else {
        tryCatch({
          # For each node not full with edges, sample nodes that are not full with edges
          contacts<-sample(setdiff(which(collect<=(degree-1)),i),degree-collect[i,1])
          # Add edge (+1) to each new neighbors
          if(length(collect[c(contacts),])>0){collect[c(contacts),]<-collect[setdiff(contacts,i),]+1}
          # Add i to all its new neighbors
          if(length(collect[c(contacts),])>0){for(k in 1:length(contacts)){
            temp[contacts[k],min(which(is.na(temp[contacts[k],])))]<-i
          }}
          # If it's not yet full with neighbors
          if(is.logical(temp[i,which(is.na(temp[i,]))])==FALSE){
            # Add them
            temp[i,which(is.na(temp[i,]))]<-contacts} else{}
            # Add degree for completion
            collect[i,]<-degree
          # Otherwise give warnings
        },warning = function(q){
        },error = function(w){
        },finally = {})
      }}
  }
  # Make edge list
  network[,1]<-rep(1:Nodes,each=degree)
  # Add edges to edge list
  for(i in 1:Nodes) network[network[,1]==i,2]<-temp[i,]
  # Return edge list
  return(network)
}

# Function to update network to search for max on metric
RW.degreeFIXmax.clustering<-function(metric,g,reps,stat){
  measure<-vector()
  # Start with measure of random graph
  measure[1]<-stat(metric(g))
  for (i in 2:reps){
    # Rewire random edge; see below for function
    net<-rewire(network)
    # Make network
    g<-graph.edgelist(net,directed=T)
    # Compute metric (e.g., clustering coefficient)
    new<-stat(metric(g))
    # If new metric is an improvement, update network
    if(new>measure[i-1] && clusters(g)$no==1){ network<-net;measure[i]<-new} else {measure[i]<-measure[i-1]
    }}
  return(network)
}

# As above for clustering 
RW.degreeFIXmin.clustering<-function(metric,g,reps,stat){
  measure<-vector()
  measure[1]<-stat(metric(g))
  for (i in 2:reps){
    net<-rewire(network)
    g<-graph.edgelist(net,directed=T)
    new<-stat(metric(g))
    if(new<measure[i-1] && clusters(g)$no==1){ network<-net;measure[i]<-new} else {measure[i]<-measure[i-1]
    }}
  return(network)
}

# As above for closeness
RW.degreeFIXmax.closeness<-function(metric,g,reps,stat){
  measure<-vector()
  measure[1]<-stat(metric(g)$res)
  for (i in 2:reps){
    net<-rewire(network)
    g<-graph.edgelist(net,directed=T)
    new<-stat(metric(g)$res)
    if(new>measure[i-1] && clusters(g)$no==1){ network<-net;measure[i]<-new} else {measure[i]<-measure[i-1]
    }}
  return(network)
}

# As above for min closeness
RW.degreeFIXmin.closeness<-function(metric,g,reps,stat){
  measure<-vector()
  measure[1]<-stat(metric(g)$res)
  for (i in 2:reps){
    net<-rewire(network)
    g<-graph.edgelist(net,directed=T)
    new<-stat(metric(g)$res)
    if(new<measure[i-1] && clusters(g)$no==1){ network<-net;measure[i]<-new} else {measure[i]<-measure[i-1]
    }}
  return(network)
}

# As above for max betweenness
RW.degreeFIXmax.betwenness<-function(metric,g,reps,stat){
  measure<-vector()
  measure[1]<-stat(metric(g,directed=T))
  for (i in 2:reps){
    net<-rewire(network)
    g<-graph.edgelist(net,directed=T)
    new<-stat(metric(g,directed=T))
    if(new>measure[i-1] && clusters(g)$no==1){ network<-net;measure[i]<-new} else {measure[i]<-measure[i-1]
    }}
  return(network)
}

# As above for min betweenness
RW.degreeFIXmin.betweenness<-function(metric,g,reps,stat){
  measure<-vector()
  measure[1]<-stat(metric(g,directed=TRUE))
  for (i in 2:reps){
    net<-rewire(network)
    g<-graph.edgelist(net,directed=T)
    new<-stat(metric(g,directed=T))
    if(new<measure[i-1] && clusters(g)$no==1){ network<-net;measure[i]<-new} else {measure[i]<-measure[i-1]
    }}
  return(network)
}

# Rewire function
rewire<-function(network){
  node2.2<-integer(0)
  while(!length(node2.2)){
    node1.1<-sample(1:Nodes,1) #Sample a node randomly
    node1.2<-sample(network[network[,1]==node1.1,2],1) #Sample one of its contacts randomly
    node2.1<-sample(setdiff(1:Nodes,network[network[,2]==node1.1,1]),1) #Sample node that is not connected to original node
    candidates<-network[network[,1]==node2.1,2] #Get candidates among contacts of unconnected node
    diff<-c(network[network[,1]==node1.1,2],network[network[,1]==node1.2,2]) #Get vector of all connected to original node or neighbor
    node2.2<-setdiff(candidates,diff) # Get difference of candidates and vector 
    if(length(node2.2)>1){node2.2<-sample(node2.2,1)} # Sample from the difference
  }
  # Reassign edges 
  pos1<-which(network[network[,1]==node1.1,2]==node1.2)
  network[network[,1]==node1.1,2][pos1]<-node2.1 # node2.1 becomes neighbor of node1.1, and so on
  pos2<-which(network[network[,1]==node1.2,2]==node1.1)
  network[network[,1]==node1.2,2][pos2]<-node2.2
  pos3<-which(network[network[,1]==node2.1,2]==node2.2)
  network[network[,1]==node2.1,2][pos3]<-node1.1
  pos4<-which(network[network[,1]==node2.2,2]==node2.1)
  network[network[,1]==node2.2,2][pos4]<-node1.2
  return(network)
}


## Make networks

##MAX-AVG-CLUSTERING
networks <- list()
# Repeat for sims to maximize along metric
for(i in 1:sims){
  # Generate network
  network<-generate_network(Nodes,degree)
  original<-network
  # Make edgelist
  g<-graph.edgelist(network,directed=T)
  # Rewire clustering  
  networks[[i]]<-RW.degreeFIXmax.clustering(igraph::transitivity,g,its,mean)
}
e<-vector()
for(i in 1:sims){
  e[i] <- mean(igraph::transitivity(graph.edgelist(networks[[i]])))
}
# Find best of simulations outcomes
network <- networks[[which.max(e)]]
# Take best
maxc <- network

# As above
##MAX-MEAN-BETWEENNESS
networks <- list()
for(i in 1:sims){
  network<-generate_network(Nodes,degree)
  original<-network
  g<-graph.edgelist(network,directed=T)
  networks[[i]]<-RW.degreeFIXmax.betwenness(igraph::betweenness,g,its,mean) #MAX AVG BETWEENNESS
}
e<-vector()
for(i in 1:sims){
  e[i] <- max(igraph::betweenness(graph.edgelist(networks[[i]])))
}
network <- networks[[which.max(e)]]
maxmbet <- network

# As above
##MAX-MAX-BETWEENNESS
networks <- list()
for(i in 1:sims){
  network<-generate_network(Nodes,degree)
  original<-network
  g<-graph.edgelist(network,directed=T)
  networks[[i]]<-RW.degreeFIXmax.betwenness(igraph::betweenness,g,its,max)
}
e<-vector()
for(i in 1:sims){
  e[i] <- max(igraph::betweenness(graph.edgelist(networks[[i]])))
}
network <- networks[[which.max(e)]]
maxmaxbet <- network
##plot(graph.edgelist(network,directed=T), vertex.size=5,vertex.label=NA,edge.arrow.size=0,vertex.color="black",main="Max max B")
##save(network, file="max-max-betweenness.Rdata")

# As above
##MIN AVG CLUSTERING
networks <- list()
for(i in 1:sims){
  network<-generate_network(Nodes,degree)
  original<-network
  g<-graph.edgelist(network,directed=T)
  networks[[i]]<-RW.degreeFIXmin.clustering(igraph::transitivity,g,its,mean)
}
e<-vector()
for(i in 1:sims){
  e[i] <- mean(igraph::transitivity(graph.edgelist(networks[[i]])))
}
network <- networks[[which.min(e)]]
minc <- network

# As above
##MIN-MEAN-BETWEENNESS
networks <- list()
for(i in 1:sims){
  network<-generate_network(Nodes,degree)
  original<-network
  g<-graph.edgelist(network,directed=T)
  networks[[i]]<-RW.degreeFIXmin.betweenness(igraph::betweenness,g,its,mean) #MIN AVG BETWEENNESS
}
e<-vector()
for(i in 1:sims){
  e[i] <- min(igraph::betweenness(graph.edgelist(networks[[i]])))
}
network <- networks[[which.min(e)]]
minmbet <- network

# As above
 #MAX-MAX-CLOSENESS
networks <- list()
for(i in 1:sims){
  network<-generate_network(Nodes,degree)
  original<-network
  g<-graph.edgelist(network,directed=T)
  networks[[i]]<-RW.degreeFIXmax.closeness(igraph::centralization.closeness,g,its,max)
}
e<-vector()
for(i in 1:sims){
  e[i] <- max(igraph::centralization.closeness(graph.edgelist(networks[[i]]))$res)
}
network <- networks[[which.max(e)]]
maxclo <- network

# As above
##MIN-MAX-CLOSENESS
networks <- list()
for(i in 1:sims){
  network<-generate_network(Nodes,degree)
  original<-network
  g<-graph.edgelist(network,directed=T)
  networks[[i]]<-RW.degreeFIXmin.closeness(igraph::centralization.closeness,g,its,max)
}
e<-vector()
for(i in 1:sims){
  e[i] <- max(igraph::centralization.closeness(graph.edgelist(networks[[i]]))$res)
}
network <- networks[[which.min(e)]]
minclo <- network



#### Figure 18-4 ####

# Plot above networks

# Plot parameters
par(mfrow=c(2, 2))
par(mar=c(1,1,1,1))
# Set network
network <- maxc
# Plot
plot(graph.edgelist(network,directed=T), vertex.size=5,vertex.label=NA,edge.arrow.size=0,vertex.color="black", main="Max C" )
# Set network
network <- maxmbet
# Plot
plot(graph.edgelist(network,directed=T), vertex.size=5,vertex.label=NA,edge.arrow.size=0,vertex.color="black", main="Max mean B")
# Set network
network <- maxmaxbet
# Plot
plot(graph.edgelist(network,directed=T), vertex.size=5,vertex.label=NA,edge.arrow.size=0,vertex.color="black",main="Max max B")
# Set network
network <- minc
# Plot
plot(graph.edgelist(network,directed=T), vertex.size=5,vertex.label=NA,edge.arrow.size=0,vertex.color="black", main="Min C" )
network <- minmbet


set.seed(1)

## List of networks
netlist <- list(maxc, minc, maxmaxbet,maxmbet,minmbet,maxclo,minclo)
# Simulations
sims = 1000
# Probabilities
probs = seq(.1,.9, .1)
# Data buffer 
propco_nets <- array(NA, dim =c(length(netlist), sims, length(probs)))

  # Payoffs
  Tr = 5
  Pr = 1
  Sr = 0
  Rr = 3
  # longkey to protect during matrix payoff assignment
  lkey <- .01 # random number

for(jm in 1:length(netlist)){ # Across all network types
  for(pr in 1:length(probs)){ # Across all probabilities
   probby = probs[pr]   
    for(ex in 1:sims){ # For number of simulations
      # Assign network
      gameNetwork <- graph.edgelist(netlist[[jm]],directed=F)
      # Assign strategies
      V(gameNetwork)$strategy <- rbinom(length(V(gameNetwork)), 1, probby)+1
      # Legacy renaming 
      gplayout <- igraph::simplify(gameNetwork)
      # Run game on network until it stabilizes -- no one changes strategy
      sts <- rep(0, length(V(gplayout)$strategy)) # List of past strategies -- these are mismatched on purpose to miss first attempt
      check = TRUE 
      roundsi <- 0
      while(check & roundsi < 30){# Stop if stable
       # Keep track of each players payoffs
       # Reset value to 0
       V(gplayout)$value <- 0
       # Get matrix
       mat1 <- as_adj(gplayout, sparse=FALSE)
       sts <- V(gplayout)$strategy # List of past strategies
       # Assign payoffs player 1
       p1plays_against <- mat1*sts
       # Assign payoffs player 2
       p2plays_against <- t(t(mat1)*sts)
       # Compute unique identifier for payoff vs payoff 
       dmat<-p1plays_against*3-p2plays_against+lkey # add key
       # Assign payoffs
       dmat[which(dmat==0+lkey)] <- NA
       dmat[which(dmat == 4+lkey)] <- Pr # defectors against defectors
       dmat[which(dmat == 5+lkey)] <- Tr # defectors against cooperator 
       dmat[which(dmat == 2+lkey)] <- Rr # cooperator against cooperator 
       dmat[which(dmat == 1+lkey)] <- Sr # cooperator against cooperator 
       payoffsi <- dmat # 
       V(gplayout)$value <- rowMeans(payoffsi, na.rm=T)
       # Payoff matrix for all nodes
       paymat <- t(t(mat1)* V(gplayout)$value)
       # add individual value to diagonal
       diag(paymat) <- V(gplayout)$value
       # find index of best strategy in each row [take first best]
       # first best may fail in case of ties, so check if you care
       indimax <- apply(paymat, 1, 
                        function(x) sts[which(x==max(x,na.rm=T))][1])
       V(gplayout)$strategybuf <- sts
    
       V(gplayout)$strategy <- indimax
      check <- sum(abs(sts-V(gplayout)$strategy)) > 0
      roundsi <- roundsi + 1
      } # End of while loop
      # Count cooperators
      propco_nets[jm, ex, pr] <- sum(V(gplayout)$strategy == 1) / length(V(gplayout))
    }
  }
}
  
propco_nets1 <- propco_nets


# Repeat as above, with shuffling
set.seed(1)
netlist <- list(maxc, minc, maxmaxbet,maxmbet,minmbet,maxclo,minclo)
sims = 1000
probs = seq(.1,.9, .1)
propco_nets <- array(NA, dim =c(length(netlist), sims, length(probs)))
  # Payoffs
  Tr = 5
  Pr = 1
  Sr = 0
  Rr = 3
  # Longkey to protect during matrix payoff assignment
  lkey <- .01 # Random number

for(jm in 1:length(netlist)){ # Across all network types
  for(pr in 1:length(probs)){ # Across all probabilities
   probby = probs[pr]   
    for(ex in 1:sims){ # For number of simulations
      # Assign network
      gameNetwork <- graph.edgelist(netlist[[jm]],directed=F)
      # Assign strategies
      V(gameNetwork)$strategy <- rbinom(length(V(gameNetwork)), 1, probby)+1
      # Legacy renaming 
      gplayout <- igraph::simplify(gameNetwork)
      # Run game on network until it stabilizes -- no one changes strategy
      sts <- rep(0, length(V(gplayout)$strategy)) # List of past strategies -- these are mismatched on purpose to miss first attempt
      check = TRUE 
      roundsi <- 0
      while(check & roundsi < 30){# Stop if stable
       # Keep track of each players payoffs
       # Reset value to 0
       V(gplayout)$value <- 0
       mat1 <- as_adj(gplayout, sparse=FALSE)
       sts <- V(gplayout)$strategy # List of past strategies
       p1plays_against <- mat1*sts
       p2plays_against <- t(t(mat1)*sts)
       dmat<-p1plays_against*3-p2plays_against+lkey # add key
       dmat[which(dmat==0+lkey)] <- NA
       dmat[which(dmat == 4+lkey)] <- Pr # Defectors against defectors
       dmat[which(dmat == 5+lkey)] <- Tr # Defectors against cooperator 
       dmat[which(dmat == 2+lkey)] <- Rr # Cooperator against cooperator 
       dmat[which(dmat == 1+lkey)] <- Sr # Cooperator against cooperator 
       payoffsi <- dmat # 
       V(gplayout)$value <- rowMeans(payoffsi, na.rm=T)
       # Matrix with payoffs for all neighbors
       paymat <- t(t(mat1)* V(gplayout)$value)
       # Add individual value to diagonal
       diag(paymat) <- V(gplayout)$value
       # Find index of best strategy in each row [take first best]
       # First best may fail in case of ties, so check if you care
       indimax <- apply(paymat, 1, 
                        function(x) sts[which(x==max(x,na.rm=T))][1])
       V(gplayout)$strategybuf <- sts
       # Randomize strategies -- dispersal (shuffle all the strategies)
       V(gplayout)$strategy <- sample(indimax)
      check <- sum(abs(sts-V(gplayout)$strategy)) > 0
      roundsi <- roundsi + 1
      } 
      # Count cooperators
      propco_nets[jm, ex, pr] <- sum(V(gplayout)$strategy == 1) / length(V(gplayout))
    }
  }
}
propco_nets2 <- propco_nets


#### Figure 18-5 ####

# Plot proportion of cooperators sustained in each network above

# Parameters
par(mfrow=c(1,1))
plot(probs, colMeans(propco_nets1[1,,]), type="b", lty=1, col = 1, ylim = c(0, .5), xlab="Starting probability of defectors", ylab="Ending proportion of cooperators")
points(probs,colMeans(propco_nets1[4,,]), type="b", lty=2, col = 2)
points(probs,colMeans(propco_nets1[3,,]), type="b", lty =3, col = 3)
points(probs, colMeans(propco_nets1[2,,]), type="b", lty =4, col =4)
# Plot the shuffle---these are all the same, so I only show results of max C
points(probs, colMeans(propco_nets2[1,,]), type="l", lty =1, lwd=10, col =alpha("gray80", alpha=.4))
# Add legend
legend(.25,.5, legend=c("Max C", "Max Mean B","Max Max B",  "Min C", "Shuffled Max C"), lty=c(1:4, 1), col = c(1:4,alpha("gray80", alpha=.8)), lwd = c(rep(1,4), 5) , cex = .8)



#### Table 18.2 ####

nm <-c("Max CC", "Min CC", "Max Max Bet","Max Mean Bet",  "Min Bet", "Max Clo", "Min Clo")
tt <- rep(NA, length(nm))
cl <- rep(NA, length(nm))
bt <- rep(NA, length(nm))
for(jm in 1:length(netlist)){
  # Compute stats for each network 
  gameNetwork <- graph.edgelist(netlist[[jm]],directed=F)
  gameNetwork <- igraph::simplify(gameNetwork)
  tt[jm] <- mean(igraph::transitivity(gameNetwork, type="local"))
  cl[jm] <- mean(igraph::closeness(gameNetwork, mode="all"))
  bt[jm] <- mean(igraph::betweenness(gameNetwork, directed=FALSE))
}
# Make data frame
df <- data.frame(nm, tt, bt)
# Name columns
names(df) <- c("Network type", "C",  "B")
# Order
df<- df[c(1,4,3,2),]
# Make table
kable(df, row.names=FALSE, escape = FALSE, caption = "Network metrics resulting from adaptive network search.", digits=2 , booktabs=T) %>% kable_classic(full_width = F, html_font = "Cambria") 


#### Figure 18-7 ####

# The computer simulations of the Game of Life and Nowak and May's chaos can be computationally expensive.  The usual first approach is to treat each game separately, marching through the interactions one-by-one and computing the payoffs accordingly. This usually takes more time than you have. In the code, this problem is handled by treating all games simultaneously as a matrix algebra problem, by subtracting matrices and assigning payoffs after appropriate transformations to entire matrices. Thousands of agent interactions can be computed simultaneously with a few simple subtractions and additions.

# Random seed 
set.seed(2)

figuresi = 9 
rounds = 10 
# Size of lattice N x N
N =59 
examples =1 
# Plot parameters 9 framges
par(mfrow=c(3,3))
par(mar=c(0,0,0,0))


  # Make network
  gameNetwork <- make_lattice(dimvector=c(N,N), nei=2)
  # Assign strategies -- everyone cooperates 
  V(gameNetwork)$strategy <- 1
  # Except the center node
  V(gameNetwork)$strategy[(N^2)/2+1.1] <- 2
  # Label them all
  V(gameNetwork)$label <- 1:length(V(gameNetwork))
  # Layout
  l = layout_on_grid(gameNetwork) 
  # Remove labels
  V(gameNetwork)$label = NA
 
# For each frame in figure 
for (jk in 1:figuresi){
  # Iterate for rounds between figures
  for(i in 1:rounds){
   # Keep track of each players payoffs
   # Reset value to 0
   V(gameNetwork)$value <- 0
   # Matrix based interactions resolves all interactions simultaneously
   mat1 <- as_adj(gameNetwork, sparse=FALSE)
   sts <- V(gameNetwork)$strategy 
   p1plays_against <- mat1*sts
   p2plays_against <- t(t(mat1)*sts)
   # Combine strategies into identifier
   dmat<-p1plays_against*3-p2plays_against
   # 3-1 = C v C = 2
   # 6-2 = D v D = 4 => 1 (below)
   # 3-2 = C v D = 1
   # 6-1 = D v C = 5
   # Assign payoffs
   dmat[which(dmat == 4)] <- 1 # D v D
   dmat[which(dmat==0)] <- NA
   # Transform
   payoffsi <- (dmat-1)^.3
   # Take means to get payoffs
   V(gameNetwork)$value <- rowMeans(payoffsi, na.rm=T)
   # Update strategies
   # Matrix with payoffs for all neighbors
   paymat <- t(t(mat1)* V(gameNetwork)$value)
   # Add individual value to diagonal
   diag(paymat) <- V(gameNetwork)$value
   # find index of best strategy in each row
   indimax <- apply(paymat, 1, function(x) sts[which(x==max(x,na.rm=T))][1])
   # Past strategy
   V(gameNetwork)$strategybuf <- sts
   # New strategy
   V(gameNetwork)$strategy <- indimax
   # Set vertex size
   V(gameNetwork)$size <- 3 
  }
  # Set vertex color based on past and present strategy
  V(gameNetwork)$col = ifelse(V(gameNetwork)$strategybuf == V(gameNetwork)$strategy & V(gameNetwork)$strategy == 2, "red", ifelse( V(gameNetwork)$strategybuf == V(gameNetwork)$strategy & V(gameNetwork)$strategy == 1,"green", ifelse(V(gameNetwork)$strategybuf != V(gameNetwork)$strategy & V(gameNetwork)$strategy == 1, "yellow", "orange")))
  # Plot lattice
  plot(gameNetwork, layout=l, vertex.frame.width=.5, vertex.color=V(gameNetwork)$col)
} 



#### Table 18.3 ####

# Hawk dove game

 all_pairs <- c("$(V-C)/2$", "$V$", "$0$", "$V/2$" )
 payoff.mat <- matrix(all_pairs, nrow=2, byrow=TRUE)
 dimnames(payoff.mat) <- c(rep(list(c("Hawk","Dove")), 2))
 kable(payoff.mat, row.names=TRUE, escape = FALSE, caption = "The Hawk-Dove Game. Payoffs are to the strategy on the left played against the strategy above.", digits=2 ) %>% kable_classic(full_width = F, html_font = "Cambria")





# Preparation for Figure 18.8
# Set random seed
set.seed(1)
# 100 simulations per network
sims = 100 
# Proportion of individuals able to switch strategies (this prevents winner take all)
probp = .1 

startingArrangements = 10 
# Make network
fulNet <- make_full_graph(30)
# Make edgelist
fulnel <- as_edgelist(fulNet)
# Add below to get Mason Watts networks above
netlist <-  list(fulnel, maxc, minc, maxmbet,minmbet)
nlabels <- c("Full network", "Max CC", "Min CC", "Max Mean Bet", "Min Mean Bet")

# Costs of hawk-hawk interactions
Clist <- c(2,3,4,5)
# Set up empty data 
propHawks <- array(NA, dim =c(length(netlist), length(Clist), startingArrangements))
# For each cost in Clist
for(cc in 1:length(Clist)){ 
  # Set cost
  C <- Clist[cc]
  propHawksSims <- matrix(NA, ncol=sims)
  # Payoffs
  V = 2 # Victory
  #C = 3 # Cost
  HD = V # Hawk against dove
  HH = (V-C)/2 # Hawk against hawk 
  DH = 0 # Dove against hawk
  DD = V/2 #Tr / 2 # Dove against dove 
  # For each network type
  for(neti in 1:length(netlist)){ 
    # For number of starting arrangments with random strategy allocations
    for(jm in 1:startingArrangements){
          # Create network
          # For testing with random networks
          # For simulations with Mason Watts networks
          gameNetwork <- graph.edgelist(netlist[[neti]],directed=F) 
          # Assign strategies
          V(gameNetwork)$strategy <- rbinom(length(V(gameNetwork)), 1, .5) + 1
          # Simplify
          gplayout <- igraph::simplify(gameNetwork)
        # For number of simulations per starting arrangments  
        for(ex in 1:sims){ 
           # Start competition
           # Keep track of each players payoffs
           # Reset value to 0
           V(gplayout)$value <- 0
           # Make adjacency matrix from network above
           mat1 <- as_adj(gplayout, sparse=FALSE)
           # Vector of current strategies
           sts <- V(gplayout)$strategy 
           # Combine with matrix
           p1plays_against <- mat1*sts
           p2plays_against <- t(t(mat1)*sts)
           # Multipliers below give unique values for each type of encounter
           dmat<-p1plays_against*3-p2plays_against # difference in arms buildup
           dmat[which(dmat==4)] <- HH # hawk against hawk
           dmat[which(dmat==1)] <- DH # dove against hawk 1*3-2
           dmat[which(dmat==2)] <- DD # dove against dove 1*3-1
           dmat[which(dmat == 5)] <- HD # hawk against dove 2*3-1 == 5
           # Gather payoffs 
           payoffsi <- dmat 
           # Take sums for each player
           V(gplayout)$value <- rowSums(payoffsi, na.rm=T)
           # Apply to matrix
           paymat <- t(t(mat1)* V(gplayout)$value)
           # Add individual value to diagonal
           diag(paymat) <- V(gplayout)$value
           # Find index of best strategy in each row [take first best]
           # First best may fail in case of ties, so check if you care (better would be to sample best)
           indimax <- apply(paymat, 1, 
                            function(x) sts[which(x==max(x,na.rm=T))][1])
           # Assign current strategies to buffer
           V(gplayout)$strategybuf <- sts
           # Hawk dove replicator -- choose best with probability probp
           listOfChangers <- which(abs(indimax-sts)==1) # 1s == changers
           # Roll dice for all who would change and change those who get a hit
           onesToChange <- listOfChangers[which(rbinom(length(listOfChangers), 1, .1)==1)]
           # Strategies
           substs <- sts
           # Substitute them in for onesToChange
           substs[onesToChange] <- indimax[onesToChange] 
           # Reassign strategies
           V(gplayout)$strategy <- substs
           # Compute proportion of hawks
           propHawksSims[1, ex] <- sum(V(gplayout)$strategy == 2)/length(V(gplayout))
        }   # End of sims
        propHawks[neti, cc,jm] <- sum(V(gplayout)$strategy == 2)/length(V(gplayout))
    } # end of jm
    
  } # end of nets

} # end of Clist



#### Figure 18-8 ####

# Plot parameters
par(mfrow=c(1,1))
par(mar=c(5,5,2,2))
# Plot proportion of hawks against costs (C) for each network type
plot(Clist, rowMeans(propHawks[1,,]), type="b", lty=1, col = 1, ylim = c(0, max(propHawks)+.1), xlab="Cost of Hawk-Hawk (C)", ylab="P(Hawks)", lwd=2)
points(Clist,rowMeans(propHawks[4,,]), type="b", lty=2, col = 2, lwd=2)
points(Clist,rowMeans(propHawks[3,,]), type="b", lty =3, col = 3, lwd=2)
points(Clist, rowMeans(propHawks[2,,]), type="b", lty =4, col =4, lwd=2)
# points(Clist, rowMeans(propHawks[5,,]), type="b", lty =5, col =5, lwd=2)
legend(2,.45, legend=c("Full Network", "Max CC",  "Max Mean Bet", "Min CC", "V/C"), lty=c(1,2,4,3, 1), col = c(1,2,4,3, "gray80"), lwd = c(rep(1, 4), 10))
predicted = 2/Clist
lines(Clist, predicted, lwd=10, col = alpha("gray80", alpha= .5) )



 comparison <- c("$m_A > m_B$", "$m_A = m_B$", "$m_A < m_B$" ) 
pA <- c("$V-m_B$", "$V/2-m_B$", "$-m_A$")
pB <- c("$-m_B$", "$V/2-m_B$", "$V-m_A$")
dfal <- data.frame(comparison, pA, pB)
names(dfal) <- c("Comparison", "Player A", "Player B")
 kable(dfal, row.names=FALSE, escape = FALSE, caption = "The war of attrition", digits=2 ) %>% kable_classic(full_width = F, html_font = "Cambria") 





#### Table 18-4 ####

# Set columns
comparison <- c("$s_A > s_B$", "$s_A = s_B$", "$s_A < s_B$", "$P(s_{minimum})$" ) 
pA <- c("$V$", "$V/2$", "$0$","$-s_{minimum} \\times C$" )
pB <- c("$0$", "$V/2$", "$V$", "$-s_{minimum} \\times C$")
# Combine into dataframe
dfal <- data.frame(comparison, pA, pB)
# Label columns
names(dfal) <- c("Comparison", "Player A", "Player B")
# Make table
kable(dfal, row.names=FALSE, escape = FALSE, caption = "The brinkmanship game.", digits=2 ) %>% kable_classic(full_width = F, html_font = "Cambria") 


#### Figure 18-9 ####

# Random seed
set.seed(3)
# Number of simulations
sims = 2000 
# Different random starting positions
startingArrangements =2
# Approach increment to best neighbor
approachFactor = .01 
# Proportion by which to exceed best neighbor
playToWin = 0.000 # Set this to zero if nations play not to lose (as opposed to playing to win)
## with positive playToWin == .005 (e.g.) the less clustered networks (min cc and min mean bet) have the highest strategies and least conflict costs and frequency of conflict--this is the same pattern when playToWin is 0. 
# Does everyone pay prep costs 
payPrepCosts = 0 # should everyone pay prep costs (alternative is to only pay when wars occur) -- this is 0 or 1 
## For visualization
vertexSize = 20

#Number of frames in figure
par(mfrow=c(4,3))
# List Mason Watts networks to use
netlist <-  list(maxc, minc, maxmaxbet,maxmbet)
# Label them
netnamelist <- c("Max C", "Min C", "Max Max B", "Max Mean B")
# Payoffs
Tr = 2 
# List of catastrophic loss amounts
Loss = c(2,4,6,8) # This is the multiplier of going to war
levelToPrint = 8 # in the code below this is Loss[lossi] == levelToPrint; it is the cost simulation to show in the figure.
  
## Capture outcomes -- list dummy outcomes below 
allNets <- list(NULL)
netname <- c()
loslevel <- c()
alln = 0
avStrat <- matrix(NA, nrow=sims+1)
maxStrat <- matrix(NA, nrow=sims+1)
warCosts <- matrix(NA, nrow=sims+1)
catevents <- matrix(NA, nrow=sims+1)
avStratEnd <- array(NA, dim=c(length(Loss), length(netlist), startingArrangements))
maxStratEnd <- array(NA, dim=c(length(Loss), length(netlist), startingArrangements))
warCostsEnd <- array(NA, dim=c(length(Loss), length(netlist), startingArrangements))
freqCatastrophe <- array(NA, dim=c(length(Loss), length(netlist), startingArrangements))

## BEGIN SIMULATION
for(lossi in 1:length(Loss)){ # For all loss values
  for(neti in 1:length(netlist)){ # Across all network types
    for(jm in 1:startingArrangements){# For each starting arrangment
          # Create network
          # Choose neti from netlist
          gameNetwork <- graph.edgelist(netlist[[neti]],directed=F) # to use mason watts networks
          # Assign random strategies to all players
          V(gameNetwork)$strategy <- runif(length(V(gameNetwork)), min = 0, max = .1)
          # Clean and rename for simulation
          gplayout <- igraph::simplify(gameNetwork)
          # Set rounds counter
          roundsi <- 0
          # Plot
          if(jm == 1 & Loss[lossi] == levelToPrint){ # uncomment plot below if you want to plot random startup network---need to set figures frames accordingly to par(mfrow=c(4,4))
            l = layout_with_fr(gplayout)
            pal <- colorRampPalette(c("seagreen1", "firebrick3"))
            cole <- pal(20)
            par(mar=c(0,0,0,0))
            V(gplayout)$label <- NA
            V(gplayout)$color <- cole[round(V(gplayout)$strategy*vertexSize)+1]
            # plot(gplayout, vertex.size = 2*V(gplayout)$strategy*vertexSize, vertex.label.cex = 1, vertex.label.color = "black", layout = l)
          } 
        # For number of simulations  
        for(ex in 1:sims){ 
           # Start competition
           # Keep track of each players payoffs
           # Reset sum of payoff values to 0
           V(gplayout)$value <- 0
           # Get adjacency matrix 
           mat1 <- as_adj(gplayout, sparse=FALSE)
           # Assign strategies
           sts <- V(gplayout)$strategy # list of past strategies
           # What player 1 plays against -- assigns strategies where neighbors are
           p1plays_against <- mat1*sts
           # What player 2 plays against
           p2plays_against <- t(t(mat1)*sts)
           # Take minimum for each cell to compute costs (minimum of arms buildup for interaction) -- not needed in fixed cost scenario
           pcosts <- pmin(p1plays_against, p2plays_against)
           # Get difference in arms build up to identify winners  
           dmat<-p1plays_against-p2plays_against # difference in arms buildup
           # Assign positive differences the reward
           dmat[which(dmat > 0)] <- Tr  # winner of escalation
           # To handle ties make tie values for all edges
           tieValuesForSplit <- mat1 * (Tr / 2) # mult by mat1 so only for neighbors
           # Assign to edges with value zero
           dmat[which(dmat==0)] <- tieValuesForSplit[which(dmat==0)] # what happens if they tie? 
           # Set diagonal to zero after above
           diag(dmat) <- 0 
           # Loser gets nothing
           dmat[which(dmat < 0)] <- 0 
           # Which victims experience catastrophe 
           # Reduce to lower triangle -- so we only compute once for each pair
           riskfactor <- pcosts*lower.tri(pcosts)
           # Roll the dice
           catastrophe <- runif(length(pcosts)) < riskfactor
           # Make symmetric as both sides suffer
           catastrophe <- catastrophe + t(catastrophe)
           # War costs are scaled by arms buildup 
            costmat <- catastrophe*pcosts*Loss[lossi]
           # fixed costs, like russian roulette !!!! (another possibility you can try: uncomment below)
           # costmat <- catastrophe*Loss[lossi] # assign catastrophe to victims 
            # Divide by 2 as nodes share catastrophies in a pair 
            catevents[ex,1] <- sum(costmat > 0)/2 
            # Compute total costs
            totalcosts <- costmat + pcosts*payPrepCosts
            # Wins - losses
            dmat <-dmat - totalcosts
            # Payoffs 
            payoffsi <- dmat 
            # Assign sum of payoffs
            V(gplayout)$value <- rowSums(payoffsi, na.rm=T)
            # Wash hands
            diag(totalcosts) <- NA
            # Get mean of costs for each node   
            V(gplayout)$costs <- rowMeans(totalcosts, na.rm=T)
            # Imitate best neighbors
            # Matrix with payoffs for all neighbors
            paymat <- t(t(mat1)* V(gplayout)$value)
            # Add individual value to diagonal
            diag(paymat) <- V(gplayout)$value
           
           # Find index of best strategy in each row [take first best]
           # First best may fail in case of ties, so check if you care
           indimax <- apply(paymat, 1, 
                            function(x) sts[which(x==max(x,na.rm=T))][1])
           # Store past strategies
           V(gplayout)$strategybuf <- sts
           # Who will be reset from total loss
           zeroers <- V(gplayout)$value < 0
           # Make reverse list
           nonzeroers <- ifelse(zeroers, FALSE, TRUE)
           # Replicator Dynamics
           # First compute approach for everyone to best neighbors strategy
           V(gplayout)$strategy <- sts + as.numeric(indimax>sts)*approachFactor -
              as.numeric(indimax<sts)*approachFactor + playToWin # playToWin adds a bump, but its set to zero for text -- in practice this doesn't matter so much, as leapfrog still occurs due to sig digits of strat 
            # Nonzeroers get above strategy, random reassignment for zeroers
             V(gplayout)$strategy <- nonzeroers * V(gplayout)$strategy + zeroers*runif(length(V(gplayout)), min = 0, max = V(gplayout)$strategy)
          # Iterate round  
          roundsi <- roundsi + 1
          # Compute average escalation
          avStrat[ex,1] <- mean(V(gplayout)$strategy)
          # Compute max escalation
          maxStrat[ex,1] <- max(V(gplayout)$strategy)
          # Compute catastrophe costs 
          warCosts[ex,1] <- sum(V(gplayout)$costs)
          # Plot networks at midpoints of simulations
          if(jm == 1 & Loss[lossi] == levelToPrint & ex == (sims/2)){
             V(gplayout)$color <- cole[round(V(gplayout)$strategy*vertexSize)+1]
             plot(gplayout, vertex.size = V(gplayout)$strategy*vertexSize, vertex.label.cex = 1, vertex.label.color = "black", layout = l)
          } 
        }  
          # plot final network
          if(jm == 1 & Loss[lossi] == levelToPrint ){
            # Plot scale color to vertex size
            V(gplayout)$color <- cole[round(V(gplayout)$strategy*vertexSize)+1]
            plot(gplayout, vertex.size = V(gplayout)$strategy*vertexSize, vertex.label.cex = 1, vertex.label.color = "black", layout = l)
            par(mar=c(3,3,2,2))
            par(mgp=c(2,1,0))
            xn <-1:length(avStrat) 
            plot(xn, avStrat, ylim=c(0, .5), xlab = "Time steps", ylab = "Mean strategy", cex=.2, main = "")
            text(500, .4, netnamelist[neti])
          } 
          # Save data 
          alln = alln + 1
          allNets[[alln]] <- gplayout
          netname[alln] <- neti
          loslevel[alln] <- lossi
          avStratEnd[lossi,neti,jm ] <- mean(avStrat[(length(avStrat)-500):length(avStrat)],na.rm=TRUE)
          maxStratEnd[lossi,neti,jm ] <- max(avStrat[(length(avStrat)-500):length(avStrat)],na.rm=TRUE)
          warCostsEnd[lossi,neti,jm ] <- mean(warCosts[(length(warCosts)-500):length(warCosts)],na.rm=TRUE)
          freqCatastrophe[lossi, neti,jm] <- mean(catevents[(length(catevents)-500):length(catevents)],na.rm=TRUE) 
    } # end of jm
  } # end of nets
} # end of lossi



## Code to verify the leapfrog claim: It identifies the max strategy, and the neighbors of that strategy.
## Then these are followed one 'sim' (or 'ex') at a time in the above code.
## It easy to see that neighbors jump towards the highest strategy when that strategy
## has the highest value.  Doing this repeatedly eventually arrives at a point
## where a neighbor node jumps past the highest value.  It may take some searching to find
## an instance of this, but it's useful to see the code in action at this level of detail.
max(V(gplayout)$strategy)
which(V(gplayout)$strategy == max(V(gplayout)$strategy))
V(gplayout)$value[c(27,neighbors(gplayout,27 ) )]
## Catastrophes
rowMeans(catastrophe)[c(27, neighbors(gplayout,27 ))]
## Past strategy
sts[c(27, neighbors(gplayout,27 ))]
## Present strategy
V(gplayout)$strategy[c(27, neighbors(gplayout,27 ))]
## Value of last interactions
V(gplayout)$value[c(27,neighbors(gplayout,27 ) )]




#### Figure 18-10 ####

# Plot summary stats for various network outcomes across costs
par(mfrow=c(2,2))
par(mar=c(5,5,2,2))
plot(Loss, rowMeans(avStratEnd[,1,]), type="b", lty=1, col = 1, ylim = c(0, max(avStratEnd)), xlab="Cost of catastrophe", ylab="Average escalation values", lwd=2)
points(Loss,rowMeans(avStratEnd[,2,]), type="b", lty=2, col = 2, lwd=2)
points(Loss,rowMeans(avStratEnd[,3,]), type="b", lty =3, col = 3, lwd=2)
points(Loss,rowMeans(avStratEnd[,4,]), type="b", lty =4, col = 4, lwd=2)
legend(4,1, legend=c("Max C","Min C",  "Max Max B", "Max Mean B"), lty=1:4, col = 1:4, cex = .8)

 plot(Loss, 40*rowMeans(warCostsEnd[,1,]), type="b", lty=1, col = 1, xlab="Cost of catastrophe", ylab="Total conflict costs", lwd=2, ylim = c(0,40*max(warCostsEnd)))
 points(Loss,40*rowMeans(warCostsEnd[,2,]), type="b", lty=2, col = 2, lwd=2)
 points(Loss,40*rowMeans(warCostsEnd[,3,]), type="b", lty =3, col = 3, lwd=2)
 points(Loss,40*rowMeans(warCostsEnd[,4,]), type="b", lty =4, col = 4, lwd=2)

plot(Loss, rowMeans(freqCatastrophe[,1,]), type="b", lty=1, col = 1, xlab="Cost of catastrophe", ylab="Frequency of catastrophe", lwd=2 , ylim=c(0, max(freqCatastrophe)))
points(Loss,rowMeans(freqCatastrophe[,2,]), type="b", lty=2, col = 2, lwd=2)
points(Loss,rowMeans(freqCatastrophe[,3,]), type="b", lty =3, col = 3, lwd=2)
points(Loss,rowMeans(freqCatastrophe[,4,]), type="b", lty =4, col = 4, lwd=2)

# Compute assortativity
assamounts <- rep(NA, length(allNets))
for(i in 1:length(allNets)){
  assamounts[i] <- assortativity(allNets[[i]], types1 =V(allNets[[i]])$strategy , directed = FALSE)
}
assdat <- data.frame(netname, loslevel, assamounts)

# Find means
sse <- summarySE(assdat, measurevar="assamounts", groupvars=c("netname","loslevel"))
# Plot
plot(Loss, subset(sse, netname == 1)$assamounts, type="b", ylim = c(-.8, 0), col = 1, lty=1, lwd=2, xlab = "Cost of catastrophe", ylab="Assortativity")
lines(Loss, subset(sse, netname == 2)$assamounts, type="b", col = 2, lty=2, lwd=2)
lines(Loss, subset(sse, netname == 3)$assamounts, type="b", col = 3, lty=3, lwd=2)
lines(Loss, subset(sse, netname == 4)$assamounts, type="b", col = 4, lty=4, lwd=2)


# Mean of maximum strategies for each network == not plotted in book
plot(Loss, rowMeans(maxStratEnd[,1,]), type="b", lty=1, col = 1, ylim = c(0, max(avStratEnd)), xlab="Cost of catastrophe", ylab="Max strategy", lwd=2)
points(Loss,rowMeans(maxStratEnd[,2,]), type="b", lty=2, col = 2, lwd=2)
points(Loss,rowMeans(maxStratEnd[,3,]), type="b", lty =3, col = 3, lwd=2)
points(Loss,rowMeans(maxStratEnd[,4,]), type="b", lty =4, col = 4, lwd=2)
##points(Loss,rowMeans(avStratEnd[,5,]), type="b", lty =5, col = 5, lwd=2)
legend(4,.35, legend=c("Max C","Min C",  "Max Mean B", "Min Mean B"), lty=1:4, col = 1:4, cex = .8)


#### Figure 18-11 ####

# Plot parameters
par(mfrow=c(1,1))
par(mar=c(5,5,2,2))

## From above
assdat <- data.frame(netname, loslevel, assamounts, netid = 1:length(allNets))

i = 1 # just plotting Max C
# If needed to plot for each network type
#for(i in 1:length(unique(assdat$netname))){
  for(ll in 1:length(Loss)){
    # Get network IDs
    netis <- subset(assdat, loslevel == ll & netname == i)$netid
    stratVector <- c()
    # Get strategies
    for(nl in 1:length(netis)){
      stratVector <- c(stratVector, V(allNets[[netis[nl]]])$strategy)   
    }
    # Order strategies
    orderStrat <- stratVector[order(stratVector, decreasing=TRUE)]  
    # Plot each output, use first to set up plot
    if(ll == 1){
      plot(1:length(orderStrat), orderStrat, log="y", col = ll, type="b", ylab = "Escalation value", xlab = "Rank", cex = .4, ylim = c(0.01,3.5))
    } else {
      lines(1:length(orderStrat), orderStrat,col = ll, type="b", cex = .4)
    }
  }
# }
# Make legend
legend(65, 2, title="C", legend=c("2", "4", "6", "8"), col = 1:4, fill = 1:4)

