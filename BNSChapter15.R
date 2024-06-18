
### Behavioral Network Science
### Language, Mind, and Society
# By Thomas T. Hills

# Code to reproduce figures and analyses for

#### Chapter 15 -- Group Problem Solving: Harnessing the Wisdom of the Crowds ####


knitr::opts_chunk$set(echo = FALSE)
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(comment = NA)
knitr::opts_chunk$set(warning = FALSE)
options(knitr.table.format = "latex")

rm(list=ls())
library(igraph)
library(tidyverse)
library("gridExtra")                        # Load gridExtra package
library(gtools)
library(latex2exp)



#### Figure 15-1 ####

# Plot parameters 
par(mar=c(2,2,2,2))
par(mfrow=c(1,2))
# Make a sequence for x and y coordinates
x <- y <- seq(0, 5, len = 200)
# Make all pairs of x and y values
X <- expand.grid(x = x, y = y)
# Create a z value normally distributed over the x and y range (centered at 2.5 and 2)
X <- transform(X, z = dnorm(x, 2.5)*dnorm(y, 2))
# make a matrix from the z values in X
z <- matrix(X$z, nrow = 200)

# Plot 3D graph
persp(x, y, z, col = "pink", border = NA,
      theta = 45, phi = 40, ticktype = "simple", 
      ltheta = -120, shade = 0.25, box = TRUE, expand = 2)

# Make rugged landscape
# Set random seed
set.seed(4)
# Make x and y range
x <- y <- seq(0, 1, len = 200)
# Make pairs of x and y
X <- expand.grid(x = x, y = y)
# Add together numerous random z-value matrices with different uniformly chosen peaks 
for(i in 1: 1000){
  X <- transform(X, z = dnorm(x, runif(1), runif(1, .02, .1))*dnorm(y, runif(1), 
                              runif(1, .02, .1)))
  zadd <- matrix(X$z, nrow = 200)
  z <- z + zadd
}

# Plot 3D 
persp(x, y, z, col = "pink", border = NA,
      theta = 45, phi = 40, ticktype = "simple", 
      ltheta = -120, shade = 0.25, box = TRUE, expand = 2)



#### Figure 15-2 ####

# Make figures like those in Mason et al. 

# Make 3 plot frames
par(mfrow=c(1,3))
# Make sequence of X values
x <- seq(0,100, .01)
# Set parameters for y-function of x
a1 = 0
a2 = 100 
a3 = 0
b1 = 1
b2 = 1/300
b3 = 1/150
# Make y-values
y <- a1*exp(-(b1*(x-30)^2))+a2*exp(-(b2*(x-60)^2))+a3*exp(-(b3*(x-90)^2))
# Plot
plot(x,y,type="n", ylab = "Payoff", xlab = "Guess", ylim= c(0,100), main = "Easy")
lines(x,y)
# As above
a1 = 60 
a2 = 80 
a3 = 70 
b1 = 1/150
b2 = 1/150
b3 = 1/150
y <- a1*exp(-(b1*(x-30)^2))+a2*exp(-(b2*(x-60)^2))+a3*exp(-(b3*(x-90)^2))
plot(x,y,type="n", ylab = "Payoff", xlab = "Guess", ylim= c(0,100), main = "Difficult")
lines(x,y)
# As above
a1 = 100 
a2 = 50 
a3 = 0 
b1 = 1/5
b2 = 1/150
b3 = 1/150
y <- a1*exp(-(b1*(x-20)^2))+a2*exp(-(b2*(x-60)^2))+a3*exp(-(b3*(x-90)^2))
plot(x,y,type="n", ylab = "Payoff", xlab = "Guess", ylim= c(0,100), main = "The Needle")
lines(x,y)



#### Figure 15-3 ####

## PLOT MASON GOLDSTONE NETWORKS
##Lattice
g <- make_ring(10)
l <- layout_in_circle(g)
gring <- add_edges(g, c(1,3))
gring <- add_edges(gring, c(5,7))
## Full graph
gfull <- make_full_graph(10)
## ER random graph
ger <- erdos.renyi.game(10, 13, type="gnm")
## Small world
gsw <- add_edges(g, c(1,5))
gsw <- add_edges(gsw, c(7, 3))

# Plot
par(mfrow=c(2,2))
par(mar=c(2,2,2,2))
plot(gring, layout=l,
     vertex.color = 'white', 
     vertex.frame.color='black',
     vertex.frame.width=2,
     vertex.size = 15, 
     edge.width=2,
     vertex.label =NA, main="Regular Lattice" ) 
plot(ger, layout=l,
     vertex.color = 'white', 
     vertex.frame.color='black',
     vertex.frame.width=2,
     vertex.size = 15, 
     edge.width=2,
     vertex.label =NA, main = "Random") 
plot(gsw, layout=l,
     vertex.color = 'white', 
     vertex.frame.color='black',
     vertex.frame.width=2,
     vertex.size = 15, 
     edge.width=2,
     vertex.label =NA, main = "Small-world") 
plot(gfull, layout=l,
     vertex.color = 'white', 
     vertex.frame.color='black',
     vertex.frame.width=2,
     vertex.size = 15, 
     edge.width=2,
     vertex.label =NA, main = "Fully Connected") 



#### Figure 15-4 ####

## Generate NK Environment example
## Much of this is adapted from Barkoczi & Galesic (2016) who provide their code
rm(list=ls())
set.seed(2)
#NK PARAMETERS
N=3; K=1 
##GENERATE LANDSCAPE
## Generate fitness values for K=1 (meaning sequences of two)
# Make all possible genome arrangements for K+1 values
LS<-gtools::permutations(2,N,v=c(0,1),repeats.allowed=TRUE) 
LS<-as.data.frame(LS)
if (K==0){
  depends <- as.vector(1:N)
  # Combinations: possible arrangements of bits for each value
  combinations <- gtools::permutations(2,K+1,v=c(0,1),repeats.allowed=TRUE)
  # Values: fitness values for each arrangement in each position
  values <- replicate(N,round(runif(2,0,1),1))
  # Fitness assigns combinations (rows) to N genome locations 
  fitness <- values
} else {
  # This sets up dependencies-- I have set it up so it is ordered 
  replist <- rep(1:N,(K+1))
  depends <- t(sapply(1:(K+1), function(x)replist[x:(N+x-1)]))
  # Combinations: possible arrangements of bits for each value
  combinations <- gtools::permutations(2,K+1,v=c(0,1),repeats.allowed=TRUE)
  # Values: fitness values for each arrangment in each position
  values <- replicate(N,round(runif(nrow(combinations),0,1),1))
  # fitness assigns K+1 combinations (rows) to N genome locations (columns, starting after K+1)
}
  # Fitness table: first two columns represent possible genome values for two elements
  # Last three columsn indicate fitness values for the genome values starting from different positions in the genome
  fitness <- cbind(combinations,values)



## Function to generate the fitness score for all possible genotypes
generate_landscape<-function(N,K,LS,fitness,depends){
  if(K==0){
    fitness.score <- vector()
    for (ag in 1:nrow(LS)){ # LS is all possible permutations in genome
      rows<-as.numeric(LS[ag,])+1 # Assign correct rows for genome readoff
      values<-sapply(1:N, function(y) fitness[rows[y],y+(K+1)] ) # Read off fitness function for genome permutation, adjust columns by  K+1
      fitness.score[ag]<-mean(values) # Take the mean of the values for fitness score
    }
  } else {
    # If K > 0, these values must combine multiple genome values (reading from 'fitness'), using 'depends' to track genome elements for each fitness reading
    indx1<-do.call(`paste0`,as.data.frame(fitness[,c(1:(K+1))]))
    indx2<-sapply(1:N, function(y) do.call(`paste0`,as.data.frame(LS[,depends[,y]])))
    fitness.score <- sapply(1:nrow(LS), function(o) mean(diag(sapply(indx2[o,], function(x) fitness[which(indx1 %in% x),(K+2):ncol(fitness)]))))
  }
  # Combine them
  landscape<-cbind(LS[,1:N],fitness.score)
  return(landscape)
}

# Compute fitness for each genome
landscape <- generate_landscape(N,K,LS,fitness,depends)
# Round values for display
landscape$fitness.score <- round(landscape$fitness.score, 2)
# Make names
sequenceNames <- sapply(1:(K+1), function(x) paste('S',x, sep=""))
# Assign names (this would need adapting for different values of N and K)
colnames(fitness) <- c(sequenceNames, "G1", "G2", "G3")
colnames(landscape) <- c("P1","P2","P3", "Fitness Score")
# Make table
library(grid)
gridExtra::grid.arrange(
  tableGrob(fitness),
  tableGrob(as.matrix(landscape)), nrow = 1
)
###  


#### Figure 15-5 ####

# The output of this is saved in the book, but I replicate it here.
## Example smooth rugged landscape figure

# Function to make graphs with values of N and K
makeNKgraphs <- function(N,K){
  # Make fitness table (as above)
  LS<-gtools::permutations(2,N,v=c(0,1),repeats.allowed=TRUE)
  LS<-as.data.frame(LS)
  if (K==0){
    depends <- as.vector(1:N)
    values <- replicate(N,round(runif(2,0,1),1))
    fitness <- values
  } else {
    # Depends determines which other bits the kth bit depends on
    depends <- rbind(1:N,replicate(N,sample(c(1:N),K,replace=F)))
    depends2 <- rbind(1:N,replicate(N,sample(c(1:N),K,replace=F))) # 2nd Env
    # Combinations: possible arrangements of bits for each value
    combinations <- gtools::permutations(2,K+1,v=c(0,1),repeats.allowed=TRUE)
    # Values: fitness values for each arrangment in each position
    values <- replicate(N,round(runif(nrow(combinations),0,1),1))
    # Fitness assigns K+1 combinations (rows) to N genome locations (columns, starting after K+1)
    fitness <- cbind(combinations,values)
  }
  landscape <- generate_landscape(N,K,LS,fitness,depends)
  landscape[,N+1] <- (landscape[,N+1]/max(landscape[,N+1]))^8
  ## Compute network with 2^N nodes
  genomeGraph <- matrix(0,nrow=2^N,ncol=2^N)
  # Assign names
  gene.names <- sapply(1:nrow(landscape), function(x) paste(landscape[x,1:N], collapse=""))
  landscape <-cbind(landscape, gene.names)
  colnames(genomeGraph) <- c(gene.names)
  rownames(genomeGraph) <- c(gene.names)
  # For all genomes
  for(i in 1:nrow(genomeGraph)){
   for(j in 1:ncol(genomeGraph)) {
     landi <- landscape[landscape$gene.names==rownames(genomeGraph)[i],]
     landj <- landscape[landscape$gene.names==rownames(genomeGraph)[j],]
     # Check distance
     distance <- sum(abs(landj[1:N]-landi[1:N]))
     # If distance is one, point arrow from lowest to highest 
     # Since this compares all pairs in both directions, it gets it right
     if(distance == 1){
       if(landi$fitness.score < landj$fitness.score){
        genomeGraph[i,j] <- 1
       }
     }
   }
  } 
  
  # Make graph from edges above 
  gG <- graph_from_adjacency_matrix(genomeGraph)
  peak<-ifelse(igraph::degree(gG, mode="out")==0, 1,0)
  V(gG)$type = peak
  V(gG)$fitness = landscape$fitness.score
  V(gG)$color = ifelse(peak==0, "white", "black")
  return(gG)
}

## Call function above to make graphs
set.seed(12) 
g31 <- makeNKgraphs(3,1)
g32 <- makeNKgraphs(3,2)
g51 <- makeNKgraphs(5,1)
g54 <- makeNKgraphs(5,4)
l31 = layout_with_fr(g31)
l51 = layout_with_fr(g51)
## Adjust N K above to produce Different Figures
# To save figure
##pdf("NK_genome_graphs.pdf")
# Label size
ls = .7
# Figure parameters
par(mfrow=c(2,2))
par(mar=c(2,2,2,2))
# Plot graphs
plot(g31,layout=l31, vertex.size=15*V(g31)$fitness^(1/3), vertex.label.dist = 3, main="N=3,K=1", edge.arrow.size=.3, vertex.label.cex = ls, vertex.label.family="Helvetica")
plot(g32,layout=l31, vertex.size=15*V(g32)$fitness^(1/3), vertex.label.dist = 3, main="N=3,K=2", edge.arrow.size =.3, vertex.label.cex = ls, vertex.label.family="Helvetica")
plot(g51,layout=l51, vertex.size=15*V(g51)$fitness^(1/3), vertex.label.dist = 3, main="N=5,K=1", edge.arrow.size =.3, vertex.label.cex = ls, vertex.label.family="Helvetica")
plot(g54,layout=l51, vertex.size=15*V(g54)$fitness^(1/3), vertex.label.dist = 3, main="N=5,K=4", edge.arrow.size =.3, vertex.label.cex = ls, vertex.label.family="Helvetica")
##dev.off()

## some stats on above
## sum(degree(g31, mode="out")==0)
## sum(degree(g32, mode="out")==0)
## sum(degree(g51, mode="out")==0)
## sum(degree(g54, mode="out")==0)

## If using Rmarkdown, put this outside the code chunk to import figure saved above: ![Plotting the NK landscape as a network. Arrows show possible one-step transitions in the space from genomes of lower fitness to genomes of higher fitness. The solid black nodes are local minima with zero outdegree, and inescapable using gradient ascent. The size of the node corresponds to its relative fitness.  N=3, K=1 and N=5,K=1 both have one maximum. N=3, K= 2 has one local maximum and one global maximum. N=5, K=4 has 6 maxima, 5 of which are local.  ](./images/NK_genome_graphs.pdf)





### SIMULATION Code
### WARNING: This takes a while to run with these parameters.
### In the book, I save the output and then import it.

### Make some networks
### Make some landscapes
### Simulate network search over landscapes
set.seed(2)
N=15; K=10 #NK PARAMETERS
n.agents=30 #NO. AGENTS
tsteps=150 #NO. TIME STEPS. 
repetitions = 200 
#### Make networks
nets <- list()
nets[[1]] <-  make_full_graph(n.agents)
nets[[2]] <-  make_ring(n.agents)
nets[[3]] <- make_empty_graph(n.agents, directed=FALSE)
nets[[4]] <- make_empty_graph(n.agents, directed=FALSE) # Annealing network, will add random edges
nets[[5]] <- make_empty_graph(n.agents, directed=FALSE) # Annealing network will add random edges

##GENERATE LANDSCAPE (as above)
LS<-permutations(2,N,v=c(0,1),repeats.allowed=TRUE)
LS<-as.data.frame(LS)
if (K==0){
  depends <- as.vector(1:N)
  values <- replicate(N,round(runif(2,0,1),1))
  fitness <- values
} else {
  # 'depends' determines which other bits the kth bit depends on
  depends <- rbind(1:N,replicate(N,sample(c(1:N),K,replace=F)))
  depends2 <- rbind(1:N,replicate(N,sample(c(1:N),K,replace=F))) # 2nd Env
  # Combinations: possible arrangements of bits for each value
  combinations <- permutations(2,K+1,v=c(0,1),repeats.allowed=TRUE)
  # Values: fitness values for each arrangment in each position
  values <- replicate(N,round(runif(nrow(combinations),0,1),1))
  values2 <- replicate(N,round(runif(nrow(combinations),0,1),1)) # 2nd Env
  # Fitness assigns K+1 combinations (rows) to N genome locations (columns, starting after K+1)
  fitness <- cbind(combinations,values)
  
}
# Make landscape
landscape1 <- generate_landscape(N,K,LS,fitness,depends) 
landscape1[,N+1] <- (landscape1[,N+1]/max(landscape1[,N+1]))^8 # Take fitness to 8th power: amplify differences
gene.names <- sapply(1:nrow(landscape1), function(x) paste(landscape1[x,1:N], collapse=""))
landscape1 <-cbind(landscape1, gene.names)


# GROUP SEARCH SIMULATION
netdata_perf <- list() # Store data here
netdata_div <- list() # Store data herej
# Each repetition we run through each network
for (rep in 1:repetitions){ 
    # Reset landscape every 10 repetitions---randomize landscapes
    if(rep %% 10 == 0){
      if (K==0){
        depends <- as.vector(1:N)
        values <- replicate(N,round(runif(2,0,1),1))
        fitness <- values
      } else {
        # 'depends' determines which other bits the kth bit depends on
        depends <- rbind(1:N,replicate(N,sample(c(1:N),K,replace=F)))
        depends2 <- rbind(1:N,replicate(N,sample(c(1:N),K,replace=F))) # 2nd Env
        # Combinations: possible arrangements of bits for each value
        combinations <- permutations(2,K+1,v=c(0,1),repeats.allowed=TRUE)
        # Random fitness values for each arrangement in each position
        values <- replicate(N,round(runif(nrow(combinations),0,1),1))
        # Fitness assigns K+1 combinations (rows) to N genome locations (columns, starting after K+1)
        fitness <- cbind(combinations,values)
      }
      # Make landscape
      landscape1 <- generate_landscape(N,K,LS,fitness,depends) #  computes fitness for all possible genomes ahead of time bsased on values above
      landscape1[,N+1] <- (landscape1[,N+1]/max(landscape1[,N+1]))^8 # take fitness to 8th power
      gene.names <- sapply(1:nrow(landscape1), function(x) paste(landscape1[x,1:N], collapse=""))
      landscape1 <-cbind(landscape1, gene.names)
    }
    for (net in 1:length(nets)){
      # Set data collectors for performance and diversity
      total.perftime  <- matrix(0,ncol=tsteps, nrow = repetitions)
      total.diversity <- matrix(0,ncol=tsteps, nrow = repetitions)
      
      # Reset agents  : #ALLOCATE RANDOM STARTING POSITIONS
      agents<-list()
      agents[[1]] <- landscape1[sample(1:(2^N),n.agents,replace=F),] # sample from possible genomes (2^N) and get fitness alongside
      # reset networks for annealing graphs
      if(net==4){
      nets[[4]] <- make_empty_graph(n.agents, directed=FALSE) # 
      }
      if(net==5){
      nets[[5]] <- make_empty_graph(n.agents, directed=FALSE) # 
      }

        
        #FOR EACH TIME STEP find best neighbor or explore
        for (i in 2:tsteps){
          # Bring forward genomes
          agents[[i]] <- agents[[i-1]]
          # Identify neighbors for each node
          samples <- lapply(1:n.agents, function(x) as.vector(neighbors(nets[[net]],x)))
          # Make dummy vector for individual learning, updated below
          ind.learn<-vector(length=n.agents) 
          # Replacements for social learning, so we don't copy over one another during replacement
          agents2 <- agents[[i]] 
          # For each agent
          for (j in 1:n.agents){
            payoffs <- agents[[i]][samples[[j]],'fitness.score'] # Get fitness of neighbors 
            if(length(payoffs) > 0){
              best<-which(payoffs==max(payoffs)) # Which of sample has highest payoff
              if(length(best)>1) best <- sample(best,1) # If there's a tie in highest payoff, sample randomly
           
            ## If this were partial copying, this is where it would happen
            ## At present, copying is complete
              agents2[j,] <- agents[[i]][samples[[j]][best],] # Adopt choice of best member
            } 
            if(agents2[j,'fitness.score']<=agents[[i-1]][j,'fitness.score']){ # If payoff worse or equal, learn individually
              ind.learn[j] <- j
            }
          }
          # Reduce ind.learn list to index for self-learners (explorers) 
          ind.learn<-ind.learn[ind.learn!=0]
          # Replace those who have a better neighbor
          no.learn <- setdiff(1:n.agents,ind.learn) # Removes all ind.learn from list
          if(length(no.learn)>0){
            agents[[i]][no.learn,] <- agents2[no.learn,] # Assigns learned best from above 
          }
          #IMPLEMENT INDIVIDUAL LEARNING
          if(length(ind.learn)>=1){
            for (n in c(ind.learn)){
              # Copy the genome
              gene_buffer <- agents[[i-1]][n,1:N]
              # flip one bit
              # sample bit
              bit <- sample(1:N, 1)
              # flip it
              gene_buffer[bit] <- ifelse(gene_buffer[bit]==1, 0, 1) 
              # Fix name
              gene_buffer.name <- paste(gene_buffer, collapse="")
              # Recompute fitness
                gene_buffer.fitness <- landscape1[which(landscape1$gene.names==gene_buffer.name),'fitness.score']
            }
              # Replace the original
              agents[[i]][n,] <- data.frame(gene_buffer, fitness.score=gene_buffer.fitness, gene.names = gene_buffer.name)
          }
          #COMPARE PAYOFFS AFTER LEARNING AND DECIDE WHETHER OR NOT TO SWITCH
          switching <- ifelse(agents[[i-1]][,'fitness.score'] < agents[[i]][,'fitness.score'],1,0)*1:n.agents
          not.switching <- setdiff(1:n.agents,switching[switching!=0])
          agents[[i]][not.switching,] <- agents[[i-1]][not.switching,] # Those who didn't improve stay the same
          if(net==4){ # Annealing network
             if(runif(1)<=1){ # Always add an edge
             nets[[net]] <- nets[[net]] %>% igraph::add.edges(sample(1:n.agents,2))
             nets[[net]]<-igraph::simplify(nets[[net]])
             }
          }
          if(net==5){ # Second annealing network
             if(runif(1)<=.75){
             nets[[net]] <- nets[[net]] %>% igraph::add.edges(sample(1:n.agents,2))
             nets[[net]]<-igraph::simplify(nets[[net]])
             }
          }
        } # End each time step here
        # Average of fitness 
        perf.time <- sapply(1:tsteps, function(x) mean(agents[[x]][,'fitness.score'])) 
        # Number of unique genomes
        diversity <- sapply(1:tsteps, function(x) nrow(unique(agents[[x]]))) 
      # Record results 
      if (rep == 1){
        netdata_perf[[net]] <- perf.time
        netdata_div[[net]] <- diversity
      } else {
        netdata_perf[[net]] <- rbind(netdata_perf[[net]], perf.time)
        netdata_div[[net]] <- rbind(netdata_div[[net]], diversity)
      }
        
    }      # End each network repetition here

  } # End group of repetitions here
  
# Keep track of total performance over time and total diversity over time
netdata_perf.stable <- netdata_perf[c(1:5)]
netdata_div.stable <- netdata_div[c(1:5)]


#### FIGURES
# This saves the figure commands as a pdf up until dev.off()
pdf(file="FollowTheBest_1.pdf", width=12, height=5)
par(mfrow=c(1,2))
par(mar=c(5,5,2,2))
plot(colMeans(netdata_perf.stable[[1]]), ylab = "Fitness", xlab = "Timesteps", ylim = c(0,.45), type ="n", cex.lab = 1.5, cex.axis = 1.2)
for(i in 1:length(nets)){
 lines(colMeans(netdata_perf.stable[[i]]), col = i, lwd = 2, lty=i) 
}

legend(75,.3, legend=c("Full", "Ring", "Independent", TeX("Annealing: $\\gamma= 1.0$"), TeX("Annealing: $\\gamma=.75$")),lwd=2, lty=1:5, col=c(1,2,3,4, 5), bty="n")

plot(colMeans(netdata_div.stable[[1]]), ylab = "Unique solutions", xlab = "Timesteps", ylim = c(0,n.agents +1), type ="n", cex.lab = 1.5, cex.axis = 1.2)
for(i in 1:length(nets)){
  lines(colMeans(netdata_div.stable[[i]]), col = i, lwd = 2, lty=i) 
}
# This ends the figure save
dev.off()

## ![figure](./images/FollowTheBest_1.pdf)

