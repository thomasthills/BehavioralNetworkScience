knitr::opts_chunk$set(echo = FALSE)
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(comment = NA)
knitr::opts_chunk$set(warning = FALSE)
options(knitr.table.format = "latex")


### Behavioral Network Science
### Language, Mind, and Society
# By Thomas T. Hills

# Code to reproduce figures and analyses for

#### Chapter 16 -- The Segregation of Belief: How Structure Facilitates False Consensus ####

rm(list=ls()) # mental hygiene
library(tinytex)
library(igraph)
library(tidyverse)
library(kableExtra)
library(fitdistrplus)


#### Figure 16-1 ####

# Schelling segregation model simulation

# Random number seed
set.seed(1)
# Size of matrix is 19 x 19 
n=19
# Vertex size
cs = 7 
# Plot parameters
par(mfrow=c(2,2))
par(mar=c(1,1,1,1))
# Make graph
g <- make_lattice(length= n, dim = 2 )
# Set colors
V(g)$color = sample(c("black", "white"), length(V(g)), replace = TRUE)
# Plot
plot(g, vertex.size = cs, vertex.label = NA, layout=layout_on_grid)

# Schelling migration function that computed proportion of neighbors
swaplist <- function(i){
  mat <- as_adj(g)
  # Returns proportion of neighbors who share same color
  sum(as.numeric(V(g)$color[i]==V(g)$color[which(mat[i,]==1)]))/sum(mat[i,])
}

# Number of iterations
sims = 100
# Threshold for satisfaction 
threshold = .5
# Count of swaps
swapcount = 0
# For each of sims
for(i in 1:sims){
  # Find individuals who are not satisfied (output of swaplist function is less than threshold)
  whitelist <- which(sapply(V(g), swaplist) < threshold)[V(g)$color[which(sapply(V(g), swaplist) < threshold)]=="white"]
  # sapply takes each numbered vertex in turn and puts it in swaplist---only count outputs of the right color
  blacklist <- which(sapply(V(g), swaplist) < threshold)[V(g)$color[which(sapply(V(g), swaplist) < threshold)]=="black"]
  # If both lists aren't empty
  if(!is_empty(whitelist) & !is_empty(blacklist)){
    # Sample a black and white node
    white <- sample(whitelist, 1)
    black <- sample(blacklist, 1)
    nw <- neighbors(g, white)
    nwt <- table(V(g)$color[nw])
    nb <- neighbors(g, black)
    nbt <- table(V(g)$color[nb])
    # Sanity check to make sure they have the correct numbers of neighbors
    swapnbt <- ifelse(nbt["black"]/sum(nbt) < threshold | is.na(nbt["black"]),TRUE, FALSE)
    swapnwt <- ifelse(nwt["white"]/sum(nwt) < threshold || is.na(nwt["white"]), TRUE, FALSE)
    # If they both pass sanity check, then swap
    if(swapnbt == TRUE & swapnwt == TRUE){
     V(g)$color[black] <- "white" 
     V(g)$color[white] <- "black" 
       #print("swap")
     # Add 1 to swapcount
       swapcount <- swapcount + 1
    }
  } else {
    #print(paste("after " , swapcount , " swaps, no more to swap"), sep = " " )
    break
    }
}

# Plot lattice with above assigned colors
plot(g, vertex.size = cs, vertex.label = NA, layout=layout_on_grid)
##x2 <- ~plot(g, vertex.size = cs, vertex.label = NA, layout=layout_on_grid)

# Set random seed
set.seed(1)
# Size of lattice
n=19
# Make lattice
g <- make_lattice(length= n, dim = 2 )
# Set colors
V(g)$color = sample(c("black", "white"), length(V(g)), replace = TRUE, prob = c(.7, .3))
# Plot lattice
plot(g, vertex.size = cs, vertex.label = NA, layout=layout_on_grid)

# Swaplist function as above
swaplist <- function(i){
  mat <- as_adj(g)
  sum(as.numeric(V(g)$color[i]==V(g)$color[which(mat[i,]==1)]))/sum(mat[i,])
}

# Repeat as above
swapcount = 0
for(i in 1:sims){
whitelist <- which(sapply(V(g), swaplist) < threshold)[V(g)$color[which(sapply(V(g), swaplist) < threshold)]=="white"]
blacklist <- which(sapply(V(g), swaplist) < threshold)[V(g)$color[which(sapply(V(g), swaplist) < threshold)]=="black"]
if(!is_empty(whitelist) & !is_empty(blacklist)){
  white <- sample(whitelist, 1)
  black <- sample(blacklist, 1)
  nw <- neighbors(g, white)
  nwt <- table(V(g)$color[nw])
  nb <- neighbors(g, black)
  nbt <- table(V(g)$color[nb])
  swapnbt <- ifelse(nbt["black"]/sum(nbt) < threshold | is.na(nbt["black"]),TRUE, FALSE)
  swapnwt <- ifelse(nwt["white"]/sum(nwt) < threshold || is.na(nwt["white"]), TRUE, FALSE)
  if(swapnbt == TRUE & swapnwt == TRUE){
   V(g)$color[black] <- "white" 
   V(g)$color[white] <- "black" 
     #print("swap")
     swapcount <- swapcount + 1
  }
} else {
  #print(paste("after " , swapcount , " swaps, no more to swap"), sep = " " )
  break
  }
}

# Plot output lattice
plot(g, vertex.size = cs, vertex.label = NA, layout=layout_on_grid)



#### Figure 16-2 ####

## Beta distribution

# Plot parameters
par(mfrow=c(1,1))
par(mar=c(5,5,5,5))
# X-axis
p = seq(0,1, length=100)
# Plot along x-axis
plot(p, dbeta(p, 4,6), type='l', ylab="Density", ylim = c(0,6))
# Add line for beta with different values
lines(p, dbeta(p, 20, 30), type='l', lty = 2)


#### Figure 16-3 ####

## Extremeness aversion

# Values for Beta distributions
i1 <- c(4,9)
is <- c(6,2)
# X-axis
p = seq(0,1, length=100)
# Plot both functions 
plot(p, dbeta(p, i1[1], i1[2]), type='l',ylim=c(0,4), ylab="Density")
lines(p, dbeta(p, is[1], is[2]), lty=2)

## Authenticity preference function
Ap <- function(a,b,x){
  abs(pbeta(x,a,b)-.5) # Utility lost
}

## Extremeness aversion function
Ea <- function(a,b,x){
  abs(.5-pbeta(x,a,b)) # Utility lost
}

# Utility function as combination of above
oU <- function(x, i1,is,w=.5, gamma=20){
  # Authenticity preference
  ap<-Ap(i1[1],i1[2],x)
  # Extremeness aversion
  ea<-Ea(is[1],is[2],x)
  # Combine them into disutility
  totalDisutility <- w*exp(gamma*(ap-.5))+(1-w)*exp(gamma*(ea-.5))
  # Subtract disutility from 1
  OverallUtility <- 1-totalDisutility
  # Return output
  return(OverallUtility)
}

# Compute values
value <- oU(p, i1, is)
value <- value-min(value)
value <- value/max(value)

# Plot line for utility
lines(p, value, lty=3)
# Add utility axis on right hand side
axis(side = 4, at = pretty(range(c(0,4))))      # Add second axis
# Add label
mtext("Utility", side = 4, line = 3)  
# Add legend
legend(0, 4, legend=c("Authentic beliefs", "Social Environment", "Utility of expressed behavior"), cex = .8, lty=c(1,2,3))
# Draw line at expressed behavior
abline(v=p[which(value==max(value))], xpd=FALSE,  lwd = 2, col="gray80")


#### Figure 16-4 ####

# Random number seed
set.seed(2)
just = 4
# Plot parameters
par(mfrow=c(4,3))
par(mar=c(2,2,2,2))
# Set text size
textsize = 1.2

## top row with 1 node
##plot(1, type = "n")
plot.new()
# Add label
text(0,0,"Authentic behavior", cex = textsize, pos =just)
# Make network with 1 node
g1 <- graph_from_literal(1)
V(g1)$name = NA
# Plot it
plot(g1, vertex.color = "white")
# Set Beta dist parameters
i1 <- c(4,9)
# X-axis
p = seq(0,1, length=100)
# Set margins
par(mar=c(4,4,2,2))
# Plot authenticity pref
plot(p, dbeta(p, i1[1], i1[2]), type='l',ylim=c(0,4), ylab="Density")
# Set beta
x1q = qbeta(.5, 4,9) 
# Add expressed behavior
abline(v=x1q, xpd=FALSE, lwd =2, col="gray80")

## Second row with 2 nodes and social influence
par(mar=c(2,2,2,2))
plot.new()
text(0,0,"Social influence", cex =textsize, pos =just)
g2 <- graph_from_literal(1-2 )
V(g2)$name = NA
plot(g2, vertex.color = c("white", "black"))
par(mar=c(4,4,2,2))
plot(p, dbeta(p, i1[1], i1[2]), type='l',ylim=c(0,4), ylab="Density")
i2 <- c(6,2) # other agent
x2q = qbeta(.5, i2[1],i2[2]) # other agents expressed behavior 
abline(v=x2q, xpd=FALSE, lwd =2, lty=2, col="gray80")
fit <- fitdist(c(x2q, x1q), "beta") # fit beta to other agents expressed behavior (plus agent's behavior)
value <- oU(p, i1, c(fit$estimate[1], fit$estimate[2]))
value <- value-min(value)
value <- value/max(value)
lines(p, value, lty=3)
abline(v=p[which(value==max(value))], xpd=FALSE,  lwd = 2, col="gray80")


## Third row with 3 nodes -- conformity
par(mar=c(2,2,2,2))
plot.new()
text(0,0,"Conformity effect", cex = textsize, pos =just)
g2 <- graph_from_literal(1-2, 1-3 )
V(g2)$name = NA
plot(g2, vertex.color = c("white", "black", "black")) 
par(mar=c(4,4,2,2))
plot(p, dbeta(p, i1[1], i1[2]), type='l',ylim=c(0,4), ylab="Density")
i2 <- c(6,2) # other agent
i3 <- c(9,4)
x2q = qbeta(.5, i2[1],i2[2]) # other agents expressed behavior 
x3q = qbeta(.5, i3[1],i3[2]) # other agents expressed behavior 
abline(v=x2q, xpd=FALSE, lwd =2, lty=2, col="gray80")
abline(v=x3q, xpd=FALSE, lwd =2, lty=2, col="gray80")
fit <- fitdist(c(x2q,x3q), "beta") # fit beta to other agents expressed behavior
##lines(p, dbeta(p, fit$estimate[1], fit$estimate[2]), type='l')
value <- oU(p, i1, c(fit$estimate[1], fit$estimate[2]))
value <- value-min(value)
value <- value/max(value)
lines(p, value, lty=3)
abline(v=p[which(value==max(value))], xpd=FALSE,  lwd = 2, col="gray80")

## Fourth row with 6 nodes -- backfire effect
par(mar=c(2,2,2,2))
plot.new()
text(0,0,"Backfire effect", cex = textsize, pos =just)
g2 <- graph_from_literal(1-2, 1-3,1-4 )
V(g2)$name = NA
plot(g2, vertex.color = c("white", "black", "black", "black"))
par(mar=c(4,4,2,2))
plot(p, dbeta(p, i1[1], i1[2]), type='l',ylim=c(0,4), ylab="Density")
i2 <- c(8,1) # other agent
i3 <- c(100,4)
i4 <- c(10,1)
x2q = qbeta(.5, i2[1],i2[2]) # other agents expressed behavior 
x3q = qbeta(.5, i3[1],i3[2]) # other agents expressed behavior 
x4q = qbeta(.5, i4[1],i4[2]) # other agents expressed behavior 
abline(v=x2q, xpd=FALSE, lwd =2, lty=2, col="gray80")
abline(v=x3q, xpd=FALSE, lwd =2, lty=2, col="gray80")
abline(v=x4q, xpd=FALSE, lwd =2, lty=2, col="gray80")
fit <- fitdist(c(x2q,x3q, x4q), "beta") # fit beta to other agents expressed behavior
value <- oU(p, i1, c(fit$estimate[1], fit$estimate[2]))
value <- value-min(value)
value <- value/max(value)
lines(p, value, lty=3)
abline(v=p[which(value==max(value))], xpd=FALSE,  lwd = 2, col="gray80")


#### Figure 16-5 ####

## Grid along beta distribution
p = seq(0,1, length=100) 
## Authenticity preference
Ap <- function(a,b,x){
  abs(pbeta(x,a,b)-.5) # utility lost
}
## Extremeness aversion
Ea <- function(a,b,x){
  abs(.5-pbeta(x,a,b)) # utility lost
}
## Utility function for expressed behavior
oU <- function(x, i1,is,w=.4, gamma=5){
  ap<-Ap(i1[1],i1[2],x)
  ea<-Ea(is[1],is[2],x)
  totalDisutility <- w*exp(gamma*(ap-.5))+(1-w)*exp(gamma*(ea-.5))
  OverallUtility <- 1-totalDisutility
  return(OverallUtility)
}
### Expressed behavior
expressed <- function(x){
  value <- x-min(x)
  value <- value/max(value)
  v=p[which(value==max(value))]
  return(v)
}
# Color palette
pal = colorRampPalette(c("orange", "blue"))


## Iterate over expressed behavior updating based on social network

### SSTSchellingFunction

ssts <- function(graph, sims=10, migration=FALSE, S =0, CYF=FALSE, fractionToSwap = .05){
  g <- graph
  ## Get adjacency for neighbors
  gmat <- as_adj(g, sparse=F)
  ## Iterate for sims
  for(i in 1:sims){
    ## Adapt behavior to social environment
    ## Identify neighbors (gmat)
    ## Get expressed behaviors
    for(j in 1:length(V(g))){
      if(sum(gmat[j,])> 1){
        #Get beta dist from neighbor expressed behavior
        neighborexpressed <- V(g)$express[which(gmat[j,]==1)]
        if(length(unique(neighborexpressed))==1){ # add some noise if they're identical
          fit <- fitdist(neighborexpressed+runif(length(neighborexpressed),0,.01), "beta") 
        } else {
          fit <- fitdist(c(V(g)$express[which(gmat[j,]==1)]), "beta") 
        }
        fit$estimate <- fit$estimate/sum(fit$estimate)*20
        V(g)$sfite1[j] <-  fit$estimate[1]
        V(g)$sfite2[j] <-  fit$estimate[2]
        # Create value function for egos expressed behavior
        val <- oU(p, c(V(g)$s1[j], V(g)$s2[j]), c(fit$estimate[1], fit$estimate[2]))
        # Get max of val (happiness now)
        V(g)$express_buf[j] <- expressed(val)
        V(g)$happiness_now[j] <- max(val)
        # Some plotting if needed (change to TRUE)
        if(FALSE){
          plot(p, dbeta(p, V(g)$s1[j], V(g)$s2[j]), type='l',ylim=c(0,4), ylab="Density")
          lines(p, dbeta(p, fit$estimate[1], fit$estimate[2]), lty = 3, lwd  = 3)
          abline(v=c(V(g)$express[which(gmat[j,]==1)]), xpd=FALSE, lwd =2, lty=2, col="gray80")
          lines(p, val, lty=3)
          abline(v=V(g)$express_buf[j], xpd=FALSE,  lwd = 2, col="gray80")
        }
        
      } else {
        print("not enough neighbors: fix this")
      }
    }
    ## Reset expressed behavior of everyone
    V(g)$express <- V(g)$express_buf
    
    ## Migrate if migration == TRUE
    if(migration){
      # Assign pairs
      pairs <- matrix(sample(1:length(V(g)), length(V(g)) ), ncol = 2)
      # Sample a subset of pairs to swap
      pairsToSwap <- ceiling(nrow(pairs)*fractionToSwap )
      for(k in 1:pairsToSwap){ # Every agent in fraction gets a chance to move 
        switch1 <- 0
        switch2 <- 0
        n1 <- pairs[k,1]
        n2 <- pairs[k,2]
        # Check 1 in 2s env
        valcheck1 <- oU(p, c(V(g)$s1[n1], V(g)$s1[n1]), 
                        c(V(g)$sfite1[n2], V(g)$sfite2[n2]))
        max1_2 <- max(valcheck1)
        # Check 2 in 1s env
        valcheck2<- oU(p, c(V(g)$s1[n2], V(g)$s1[n2]), 
                       c(V(g)$sfite1[n1], V(g)$sfite2[n1]))
        max2_1 <- max(valcheck2)
        # If both switches exceed happiness now
        if(max1_2 > V(g)$happiness_now[n1]){switch1 <- 1}
        if(max2_1 > V(g)$happiness_now[n2]){switch2 <- 1}
        # If both agree, then switch beliefs (shape parameters )
        if(switch1 + switch2 == 2){
          s1buf <- V(g)$s1[n1] 
          s2buf <- V(g)$s2[n1] 
          name1_buf <- as.numeric(V(g)$name[n1])
          V(g)$s1[n1] <- V(g)$s1[n2]
          V(g)$s2[n1] <- V(g)$s2[n2]
          V(g)$s1[n2] <- s1buf
          V(g)$s2[n2] <- s2buf
          
          # Expressed for 1 in 2's env goes to new 2 and vice versa
          V(g)$express[n2] <- expressed(valcheck1)
          V(g)$express[n1] <- expressed(valcheck2)
          V(g)$happiness_now[n2] <- max(valcheck1)
          V(g)$happiness_now[n1] <- max(valcheck2)
          xnames <- as.numeric(V(g)$name)
          xnames[n1] <- V(g)$name[n2]
          xnames[n2] <- name1_buf
          V(g)$name <- xnames 
          #print(paste("swap", n1, n2))
        }
      }
      #print("swap complete")
    }
    ## Choose your friends
    if(CYF){
      dg <- distances(g)
      dg[which(dg > S)] <- 0 
      dg[which(dg > 0)] <- 1 
      for(l in 1:length(V(g))){
       samplelist <- sample(which(dg[l,]>0),3) 
      }
    } # end CYF
    #print(paste("mean happiness before:", mean(V(g)$happiness))) 
    #print(paste("mean happiness now:", mean(V(g)$happiness_now))) 
    #print(V(g)$happiness_now-V(g)$happiness) 
    V(g)$happiness <- V(g)$happiness_now
    #print("individual happiness")
    #print(V(g)$happiness)
  } # end sims
  # return graph
  return(g)
} # end function




set.seed(1)
n=10 # Needs to be even -- x and y dimension of lattice
cs = 20 # Vertex size
## Make graph
g <- make_lattice(length= n, dim = 2 )
vc <- length(V(g))
### Assign beliefs: uniform draws from 1 to 10 for each shape parameter
V(g)$s1[1:vc] <- round(runif(vc,1,10))
V(g)$s2[1:vc] <- round(runif(vc,1,10))
betashape <- data.frame(V(g)$s1, V(g)$s2)
### Express behavior based on authentic beliefs
V(g)$express[1:vc] <-  mapply(function(x,y) qbeta(.5, x, y), betashape[,1], betashape[,2])
### Assign colors
V(g)$color = pal(100)[round(V(g)$express*100, 2)] 
# Plot parameters
par(mfrow=c(1,4))
par(mar=c(1,1,1,1))
# Plot lattice
plot(g, vertex.size = cs,  layout=layout_on_grid, vertex.label = NA)
text(0, 1.5, "G=0", cex = 1.5)
# Run 1 iteration and allow nodes to update expressed behavior
g1 <- ssts(g, sims = 1)
# Reassign colors
V(g1)$color = pal(100)[V(g1)$express*100] 
# plot
plot(g1, vertex.size = cs,  layout=layout_on_grid, vertex.label = NA)
text(0, 1.5, "G=1", cex = 1.5)
# Repeat for 1 sim
g2m <- ssts(g1, sims = 1, migration = FALSE)
V(g2m)$color = pal(100)[V(g2m)$express*100] 
plot(g2m, vertex.size = cs,  layout=layout_on_grid, vertex.label = NA)
text(0, 1.5, "G=2", cex = 1.5)
# Repeat for 98 sims
g3m <- ssts(g2m, sims = 98, migration = FALSE)
V(g3m)$color = pal(100)[V(g3m)$express*100] 
plot(g3m, vertex.size = cs,  layout=layout_on_grid, vertex.label = NA)
text(0, 1.5, "G=100", cex = 1.5)

## parameters
## mean(V(g3m)$express) # [1] 0.4774747
## max(V(g3m)$express) # [1] 0.5858586
## min(V(g3m)$express) # [1] 0.4040404
## sd(V(g3m)$express) # [1] 0.03645803
## mean(V(g3m)$happiness) # [1] 0.7864665



#### Figure 16-6 ####

par(mfrow=c(1,3))
par(mar=c(0,0,0,0))
set.seed(1)
# Size of lattice
n=10
cs = 25 # Vertex size
## Make graph
g <- make_lattice(length= n, dim = 2 )
vc <- length(V(g))
V(g)$name <- V(g)
### Assign beliefs: uniform draws from 1 to 10 for each shape parameter
V(g)$s1[1:vc] <- round(runif(vc,1,10))
V(g)$s2[1:vc] <- round(runif(vc,1,10))
betashape <- data.frame(V(g)$s1, V(g)$s2)
### Express behavior based on authentic beliefs
V(g)$express[1:vc] <-  mapply(function(x,y) qbeta(.5, x, y), betashape[,1], betashape[,2])

### Assign colors
V(g)$color = pal(100)[round(V(g)$express*100, 2)] 
# Plot lattice
plot(g, vertex.size = cs,  layout=layout_on_grid )
# Label
text(0, 1.5, "G=0", cex = 1.5)
V(g)$happiness <- 1
# Run for 2 sims
g1 <- ssts(g, sims = 1)
V(g1)$color = pal(100)[V(g1)$express*100] 
g1 <- ssts(g1, sims = 1)
V(g1)$color = pal(100)[V(g1)$express*100] 
# Run  with migration == TRUE
g1 <- ssts(g1, sims = 98, migration = TRUE) # iterate
V(g1)$color = pal(100)[V(g1)$express*100] 
plot(g1, vertex.size = cs,  layout=layout_on_grid)
text(0, 1.5, "G=100", cex = 1.5)
# Run 900 more times with migration
g1000 <- ssts(g1, sims = 900, migration = TRUE) # iterate
V(g1000)$color = pal(100)[V(g1000)$express*100] 
plot(g1000, vertex.size = cs,  layout=layout_on_grid)
text(0, 1.5, "G=1000", cex = 1.5)


## parameters
## mean(V(g1)$express) # [1] 0.4686869
## max(V(g1)$express) # [1] 0.6363636
## min(V(g1)$express) # [1] 0.3131313
## sd(V(g1)$express) # [1] 0.08521549
## mean(V(g1)$happiness) # [1]0.802637
 
 
## parameters
## mean(V(g1000)$express) # [1] 0.4220202
## max(V(g1000)$express) # [1] 0.6767677
## min(V(g1000)$express) # [1] 0.2121212
## sd(V(g1000)$express) # [1] 0.1372766
## mean(V(g1000)$happiness) # [1] 0.8381823


rm(list=ls()) #  Good hygiene
## Grid along beta distribution
p = seq(0,1, length=100) 
## Authenticity preference
Ap <- function(a,b,x){
  abs(pbeta(x,a,b)-.5) # utility lost
}
## Extremeness aversion
Ea <- function(a,b,x){
  abs(.5-pbeta(x,a,b)) # utility lost
}
## Utility function for expressed behavior
oU <- function(x, i1,is,w=.4, gamma=5){
  ap<-Ap(i1[1],i1[2],x)
  ea<-Ea(is[1],is[2],x)
  totalDisutility <- w*exp(gamma*(ap-.5))+(1-w)*exp(gamma*(ea-.5))
  OverallUtility <- 1-totalDisutility
  return(OverallUtility)
}
### Expressed behavior
expressed <- function(x){
  value <- x-min(x)
  value <- value/max(value)
  v=p[which(value==max(value))]
  return(v)
}

# Color palette
pal = colorRampPalette(c("orange", "blue"))


## Iterate over expressed behavior updating based on social network

### SSTSchellingFunction (repeated from above with CYF expanded)
 ssts <- function(graph, sims=10, migration=FALSE, S=2, CYF=FALSE, fractionToSwap = .05){
  g <- graph
  
  ## Iterate for sims
  for(i in 1:sims){
    ## Get adjacency for neighbors
    gmat <- as_adj(g, sparse=F)
    # What's authentic
    betashape <- data.frame(V(g)$s1, V(g)$s2)
    ## Express behavior based on authentic beliefs
    V(g)$authentic <-  mapply(function(x,y) qbeta(.5, x, y), betashape[,1], betashape[,2])
    
    ## Adapt behavior to social environment
    ## Identify neighbors (gmat)
    ## Get expressed behaviors
    for(j in 1:length(V(g))){
      if(sum(gmat[j,])> 1){
        #Get beta dist from neighbor expressed behavior
        neighborexpressed <- V(g)$express[which(gmat[j,]==1)]
        if(length(unique(neighborexpressed))==1){ # add some noise if they're identical
          fit <- fitdist(neighborexpressed+runif(length(neighborexpressed),0,.01), "beta") 
        } else {
          fit <- fitdist(c(V(g)$express[which(gmat[j,]==1)]), "beta") 
        }
        fit$estimate <- fit$estimate/sum(fit$estimate)*20
        V(g)$sfite1[j] <-  fit$estimate[1]
        V(g)$sfite2[j] <-  fit$estimate[2]
        # Create value function for egos expressed behavior
        val <- oU(p, c(V(g)$s1[j], V(g)$s2[j]), c(fit$estimate[1], fit$estimate[2]))
        # Get max of val
        V(g)$express_buf[j] <- expressed(val)
        V(g)$happiness_now[j] <- max(val)
        # Some plotting if needed
        if(FALSE){
          plot(p, dbeta(p, V(g)$s1[j], V(g)$s2[j]), type='l',ylim=c(0,4), ylab="Density")
          lines(p, dbeta(p, fit$estimate[1], fit$estimate[2]), lty = 3, lwd  = 3)
          abline(v=c(V(g)$express[which(gmat[j,]==1)]), xpd=FALSE, lwd =2, lty=2, col="gray80")
          lines(p, val, lty=3)
          abline(v=V(g)$express_buf[j], xpd=FALSE,  lwd = 2, col="gray80")
        }
        
      } else {
        #print(paste( j, "doesn't have enough neighbors to fit beta: fix this"))
      }
    }
    ## Reset expressed behavior of everyone
    V(g)$express <- V(g)$express_buf
    
    ## Migrate
    if(migration){
      # Assign pairs
      pairs <- matrix(sample(1:length(V(g)), length(V(g)) ), ncol = 2)
      # Sample a subset of pairs to swap
      pairsToSwap <- ceiling(nrow(pairs)*fractionToSwap )
      for(k in 1:pairsToSwap){ # every agent gets a chance to move 
        switch1 <- 0
        switch2 <- 0
        n1 <- pairs[k,1]
        n2 <- pairs[k,2]
        # Check 1 in 2s env
        valcheck1 <- oU(p, c(V(g)$s1[n1], V(g)$s1[n1]), 
                        c(V(g)$sfite1[n2], V(g)$sfite2[n2]))
        max1_2 <- max(valcheck1)
        # Check 2 in 1s env
        valcheck2<- oU(p, c(V(g)$s1[n2], V(g)$s1[n2]), 
                       c(V(g)$sfite1[n1], V(g)$sfite2[n1]))
        max2_1 <- max(valcheck2)
        if(max1_2 > V(g)$happiness_now[n1]){switch1 <- 1}
        if(max2_1 > V(g)$happiness_now[n2]){switch2 <- 1}
        # If both agree, then switch beliefs (shape parameters )
        if(switch1 + switch2 == 2){
          s1buf <- V(g)$s1[n1] 
          s2buf <- V(g)$s2[n1] 
          name1_buf <- V(g)$name[n1]
          V(g)$s1[n1] <- V(g)$s1[n2]
          V(g)$s2[n1] <- V(g)$s2[n2]
          V(g)$s1[n2] <- s1buf
          V(g)$s2[n2] <- s2buf
          
          # expressed for 1 in 2's env goes to new 2 and vice versa
          V(g)$express[n2] <- expressed(valcheck1)
          V(g)$express[n1] <- expressed(valcheck2)
          V(g)$happiness_now[n2] <- max(valcheck1)
          V(g)$happiness_now[n1] <- max(valcheck2)
          V(g)$name[n1] <- V(g)$name[n2]
          V(g)$name[n2] <- name1_buf
          #print(paste("swap", n1, n2))
        }
      }
      #print("swap complete")
    }
    ## Choose your friends -- kills and creates edges
    if(CYF){
      # Get fraction to check
      listToSample <- sample(1:length(V(g)),ceiling(fractionToSwap*length(V(g))))
      #print(listToSample)
      for(l in listToSample){
        # This is needed inside because the network changes with each node
        dg <- distances(g)
        # If already a neighbor, set to 0
        dg[which(dg == 1)] <- 0 
        # If too far away, set to 0
        dg[which(dg > S)] <- 0 
        # Everyone else is a possible choice
        dg[which(dg > 0)] <- 1 
       # Get new neighbor list
       samplelist <- which(dg[l,]>0) 
       # Get existing neighbors list
       neighborsnow <- igraph::neighborhood(g,1,l)[[1]][-1]
       # See who's closest, swap if new guy is closer
       if(!is_empty(samplelist)){
         # Closest new neighbor list
          nnlist <- samplelist[which(abs(V(g)$express[samplelist]-V(g)$authentic[l])==min(abs(V(g)$express[samplelist]-V(g)$authentic[l])))]
          closest <- nnlist[sample(1:length(nnlist),1)]
          # Furthest old neighbor list
          fnlist <- neighborsnow[which(abs(V(g)$express[neighborsnow]-V(g)$authentic[l])==max(abs(V(g)$express[neighborsnow]-V(g)$authentic[l])))]
          furthest <- fnlist[sample(1:length(fnlist),1)]
          # Swap if condition is met
          if(abs(V(g)$express[closest]-V(g)$authentic[l]) < 
            abs(V(g)$express[furthest]-V(g)$authentic[l]) ) {
            g <- igraph::add_edges(g, c(l, closest))
            g <- igraph::delete_edges(g, get.edge.ids(g, c(l,furthest)))
            #print(paste("connected ", l, "and", closest))
            #print(paste("deleted", l, "and", furthest))
         }
       }
      } # end loop through listToSample (possible network edge swaps)
    } # end CYF
     # Repair isolates -- make a random edge
     listIsolate <- which(igraph::degree(g)==0)
      for(is in listIsolate){
        g <- igraph::add_edges(g, c(is, sample(1:length(V(g)), 1)))
      } 
    # Remove loops and multiples 
    g <- igraph::simplify(g)
    #print(paste("mean happiness before:", mean(V(g)$happiness))) 
    #print(paste("mean happiness now:", mean(V(g)$happiness_now))) 
    #print(V(g)$happiness_now-V(g)$happiness) 
    # Update happiness
    V(g)$happiness <- V(g)$happiness_now
    #print("individual happiness")
    #print(V(g)$happiness)
  } # end sims
  # return graph
  return(g)
} # end function




set.seed(1)
# Lattice size
n=10
## Make graph
g <- make_lattice(length= n, dim = 2 )
vc <- length(V(g))
V(g)$name <- V(g)
### Assign beliefs: uniform draws from 1 to 10 for each shape parameter
V(g)$s1[1:vc] <- round(runif(vc,1,10))
V(g)$s2[1:vc] <- round(runif(vc,1,10))
betashape <- data.frame(V(g)$s1, V(g)$s2)
### Express behavior based on authentic beliefs
V(g)$express[1:vc] <-  mapply(function(x,y) qbeta(.5, x, y), betashape[,1], betashape[,2])
V(g)$happiness <- 1

V(g)$name = NA
# Iterate with edge swaps
g100s2 <- ssts(g, sims = 100, CYF=TRUE, S=2, fractionToSwap = .01)
g500s2 <- ssts(g100s2, sims = 100, CYF=TRUE, S=2, fractionToSwap = .01)

g100s4 <- ssts(g, sims = 100, CYF=TRUE, S=4, fractionToSwap = .01)
g500s4 <- ssts(g100s4, sims = 100, CYF=TRUE, S=4, fractionToSwap = .01)

g100s6 <- ssts(g, sims = 100, CYF=TRUE, S=6, fractionToSwap = .01)
g500s6 <- ssts(g100s6, sims = 100, CYF=TRUE, S=6, fractionToSwap = .01)




#### Figure 16-7 ####

# Random seed and plot parameters
set.seed(1)
par(mfrow=c(3,2))
par(mar=c(2,2,2,2))
# Vertex size parameter
cs =20 

# Assign happiness
V(g)$happiness <- 1
# Remove labels
V(g)$name = NA

# Set colors (using data from above)
V(g100s2)$color = pal(100)[V(g100s2)$express*100] 
# Plot network
plot(g100s2, vertex.size = (V(g100s2)$happiness^3)*cs)
# Labels
text(-1, 1, "G=100", cex = 1.5)
text(-1.5, 0, "S=2", cex = 1.5)

# Set colors (using data from above), search = 2
V(g500s2)$color = pal(100)[V(g500s2)$express*100] 
# Plot network
plot(g500s2, vertex.size = (V(g500s2)$happiness^3)*cs)
# Label
text(-1,1, "G=400", cex = 1.5)

# Set colors (using data from above), search = 4
V(g100s4)$color = pal(100)[V(g100s4)$express*100] 
# Plot network
plot(g100s4, vertex.size = (V(g100s4)$happiness^3)*cs)
# Label
text(-1.5, 0, "S=4", cex = 1.5)

# Set colors (using data from above)
V(g500s4)$color = pal(100)[V(g500s4)$express*100] 
# Plot network
plot(g500s4, vertex.size = (V(g500s4)$happiness^3)*cs)

# Repeate for last two networks with search = 6
V(g100s6)$color = pal(100)[V(g100s6)$express*100] 
plot(g100s6, vertex.size = (V(g100s6)$happiness^3)*cs)
text(-1.5, 0, "S=6", cex = 1.5)

V(g500s6)$color = pal(100)[V(g500s6)$express*100] 
plot(g500s6, vertex.size = (V(g500s6)$happiness^3)*cs)




# Random seed
set.seed(1)
# Network lattice size
n=10
# Number of replicates for each search and migration type
replicates =50 
# How many iterations per replicate
runs = 200
# Keep track of time 
start_time <- Sys.time()
# Data buffers
gm <- list()
gs2 <- list()
gs4 <- list()
gs6 <- list()
# For each replicate
for(it in 1:replicates){
  # Make graph
  g <- make_lattice(length= n, dim = 2 )
  # Number of vertices
  vc <- length(V(g))
  # Name them
  V(g)$name <- V(g)
  ## Assign beliefs: uniform draws from 1 to 10 for each shape parameter
  V(g)$s1[1:vc] <- round(runif(vc,1,10))
  V(g)$s2[1:vc] <- round(runif(vc,1,10))
  betashape <- data.frame(V(g)$s1, V(g)$s2)
  ## Express behavior based on authentic beliefs
  V(g)$express[1:vc] <-  mapply(function(x,y) qbeta(.5, x, y), betashape[,1], betashape[,2])
  # Set happiness
  V(g)$happiness <- 1
  V(g)$name = NA
  # Iterate for sims = runs with various parameter settings 
  gm[[it]] <- ssts(g, sims = runs, migration = TRUE, fractionToSwap = .01) 
  gs2[[it]] <- ssts(g, sims = runs, CYF=TRUE, S=2, fractionToSwap = .01)
  gs4[[it]] <- ssts(g, sims = runs, CYF=TRUE, S=4, fractionToSwap = .01)
  gs6[[it]] <- ssts(g, sims = runs, CYF=TRUE, S=6, fractionToSwap = .01)
  #print(paste("completed", it, "out of", sims))
}
end_time <- Sys.time()
##end_time - start_time

#save.image(file="16.Rdata")

#load(file="16.Rdata")

# Run basic without migration or network rewiring
gbasic <- list()
for(i in 1:1){
  gbasic[[i]] <- ssts(g, sims = 20)
}


# Find mean value of happiness for each repetition of simulation at end-state
hapgbasic <- lapply(gbasic, function(x) mean(V(x)$happiness))
hapgm <- lapply(gm, function(x) mean(V(x)$happiness))
hapgs2 <- lapply(gs2, function(x) mean(V(x)$happiness))
hapgs4 <- lapply(gs4, function(x) mean(V(x)$happiness))
hapgs6 <- lapply(gs6, function(x) mean(V(x)$happiness))

# Find mean of means
mb <- mean(unlist(hapgbasic))
mm <- mean(unlist(hapgm))
m2 <- mean(unlist(hapgs2))
m4 <- mean(unlist(hapgs4))
m6 <- mean(unlist(hapgs6))

# Find sd
sdmb <- sd(unlist(hapgbasic))
sdmm <- sd(unlist(hapgm))
sdm2 <- sd(unlist(hapgs2))
sdm4 <- sd(unlist(hapgs4))
sdm6 <- sd(unlist(hapgs6))

# Do same for expressed behaviors
expgb <- lapply(gbasic, function(x) sd(V(x)$express))
expgm <- lapply(gm, function(x) sd(V(x)$express))
expgs2 <- lapply(gs2, function(x) sd(V(x)$express))
expgs4 <- lapply(gs4, function(x) sd(V(x)$express))
expgs6 <- lapply(gs6, function(x) sd(V(x)$express))

emb <- mean(unlist(expgb))
em <- mean(unlist(expgm))
e2 <- mean(unlist(expgs2))
e4 <- mean(unlist(expgs4))
e6 <- mean(unlist(expgs6))

sdemb <- sd(unlist(expgb))
sdem <- sd(unlist(expgm))
sde2 <- sd(unlist(expgs2))
sde4 <- sd(unlist(expgs4))
sde6 <- sd(unlist(expgs6))

# Find number of components
compgb <- lapply(gbasic, function(x) igraph::components(x)$no)
compgm <- lapply(gm, function(x) igraph::components(x)$no)
compgs2 <- lapply(gs2, function(x) igraph::components(x)$no)
compgs4 <- lapply(gs4, function(x) igraph::components(x)$no)
compgs6 <- lapply(gs6, function(x) igraph::components(x)$no)

cmb <- mean(unlist(compgb))
cm <- mean(unlist(compgm))
c2 <- mean(unlist(compgs2))
c4 <- mean(unlist(compgs4))
c6 <- mean(unlist(compgs6))

sdcmb <- sd(unlist(compgb))
sdcm <- sd(unlist(compgm))
sdc2 <- sd(unlist(compgs2))
sdc4 <- sd(unlist(compgs4))
sdc6 <- sd(unlist(compgs6))


## Perceived normality function
perNormality <- function(g){
  # Get all degrees
  ks <- igraph::degree(g)
  # Adjacency matrix
  amat <- as_adj(g, sparse=FALSE)
  # Normality list buffer
  normality <- rep(NA, length(V(g))) 
  # For each node 
  for(i in 1:length(V(g))){ 
    # Get neighbors
    neibs <- which(amat[i,]==1)
    # Get rank 
    r <- rank(V(g)$express[c(neibs,i)])[ks[i]+1] 
    # Compute normality
    normality[i] <- 1 - abs(1-2*(r-1)/ks[i])
  } 
  return(normality)
}

  # Compute values for normality for each network
  gbasic_pn <- mean(unlist(lapply(gbasic, function(x) mean(perNormality(x)))))
  
  gm_mpn <- mean(unlist(lapply(gm, function(x) mean(perNormality(x)))))
  gm_sdpn <- sd(unlist(lapply(gm, function(x) mean(perNormality(x)))))

  gs2_mpn <- mean(unlist(lapply(gs2, function(x) mean(perNormality(x)))))
  gs2_sdpn <- sd(unlist(lapply(gs2, function(x) mean(perNormality(x))))) 

  gs4_mpn <- mean(unlist(lapply(gs4, function(x) mean(perNormality(x), na.rm=T))))
  gs4_sdpn <- sd(unlist(lapply(gs4, function(x) mean(perNormality(x), na.rm=T)))) 
  
  gs6_mpn <- mean(unlist(lapply(gs6, function(x) mean(perNormality(x)))))
  gs6_sdpn <- sd(unlist(lapply(gs6, function(x) mean(perNormality(x))))) 
  # Save as vector 
  PerceivedNormality <- c(gbasic_pn, gm_mpn, gs2_mpn, gs4_mpn, gs6_mpn) 
  sdPerceivedNormality <- c(0, gm_sdpn, gs2_sdpn, gs4_sdpn, gs6_sdpn) 
 
  # Save as data frame 
  dat <- data.frame("Depth"=c("No movement", "Schelling migration", 2,4,6), "Utility"=c(mb, mm, m2, m4, m6), "SD"=c(0, sdmm, sdm2, sdm4,sdm6), "Variance"=c(emb, em, e2,e4,e6), "SD"=c(0, sdem, sde2, sde4, sde6), "Components"=c(cmb, cm, c2,c4, c6), "SD"=c(0, sdcm, sdc2,sdc4,sdc6), "Normality"=PerceivedNormality, "SD"=sdPerceivedNormality)

# Make table
kable(dat,row.names=FALSE, booktabs = T, escape = FALSE, caption = "Impact of search on social dynamics. The table shows the impact of search depth on various aspects of belief (utility, variance, and normality) and network structure (components). Results show the average of 50 network simulations after 200 iterations each. Schelling migration picks two individuals at random as in Figure 6.", digits=3) %>%
kable_classic(full_width = F, html_font = "Cambria")


#### Figure 16-8 ####

# Torus function
torusR <- function(n, phi = 0, L = 0){
  # Make n x n lattice
  g = graph.lattice(c(n,n)) # create a square lattice (nxn)
  # Make sequence from 1 to n for columns
  v1 = seq(from=1,to=n,by=1)
  # Make sequence for first item in each row 1..11..21 (if n=10)
  v2 = seq(from=1, to = n^2-n+1, by = n)
  # Make sequence of top row nodes 
  v1.1 = seq(from = n^2-n+1, to = n^2, by = 1)
  # Make sequence of last item in each row
  v2.1 = seq(from = n,to = n^2,by = n)
  # Put them all together so the arrangment is two nodes that need an edge
  a = cbind(rbind(v1,v1.1), rbind(v2,v2.1))
  # Make a vector for edge creation
  a2 = matrix(a,nrow=length(a),ncol = 1)
  # Add the edges to sew the whole thing together -- this creates a torus
  g = igraph::add.edges(g,a2)
  
  if(L==0){ # If L=0, this could be any node
    # Number of edges
    edge_count <- length(E(g))
    # Number of new edges to make as proportion of existing edges 
    new_edges <- phi*edge_count
    # Vector for nodes
    x <- 1:length(V(g))
    # Sample as many nodes (x2) as will receive new edges
    newEdgeList<- sample(x, 2*new_edges,replace=TRUE)
    # Add edges -- some self-loops possible
    g <- igraph::add.edges(g, newEdgeList) 
  }else{
    # As above
    edge_count <- length(E(g))
    new_edges <- phi*edge_count
    # Possible edges are at a fixed distance (L)
    possible_edges<-data.frame(which(distances(g) == L, arr.ind = TRUE)) # Gives row and col vertices for distances. Sample from this and form edges.
    # Reduce to bottom diagonal of matrix
    possible_edges <- subset(possible_edges, row < col)
    # Make sure it's possible  
    if(nrow(possible_edges) < new_edges) {
     stop("New edges required is more than possible edges. To resolve this, lower phi.")
    }
    # Set random number seed
    set.seed(1)
    # Get new edge list
    newEdgeList <- possible_edges[sample(nrow(possible_edges),new_edges),]
    # Make edge list
    elist <- as.vector(t(as.matrix(newEdgeList))) # take transpose here to get the order right
    # Add edges
    g <- igraph::add.edges(g, elist) 
  }
  return(g)
}

####

# Make torus using function above
tor1 <- torusR(10) # n x n, here n = 10
tor2 <- torusR(10, phi = .2, L = 2) # n x n, here n = 10
tor3 <- torusR(10, phi = .2, L = 3) # n x n, here n = 10

# Get means
mt1 <- mean(distances(tor1))
mt2 <- mean(distances(tor2))
mt3 <- mean(distances(tor3))

# Compute transitivity
tr1 <- igraph::transitivity(tor1)
tr2 <- round(igraph::transitivity(tor2), 2)
tr3 <- igraph::transitivity(tor3)

## Plot torus
par(mfrow=c(1,3))
set.seed(3)

# Fix layout for all
l = layout_nicely(tor1, dim = 3)
# Plot torus
plot(tor1,vertex.size = 1,vertex.label = NA, layout=l, vertex.color = "black")
# Add text
text(0,-1.2, "Torus")
text(0, -1.4, paste("<L>=", toString(mt1)))
text(0, -1.6, paste("CC=", toString(tr1)))
plot(tor2,vertex.size = 1,vertex.label = NA, layout=l, vertex.color = "black", edge.color = c(rep("gray80", length(E(tor1))), rep("black", length(E(tor2))-length(E(tor1)))))
text(0,-1.2, "M=2 shortcuts")
text(0, -1.4, paste("<L>=", toString(mt2)))
text(0, -1.6, paste("CC=", toString(tr2)))
plot(tor3,vertex.size = 1,vertex.label = NA, layout=l, vertex.color = "black", edge.color = c(rep("gray80", length(E(tor1))), rep("black", length(E(tor2))-length(E(tor1)))))
text(0,-1.2, "M=3 shortcuts")
text(0, -1.4, paste("<L>=", toString(mt3)))
text(0, -1.6, paste("CC=", toString(tr3)))

