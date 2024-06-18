
### Behavioral Network Science
### Language, Mind, and Society
# By Thomas T. Hills

# Code to reproduce figures and analyses for

#### Chapter 12 -- Age-Related Cognitive Decline: A Network Enrichment Account ####

# Age-Related Cognitive Decline: A Network Enrichment Account

knitr::opts_chunk$set(echo = FALSE)
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(comment = NA)
knitr::opts_chunk$set(warning = FALSE)
options(knitr.table.format = "latex")

rm(list=ls())
 library(DiagrammeR)
 library(igraph)
 library(tidyverse)
 library(kableExtra)
 library(Rmisc)
 library(network)


#### Figure 12-1 ####

# Plot parameters
par(mfrow=c(1,1))
par(mar=c(5,5,2,2))
# Scales
x <- 20:69
y <- seq(1,100, 2)
y2 <- y[length(y):1]
# Plot lines
plot(x, y, type="l", ylab = "Relative score", xlab="Age (years)", cex.lab = 1.5, lwd = 3)
lines(x,y2, lwd = 2, lty=2)
legend(56, 60, legend=c("Crystallized", "Fluid"), title = "Intelligence", lty=1:2, lwd=3, cex = .8)



#### Figure 12-1 ####

# Read in data for lexical neteworks
r10 <- as.matrix(read.csv('SampleDataFilesBNS/Aging/age10.txt', header=F))
r11 <- as.matrix(read.csv('SampleDataFilesBNS/Aging/age11.txt', header=F))
r14 <- as.matrix(read.csv('SampleDataFilesBNS/Aging/age14.txt', header=F))
r18 <- as.matrix(read.csv('SampleDataFilesBNS/Aging/age18.txt', header=F))
r30 <- as.matrix(read.csv('SampleDataFilesBNS/Aging/age30.txt', header=F))
r40 <- as.matrix(read.csv('SampleDataFilesBNS/Aging/age40.txt', header=F))
r50 <- as.matrix(read.csv('SampleDataFilesBNS/Aging/age50.txt', header=F))
r60 <- as.matrix(read.csv('SampleDataFilesBNS/Aging/age60.txt', header=F))
r70 <- as.matrix(read.csv('SampleDataFilesBNS/Aging/age70.txt', header=F))



# Threshold matrices
r10t <- matrix(as.numeric(r10>5),nrow=dim(r10)[1]) 
r18t <- matrix(as.numeric(r18>5),nrow=dim(r18)[1]) 
r30t <- matrix(as.numeric(r30>5),nrow=dim(r30)[1]) 
r40t <- matrix(as.numeric(r40>5),nrow=dim(r40)[1]) 
r50t <- matrix(as.numeric(r50>5),nrow=dim(r50)[1]) 
r60t <- matrix(as.numeric(r60>5),nrow=dim(r60)[1]) 
r70t <- matrix(as.numeric(r70>5),nrow=dim(r70)[1]) 

#Plot parameters
par(mfrow=c(2,3))
par(mar=c(1,1,2,2))
# Plots
plot(as.network(r18t, directed=FALSE), mode="kamadakawai", main = "20", cex.main = 1.5, vertex.cex = .2)
plot(as.network(r30t, directed=FALSE), mode="kamadakawai", main = "30", cex.main = 1.5, vertex.cex = .2)
plot(as.network(r40t, directed=FALSE), mode="kamadakawai", main = "40", cex.main = 1.5, vertex.cex = .2)
plot(as.network(r50t, directed=FALSE), mode="kamadakawai", main = "50", cex.main = 1.5, vertex.cex = .2)
plot(as.network(r60t, directed=FALSE), mode="kamadakawai", main = "60", cex.main = 1.5, vertex.cex = .2)
plot(as.network(r70t, directed=FALSE), mode="kamadakawai", main = "70", cex.main = 1.5, vertex.cex = .2)





#### Figure 12-3 ####

# Make layout for multi-panel plot
layout(matrix(c(1,1,2,2,1,1,3,3), 2, 4, byrow = TRUE))
# Read in data file of similarity ratings
wd <- read.csv(file="SampleDataFilesBNS/Aging/WulffHillsMata2023Structural_simimilarity_ratings.csv", header = TRUE)
# Make tibble
wd <- tibble(wd)
# Add meansimilarity
wdsidmeds <- wd %>% dplyr::group_by(group, id) %>% dplyr::summarize(meansim = median(norm_rating))
# Compute summary statistics
sumse <- summarySE(wdsidmeds, measurevar = "meansim", groupvars = "group")
# Plot parameters
par(mar=c(5,5,2,2))
# Barplot
x <- barplot(sumse$meansim, col = "white", ylab = "Median similarity", xlab = "Age group", names=c("Old", "Young"), cex.lab = 1.42, ylim =c(0, .25))
# Error bars
arrows(x, sumse$meansim + sumse$se, x, sumse$meansim-sumse$se, code = 3, angle = 90, lwd = 2)

# Stats
##with(wdsidmeds, t.test(meansim~group))

### Representative aggregate plot for each group

# Separate groups
wdold <- wd %>% filter(group=='old')
wdyoung <- wd %>% filter(group=='young')

# Filter out words and ratings
wdo <- wdold %>% dplyr::select(left_word, right_word, weight=norm_rating)
wyo <- wdyoung %>% dplyr::select(left_word, right_word, weight=norm_rating)

# Make graphs and simplify
wgo <- graph_from_data_frame(wdo)
wgo <- igraph::simplify(wgo, edge.attr.comb="median")

wyo <- graph_from_data_frame(wyo)
wyo <- igraph::simplify(wyo, edge.attr.comb="median")

# Compute mean values
##mean(E(wgo)$weight)
##mean(E(wyo)$weight)

# Plot parameters
par(mar=c(2,2,2,2))
# Set layout
l = layout_in_circle(wgo)
# Plot and label
plot(wyo, vertex.size = 1, edge.width = E(wyo)$weight*2, layout=l, edge.arrow.size=0, edge.color = alpha("black", alpha = E(wyo)$weight/2), vertex.label = NA, main = "Young")
plot(wgo, vertex.size = 1, edge.width = E(wgo)$weight*2, layout=l, edge.arrow.size=0, edge.color = alpha("black", alpha = E(wgo)$weight/2), vertex.label = NA, main = "Old")




#### Figure 12-4 ####

## Rescorla Wagner function, take value matrix, cues and outcomes
## Returns value matrix
rescorlaWagner <- function(vmat = vmat, cue, outcome, alpha=1, beta=.2) {
  # Cue can be compound: e.g., cue = c("A", "B")
  # Outcome can be compound: e.g., outcome = c("A", "B")
  cuevalues <- matrix(vmat[cue, ], nrow = length(cue))
  valueSums <- apply(cuevalues,2,sum )     # cumulative cue value
  # Make Lambda = 1 when present 0 when absent
  lambda = as.numeric(rownames(vmat) %in% outcome)
  pError <- lambda - valueSums     # prediction error = observed - expected
  valueChange <- alpha * beta * pError # value change 
  vmat[cue, ] <- t(t(vmat[cue, ]) + valueChange)  # update value
  return(vmat)
}

# Set plot parameters
par(mfrow=c(2,2))
## Basic classical conditioning
## number of cues and outcomes
n = 2
## initialize zero value matrix
vmat <- matrix(0, nrow = n, ncol = n)
rownames(vmat) <- LETTERS[1:n] 
colnames(vmat) <- LETTERS[1:n] 
# Set learning trials
trials = 10
tdata <- data.frame(cue="A", outcome = "B")
## Repeat rows for number of trials to represent repeated learning trails
tdata <- tdata[rep(1, 10),]
# Feed learning trails in RescorlaWagner function, keep track of matrix output
vmatAB <- rep(NA, trials)
for(i in 1:nrow(tdata)){
  vmat <- rescorlaWagner(vmat, cue=tdata$cue[i], outcome=tdata$outcome[i]) 
  vmatAB[i] <- vmat["A", "B"] 
  
}
plot(1:trials, vmatAB, type="b", xlab = "Trial", ylab="Association strength", main = "Associative learning")


## Blocking
## Number of cues and outcomes
n = 3
## Initialize zero value matrix
vmat <- matrix(0, nrow = n, ncol = n)
rownames(vmat) <- LETTERS[1:n] 
colnames(vmat) <- LETTERS[1:n] 
trials = 20
tdata <- data.frame(cue=c("A","AB"), outcome = "C")
## Set up training data: Repeat rows for number of trials to produce blocking paradigm
tdata <- tdata[rep(seq_len(nrow(tdata)), each=10),]
# Set up data buffers
vmatAC <- rep(NA, trials)
vmatBC <- rep(NA, trials)
# Feed learning trails into Rescorla Wagner
for(i in 1:nrow(tdata)){
  vmat <- rescorlaWagner(vmat, cue=strsplit(tdata$cue[i],split="")[[1]], outcome=tdata$outcome[i]) 
  vmatAC[i] <- vmat["A", "C"] 
  vmatBC[i] <- vmat["B", "C"] 
  
}
plot(1:trials, vmatAC, type="b", xlab = "Trial", ylab="Association strength", ylim =c(0, 1), main = "Blocking")
points(1:trials,vmatBC, type = "b", pch = 16)


##  Extinction
## Number of cues and outcomes
n = 2
## initialize zero value matrix
vmat <- matrix(0, nrow = n, ncol = n)
rownames(vmat) <- LETTERS[1:n] 
colnames(vmat) <- LETTERS[1:n] 
trials = 20
# Set up training data for Extinction
tdata <- data.frame(cue=c("A"), outcome = c("B", ""))
## Repeat row for number of trials
tdata <- tdata[rep(seq_len(nrow(tdata)), each=10),]
# Data buffer
vmatAB <- rep(NA, trials)
# Feed training data to Rescorla Wagner function
for(i in 1:nrow(tdata)){
  vmat <- rescorlaWagner(vmat, cue=strsplit(tdata$cue[i],split="")[[1]], outcome=tdata$outcome[i]) 
  vmatAB[i] <- vmat["A", "B"] 
}
plot(1:trials, vmatAB, type="b", xlab = "Trial", ylab="Association strength", ylim =c(0, 1), main = "Extinction")

##  Inhibition
## Number of cues and outcomes
n = 3
## Initialize zero value matrix
vmat <- matrix(0, nrow = n, ncol = n)
rownames(vmat) <- LETTERS[1:n] 
colnames(vmat) <- LETTERS[1:n] 
trials = 20
tdata <- data.frame(cue=c("A","AB"), outcome = c("C", ""))
## Repeat rows for number of trials
tdata <- tdata[rep(seq_len(nrow(tdata)), each=10),]
# Data buffers
vmatAC <- rep(NA, trials)
vmatBC <- rep(NA, trials)
# Feed training data to Rescorla Wagner function
for(i in 1:nrow(tdata)){
  vmat <- rescorlaWagner(vmat, cue=strsplit(tdata$cue[i],split="")[[1]], outcome=tdata$outcome[i]) 
  vmatAC[i] <- vmat["A", "C"] 
  vmatBC[i] <- vmat["B", "C"] 
  
}
plot(1:trials, vmatAC, type="b", xlab = "Trial", ylab="Association strength", ylim =c(-1, 1), main = "Inhibition")
points(1:trials,vmatBC, type = "b", pch = 16)



#### Figure 12-5 ####

# Replicating Ramscar et al. data

# Set random number generator seed
set.seed(1)
## Make training data 
tdata <- data.frame(cue=c("C0_jury", # jury
                          "C0_baby", # baby 
                          "C0_jury", 
                          "C0_baby", 
                          "C0_jury", 
                          "C0_baby",
                          "C0_elephant"), 
                    outcome = c("eagle", 
                                "summons", 
                                "duty", 
                                "cries",
                                "summons", 
                                "eagle",
                                "baby"),
                    times = c(0, 0, 40, 60, 80, 60, 0)) 
## Repeat row for number of trials
tdata_long <- tdata[rep(seq_len(nrow(tdata)), times=tdata$times),1:2]
## Randomize training data
tdata_long<- tdata_long[sample(seq_len(nrow(tdata_long)), nrow(tdata_long)),]
# Rename
tdataf <- tdata_long
## Build value matrix
cues_all <- unique(unlist(strsplit(tdata$cue, split ="_")))
targets_all <- unique(unlist(strsplit(tdata$outcome, split="_")))
cuesPlusTargets <- unique(c(cues_all, targets_all))
n = length(cuesPlusTargets)
## Initialize zero value matrix
vmat <- matrix(0, nrow = n, ncol = n)
rownames(vmat) <- cuesPlusTargets
colnames(vmat) <- cuesPlusTargets

# Train matrix on training pairs
vmat_learning <- array(0,c(n,n,nrow(tdataf)) )
for(i in 1:nrow(tdataf)){
  vmat <- rescorlaWagner(vmat, cue=strsplit(tdataf$cue[i],split="_")[[1]], outcome=tdataf$outcome[i], beta = .02) 
  vmat_learning[,,i] <- vmat
}
#
x = 1:nrow(tdataf)
# Extract out the learning time series for each pair
values_baby_eagle <- vmat_learning[which(colnames(vmat)=="baby"), which(colnames(vmat)=="eagle"),]
values_baby_cries <- vmat_learning[which(colnames(vmat)=="baby"), which(colnames(vmat)=="cries"),]
values_baby_summons <- vmat_learning[which(colnames(vmat)=="baby"), which(colnames(vmat)=="summons"),]
values_jury_summons <- vmat_learning[which(colnames(vmat)=="jury"), which(colnames(vmat)=="summons"),]
values_jury_eagle<- vmat_learning[which(colnames(vmat)=="jury"), which(colnames(vmat)=="eagle"),]
values_jury_duty<- vmat_learning[which(colnames(vmat)=="jury"), which(colnames(vmat)=="duty"),]
# Plot parameters
par(mfrow=c(1,1))
# Plot each time series
plot(x, values_baby_summons, type="l", xlab = "Trial", ylab="Association strength", ylim =c(-.2, .45), main = "Paired-associate learning", lty = 1)
lines(x, values_jury_eagle, lty = 2)
lines(x, values_baby_cries, lty = 3)
lines(x, values_baby_eagle, lty = 4)
lines(x, values_jury_duty, lty = 5)
lines(x, values_jury_summons, lty = 6)
# Add legend
legend( 170, .12, legend=c("baby-summons", "jury-eagle", "baby-cries", "baby-eagle", "jury-duty", "jury-summons"), lty = c(1:6), cex = .8)


#### Figure 12-6 ####

# Use grViz to make box and arrows diagram
grViz(diagram = "digraph flowchart {
  graph[rankdir=LR]
  node [fontname = arial, shape = square, cex = 1, fixedsize=TRUE, width=2.3]
  tab1 [label = '@@1']
  tab2 [label = '@@2']
  tab3 [label = '@@3']
  
  tab1 -> tab2 -> tab3;
}
  
  [1]: 'Environment'
  [2]: 'Representation'    
  [3]: 'Behavior'    
  ")


#### Figure 12-7 ####

## Herdan's law

# Make x and y values
x <- seq(1000, 10000, 1000)
y <- 10*x^(.5)
# Plot parameters
par(mfrow=c(1,2))
par(mar=c(5,5,2,2))
# Plot
plot(x,y, xlab="Number of word tokens", ylab="Vocabulary size (word types)", type="b", xlim = c(500, 10000), ylim=c(300, 1000))
plot(x,y, log="xy", xlab="Number of word tokens", ylab="Vocabulary size (word types)", type="b", xlim = c(1000, 10000), ylim=c(300, 1000))



#### Figure 12-8 ####

## Let the language grow by a 1000 new learning events each year
## Let the number of new words follow Herdan's law (this isn't a requirement for the aging effect to occur)


## Build the 1000 word lexical network, 
## Then sample from a larger portion of the lexical network in each age group.

## This is the same as above, but here again for testing


rescorlaWagner <- function(vmat = vmat, cue, outcome, alpha=1, beta=.2) {
  # Cue can be compound: e.g., cue = c("A", "B")
  # Outcome can be compound: e.g., outcome = c("A", "B")
  cuevalues <- matrix(vmat[cue, ], nrow = length(cue))
  valueSums <- apply(cuevalues,2,sum )     # cumulative cue value
  # Make Lambda = 1 when present 0 when absent
  lambda = as.numeric(rownames(vmat) %in% outcome)
  pError <- lambda - valueSums     # prediction error = observed - expected
  valueChange <- alpha * beta * pError # value change 
  vmat[cue, ] <- t(t(vmat[cue, ]) + valueChange)  # update value
  return(vmat)
}

# Set random number seed
set.seed(3)
# Set parameters
wordsInWorld=1000
Associates = 2000
learningEvents <- 400 
ageEpochs = 4
x <- 1:wordsInWorld
a = 1
pairs <- c()
for(i in 1:Associates){
  # Create random associates: Rank based network
  pairs <- rbind(pairs, sample(x, size = 2, prob=x^(-a), replace = FALSE))
}

# Make graph
ii <- graph_from_edgelist(pairs,directed=FALSE) 

## Plot experience lexicon across ages
par(mfrow=c(2,ageEpochs))
par(mar=c(2,2,2,2))
# Create subgraphs and plot
for(agee in 1:ageEpochs){
  # Take subgraph with only y[pt] words
  ageWords <- round(wordsInWorld*y[agee]/y[length(y)])
  iis <- subgraph(ii, 1:ageWords) 
  # Plot subgraph
  plot(iis, vertex.size = 1, edge.arrow.size = 0, vertex.label = NA, layout = layout_with_fr(iis, dim=2, niter=30, start.temp = .5))
  # Label
  if(agee == 1){
    text(0, 1.5, "Experienced lexicon")
  }
  text(0,-1.5, paste("Age = ", agee))
}

# Assign number of cues plus context for learning matrix
n = length(V(ii))+1 # number of cues + context
## initialize zero value matrix
vmat <- matrix(0, nrow = n, ncol = n)
rownames(vmat) <- 1:n
colnames(vmat) <- 1:n

# Data buffers
edgeE <- rep(NA, ageEpochs)
nodeCount <- rep(NA, ageEpochs)
ageNetworkList <- list(NULL)
# Plot parameters
par(mar=c(2,2,2,2))
# Learn over epochs
for(lage in 1:ageEpochs){
  # Take subgraph with only y[pt] words
  ageWords <- round(wordsInWorld*y[lage]/y[length(y)])
  iis <- subgraph(ii, 1:ageWords) 
  # Make training data set
  traindata <- c()
  for(i in 1:learningEvents){
  # Samle edges for learning events
   traindata <- rbind(traindata, cue_outcome <- ends(iis, sample(E(iis), 1))) 
  }
  # Keep track of firsttraindata
  if(lage == 1){
    firsttraindata <- traindata
  }
  # Update vmat across learning events with RescorlaWagner
  for(i in 1:learningEvents){
    #grab random edge
    cue_outcome <- traindata[i,]
    vmat <- rescorlaWagner(vmat, cue=c(n, cue_outcome[1]), outcome=cue_outcome[2], beta = .02) 
  }
  
  # Make graph
  gle <- graph_from_adjacency_matrix(vmat, weighted=TRUE, diag=FALSE, mode = "undirected")
  # Delete context
  gle <- igraph::delete_vertices(gle, n)
  # Take subgraph
  gle <- igraph::subgraph(gle, 1:ageWords)
  # Get node count
  nodeCount[lage] <- length(V(gle))
  # Remove negative edges
  negEdges <- which(E(gle)$weight < 0)
  gle <- igraph::delete_edges(gle, negEdges)
  # Save network
  ageNetworkList[[lage]] <- gle
  # Rename
  g2 <- gle
  # Get edge weights 
  weightMatrix <- as_adjacency_matrix(g2, attr="weight", sparse=FALSE)
  # Make them all positive (remove negative weights)
  weightMatrix[weightMatrix < 0] <- 0
  # Normalize
  ww<-weightMatrix/rowSums(weightMatrix, na.rm=TRUE)
  # Entropy function
  entropy <- function(p) rowSums(-(p * log(p)), na.rm = TRUE)
  # Compute entropy
  node_entropy <- entropy(ww) 
  # Get mean entropy
  #edgeE[lage] <- median(node_entropy)
  edgeE[lage] <- mean(node_entropy)
  # Remove small values for plotting purposes
  g2 <- igraph::delete.edges(gle, which(E(gle)$weight <=0.0)) # very small values push this graph apart
  # Set edge weights 1
  E(g2)$weight <- 1
  # Plot
  plot(g2, vertex.size = 1, edge.arrow.size = 0, vertex.label=NA, layout=layout_with_fr(g2, dim=2, niter=30, start.temp = .5))
  if(lage == 1){
    text(0, 1.5, "Learned lexicon")
  }
  text(0, -1.5, paste("t =",lage,"000"))
  
  # Below is a set of tests I used to investigate negative inhibition:
  # Understanding negative inhibition
  # to check specific edge weights from node '4' for example:
  # nodei = 4
  # p = data.frame(from =nodei, to=as.vector(neighbors(gle, nodei)))
  # E(gle, as.vector(t(p)))$weight
  # neighbors(iis, 4)
  # sum(E(gle, as.vector(t(p)))$weight > 0) # only 8 are positive
  # neighbors(g2, 4) # same for g2 
  # which(degree(g2)==max(degree(g2))) # node with max degree
  # neighbors(g2, 38)
  # neighbors(iis, 38)
  # This seems wrong at first glance: there are four instances where 38 is the target from node 10
  # but 38 has 14 positive edgeweights with other neighbors (only 1 of which is 10)
  # vmat[,38] # this 14 positive is reflected in vmat
  # which(vmat[,38]>0) 
  # None of the other positive vmat cells with 38 are in the traindata with 38.
  # This is spillover from the context cue.
  # Here is the analogy: Suppose the context, X, becomes negatively associated with G
  # If B appears with X and the absence of G, the delta Value will be positive
  # That's because X is predicting a negative value, but the true value is only 0
  # To correct the negative X, the delta Value is positive, but since B is present
  # B also picks up the positive delta (things weren't as negative as predicted)
  
}







#### Figure 12-9 ####

# Plot parameters
par(mfrow=c(1,2))
par(mar=c(5,5,2,2))
x <- 1:ageEpochs
# Plot median node entropy from above
plot(x, edgeE[1:ageEpochs], xlab = "Age", ylab = "Entropy", type = "b", cex.lab = 1.4)

# Set random number seed
set.seed(5)
## Choose random pairs from first training data (firstttraindata)
learningPairs = 20 # This number can be bigger
simpair <- firsttraindata[sample(seq(1:nrow(firsttraindata)), learningPairs ),]
# Make data frame with activation for spreadr 100
simpair <- data.frame(simpair, activation = 100)
# Label data frame
names(simpair) <- c("node", "node", "activation")
## Create data store for similarity judgements
simJudge <- c()
## For each age networks
for(sage in 1:length(ageNetworkList)){
## Initiate activation at each node and measure activation at the other 
  for(testrow in 1:nrow(simpair)){
      # Initiate activation at the cue
      df1 <- spreadr::spreadr(start_run = simpair[testrow,c(1,3)], decay = 0,
                                    retention = 0, suppress = 0,
                                    network = ageNetworkList[[sage]], time = 10)
      # Measure max activation at the target node
      maxActivation12 <- max(subset(df1, node == simpair[testrow,2])$activation)
      # Initiate activation at the new cue (the previous target node)
      df2 <- spreadr::spreadr(start_run = simpair[testrow,c(2,3)], decay = 0,
                                    retention = 0, suppress = 0,
                                    network = ageNetworkList[[sage]], time = 10)
      # Measure max activation at the new target node
      maxActivation21 <- max(subset(df2, node == simpair[testrow,1])$activation)
      # Sum the activations for the similarity judgment
      simval <- maxActivation12 + maxActivation21
      # Add data to output table
      simJudge <- rbind(simJudge, c(simpair[testrow,1],simpair[testrow,2], simval, sage))
  }
## Take average co-activation for all node pairs in each ageNetworkList
}
simJudge <- data.frame(simJudge)
# Label
names(simJudge) <- c("node1", "node2", "similarity", "age")
# Get summary stats  
sed <- summarySE(simJudge, measurevar="similarity", groupvars="age")
agex <- 1:ageEpochs
# Plot it
plot(agex, sed$similarity[1:ageEpochs], xlab = "Age", ylab = "Similarity", type="b", cex.lab = 1.3)


#### Figure 12-10 ####

knitr::include_graphics(c("images/Figure12-10.pdf"))

