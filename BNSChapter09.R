knitr::opts_chunk$set(echo = FALSE)
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(comment = NA)
knitr::opts_chunk$set(warning = FALSE)
options(knitr.table.format = "latex")


### Behavioral Network Science
### Language, Mind, and Society
# By Thomas T. Hills

# Code to reproduce figures and analyses for

#### Chapter 9 -- Agent-Based Models of Language Emergence: Structure Favors the Orangutan ####

rm(list=ls()) # to health!
library(igraph)
library(tidyverse)
library(igraphdata)
library(kableExtra)


#### Figure 9-1 ####

set.seed(1)
# figure parameters
par(mfrow=c(2,2))
par(mar=c(2,2,2,2))
# edgelist for dyads
x <- matrix(1:10,ncol = 2)
pa <- graph_from_edgelist(x, directed=FALSE)
# word names
namelist <- c("gol", "mip", "oog", "suv", "dim", "yyl", "brk", "hal", "pop", "zul")

c = 10
# label nodes with names
V(pa)$name[1:10] <- namelist[1:5]
plot(pa, vertex.label.dist = 3, vertex.color = "gray50", vertex.size = c, vertex.label.family="Helvetica")
# make star network for speaker first
st1 <- make_star(10, mode = "undirected")
plot(st1, vertex.label = namelist[1:9], vertex.label.dist = 3, vertex.color = c("gray80", rep("gray20", 9)), vertex.size = c, vertex.label.family="Helvetica",vertex.label.color = "black")
# make star network for speaker second (hearer first)
st1 <- make_star(10, mode = "undirected")
plot(st1, vertex.label = "gol", vertex.label.dist = 3, vertex.color = c("gray20", rep("gray80", 9)), vertex.size = c , vertex.label.family="Helvetica",vertex.label.color = "black")
# make fully connected with question marks for labels?
plot(make_full_graph(10), vertex.label.dist = 3, vertex.color = "gray50", vertex.size = c, vertex.label = "?", vertex.label.family="Helvetica",vertex.label.color = "black")



#### Figure 9-2 ####

rm(list=ls())
# networks as above
x <- matrix(1:10,ncol = 2)
pa <- graph_from_edgelist(x, directed=FALSE)
st1 <- make_star(10, mode = "undirected")
fg <- make_full_graph(10)
# list of network structures
netlist <- list(pa, st1, st1, fg)
# speaker first for for loop
speakerFirst <- c(0,0,1, 1)
# simulations
bigSims = 100
# how many runs-iterations each
runs = 100
# data buffers for output
uniqs <- matrix(NA, nrow = length(netlist), ncol=runs)
words <- matrix(NA, nrow = length(netlist), ncol=runs)
# for each network in netlist
for(neti in 1:4){
  # Naming Game
  wordsSims<- matrix(NA, ncol=runs, nrow=bigSims)
  unisSims<- matrix(NA, ncol=runs, nrow=bigSims)
  # for each simulation
  for(j in 1:bigSims){
    # use appropriate network in net list
    gi <- netlist[[neti]]
    # get adjacency matrix
    gim <- as_adj(gi, sparse= FALSE)
    # create memories for all agents -- initially empty
    memory_list <- vector(mode = "list", length = length(V(gi)))
    # for each run update as follows
    for(i in 1:runs){
      # hearer is sampled from network
      hearer <- sample(length(V(gi)), 1)
      # speaker is sampled from hearers neighbors
      possible_speakers <- which(gim[hearer,]==1)
      speaker <- possible_speakers[sample(length(possible_speakers), 1)]
      # if speaker first, then swap the above
      if(speakerFirst[neti]){
        buf <- hearer
        hearer <- speaker
        speaker <- buf
      }
      # if speaker memory is empty
      if(is_empty(memory_list[[speaker]]))  {
        # make up a word and add it to the memory list
        memory_list[[speaker]] <- round(runif(1, 0, 10)*100000)
      }
      # sample a word from the memory list 
      chosen_word <- memory_list[[speaker]][sample(length(memory_list[[speaker]]), 1)]
      # if chosen word is in hearers memory list
      if(chosen_word %in% memory_list[[hearer]]){
        # update memory list for both hearer and speaker
        memory_list[[hearer]] <- chosen_word
        memory_list[[speaker]] <- chosen_word # cuz success!
      } else {
        # only update for hearer
        memory_list[[hearer]] <- c(memory_list[[hearer]], chosen_word)
      }
      # data outputs
      # memory size
      wordsSims[j, i] <- sum(lengths(memory_list))
      # word types
      unisSims[j, i] <- length(unique(unlist(memory_list)))
    }
  }
  # summarize data outputs 
  uniqs[neti,] <- colMeans(unisSims)
  words[neti,] <- colMeans(wordsSims)/length(V(gi))
}

# figure parameters
par(mfrow=c(1,2))
par(mar = c(4,4,2,2))
plot(uniqs[1,], xlab = "Iterations", ylab = "Total word types", cex = 0, ylim = c(0, max(uniqs)+3))
for(i in 1:4) lines(uniqs[i,], lwd = 2, lty = i)
legend(20,10.4, legend = c("Dyads", "Star (hearer-first)", "Star (speaker-first)", "Full"), lty =1:4, cex = .7, bty="n")
plot(words[1,], xlab = "Iterations", ylab = "Average memory size", cex = 0, ylim = c(0, max(words)+.5))
for(i in 1:4) lines(words[i,], lwd = 2, lty = i)


#### Figure 9-3 ####

# Function to create networks
girvanNewman <- function(n, numGroups, ratio_clustering){
  # Probabilities for ingroup vs outgroup linkage
  po = 1/(ratio_clustering +1)
  pc = 1-po 
  # Make a graph from n nodes
  ggnc <- make_full_graph(n) # start with a full network to create all edges for evaluation
  # Label group members
  groupcol <- rep(1:numGroups, each = ceiling(n/numGroups)) 
  # Assign groups to nodes
  V(ggnc)$color <- sample(groupcol,n)
  # Dummy list 
  list_of_edges_to_delete <- c()
  # For all edges
  for(i in 1:length(E(ggnc))){
    # If two nodes are the in same group
    if(V(ggnc)$color[ends(ggnc,i)[,1]] == V(ggnc)$color[ends(ggnc,i)[,2]]){
      # Roll dice and see if they make an edge (if not, add to delete edge list)
      if(runif(1) <= 1-pc) list_of_edges_to_delete <- c(list_of_edges_to_delete, i)
    }
    # If in different groups, ...
    if(V(ggnc)$color[ends(ggnc,i)[,1]] != V(ggnc)$color[ends(ggnc,i)[,2]]){
      if(runif(1) <= 1-po) list_of_edges_to_delete <- c(list_of_edges_to_delete, i)
    }
  }
  # Remove edges
  #ggnc_empty <- ggnc %>% delete_edges(1:length(E(ggnc)))
  ggnc_final <- ggnc %>% delete_edges(list_of_edges_to_delete)
  return(ggnc_final)
}

# Use above function to make networks
set.seed(1)
par(mfrow=c(1,3))
n= 18
numGroups = 6
ratio_clustering = 1
gi <- girvanNewman(n, numGroups, ratio_clustering)
plot(gi, vertex.label = NA)
text(0,-1.5, "r = 1")
ratio_clustering = 10 
gi <- girvanNewman(n, numGroups, ratio_clustering)
plot(gi, vertex.label = NA)
text(0,-1.5, "r = 10")
ratio_clustering = 20 
gi <- girvanNewman(n, numGroups, ratio_clustering)
plot(gi, vertex.label = NA)
text(0,-1.5, "r = 20")



#### Figure 9-4 ####

# Same function as above without comments
girvanNewman <- function(n, numGroups, ratio_clustering){
  po = 1/(ratio_clustering +1)
  pc = 1-po 
  par(mfrow=c(1,2))
  ggnc <- make_full_graph(n)
  groupcol <- rep(1:numGroups, each = ceiling(n/numGroups)) 
  V(ggnc)$color <- sample(groupcol,n)
  
  list_of_edges_to_delete <- c()
  for(i in 1:length(E(ggnc))){
    if(V(ggnc)$color[ends(ggnc,i)[,1]] == V(ggnc)$color[ends(ggnc,i)[,2]]){
      if(runif(1) <= 1-pc) list_of_edges_to_delete <- c(list_of_edges_to_delete, i)
    }
    if(V(ggnc)$color[ends(ggnc,i)[,1]] != V(ggnc)$color[ends(ggnc,i)[,2]]){
      if(runif(1) <= 1-po) list_of_edges_to_delete <- c(list_of_edges_to_delete, i)
    }
  }
  ggnc_final <- ggnc %>% delete_edges(list_of_edges_to_delete)
  return(ggnc_final)
}

# Innovate and propagate model with Girvan Newman
set.seed(2)
netlist <- c(1,2,3, 1,2,3)
ratio_clustering = c(1,10, 20, 1, 10, 20) 
numGroups = 6
n = 18 
# All are speaker first
speakerFirst <- c(1,1,1, 1, 1, 1)
# Number of sims per network type
bigSims = 200
# Number of learning trials per sim
runs = 200 
# Probability of forgetting: we're making these folks a bit thick
pfor = rep(1/5, 6)
# word difficulty
word_dif <- c(1,1,1,2,2,2)
# Data holders
uniqs <- matrix(NA, nrow = length(netlist), ncol=runs)
easyw <- matrix(NA, nrow = length(netlist), ncol=runs)
hardw <- matrix(NA, nrow = length(netlist), ncol=runs)
learn_events <- matrix(NA, nrow = length(netlist), ncol=runs)
degreenet <- matrix(NA, nrow = length(netlist), ncol=bigSims)
ccnet <- matrix(NA, nrow = length(netlist), ncol=bigSims)
# Simulate each network type
for(neti in 1:length(netlist)){
  # Data holders for each network across runs
  unisSims<- matrix(NA, ncol=runs, nrow=bigSims)
  easyWords <- matrix(NA, ncol=runs, nrow=bigSims)
  hardWords <- matrix(NA, ncol=runs, nrow=bigSims)
  learne <- matrix(NA, ncol=runs, nrow=bigSims)
  # Forgetting probability: they're all the same here, but could be different
  pf = pfor[neti]
  # Run bigSim simulations
  for(j in 1:bigSims){
    # Generate networks 
    gi <- girvanNewman(n, numGroups, ratio_clustering[neti])
    # Net stats
    degreenet[neti,j] <- mean(igraph::degree(gi))
    ccnet[neti,j] <- mean(igraph::transitivity(gi, type="local"))
    # Get adjacency matrix
    degreenet[neti,j] <- mean(igraph::degree(gi))
    gim <- as_adj(gi, sparse= FALSE)
    # Initialize Chinese restaurant memory for each agent
    memory_list <- vector(mode = "list", length = length(V(gi)))
    # Initialize innovation list for each agent
    innovation_list <- vector(mode = "list", length = length(V(gi)))
    # Begin learning trials (== runs)
    for(i in 1:runs){
        # Sample hearer
        hearer <- sample(length(V(gi)), 1)
        # Get possible speakers from hearer row of adjacency matrix
        possible_speakers <- which(gim[hearer,]==1)
        # Sample from possible speakers
        speaker <- possible_speakers[sample(length(possible_speakers), 1)]
        # If direct (speaker first), swap these
        if(speakerFirst[neti]){
         buf <- hearer
         hearer <- speaker
         speaker <- buf
        }
        if(is_empty(hearer) || is_empty(speaker)){ # isolates talk to themselves, but no learning will happen
          hearer <- speaker
        } 

          # Build speaker repertoire
          # Forget items from speaker's memory list
          memory_remains <- memory_list[[speaker]][runif(length(memory_list[[speaker]])) > pf]
          if(is_empty(memory_remains)){
            memory_list[speaker] <- list(NULL)
          } else {
            memory_list[[speaker]] <- memory_remains
          }
          # Build choice table from memory list
          choice_table <-  table(memory_list[[speaker]])
          # Get names from memory list (hence, difficulty)
          sample_list <- names(choice_table)
          # Add all words to wordsICanProduce if last digit of word (indicating times needed to  appear in choice table (exposure)) is below or equal to count in choice table, plus add innovation_list
          wordsICanProduce <- c(sample_list[as.numeric(substr(sample_list, nchar(sample_list), nchar(sample_list))) <= choice_table], innovation_list[[speaker]])
          # Final choice table is table of wordsICanProduce
          final_choice_table <- choice_table[names(choice_table) %in% wordsICanProduce]
          # Does speaker innovate, if yes, add word to memory list
        innovate <- runif(1) <= 1/(sum(final_choice_table)+1)
        if(innovate){
          ## Last digit of new word indicates difficulty
          mean_dif = word_dif[neti] 
          newword <- paste(round((runif(1, 0, 1)+1)*100000), round(rnorm(1,mean_dif, sd=0)),sep="") 
          # Add word to speaker memory list
          memory_list[[speaker]] <- c(newword, memory_list[[speaker]])
          # Add word to agent's innovation list so it is always in wordsICanProduce list 
          innovation_list[[speaker]] <- c(newword, innovation_list[[speaker]]) 
          # Add word to hearer memory list
          memory_list[[hearer]] <- c(newword, memory_list[[hearer]])
        } else {
          # Sample word from choice table based on times they appear in choice table
          chosen_word <- sample(names(final_choice_table),1, prob = final_choice_table)
          # Both speaker and hearer add chosen_word to memory_list
          memory_list[[hearer]] <- c(chosen_word, memory_list[[hearer]])
          memory_list[[speaker]] <- c(chosen_word, memory_list[[speaker]])
        }
      # After each learning event compute following
      # Statistics for words that are learned, i.e., in use (unlearned innovations don't count)
      heard_words <- mapply(function(x,y) unique(x)[!(unique(x)) %in% y], memory_list, innovation_list)
      # Are they learned?
      wordsILearned <- vector(mode = "list", length = length(V(gi)))
      for(ind in 1:n){
          # Build choice table from memory list
          choice_table <-  table(memory_list[[ind]])
          # Reduce choice table to words in heard words--to remove personal innovations
          choice_table <- choice_table[names(choice_table) %in% heard_words[[ind]]]
          # Get names from memory list (hence, difficulty)
          sample_list <- names(choice_table)
          # Add all words to wordsILearned if last digit of word (indicating times needed to  appear in choice table) is below or equal to count in choice table 
          wordsILearned[ind] <- list(c(sample_list[as.numeric(substr(sample_list, nchar(sample_list), nchar(sample_list))) <= choice_table]))
      }
      # Create list of unique words (types) that have been learned
      wordTypes <-   unique(unlist(wordsILearned))
      # How many types 
      unisSims[j, i] <- length(wordTypes)
      # How many word tokens have been learned
      learne[j, i] <- length(unlist(wordsILearned))/n
      # How many of the word types are easy or hard
      easyWords[j, i] <- sum(as.numeric(substr(wordTypes, nchar(wordTypes), nchar(wordTypes)))<mean_dif)
      hardWords[j, i] <- sum(as.numeric(substr(wordTypes, nchar(wordTypes), nchar(wordTypes)))>=mean_dif)
      
    }
  }
  
  # Summarize output data for plotting
  uniqs[neti,] <- colMeans(unisSims)
  easyw[neti,] <- colMeans(easyWords)
  hardw[neti,] <- colMeans(hardWords)
  learn_events[neti,] <- colMeans(learne)
}
  # Plotting parameters
  par(mfrow=c(1,2))
  par(mar = c(4,4,2,2))
  ltytype = c(1,2,3,1,2,3)
  xlimit =  runs
    
  # Total learning events
    cols = c(rep("black",3), rep("gray60",3))
    plot(uniqs[1,1:xlimit], xlab = "Runs", ylab = "Words types in population", cex = 0, ylim = c(0, max(uniqs[,1:xlimit])+3))
  for(i in 1:length(netlist)) lines(uniqs[i,1:xlimit], lwd = 2,  lty = ltytype[i], col = cols[i])
  legend(100,20, legend = c("1", "10", "20"), lty =1:3, cex = .7, bty="n", title="Clustering ratio")
  legend(00,35, legend = c("Easy", "Hard"), lty =1,lwd=2, cex = .7, bty="n", title="Learning", col = c("black", "gray60"))
  plot(learn_events[1,1:xlimit], xlab = "Runs", ylab = "Average vocabulary size", cex = 0, ylim = c(0, max(learn_events[,1:xlimit])+1))

  for(i in 1:length(netlist)) lines(learn_events[i,1:xlimit], lwd = 2, lty = ltytype[i], col = cols[i])

