
### Behavioral Network Science
### Language, Mind, and Society
# By Thomas T. Hills

# Code to reproduce figures and analyses for

#### Chapter 11 -- Cognitive Foraging: Exploration versus Exploitation ####

knitr::opts_chunk$set(echo = FALSE)
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(comment = NA)
knitr::opts_chunk$set(warning = FALSE)
options(knitr.table.format = "latex")

rm(list=ls())

 library(igraph)
 library(gridExtra)
 library(tidyverse)
 library(kableExtra)
 library(Rmisc)
 library(latex2exp)

## # knitr::include_graphics(c("images/MVTbird.jpeg"))


#### Figure 11-1 ####

knitr::include_graphics(c("images/Figure11-1.png"))


#### Figure 11-2 ####

knitr::include_graphics(path = "./images/Figure11-2.pdf")



#### Table 11.1 ####

# Upload data
al <- read.table(file="SampleDataFilesBNS/semanticSearch/dataentrylist.txt")
am <- read.table(file="SampleDataFilesBNS/semanticSearch/dataancosim.txt")
amlabs <- read.table(file="SampleDataFilesBNS/semanticSearch/datamatrixlabels.txt")
# Set names for cosine matrix
rownames(am) <- amlabs[,1]
colnames(am) <- amlabs[,1]
# Set diagonals to zero
diag(am) <- 0
# Label table with sequences
names(al) <- c("sid", "animals")
# Make example list
animallist <- c("cat", "dog", "kitten", "whale", "squid", "puppy", "alpaca")
# Get example list matrix
aml <- am[animallist, animallist]
# Make it a matrix
aml <- as.matrix(aml)
# Set diagonals to 1 (probably it should be a 1 above as well but this isn't relevant below)
diag(aml) <- 1.0
# Print example table
knitr::kable(aml,row.names=TRUE, escape = FALSE, caption = "A subset of the BEAGLE cosine similarity matrix.", digits=2 ) %>% kable_classic(full_width = F, html_font = "Cambria")

# Set plotting parameters
par(mfrow=c(1,2))
par(mar=c(1,1,1,1))
## Make 3 plots
for(i in 1:2){
  # Get participant IDs with good examples
  ilist <- c(680, 292) 
  # Get ith list
  al51 <- subset(al, sid == ilist[i]) 
  # Lag the list by 1 so we can compute the next entry
  al51top15 <- al51 %>% mutate("n+1"=lead(animals, n = 1))
  # Get first 15 rows
  al51top15 <- al51top15[1:15, ]

  ## Mapply gets values for each pari of items in list, e.g., al51top15[,2] and [,3]
  cosineSimilarity <- mapply(function(x,y) am[x,y], al51top15[,2], al51top15[,3])
  ## Add similarity column
  al51top15 <- al51top15 %>% add_column(cosineSimilarity)
  ## Cut first column (ID)
  al51top15 <- al51top15[,-1]
  # Number rows
  al51top15 <- data.frame(number =1:nrow(al51top15), al51top15)
  # Label column
  names(al51top15)[2] <- "n"
  # First 4 columns
  al51top15 <- al51top15[,c(1,2,3,4)]
  # make numeric column
  al51top15$cosineSimilarity <- as.numeric(al51top15$cosineSimilarity)
  # Round column to 2
  al51top15[,4] <- round(al51top15[,4], 2)

  ## make plot
  am <- as.matrix(am)
  # Make network
  animalnet <- graph_from_adjacency_matrix(am, weighted=TRUE,mode="undirected" )
  animalnet <- igraph::simplify(animalnet)
  # Get subgraph
  amt <- igraph::induced_subgraph(animalnet, al51$animals)
  # Rename
  amt2 <- amt
  # Set visualization parameters
  V(amt2)$size <- 0
  V(amt2)$lsize <- .5
  ## Change label colors
  V(amt2)$lcol <- ifelse(V(amt2)$name %in% al51top15$n, "black", alpha("gray80",alpha=.5))
  # Rename
  amt3 <- amt2
  # Make edgelist table
  edgelist <-rep(NA, nrow(al51top15)) 
  # Get edge ids for edgelist table
  for(i in 1:nrow(al51top15)){
   edgelist[i] <- get.edge.ids(amt3, c(al51top15$n[i], al51top15$n.1[i]))
  }
  # Take top elements
  edgelist <- edgelist[1:9]
  # Set colors of edges
  ecol <- rep(alpha("white", alpha=0), length(E(amt)))
  ecol[edgelist] <- alpha("black" , alpha = .4)
  # Make layout
  set.seed(3) 
  l = layout_with_fr(amt3)
}

# These aren't plotted in the book but they could be, to see the trajectories through the network


#### Table 11.2 ####

knitr::kable(al51top15,row.names=FALSE, booktabs = T, escape = FALSE, caption = "Example participant data from the animal fluency task from Hills et al., 2012. Cosine similarity shows the similarity between the N and N+1 items.", digits=2, col.names = c("Count", "Name", "Name+1", "Cosine similarity" )) %>% kable_classic(full_width = F, html_font = "Cambria")



#### Figure 11.3 Prep ####

## Thresh animalnet so all > 0: this is a threshold function that takes a graph and produces a graph with only edges above threshold
thresh_graph <- function(g, thresh, unweighted=FALSE) {
  gt <- delete_edges(g, E(g)[E(g)$weight < thresh])
  if(unweighted){
    E(gt)$weight <- 1
  }
  return(gt)
}

# Use above function to threshold the graph
animalnet <- thresh_graph(animalnet, 0)

## Closeness
## Careful here: the weights are similarity and closeness takes 1/sum(weights) as closeness, treating the weights not as similarity but distances.  To correct, we take 1/weight.
clamt <- igraph::closeness(animalnet, weights = (1/E(animalnet)$weight), normalized = TRUE)
clamt <- data.frame(animals=names(clamt), clamt)  
## Frequency
fl <- read.table(file="SampleDataFilesBNS/semanticSearch/datafreqlistlog.txt")
names(fl) <- c("word", "logfreq")
## Eigenvector centrality
eigamt <- eigen_centrality(animalnet, scale=TRUE)$vector
eigamt <- data.frame(animals=names(eigamt), eigamt)
# Lag names
alp <- al %>% mutate("n+1"=lead(animals, n = 1))
# Put down an ID counter
productionsPerSID <- table(alp$sid)
# Compute productions per person
counter <- unlist(sapply(productionsPerSID, function(x) seq(1:x)))
# Make data frame
alp <- data.frame(counter, alp)
# Remove last entry for each participant (this doesn't have a similarity to the next)
alp_minus <- alp[-cumsum(productionsPerSID),]
# Make similarity table
cosineSimilarity <- mapply(function(x,y) am[x,y], alp_minus[,3], alp_minus[,4])
alp_minus <- alp_minus %>% add_column(cosineSimilarity)
# Recombine with alp 
alpagain <- alp %>% left_join(alp_minus[,c(1,2,5)], by=c("counter"="counter", "sid"="sid"))
# Replace NAs for last animal on n.1
alpagain$n.1[cumsum(productionsPerSID)] <- NA
## Add freq 
alpagain <- alpagain %>% left_join(fl, by=c("animals"="word"))
## Add closeness
alpagain <- alpagain %>% left_join(clamt, by=c("animals"="animals"))
## Add eigenvector
alpagain <- alpagain %>% left_join(eigamt, by=c("animals"="animals"))

## Cut to top 40
alpagain40 <- alpagain %>% filter(counter < 41)


#### Figure 11-3 ####

  # Lag the animal names to compute similarities forwards and backwards
  wave <- alpagain[,1:3]
  wave <- wave %>% mutate("n-5"=lag(animals, n = 5))
  wave <- wave %>% mutate("n-4"=lag(animals, n = 4))
  wave <- wave %>% mutate("n-3"=lag(animals, n = 3))
  wave <- wave %>% mutate("n-2"=lag(animals, n = 2))
  wave <- wave %>% mutate("n-1"=lag(animals, n = 1))
  wave <- wave %>% mutate("n+1"=lead(animals, n = 1))
  wave <- wave %>% mutate("n+2"=lead(animals, n = 2))
  wave <- wave %>% mutate("n+3"=lead(animals, n = 3))
  wave <- wave %>% mutate("n+4"=lead(animals, n = 4))
  wave <- wave %>% mutate("n+5"=lead(animals, n = 5))

  # Make empty matrix
  mat10 <- matrix(NA, ncol=10, nrow=nrow(wave))
  # Get similarity values for all pairs across all participant productions (~5000)
  for(i in 1:nrow(wave)){
    for(j in 1:10){
      if(!is.na(wave[i,j+3])){
            mat10[i,j] <- am[wave[i,3], wave[i,j+3]]
      }
    }
  }

  # Compute column means
  simwindow <- apply(mat10, 2, mean, na.rm=T)
  # Compute column SD
  simwindowsd <- apply(mat10, 2, sd, na.rm=T)
  # Get number of rows
  lgt <- nrow(mat10)
  # Compute SE (This could also be done first within participant, then across)
  simwindowse <- simwindowsd/sqrt(lgt)
  # Set plotting parameters
  par(mfrow=c(1,1))
  par(mar=c(5,5,2,2))
  plot(simwindow, ylim = c(.30, .36), ylab = "Similarity to current animal", xaxt="n", xlab="Order relative to current animal")
  lines(simwindow)
  x <- 1:10
  # Plot error bars
  arrows(x, simwindow+simwindowse, x, simwindow-simwindowse, code=3, angle=90, len=.05)
  axis(1, at=1:10, labels=c(names(wave)[4:13]))
  
  ws <- data.frame(wave, mat10)



#### Figure 11-4 ####

## Set plot parameters 
par(mar=c(5,4,2,1))
par(mfrow=c(1,3))
# Get summary info on log freq
sumout <- summarySE(alpagain40, measurevar = "logfreq", groupvar = c("counter"), na.rm=T)
# Plot
plot(sumout[,1], sumout[,3], xlab="Number", ylab="Log(frequency)", ylim =c(3, 4.2), cex = .1)
# Plot error
arrows(sumout[,1], sumout[,3]+sumout[,5], sumout[,1],sumout[,3]-sumout[,5],code=3, angle=90,len=.02 )
# Get summary info on closeness centrality
sumout <- summarySE(alpagain40, measurevar = "clamt", groupvar = c("counter"), na.rm=T)
# Plot 
plot(sumout[,1], sumout[,3], xlab="Number", ylab="Closeness centrality", cex = .1, ylim=c(.19, .24))
arrows(sumout[,1], sumout[,3]+sumout[,5], sumout[,1],sumout[,3]-sumout[,5],code=3, angle=90,len=.02 )
# Get summary info on eigenvector centrality
sumout <- summarySE(alpagain40, measurevar = "eigamt", groupvar = c("counter"), na.rm=T)
plot(sumout[,1], sumout[,3], xlab="Number", ylab="Eigenvector centrality", cex = .1, ylim = c(0.6, .78))
arrows(sumout[,1], sumout[,3]+sumout[,5], sumout[,1],sumout[,3]-sumout[,5],code=3, angle=90,len=.02 )



## Maximum likelihood method to evaluate SAM on different cues and representations

## Frequency
frequencyTable <- fl
# Label table
names(frequencyTable) <- c("word", "value")
## eigamt # eigenector
eigenTable <- eigamt
# Label table
names(eigenTable) <- c("word", "value")
# Add .01 for  log value
eigenTable$value <- eigenTable$value + .01
## clamt # closeness
closenessTable <- clamt
names(closenessTable) <- c("word", "value")
# Replace all NA values with minimum
closenessTable[is.na(closenessTable$value), 2] <- min(closenessTable$value, na.rm=TRUE) # There are other ways to handle this, possibly some are better

## Function for compute LL
samfit <- function(beta, wordlist, rep){
  ll = 0
  # For each word in list
  for(i in 1:length(wordlist)){
    # Compute numerator
    num <- rep[rep$word == wordlist[i], "value"]^beta
    # Compute denominator
    den <- sum(rep$value^beta, na.rm=TRUE)
    # Subtract (it's a negative value)
    ll = ll - log(num/den)
    # Remove word from 'rep' list
    rep <- rep[-which(rep$word==wordlist[i]),]
 }
 return(ll)
} 

## Subject ID list
sidlist <- unique(al$sid)
# LL for each ID 
lllist <- rep(NA, length(sidlist))
# beta for each ID
betalist <- rep(NA, length(sidlist))
# For each ID compute LL values
for(sidi in 1:length(sidlist)){
  # Optimize the beta given to the samfit function
  res <- optim(par = c(8), fn = samfit, method = "BFGS", wordlist = subset(al, sid==sidlist[sidi])[,"animals"], rep = frequencyTable)   
  # Assign values
  lllist[sidi] <- res$value
  betalist[sidi] <- res$par
}
# Make a vector
animalproductions <- as.vector(table(al$sid))
# Save as data frame with all the bits
datarun <- data.frame(sidlist, n = animalproductions, frll = lllist, frbeta = betalist)

## Repeat for eigenvector centrality
sidlist <- unique(al$sid)
lllist <- rep(NA, length(sidlist))
betalist <- rep(NA, length(sidlist))
for(sidi in 1:length(sidlist)){
  res <- optim(par = c(8), fn = samfit, method = "BFGS", wordlist = subset(al, sid==sidlist[sidi])[,"animals"], rep = eigenTable)   
  lllist[sidi] <- res$value
  betalist[sidi] <- res$par
}
datarun9 <- data.frame(datarun, eill = lllist, eibeta = betalist)

## Repeat for closeness centrality
sidlist <- unique(al$sid)
lllist <- rep(NA, length(sidlist))
betalist <- rep(NA, length(sidlist))
for(sidi in 1:length(sidlist)){
  res <- optim(par = c(8), fn = samfit, method = "BFGS", wordlist = subset(al, sid==sidlist[sidi])[,"animals"], rep = closenessTable)   
  lllist[sidi] <- res$value
  betalist[sidi] <- res$par
}
datarun8 <- data.frame(datarun9, clll = lllist, clbeta = betalist)

## Repeat for random (beta = 0)
sidlist <- unique(al$sid)
lllist <- rep(NA, length(sidlist))
betalist <- rep(NA, length(sidlist))
for(sidi in 1:length(sidlist)){
  res <- samfit(beta=0, wordlist = subset(al, sid==sidlist[sidi])[,"animals"], rep = closenessTable)   
  lllist[sidi] <- res
}
datarun7 <- data.frame(datarun8, rall = lllist)


# Function for two betas, fixed betag
samfitsim <- function(beta, betag, wordlist, rep, repg){
  ll = 0
  for(i in 1:length(wordlist)){
    if(i == 1){
      num <- repg[repg$word == wordlist[i], "value"]^betag
      den <- sum(repg$value^betag, na.rm=TRUE)
      ll = ll - log(num/den)
    } else {
      num <- rep[wordlist[i-1],wordlist[i]]^beta
      den <- sum(rep[wordlist[i-1],]^beta, na.rm=TRUE)
      ll = ll - log(num/den)
    }
    # Reduce similarity of previously retrieved items
    rep[, wordlist[i]] <- .001
  }
  return(ll)
} 

## Some similarities are negative, make 0 and add a little to avoid log(0)
am[which(am <= 0)] <- .01

## Run  LL over all SIDS 
sidlist <- unique(al$sid)
lllist <- rep(NA, length(sidlist))
betalist <- rep(NA, length(sidlist))
for(sidi in 1:length(sidlist)){
  # Freq beta for sidi
  frbetasidi <- datarun$frbeta[sidi]
  # Optimize beta for similarity
  res <- optim(par = c(4.34), 
               fn = samfitsim, 
               method = "BFGS", 
               wordlist = subset(al, sid==sidlist[sidi])[,"animals"], 
               rep = am, 
               betag = frbetasidi, 
               repg=frequencyTable)   
  lllist[sidi] <- res$value
  betalist[sidi] <- res$par
}
# Save as data frame 
datarun6 <- data.frame(datarun7, simll = lllist, simbeta = betalist)

# Function optimizing multiple betas
samfitcombined <- function(beta, wordlist, rep, repg){
  ll = 0
  for(i in 1:length(wordlist)){
    if(i == 1){
      num <- repg[repg$word == wordlist[i], "value"]^beta[1]
      den <- sum(repg$value^beta[1], na.rm=TRUE)
      ll = ll - log(num/den)
    } else {
      num <- (rep[wordlist[i-1],wordlist[i]]^beta[2])*(repg[repg$word == wordlist[i], "value"]^beta[1])
      den <- sum(((rep[wordlist[i-1],]^beta[2])*(repg$value^beta[1])), na.rm=TRUE)
      ll = ll - log(num/den)
    }
    # Reduce similarity of previously retrieved items
    rep[, wordlist[i]] <- .001
  }
  return(ll)
} 


## Run  LL over sids
sidlist <- unique(al$sid)
lllist <- rep(NA, length(sidlist))
betalistGlobal <- rep(NA, length(sidlist))
betalistLocal <- rep(NA, length(sidlist))
for(sidi in 1:length(sidlist)){
  # Freq beta for sidi as seed
  frbetasidi <- datarun$frbeta[sidi]
  # Sim beta for sidi as seed
  simbetasidi <- datarun6$simbeta[sidi]
  # Optimize beta for similarity
  res <- optim(par = c(frbetasidi, simbetasidi), 
               fn = samfitcombined, 
               method = "BFGS", 
               wordlist = subset(al, sid==sidlist[sidi])[,"animals"], 
               rep = am, 
               repg=frequencyTable)   
  lllist[sidi] <- res$value
  betalistGlobal[sidi] <- res$par[1]
  betalistLocal[sidi] <- res$par[2]
}

datarun5 <- data.frame(datarun6, simcombll = lllist, combbetaGlobal = betalistGlobal,
                      combbetaLocal=betalistLocal)

# Function to optimize multiple betas---relaxing frequency 
samfitdual <- function(beta, wordlist, rep, repg, jump){
  ll = 0
  for(i in 1:length(wordlist)){
    if(i == 1){
      num <- repg[repg$word == wordlist[i], "value"]^beta[1]
      den <- sum(repg$value^beta[1], na.rm=TRUE)
      ll = ll - log(num/den)
    } else {
      if(i == 2){
        beta2 <- beta[1]*exp(-beta[3])
      } else { 
        previousSimilarity <- rep[wordlist[i-2],wordlist[i-1]]
        beta2 <- beta[1]*exp(-beta[3]*previousSimilarity)
      }
      num <- (rep[wordlist[i-1],wordlist[i]]^beta[2])*(repg[repg$word == wordlist[i], "value"]^(beta2))
      den <- sum(((rep[wordlist[i-1],]^beta[2])*(repg$value^beta2)), na.rm=TRUE)
      ll = ll - log(num/den)
    }
    # Reduce similarity of previously retrieved items
    rep[, wordlist[i]] <- .001
  }
  return(ll)
} 

## Run  LL over SIDS
sidlist <- unique(al$sid)
lllist <- rep(NA, length(sidlist))
betalistGlobal <- rep(NA, length(sidlist))
betalistLocal <- rep(NA, length(sidlist))
betalistRelax <- rep(NA, length(sidlist))
for(sidi in 1:length(sidlist)){
  # Freq beta for sidi as seed
  frbetasidi <- datarun$frbeta[sidi]
  # Sim beta for sidi as seed
  simbetasidi <- datarun6$simbeta[sidi]
  # Optimize beta for similarity
  res <- optim(par = c(frbetasidi, simbetasidi, 1), 
               fn = samfitdual, 
               method = "BFGS", 
               wordlist = subset(al, sid==sidlist[sidi])[,"animals"], 
               rep = am, 
               repg=frequencyTable)   
  lllist[sidi] <- res$value
  betalistGlobal[sidi] <- res$par[1]
  betalistLocal[sidi] <- res$par[2]
  betalistRelax[sidi] <- res$par[3]
}

datarun4 <- data.frame(datarun5, simrellll = lllist, 
                      betarelGlobal= betalistGlobal,
                      betarelLocal=betalistLocal,
                      betarelrel=betalistRelax)



# Function for residual proximity
samfitres <- function(beta, wordlist, rep, repg){
  ll = 0
  for(i in 1:length(wordlist)){
    if(i == 1){
      num <- repg[repg$word == wordlist[i], "value"]^beta[1]
      den <- sum(repg$value^beta[1], na.rm=TRUE)
      ll = ll - log(num/den)
    } else {
      beta2 <- beta[1]*exp(-beta[3]*resp)
      num <- (rep[wordlist[i-1],wordlist[i]]^beta[2])*(repg[repg$word == wordlist[i], "value"]^(beta2))
      den <- sum(((rep[wordlist[i-1],]^beta[2])*(repg$value^beta2)), na.rm=TRUE)
      ll = ll - log(num/den)
    }
    # Reduce similarity of previously retrieved items
    rep[, wordlist[i]] <- .001
    resp <- mean(rep[wordlist[i],], na.rm=T)
  }
  return(ll)
} 


## Run  LL over sids
sidlist <- unique(al$sid)
lllist <- rep(NA, length(sidlist))
betaresGlobal <- rep(NA, length(sidlist))
betaresLocal <- rep(NA, length(sidlist))
betaresid<- rep(NA, length(sidlist))
for(sidi in 1:length(sidlist)){
  # freq beta for sidi as seed
  frbetasidi <- datarun$frbeta[sidi]
  # sim beta for sidi as seed
  simbetasidi <- datarun6$simbeta[sidi]
  # optimize beta for similarity
  res <- optim(par = c(frbetasidi, simbetasidi, .3), 
               fn = samfitres, 
               method = "BFGS", 
               wordlist = subset(al, sid==sidlist[sidi])[,"animals"], 
               rep = am, 
               repg=frequencyTable)   
  lllist[sidi] <- res$value
  betaresGlobal[sidi] <- res$par[1]
  betaresLocal[sidi] <- res$par[2]
  betaresid[sidi] <- res$par[3]
}

datarun3 <- data.frame(datarun4, simreslll = lllist, 
                      betaresGlobal= betalistGlobal,
                      betaresLocal=betalistLocal,
                      betaresid=betaresid)



# Similarity drop with 3 > 1 > 2
# Create empty list  
sdrop <- rep(0, nrow(alpagain))
# Find the switches for simdrop
for(i in 3:length(sdrop)){
  seqorder <- order(alpagain[(i-2):i,'cosineSimilarity'], decreasing=T) 
  if(seqorder[3] == 2){ # if the lowest element is the second
    sdrop[i] <- 1
  }
}

## Remove drops in last item and first item
sdrop[cumsum(productionsPerSID)+1] <-0  # This adds one to the sdrop length
sdrop[cumsum(productionsPerSID)+2] <-0 # This adds another to the sdrop  
sdrop<- sdrop[-length(sdrop)] # Cut the last one
sdrop<- sdrop[-length(sdrop)] # Cut the last one again (because you added two above)

# Add sdrop column
alpagainsimdrop <- alpagain %>% add_column(sdrop)
# Count drops
countpersid <- alpagainsimdrop %>% group_by(sid) %>% dplyr::summarise(n = n())
# Get mean
meanpersid <- mean(countpersid$n)
# Get sd
sdpersid <- sd(countpersid$n)
# Function for sdrop
samfitsdrop <- function(beta, wordlist, rep, repg, jump){
  ll = 0
  for(i in 1:length(wordlist)){
    # When sdrop default back to single beta
    if(i == 1 || jump[i] == 1){
      num <- repg[repg$word == wordlist[i], "value"]^beta[1]
      den <- sum(repg$value^beta[1], na.rm=TRUE)
      ll = ll - log(num/den)
    } else {
      num <- (rep[wordlist[i-1],wordlist[i]]^beta[2])*(repg[repg$word == wordlist[i], "value"]^beta[1])
      den <- sum(((rep[wordlist[i-1],]^beta[2])*(repg$value^beta[1])), na.rm=TRUE)
      ll = ll - log(num/den)
    }
    # Reduce similarity of previously retrieved items
    rep[, wordlist[i]] <- .001
  }
  return(ll)
} 


## Run over sids
sidlist <- unique(al$sid)
lllist <- rep(NA, length(sidlist))
betasdropGlobal <- rep(NA, length(sidlist))
betasdropLocal <- rep(NA, length(sidlist))
for(sidi in 1:length(sidlist)){
  # Freq beta for sidi as seed
  frbetasidi <- datarun$frbeta[sidi]
  # Sim beta for sidi as seed
  simbetasidi <- datarun6$simbeta[sidi]
  # Cut sdrop to individual
  
  # Optimize beta for similarity
  res <- optim(par = c(frbetasidi, simbetasidi), 
               fn = samfitsdrop, 
               method = "BFGS", 
               wordlist = subset(al, sid==sidlist[sidi])[,"animals"], 
               rep = am, 
               repg=frequencyTable,
               jump = subset(alpagainsimdrop, sid==sidlist[sidi])[,'sdrop'])   
  lllist[sidi] <- res$value
  betasdropGlobal[sidi] <- res$par[1]
  betasdropLocal[sidi] <- res$par[2]
}
# Add to data list
datarun2 <- data.frame(datarun3, simsdroplll = lllist, 
                      betasdropGlobal= betalistGlobal,
                      betasdropLocal=betalistLocal)




#### Table 11.3 ####

# Compute all BICs
datar <- datarun2
datar <- datar %>% mutate(ranBIC = 2*rall) %>% 
  mutate(frBIC = ranBIC- (1*log(n)+2*frll)) %>% 
  mutate(eiBIC = ranBIC- (1*log(n)+2*eill)) %>% 
  mutate(clBIC = ranBIC- (1*log(n)+2*clll))  %>% 
  mutate(simBIC =ranBIC-  (1*log(n)+2*simll)) %>% 
  mutate(simcombBIC = ranBIC- (2*log(n)+2*simcombll)) %>%
  mutate(simrelBIC = ranBIC-(3*log(n)+2*simrellll)) %>% 
  mutate(simresBIC = ranBIC-(3*log(n)+2*simreslll))  %>% 
  mutate(simsdropBIC = ranBIC-(2*log(n)+2*simsdroplll)) 

# Compute medians
medtab <- c(median(datar$frBIC), median(datar$eiBIC), median(datar$clBIC),
            median(datar$simBIC), median(datar$simcombBIC))
medtab <- c(round(medtab, 2), '-', round(median(datar$simrelBIC),2), '-', '-')
medtab <- c(medtab, round(median(datar$simresBIC), 2), '-','-')
medtab <- c(medtab, round(median(datar$simsdropBIC), 2), '-')
sdtab <- c(sd(datar$frBIC), sd(datar$eiBIC), sd(datar$clBIC), sd(datar$simBIC),
           sd(datar$simcombBIC))
sdtab <- c(round(sdtab, 2), '-', round(sd(datar$simrelBIC), 2), '-', '-')
sdtab <- c(sdtab, round(sd(datar$simresBIC), 2), '-','-')
sdtab <- c(sdtab, round(sd(datar$simsdropBIC), 2), '-')
betatab <- c(median(datar$frbeta), median(datar$eibeta), median(datar$clbeta),
             median(datar$simbeta), median(datar$combbetaGlobal),
             median(datar$combbetaLocal), median(datar$betarelGlobal),
             median(datar$betarelLocal), median(datar$betarelrel),
             median(datar$betaresGlobal), median(datar$betaresLocal),
             median(datar$betaresid), median(datar$betasdropGlobal),
             median(datar$betasdropLocal))
sdbetatab <- c(sd(datar$frbeta), sd(datar$eibeta), sd(datar$clbeta),
               sd(datar$simbeta), sd(datar$combbetaGlobal),
               sd(datar$combbetaLocal),sd(datar$betarelGlobal),
               sd(datar$betarelLocal), sd(datar$betarelrel),
               sd(datar$betaresGlobal), sd(datar$betaresLocal),
               sd(datar$betaresid), sd(datar$betasdropGlobal),
               sd(datar$betasdropLocal))

# Put it all together
BICout <- data.frame(model = c("Random model", "Frequency (global)", "Eigenvector centrality (global)", "Closeness (global)", "Similarity (local)", "Frequency (global) +", "Similarity (local)", 'Frequency (global) +', 'Similarity (local) +', "$\\lambda$", 'Frequency (global) +', 'Similarity (local) +', "$\\lambda$",  "Frequency (global) +", "Similarity (local)"), beta = c("-", round(betatab,2)), betasd = c("-",round(sdbetatab,2)), BIC = c("0",medtab), sdBIC = c("-",sdtab))

# Output table
knitr::kable(BICout, booktabs = TRUE, row.names=FALSE, escape = FALSE, caption = "Median Betas and improvement in BICs relative to a random model.  Higher BIC improvements indicate the preferred model. ", digits=2, col.names = c("Model", "Beta", "(SD)", "$\\Delta$ BIC", "(SD)" ) ) %>% kable_classic(full_width = F, html_font = "Cambria") %>% pack_rows("Single cue models", 2, 5) %>%
pack_rows("Combined cue model", 6, 7) %>% 
pack_rows("Dynamic relaxation model", 8, 10)  %>% 
pack_rows("Residual proximity model", 11, 13)  %>% 
pack_rows("Similarity drop", 14, 15) 
  


# Download inter-item retrieval time
irtd <- read.csv(file="sample_data/semanticSearch/irtHillsetal2012.csv")
# Add to dataframe from above
irtds <- data.frame(irtd, alpagainsimdrop)

##with(irtds, sum(entry==animals, na.rm=T)) # check
## Bin cosineSimilarity
dits <- irtds %>% mutate(tbin = cut(cosineSimilarity, breaks=c(seq(0, 1, .1))))
dits <- dits[!is.na(dits$tbin),]
# Get mean and SE
ss <- summarySE(dits, measurevar ='irt', groupvars = c('tbin'))

x <- 1:nrow(ss)
# Regressions
at <-with(dits, lm(irt~cosineSimilarity))
##summary(at)
at <-with(dits, lm(irt~logfreq))


#### Figure 11-5 ####

# Plot parameters
par(mfrow=c(1,2))
par(mar=c(5,4,3,3))
### Is similarity degrading over time?
# Find mean and SE
sdat <- summarySE(dits, measurevar='cosineSimilarity', groupvars='counter', na.rm=T)[1:30,]
x = 1:30
# Plot
plot(x, sdat$cosineSimilarity, xlab="Recall order", ylab="Cosine similarity", cex.lab = 1.2, ylim = c(.2, .5), pch = 16, cex.axis=.8, cex = .5)
# Add error bars
arrows(x, sdat$cosineSimilarity+sdat$se, x, sdat$cosineSimilarity-sdat$se, code=3, angle = 90, len=.05, lwd=1.2)

### Is the rate of retrieval decreasing over time?
# Get mean and se
sdat <- summarySE(dits, measurevar='irt', groupvars='counter', na.rm=T)[1:30,]
x = 1:30
# Plot
plot(x, sdat$irt, xlab="Recall order", ylab="Inter-item retrieval time (s)", cex.lab = 1.2, pch = 16, ylim =c(0, 8), cex.axis=.8, cex = .5)
arrows(x, sdat$irt+sdat$se, x, sdat$irt-sdat$se, code=3, angle = 90, len=.05, lwd=1.2)


#### Figure11-6 ####

# Create cumulative animals recovered in patch 
## 
aftercounter <- rep(NA, nrow(irtds))
counter = NA
for(i in 2:nrow(irtds)){
 # Reset when sdrop
 if(irtds$sdrop[i]==1){
   counter <- 1
 }
 # Reset when not same individual
 if(irtds$sid[i] != irtds$sid[i-1]){
   counter <- NA 
 }
 aftercounter[i] <- counter
 counter = counter + 1
}
# Add counter data
mvts <- irtds %>% add_column(afterswitch= aftercounter)

## Count patch items within patch, from other direction
beforecounter <- rep(NA, nrow(irtds))
counter = NA 
for(i in (nrow(irtds)-1):1){
 if(irtds$sid[i] != irtds$sid[i+1] | irtds$sdrop[i]==1){
   counter <- NA 
 }
 beforecounter[i] <- counter
 counter = counter + 1
  if(irtds$sdrop[i]==1){
   counter <- 1
  }

}
mvts <- mvts %>% add_column(beforeswitch= beforecounter)
# Find mean IRT (inter-item retrieval time)
mvts <- mvts %>% dplyr::group_by(sid) %>%  dplyr::mutate(meanirt = mean(irt))
# Select columns
mvts <- mvts %>% dplyr::select(sid, entry, irt, counter, sdrop, meanirt, cosineSimilarity, logfreq, afterswitch, beforeswitch)

## Find mean in patch time
timeInPatch <- rep(NA, nrow(mvts))
timeaccumulator <- 0
itemsInPatch <- rep(NA, nrow(mvts))
itemaccumulator <- 0
# Add up times within patch
for(i in 1:nrow(mvts)){
  timeaccumulator <- timeaccumulator + mvts$irt[i]  
  itemaccumulator <- itemaccumulator + 1
  # Reset when sdrop or first item for SID
  if(mvts$sdrop[i] == 1 | mvts$counter[i] == 1){
    timeaccumulator <- mvts$irt[i]
    itemaccumulator <- 1
  }
  timeInPatch[i] <- timeaccumulator
  itemsInPatch[i] <- itemaccumulator
}
# Add column
mvta <- mvts %>% add_column(timeInPatch = timeInPatch) %>% add_column(itemsInPatch = itemsInPatch)
mvta <- mvta %>% add_column(categorydrop = sdrop)

## Travel time
betweenPatchTravelTime <- mean(subset(mvta, categorydrop==1)$irt) # 5.65
betweenPatchTravelTimesd <- sd(subset(mvta, categorydrop==1)$irt)
## Departure time
justBeforeSwitch <- subset(mvta, beforeswitch == 1)
meanDepartureTime <- mean(justBeforeSwitch$'timeInPatch', na.rm=T) # 14.74551
## Cumulative (number of items by time), take mean (binning by second)
groupcurve <- with(mvta, tapply(itemsInPatch, timeInPatch, mean, na.rm=T))
par(mfrow=c(1,1))
x <- 1:40
plot(x, groupcurve[1:40], xlab = 'Time in patch', ylab='Cumulative animals recovered in patch', pch=16, ylim = c(1.1, 5), xlim = c(-10, 41), cex.lab = 1.3)
# Add mean departure time
abline(v=meanDepartureTime, col="gray70", lty=2)
# Compute slope---Cumulative at 14-15 in groupval is roughly 3. Notice we're starting at y = 1, so 3-1 = 2 divided by the total time (travel plus departure)
slope = 2/(betweenPatchTravelTime+meanDepartureTime)
# Add line with slope from x = Travel time
abline(a=1+betweenPatchTravelTime*slope, b = slope, lty =3, lwd = 2)
abline(h=1)
par(xpd=FALSE)
lw1 <- loess(groupcurve[1:40] ~ x)
lines(x,lw1$fitted,col=alpha("gray80", .5),lwd=3)
# Labels
text(meanDepartureTime+1, 1.2, TeX("$\\hat{t}$"))
text(-7, 1.2, TeX("$\\hat{T}$"))
text(40, 3.2, TeX("$g(t)$"))



