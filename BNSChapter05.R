knitr::opts_chunk$set(echo = FALSE)
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(comment = NA)
knitr::opts_chunk$set(warning = FALSE)
options(knitr.table.format = "latex")

### Behavioral Network Science
### Language, Mind, and Society
# By Thomas T. Hills

# Code to reproduce figures and analyses for

#### Chapter 5 -- Network Learning: Growing a Lexicon by Degrees ####


library(igraph)
library(tidyverse)



#### Figure 5-1 ####

# This data can be downloaded from WordBank here: http://wordbank.stanford.edu/data?name=item_data
# By-Word Summary Data / English (American) / WS / produces / 16-30 months
# I provide a version here, but it should **not** be considered the up-to-date version from WordBank

wb <- read.delim("SampleDataFilesBNS/wordbank_item_data_CDI.csv", sep = ",")
# Make data frame from WordBank categories
allcats <- data.frame(cats = unique(wb$category))
# Make a list of items to remove
removeList = c("sounds", "pronouns", "question_words", "quantifiers", "helping_verbs", "connecting_words")
# Remove items from list above
keepList <- subset(allcats, !(cats %in%  removeList) )[,1]
wb <- wb[which(wb$category %in% keepList),]

### Kuperman AoA

# The Kuperman age of acquisition ratings can be downloaded from the article here: https://link.springer.com/article/10.3758/s13428-012-0210-4
# From this location here: https://link.springer.com/article/10.3758/s13428-013-0348-8
# Download the 'ESM 1' file and save to AoA_ratings_Kuperman_et_al_BRM.txt 
koao <- read.table('SampleDataFilesBNS/AoA_ratings_Kuperman_et_al_BRM.txt', header = T)

koao <- koao[,c(1,5)]
names(koao) <- c("word", "aoa")
## Too many mommies!  rename them all to 'mommy'
koao$word[which(koao$word %in% c("mommy", "mom", "momma", "mama", "mother"))] <- "mommy"
## Too many daddies! rename them all
koao$word[which(koao$word %in% c("daddy", "dad"))]  <- "daddy"

koao <- koao %>% group_by(word) %>% dplyr::summarise(aoa = mean(aoa))

## Find age of words learned by more than 50% of population
age = 16:30 # run through each age
wordage <- rep(NA, nrow(wb)) # create empty data vector
for(i in 1:nrow(wb)){
  if(max(wb[i,5:19]) > .5){ # check if ever known by more than .5
    wordage[i] <- age[min(which(wb[i,5:19] > .5))] # save first index greater than .5
  }
}

wb <- data.frame(word = wb$item_definition, wordage)


# Small world of words (same as from previous chapter)
swow <- read.delim("SampleDataFilesBNS/strength.SWOW-EN.R1.csv", sep = "\t")

# Find subset of SWOW words in WordBank, for both cue and response
sub <- subset(swow, cue %in% wb$word)
sub <- subset(sub, response %in% wb$word)
# Make edge list with weight
subedges <- sub[,c(1,2,5)]
# We use swowk in the figures (which is kuperman x swow)
swowk <- swow[,c(1,2,5)]

cdin <- graph_from_data_frame(subedges) # small world of words with Wordbank words
cdink <- graph_from_data_frame(swowk) # small world of words complete


## Kuperman less than 4 yo
koa.early <- koao %>% filter(aoa < 4)
## 'bantling' is archaic for 'small child': I'm removing it.
koa.early <- koa.early[-which(koa.early$word == "bantling"), ]
koa.early <- koa.early %>% arrange(aoa) # ordered by aoa


##Kuperman
## Remove all nodes from full SWOW but those in koa.early
gk <- subgraph(cdink, intersect(V(cdink)$name, koa.early$word))

koa.early <- koa.early[koa.early$word %in% intersect(V(cdink)$name, koa.early$word),]

# Figure with multiple panels
#pdf("Figure5-1.pdf")
layout(matrix(c(1,2,3,4,5,6,rep(7, 6)), 6, 2, byrow = FALSE))
## Age the nodes
nds <- data.frame(word=V(gk)$name)
# Join koa to node names
nds <- nds %>% left_join(koa.early)
# Assign age attribute to nodes
V(gk)$age <- nds[nds$word==V(gk)$name,"aoa"]
par(mar=c(1,1,1,1))

ages <- seq(2.33,3.98, .33)
# Plot 6 panels on the left
for(ag in ages){
  gkag <- subgraph(gk, V(gk)$age < ag)
  plot(igraph::simplify(gkag), vertex.size = 10, edge.arrow.size = .1, vertex.label = NA, vertex.label.cex =.2, layout=layout_nicely(gkag, dim =2), vertex.color = "white", vertex.label.color = "black", vertex.label.dist = 0, vertex.frame.color = "gray50", edge.color = adjustcolor("black", alpha.f = .1))

}
# Plot large panel on the right
par(mar=c(0,0,0,0))
plot(igraph::simplify(cdin), vertex.size = igraph::degree(cdin, mode = "in")^(1/2), edge.arrow.size = .2, vertex.label.cex =.6, layout=layout_with_lgl(cdin,area = vcount(cdin)^3), vertex.label=NA,
    # layout_nicely(cdin, dim =2), 
     vertex.color = "white", vertex.label.color = "black", vertex.label.dist = 1, vertex.frame.color = "gray50", edge.color = adjustcolor("black", alpha.f = .1))
#dev.off()

##  Indegree for words in CDI
wdeg <- data.frame(word = names(igraph::degree(cdin)), degree = igraph::degree(cdin, mode = "in"))
## Indegree for words in whole network
wdegk <- data.frame(word = names(igraph::degree(cdink)), degree = igraph::degree(cdink, mode = "in"))

# Right join WordBank with indegree
dd <- right_join(wb, wdeg)

ddk <- inner_join(koao, wdegk)

dda <- left_join(dd, ddk, by = "word")

# Correlations
# cor.test(dda[,3], dda[,5])
# cor.test(dda[,2], dda[,4])


#### Figure 5-2 ####
#pdf("Figure5-2.pdf", height=5, width = 10)
par(mar=c(4,4,4,4))
par(mfrow=c(1,2))

# Add a little jitter in y-axis so readers can see all points
dd$wordage <- jitter(dd$wordage, 1)
# Plot Degree against Age of Acquisition, log x-axes
with(dd,  plot( degree, wordage, cex = .4, pch = 16, ylab= "Age of acquisition (months)", xlab = "Degree",  col=scales::alpha("black",.2),log="x")) 
# Plot for full network
with(ddk, plot(degree,aoa,cex = .4, pch = 16, ylab = "Age of acquisition (years)", xlab = "Degree", col=scales::alpha("black",.2), log="x"))
#dev.off()
# summary(with(subset(dd,degree > 0), lm(wordage~log(degree))))
# summary(with(subset(ddk,degree > 0), lm(aoa~log(degree))))

# ddl <- dd %>% left_join(ddk, by ="word")
# with(ddl, cor.test(wordage, degree.y))
# with(ddl, cor.test(wordage, degree.x))



#### Figure 5-3 ####

knitr::include_graphics(path = "./images/Figure5-3.png")



#### Figure 5-4 ####

knitr::include_graphics(path = "./images/Figure5-4.pdf")



#### Figure 5-5 ####

# Function for model of preferential acquisition
prefacq <- function(x, g, notme, beta = 1){
  # Get degree of all known words
  degrees <- igraph::degree(g, mode = "in")   # change to "in" or "out" or "total"
  # Remove known words
  degrees <- degrees[which(!(names(degrees) %in% notme))] 
  # Compute value for word actually learned
  num <- exp((degrees[x]+1)*beta) # numerator
  # Compute value for all words that could be learned
  den <- sum(exp((degrees+1)*beta)) # denominator
  # Return value
  return(log(num/den))
}
# Function for model of preferential attachment
prefatt <- function(x, g, vocab, beta = 1){
 # Get known subgraph
  bufv <- subgraph(g, vocab) # could be clex
  # Degree of nodes in subgraph
  degrees <- igraph::degree(bufv, mode = "in") 
  # Get words that could be learned
  couldbe <- V(g)[which(!(V(g)$name %in% vocab))] 
  # Find neighbors from all couldbe learned  
  ln <- adjacent_vertices(g, couldbe)  
  # Find degree of neighbors to could be learned words
  neighdeg <- lapply(ln, function(x) degrees[names(x)]) 
  # Compute mean degree of known neighbors for each unknown word
  avg.deg.target <- lapply(neighdeg, function(x) mean(x, na.rm=T)) 
  # Replace NaNs with 0
  avg.deg.target[is.na(avg.deg.target)] <- 0 
  # Counter
  wid <- 1:length(avg.deg.target)
  # Get value for learned word
  num = exp((as.numeric(avg.deg.target[x])+1)*beta) 
  # Sum values for all could be words
  den <- sum(exp((as.numeric(avg.deg.target[wid])+1)*beta))
  # Return the probability of learned word
  return(log(num/den)) 
}

# Lure of the associates
loa <- function(x, g, vocab, beta = 1){
  # Words that could be added
  couldbe <- V(g)[which(!(V(g)$name %in% vocab))]
  # Neighbors of could be nodes
  neigh.couldbe <- adjacent_vertices(g, couldbe, mode = "in")
  # How many of could be neighbors are known for each could be neighbor 
  count.loa <- lapply(neigh.couldbe, function(x) length(intersect(names(x), vocab)) )
  # Counter for all couldbes
  wid <- 1:length(count.loa)
  # Value of learned word
  num = exp((as.numeric(count.loa[x])+1)*beta) # numerator for chosen word
  # Value of all words that could have been learned (couldbes)
  den <- sum(exp((as.numeric(count.loa[wid])+1)*beta))
  # Return the probability of learned word
  return(log(num/den)) 
}

# Grid search over range of betas
betal <- seq(0,.1,.01)
# Make empty data vectors
lpc <- rep(NA, length(betal))
lpt <- rep(NA, length(betal))
llo <- rep(NA, length(betal))

for(l in 1:length(betal)){
  vocab = "mommy" # We'll start with a random word, but this should be randomly selected and repeated many times
  # Log likelihood vectors to fill
  llpac = rep(NA, nrow(koa.early)) 
  llpat = rep(NA, nrow(koa.early))
  lloa = rep(NA, nrow(koa.early))
  # Limit to 3/4s of all words, so the set to learn is large enough at the end.
  upto <- floor(nrow(koa.early)*3/4)
  # Set beta
  beta = betal[l]
  # Run through all words up to 'upto' for each model
  for(j in 2:upto){
    # Get log likelihoods across all learned words
    llpac[j] <-  as.numeric(prefacq(x = koa.early$word[j], g=gk, vocab, beta ))
    llpat[j] <- as.numeric(prefatt(x = koa.early$word[j], g=gk, vocab, beta ))
    lloa[j] <- as.numeric(loa(x = koa.early$word[j], g = gk, vocab, beta))
    vocab <- c(vocab, koa.early$word[j])
  } # End loop of adding words
  # Sum log likelihoods
  lpc[l] <- sum(llpac, na.rm=T)
  lpt[l] <- sum(llpat, na.rm=T)
  llo[l] <- sum(lloa, na.rm=T)
}
# penalize with AIC and BIC
aicpc <- 2*(1)-2*lpc
aicpt <- 2*(1)-2*lpt
aiclo <- 2*(1)-2*llo

bicpc <- 1*log(upto-1)-2*lpc
bicpt <- 1*log(upto-1)-2*lpt
biclo <- 1*log(upto-1)-2*llo
#pdf("Figure5-5.pdf")
par(mfrow=c(1,1))
plot(betal, bicpc, xlab = expression(beta), ylab = "Bayesian Information Criterion")
points(betal, bicpt, pch = 16)
points(betal, biclo, pch = 10)
legend(0, 2440, legend=c("Preferential Acquisition", "Preferential Attachment", "Lure of the Associates"), pch = c(1,16, 10))
#dev.off()
