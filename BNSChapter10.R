knitr::opts_chunk$set(echo = FALSE)
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(comment = NA)
knitr::opts_chunk$set(warning = FALSE)
options(knitr.table.format = "latex")

### Behavioral Network Science
### Language, Mind, and Society
# By Thomas T. Hills

# Code to reproduce figures and analyses for

#### Chapter 10 -- False Memories: Spreading Activation in Memory Networks ####

rm(list=ls()) # to health!
library(igraph)
library(gridExtra)
library(tidyverse)
library(kableExtra)
library(latex2exp)
library(stargazer)



#### Figure 10-1 ####

set.seed(2)

# To download the SWOW data follow these instructions (see previous chapters): 
# go to https://smallworldofwords.org/en/project/research#download
# Download from 'English' data  SWOW-EN2008 assoc. strengths (R1) [3Mb]
# Unzip to the CSV file and place in the SampleDataFilesBNS folder
# There is a request for some information, but providing it is optional
# At the request of SWOW, I will not provide the free association data directly.

swow <- read.delim("SampleDataFilesBNS/strength.SWOW-EN.R1.csv", sep = "\t")

swow <- tibble(swow)
# Keep only those produced by at least 10% 
swowT <- swow %>% filter(R1.Strength >=.1)
# Graph from edge list
sg <- graph_from_edgelist(as.matrix(swowT[,1:2]), directed = FALSE)
sg <- igraph::simplify(sg) # undirected and unweighted

# List of lures for DRM task
lures <- data.frame(lure= c("dream",  "bed", "soft",  "pillow", "sheet",  "rest", "covers",  "wake", "alarm",  "unconscious", "dark",  "morning", "sleep" ) )
# Get distances from sleep
nw <- distances(sg, 'sleep')
# List of those < 3 away
within2 <- colnames(nw)[which(nw < 3)]
#  Make subraph of those
sgs <- subgraph(sg, within2) 
# Node attributes for plotting
V(sgs)$size = 2
V(sgs)$size[which(V(sgs)$name %in% lures$lure)] <- 5 
V(sgs)$size[which(V(sgs)$name == 'sleep')] <- 10
V(sgs)$color <- ifelse(V(sgs)$name %in% lures$lure, "aquamarine4", "white") 
V(sgs)$color[which(V(sgs)$name == 'sleep')] <- "sienna3"
# Plot
par(mfrow=c(1,1))
par(mar=c(0,0,0,0))
plot(sgs, vertex.size = V(sgs)$size, vertex.label.cex = .52, vertex.label.dist = 1, vertex.label.family="Helvetica", vertex.color = V(sgs)$color)


## ## WARNING !!!
## ## WARNING !!!
## ## WARNING !!!
## ## WARNING !!!
## 
## ## IT TAKES SEVERAL HOURS TO RUN THE FULL DISTANCE MATRIX ON MY 2022 M1 MAC.  THEREFORE, I'VE CREATED A MINIMAL VERSION OF THIS HERE (WITH ONE LINE INTRODUCED), SO WE DON'T RUN IT BY ACCIDENT.  YOU CAN UPLOAD THE OUTPUT OF THIS IN THE NEXT CHUNK.
## 
## # Make phonological neighbor network
## 
## #### LEVENSHTEIN DISTANCE
## ###########################
## # The Levenshtein distance uses dynamic programming, breaking the problem down into simpler problems. The matrix keeps track of the solution to each problem and the overall number of changes (additions, substractions, or substitions) required to translate one string into another. Each cell in the matrix is the local minimum of either the left cell, top cell, or diagonal cell + 1 if they are unequal.
## levenshteinDistance <- function(w1, w2) {
##   # Split words into parts (i.e., phonemes in case of Klattese)
##   s1 <- unlist(strsplit(paste(" ", w1, sep=""), split=""))
##   s2 <- unlist(strsplit(paste(" ", w2, sep=""), split=""))
##   # Build distance matrix to keep track of changes
##   counter1 <- length(s1)
##   counter2 <- length(s2)
##   d <- matrix(nrow = counter1, ncol = counter2)
##   # Initialize first row and column
##   for(i in 1:counter1) d[i,1] <- i-1
##   for(i in 1:counter2) d[1,i] <- i-1
##   for(i in 2:counter1) for(j in 2:counter2) d[i,j] <- min((d[i-1,j]+1) , (d[i,j-1]+1) , (d[i-1,j-1]+ifelse(s1[i] == s2[j], 0, 1)))
##   return(d[counter1,counter2])
## }
## 
## # This is the full Hoosier klattese data set (Thanks to Michael Vitevitch)
## klat <- read.csv('SampleDataFilesBNS/hoosier.csv', header = T)
## klatmat <- matrix(0, nrow=nrow(klat), ncol=nrow(klat))
## rownames(klatmat) <- klat$Phono
## colnames(klatmat) <- klat$Phono
## 
## # The next line reduces this to a smaller version to demonstrate the functionality. Remove this line if you want to run it on the entire thing. THIS WILL TAKE A LONG TIME TO RUN.
## klatmat <- klatmat[1:10,1:10]
## 
## for(i in 1:nrow(klatmat)){
##   for(j in (i+1):ncol(klatmat)){
##     if(j <= ncol(klatmat)){
##       klatmat[i,j] <-levenshteinDistance(toString(rownames(klatmat)[i]), toString(colnames(klatmat)[j]))
##     }
##   }
## }
## 
## # Uncomment below to save over the provided data.
## library(MASS)
## library(Matrix)
## # write.matrix(klatmat,'SampleDataFilesBNS/klatmat.csv',sep = ",")
## klatmat1 <-klatmat
## # Only count nearest neighbors
## klatmat1[klatmat1 > 1] <- 0
## klatmat2 <-klatmat
## # Only count two-hop neighbors
## klatmat2[klatmat2 > 2] <- 0
## # Force to save as sparse-matrix format (It's still big if you use the full matrix! ~ 1GB )
## klatmat1 <- as(klatmat1, "dgCMatrix")
## klatmat2 <- as(klatmat2, "dgCMatrix")
## #write.matrix(klatmat1,'SampleDataFilesBNS/klatmat1.csv',sep = ",")
## #write.matrix(klatmat2,'SampleDataFilesBNS/klatmat2.csv',sep = ",")


#### Figure 10-2 ####

# Read in large data files
klat <- read.csv('SampleDataFilesBNS/hoosier.csv', header = T)
k1 <- read.csv('SampleDataFilesBNS/klatmat_1.csv', header = T)
# Set as matrix
k1 <- as.matrix(k1)
# Make graph
gk1 <- graph_from_adjacency_matrix(k1, mode = "undirected")

# Prepare for plotting
V(gk1)$name <- klat$Ortho
par(mar=c(0,0,0,0))
#pdf("phonoPlot.pdf")
plot(gk1, vertex.size = 1, vertex.label = NA, vertex.color = "pink")
#dev.off()

# ![Phonological network with 19340 nodes.](images/phonoPlotCH10.pdf)

# number of isolates
# sum(degree(gk1)==0) # 10265

# Extract giant component
comps <- igraph::clusters(gk1, mode="weak")
# Identify members
giantC_id <- which.max(comps$csize)
vert_ids <- V(gk1)[comps$membership == giantC_id] 
# Extract giant component 
gk1s <- igraph::induced_subgraph(gk1, vert_ids)



#### Figure 10-3 ####

set.seed(2)
par(mar=c(0,0,0,0))
# Number of frames in Figure
fr = 5
# Number of nodes in tree
n = 121
# Color palette
pal = colorRampPalette(c("white", "aquamarine4"))
# Figure panels
par(mfrow=c(1,fr))
# Make tree network
mt <- make_tree(n, 3, mode = "undirected")
# Create data frame with values for spreading activation function: spreadr
df <- data.frame(node = 1:length(V(mt)), 
                     activation = rep(0, length(V(mt))) ,
                     stringsAsFactors = F)
# Set initial activation
df$activation[1] <- 100
# Size parameters
bg <- 1 
fc <- 1.1
V(mt)$size <-   (df$activation/2+bg)^fc
V(mt)$color = pal(max(V(mt)$size))[round(V(mt)$size, 2)] 
# Layout 
la <- layout_with_fr(mt)
# Plot initial network
plot(mt, layout=la, vertex.label = NA, vertex.frame.color = "gray70", edge.color = "gray70", edge.width=.5)
text(0,-1.85, "time-step: 1")
 
# Spreading activation 
for(i in 1:(fr-1)){
    df <- spreadr::spreadr(start_run = df, decay = 0,
                                    retention = 0, suppress = 0,
                                    network = mt, time = 1)
    df <- subset(df, time ==1)[,-3]
    # Plotting values
    V(mt)$size <-   (df$activation/2+bg)^fc
    V(mt)$color = pal(40)[round(V(mt)$size, 2)] 
  plot(mt, layout=la, vertex.label = NA, vertex.frame.color = "gray70", edge.color = "gray70", edge.width=.5)
  text(0,-1.5, paste("time-step:", i+1))
}
    
    



#### Figure 10-4 ####

set.seed(3)

# This shouldn't be needed for newer versions of R, but I leave it here just in case
# options(stringsAsFactors = FALSE)

# load data

# Data for these networks is provided in Siew. (2019) spreadr: A R package to simulate spreading activation in a network. Provided online here: https://osf.io/a9bv6/

load('SampleDataFilesBNS/ego2hopnets_24.RData') # 24 networks 
v2011 <- read.csv('SampleDataFilesBNS/24toynets.csv') # degree and clustering coefficient values for each of the 24 words
par(mfrow=c(1,2))
par(mar=c(1,1,1,1))
e2 <- ego2hopnets_24

# Visualization settings and plot for 'fount'
vi <- 4
vs = 15 
vc = .5
vnames <-V(e2[[vi]])$name
V(e2[[vi]])$color <- "white"
V(e2[[vi]])$color[which(V(e2[[vi]])$name == "fWnt;fount")] <- "olivedrab"
vnames1 <- unlist(strsplit(vnames, split=";"))[seq(2,2*length(vnames),2)]
plot(e2[[vi]], vertex.label=vnames1, vertex.color = V(e2[[vi]])$color, vertex.size = vs, vertex.label.dist = 0, vertex.label.cex = vc, vertex.frame.color = "gray70")

# Visualization settings and plot for 'comma'
e2 <- ego2hopnets_24
vi <- 6
vnames <-V(e2[[vi]])$name
V(e2[[vi]])$color <- "white"
V(e2[[vi]])$color[which(V(e2[[vi]])$name == "kamx;comma")] <- "olivedrab"
vnames1 <- unlist(strsplit(vnames, split=";"))[seq(2,2*length(vnames),2)]
plot(e2[[vi]], vertex.label=vnames1, vertex.size = vs, vertex.label.dist = 0, vertex.label.cex = vc, vertex.frame.color = "gray70")


# The information in the Vitevitch30falsephonoMemories.csv file is published in Vitevitch, M. S., Chan, K. Y., & Roodenrys, S. (2012). Complex network structure influences processing in long-term and short-term memory. Journal of memory and language, 67(1), 30-44. 

# Import list of words and neighbors
vitwords <- read.csv(file="SampleDataFilesBNS/Vitevitch30falsephonoMemories.csv", header = FALSE)

# Remove last column of NAs
vitwords <- vitwords[,-13]
# Label columns
names(vitwords) <- c("C", "Target", 1:10) #paste("Cue", 1:10, sep=""))


#### Figure 10-5 ####

set.seed(2)
# Set plot parameters
par(mar=c(0,0,0,0))
par(mfrow=c(6,5))
# Set network metric data buffers
ttivity <- rep(NA, nrow(vitwords))
nettivity <- rep(NA, nrow(vitwords))
NofNodes <- rep(NA, nrow(vitwords))
ddensity <- rep(NA, nrow(vitwords))
eeig <- rep(NA, nrow(vitwords))
edgecount <- rep(NA, nrow(vitwords))
ddegree <- rep(NA, nrow(vitwords))

# Produce network for each word and compute metrics
# Then plot networks and print text
# For each word
for(i in 1:nrow(vitwords)){
  # Get target word
  targeti <- vitwords$Target[i] 
  # Extract two-hop network around target word from giant network
  buffg <- make_ego_graph(gk1s, order=2,nodes=which(V(gk1s)$name==targeti))[[1]]
  # Set node sizes for visualization
  V(buffg)$size <- 1
  # Compute local transitivity
  ttivity[i] <- igraph::transitivity(buffg,type="local",
                                     vids=which(V(buffg)$name==targeti))
  # Compute global transitivity
  nettivity[i] <- igraph::transitivity(buffg, type="global")
  # How many nodes
  NofNodes[i] <- length(V(buffg))
  # Get degree
  ddegree[i] <- igraph::degree(buffg, v = which(V(buffg)$name==targeti))
  # How many edges
  edgecount[i] <- length(E(buffg))
  # Compute density
  ddensity[i] <- graph.density(buffg)
  # Get eigenvector centrality
  eeig[i] <- igraph::eigen_centrality(buffg)$vector[targeti]
  # Set node size of target
  V(buffg)$size[which(V(buffg)$name==targeti)] <- 10
  # Set node colors
  V(buffg)$color <- "gray50"
  V(buffg)$color[which(V(buffg)$name==targeti)] <- "yellow" 
  # Plot networks
  plot(buffg, vertex.label.cex=.2, vertex.label=NA, edge.width=.5)
  # Add text
  text(1,1, labels=paste(targeti, ""), cex = .8)
  text(-1,-1, labels=paste("C=",round(ttivity[i],2), ""), cex = .8)
}



# Make data frame for target words and values
vitd <- data.frame(Target=vitwords$Target, Degree=ddegree, Clustering=vitwords$C, C=ttivity, N=NofNodes, E=edgecount, Transitivity=nettivity, Density=ddensity, EigenvectorCentrality=eeig)
# Make the table pretty to 2 decimal places
vitd <- vitd %>% 
 mutate_if(is.numeric, round,digits=2)

# Some stats and tables

# Table of values
#kable(vitd,row.names=FALSE, booktabs = T, escape = FALSE, caption = "Table of curiosities for the words used in Vitevitch et al., 2012.", digits=2) %>%
#kable_classic(full_width = F, html_font = "Cambria") 

#with(vitd, t.test(N~Clustering))
#with(vitd, t.test(Density~Clustering))
#with(vitd, t.test(EigenvectorCentrality~Clustering))
#with(vitd, t.test(Degree~Clustering))
#with(vitd, t.test(E~Clustering))

#library(BayesFactor)
# Bayes factors
#ttestBF(formula = Density ~ Clustering, data = vitd)
#ttestBF(formula = Degree ~ Clustering, data = vitd)
#ttestBF(formula = N ~ Clustering, data = vitd)
#ttestBF(formula = E ~ Clustering, data = vitd)
#ttestBF(formula = EigenvectorCentrality ~ Clustering, data = vitd)
#ttestBF(formula = C ~ Clustering, data = vitd)
#ttestBF(formula = Transitivity ~ Clustering, data = vitd)

# Summarize data for each target network
vit.summary <- vitd %>%
  group_by(Clustering) %>%
  dplyr::summarise(
    sdDegree = sd(Degree, na.rm = TRUE),
    Degree = mean(Degree),
    sdN = sd(N, na.rm = TRUE),
    N = mean(N),
    sdC = sd(C, na.rm = TRUE),
    C = mean(C),
    sdDensity = sd(Density, na.rm = TRUE),
    Density = mean(Density),
    sdE = sd(E, na.rm = TRUE),
    E = mean(E),
    sdTransitivity = sd(Transitivity, na.rm = TRUE),
    Transitivity = mean(Transitivity),
    sdEigenvectorCentrality = sd(EigenvectorCentrality, na.rm = TRUE),
    EigenvectorCentrality = mean(EigenvectorCentrality),
  )


#### Figure 10-6 ####

set.seed(1)
#Degree
g1 <- ggplot(vitd, aes(Clustering, Degree)) +
  geom_jitter(position = position_jitter(0.2), color = "black")  + 
  geom_pointrange(aes(ymin = Degree-sdDegree, ymax = Degree+sdDegree),data = vit.summary) + theme_classic() 
# Clustering coefficient
g2 <- ggplot(vitd, aes(Clustering, C)) +
  geom_jitter(position = position_jitter(0.2), color = "black")  + 
  geom_pointrange(aes(ymin = C-sdC, ymax = C+sdC),data = vit.summary) + theme_classic() + labs(y = "Clustering coefficient")
# Number of nodes in two-hop
g3 <- ggplot(vitd, aes(Clustering, N)) +
  geom_jitter(position = position_jitter(0.2), color = "black")  + 
  geom_pointrange(aes(ymin = N-sdN, ymax = N+sdN),data = vit.summary) + theme_classic() + labs(y = "Number of nodes in two-hop")
# Density
g4 <- ggplot(vitd, aes(Clustering, Density)) +
  geom_jitter(position = position_jitter(0.2), color = "black")  + 
  geom_pointrange(aes(ymin = Density-sdDensity, ymax = Density+sdDensity),data = vit.summary) + theme_classic() 
# Number of edges in two-hop
g5 <- ggplot(vitd, aes(Clustering, E)) +
  geom_jitter(position = position_jitter(0.2), color = "black")  + 
  geom_pointrange(aes(ymin = E-sdE, ymax = E+sdE),data = vit.summary) + theme_classic() + labs(y = "Number of edges")
# Eigenvector centrality
g6 <- ggplot(vitd, aes(Clustering, EigenvectorCentrality)) +
  geom_jitter(position = position_jitter(0.2), color = "black")  + 
  geom_pointrange(aes(ymin = EigenvectorCentrality-sdEigenvectorCentrality, ymax = EigenvectorCentrality+sdEigenvectorCentrality),data = vit.summary) + theme_classic() + labs(y = "Eigenvector centrality")
# Global transitivity of two hop network
g7 <- ggplot(vitd, aes(Clustering, Transitivity)) +
  geom_jitter(position = position_jitter(0.2), color = "black")  + 
  geom_pointrange(aes(ymin = Transitivity-sdTransitivity, ymax = Transitivity+sdTransitivity),data = vit.summary) + theme_classic() + labs(y = "Global Transitivity")
grid.arrange(g1, g2, g3, g5, g6, ncol=5)


#### Figure 10-7 ####

outputall <- c()
# Simulate for each target word 
for(i in 1:nrow(vitwords)){
 targeti <- vitwords$Target[i] 
 # Set order to get 2nd, 3rd order neighbors, and so on.
  buffg <- make_ego_graph(gk1s, order=3,nodes=which(V(gk1s)$name==targeti))[[1]]
  # To plot each network---set to FALSE
  if(FALSE){ 
    V(buffg)$size <- 1
    V(buffg)$size[which(V(buffg)$name %in% vitwords[i,3:12])] <- 5 
    V(buffg)$size[which(V(buffg)$name == targeti)] <- 10
    plot(buffg, vertex.label = NA)
  }
  # Start activation at 0
  df <- data.frame(node = V(buffg)$name, 
                     activation = 0,  
                     stringsAsFactors = F)
  # Add cue word activation
  df$activation[df$node %in% as.vector(t(vitwords[i,3:12]))] <-20
  # Run spreading activation
  output <- spreadr::spreadr(start_run = df, decay = 0,
                                    retention = 0, suppress = 0,
                                    network = buffg, time = 5)
  df <- data.frame(df, time=0)
  output <- rbind(df, output)
  output <- output %>% 
    add_column(Clustering = vitwords$C[i])  %>% 
    add_column(target=targeti) %>%
    mutate(target_distance= as.numeric(distances(buffg, v=node, to=targeti))) 
 outputall <- rbind(outputall, output)
}

# gear appears as a cue with gear: high, replaced with leer
# pear is with gear: high, replaced with peer
# bray is with fray: low, replaced with brae
# flow, slow, and low are with glow: low, replaced with floe, sloe, and lo
# pie is with ply: low, replaced with pi
# seed is with side: low, replaced with cede
# slow is with slay: low, replace with sloe
# slow is with sly : low, replace with sloe
# tea is with tree: low, replace with t
# Thanks M. Vitevitch for suggesting and approving the replacements.


# Summarize activation at different distances including target word (distance = 0)
# Needs to be split because some have the same word in different target networks
datsum <- with(subset(outputall, target_distance == 0), tapply(activation, list(Clustering,time, target), sum ))
datsum <- rbind( rowMeans(datsum["high",1:6,1:30], na.rm=T), rowMeans(datsum["low",1:6,1:30], na.rm=T) )

datsum1 <- with(subset(outputall, target_distance == 1), tapply(activation, list(Clustering,time, target), sum ))
datsum1 <- rbind( rowMeans(datsum1["high",1:6,1:30], na.rm=T), rowMeans(datsum1["low",1:6,1:30], na.rm=T) )

datsum2 <- with(subset(outputall, target_distance == 2), tapply(activation, list(Clustering,time, target), sum ))
datsum2 <- rbind( rowMeans(datsum2["high",1:6,1:30], na.rm=T), rowMeans(datsum2["low",1:6,1:30], na.rm=T) )

if(max(outputall$target_distance) > 2){
  datsum3 <- with(subset(outputall, target_distance == 3), tapply(activation, list(Clustering,time, target), sum ))
  datsum3 <- rbind( rowMeans(datsum3["high",1:6,1:30], na.rm=T), rowMeans(datsum3["low",1:6,1:30], na.rm=T) )
}

# Plot Bullseye with labels
layout(matrix(c(1,2,2), 1, 3, byrow = TRUE))
par(mar=c(0,0,0,0))
plot(NULL, axes=F, type="b", xaxt="n", yaxt="n", ylab="", xlab="", xlim =c(0,1), ylim =c(0,1))
points(.5,.50, pch=16, cex = 40, col="gray90")
points(.5,.50, pch=16, cex = 30, col="gray80")
points(.5,.50, pch=16, cex = 20, col="gray50")
points(.5, .5, pch=16, cex = 10, col="gray20")
text(.5,.5, "target", col="white")
text(.5,.42, "1st", col="white")
text(.5,.35, "2nd")
text(.5,.28, "3rd")
text(.5,.22, "Beyond")
# Plot activation
par(mar=c(5,5,3,2))
x <- as.numeric(colnames(datsum))
plot(x, datsum[2,], xlab="Time-step", ylab="Activation", xlim = c(0, 5), ylim = c(0, 200), type="n", cex.axis=1.2, cex.lab = 1.5)
lines(x, datsum[1,], lty=2, col="gray20")
lines(x, datsum[2,], col = "gray20")
lines(x, datsum1[1,], lty=2, lwd=2, col="gray50")
lines(x, datsum1[2,], lty=1, lwd=2, col="gray50")
lines(x, datsum2[1,], lty=2, lwd=3, col="gray80")
lines(x, datsum2[2,], lty=1, lwd=3, col="gray80")
if(max(outputall$target_distance) > 2){
  lines(x, datsum3[1,], lty=2, lwd=4, col="gray90")
lines(x, datsum3[2,], lty=1, lwd=4, col="gray90")
}
legend(3.2, 200, legend=c("High", "Low", "Target", "1st", "2nd", "3rd"), lty=c(2,1,1,1,1, 1), lwd=c(1,1,1,2,3, 4), col=c("black", "black", "gray20", "gray50", "gray80", "gray90"))


#### Figure 10-8 ####

# All degree, for Hoosier mental lexicon
alldegs <- igraph::degree(gk1s)
# All transitivity
allcc <- igraph::transitivity(gk1s, type="local")
# Neighbors degree
neighdeg <- rep(NA, length(V(gk1s)))
# Get mean neighbor degree
for(i in 1:length(V(gk1s))){
 neighdeg[i] <- mean(alldegs[neighbors(gk1s, V(gk1s)[i])])
}

# Clip plotting
par(xpd=FALSE)
# Get data for all words
alldegcc <- data.frame(node = names(alldegs), trans=allcc, ndeg = neighdeg, mydeg = alldegs)
# Prepare plot window
par(mfrow=c(1,1))
# Plot data and make the target words orange
with(alldegcc, plot(trans, ndeg, xlab="Clustering coefficient", ylab=TeX(r'(Mean degree of neighbors)') ,cex.lab=1.2, col=ifelse(alldegcc$node %in% vitwords$Target, alpha("orange", alpha=.8), alpha("black", alpha=.2)), pch=16, cex=ifelse(alldegcc$node %in% vitwords$Target, 2, .5)))
at<-lm(ndeg~trans, data=alldegcc)
#abline(at, col="gray80")
# summary(at)


# Subset words to contain values between 23 and 25 (not inclusive) and degree > 10)
testset <- subset(alldegcc, ndeg > 23 & ndeg < 25 & mydeg > 10)
outputall <- c()
# Simulate for each word
for(i in 1:nrow(testset)){
  # Choose target node
  targ <- testset$node[i]
  # Extract 3-hop network
  buffg <- make_ego_graph(gk1s, order=3,nodes=which(V(gk1s)$name==targ))[[1]]
  # Set plot networks to FALSE (unless you want to see them)
  if(FALSE){ 
    V(buffg)$size <- 1
    V(buffg)$size[which(V(buffg)$name %in% vitwords[i,3:12])] <- 5 
    V(buffg)$size[which(V(buffg)$name == targeti)] <- 10
    plot(buffg, vertex.label = NA)
  }
  # Choose 10 neighbors and give them activation
  neebs <- as_ids(neighbors(buffg, targ))
  neighToActivate <- sample(neebs, 10)
  # Mean degree of cues
  mdegcues <- mean(igraph::degree(buffg, neighToActivate), na.rm=T)
  # Start activation at 0
  df <- data.frame(node = V(buffg)$name, 
                     activation = 0,  
                     stringsAsFactors = F)
  # Add cue word activation
  df$activation[df$node %in% neighToActivate] <-20
  # Run spreading activation
  output <- spreadr::spreadr(start_run = df, decay = 0,
                                    retention = 0, suppress = 0,
                                    network = buffg, time = 5)
  # Summarize
  df <- data.frame(df, time=0)
  output <- rbind(df, output)
  output <- output %>% 
    add_column(target=targ) %>%
    add_column(targettrans = testset$trans[i]) %>%
    mutate(target_distance= as.numeric(igraph::distances(buffg, v=node, to=targ))) %>%
    add_column(meanDegreeCues=mdegcues)
 outputall <- rbind(outputall, output)
}

# Does activation at t=1 correlate with transitivity: no
t1targetact <- subset(outputall, time ==1 & node==target)
# with(t1targetact, plot(targettrans, activation))
at1 <- with(t1targetact, lm(activation~targettrans+meanDegreeCues))
# No effect of transitivity
at2 <- with(t1targetact, lm(activation~targettrans))


# Does activation at t=1:5 correlate with trans: no
t1targetactt15 <- subset(outputall, node==target)
m <- median(outputall$targettrans)
t1t <- t1targetactt15 %>% mutate(Clustering=ifelse(targettrans > m, "High", "Low"))
t1t2 <- t1t %>% group_by(target) %>% dplyr::summarise(AUC=sum(activation)) 
t1t2 <- t1t2 %>% left_join(t1t, by = "target")
t1t2 <- t1t2 %>% filter(time==0)
at3 <- with(t1t2, lm(AUC~targettrans+meanDegreeCues))

at4 <- with(t1t2, lm(AUC~targettrans))

# Make a nice table: Thanks to Hlavac, Marek (2022). stargazer: Well-Formatted Regression and Summary Statistics Tables. R package version 5.2.3. https://CRAN.R-project.org/package=stargazer 
 
stargazer(at1, at2, at3, at4,title="Activation of target node.", align=TRUE, type="latex",column.labels = c("t=1", "t=1", "t=1-5", "t=1-5"), covariate.labels=c("C","Neighbor's <k>","Intercept"), omit.stat=c("LL","ser","f"), no.space=TRUE, dep.var.labels = c("Activation", "Cumulative activation"), header=FALSE, label = 'tab:testsimPlot1')



#### Figure 10-9 ####
# Plotting as for Figure 10.7 but for all target words
outputall <- outputall %>% mutate(Clustering=ifelse(targettrans > .307, "High", "Low"))

datsum <- with(subset(outputall, target_distance == 0), 
               tapply(activation, list(Clustering,time, target), sum ))
datsum <- rbind( rowMeans(datsum["High",1:6,1:242], na.rm=T), 
                 rowMeans(datsum["Low",1:6,1:242], na.rm=T) )

datsum1 <- with(subset(outputall, target_distance == 1), 
                tapply(activation, list(Clustering,time, target), sum ))
datsum1 <- rbind( rowMeans(datsum1["High",1:6,1:242], na.rm=T), 
                  rowMeans(datsum1["Low",1:6,1:242], na.rm=T) )

datsum2 <- with(subset(outputall, target_distance == 2), 
                tapply(activation, list(Clustering,time, target), sum ))
datsum2 <- rbind( rowMeans(datsum2["High",1:6,1:242], na.rm=T), 
                  rowMeans(datsum2["Low",1:6,1:242], na.rm=T) )

if(max(outputall$target_distance) > 2){
  datsum3 <- with(subset(outputall, target_distance == 3), 
                  tapply(activation, list(Clustering,time, target), sum ))
  datsum3 <- rbind( rowMeans(datsum3["High",1:6,1:242], na.rm=T), 
                    rowMeans(datsum3["Low",1:6,1:242], na.rm=T) )
}

layout(matrix(c(1,2,2), 1, 3, byrow = TRUE))
par(mar=c(0,0,0,0))
plot(NULL, axes=F, type="b", xaxt="n", yaxt="n", ylab="", xlab="", xlim =c(0,1), ylim =c(0,1))
points(.5,.50, pch=16, cex = 40, col="gray90")
points(.5,.50, pch=16, cex = 30, col="gray80")
points(.5,.50, pch=16, cex = 20, col="gray50")
points(.5, .5, pch=16, cex = 10, col="gray20")
text(.5,.5, "target", col="white")
text(.5,.42, "1st", col="white")
text(.5,.35, "2nd")
text(.5,.28, "3rd")
text(.5,.22, "Beyond")
par(mar=c(5,5,3,2))
x <- as.numeric(colnames(datsum))
plot(x, datsum[2,], xlab="Time-step", ylab="Activation", xlim = c(0, 5), ylim = c(0, 200), type="n", cex.axis=1.2, cex.lab = 1.5)
lines(x, datsum[1,], lty=2, col="gray20")
lines(x, datsum[2,], col = "gray20")
lines(x, datsum1[1,], lty=2, lwd=2, col="gray50")
lines(x, datsum1[2,], lty=1, lwd=2, col="gray50")
lines(x, datsum2[1,], lty=2, lwd=3, col="gray80")
lines(x, datsum2[2,], lty=1, lwd=3, col="gray80")
if(max(outputall$target_distance) > 2){
  lines(x, datsum3[1,], lty=2, lwd=4, col="gray90")
lines(x, datsum3[2,], lty=1, lwd=4, col="gray90")
}
legend(3.2, 200, legend=c("High", "Low", "Target", "1st", "2nd", "3rd"), lty=c(2,1,1,1,1, 1), lwd=c(1,1,1,2,3, 4), col=c("black", "black", "gray20", "gray50", "gray80", "gray90"))


wlist <- c("ALLERGY", "ANALOGY", "A\\_L\\_\\_GY",
           "BAGGAGE", "BRIGADE", "B\\_G\\_A\\_E",
           "CHARITY", "CHARTER", "CHAR\\_T\\_",
           "VOLTAGE", "VOYAGER", "VO\\_\\_AGE")

wlist <-matrix(wlist, ncol=3, byrow=TRUE)
colnames(wlist) <- c("Target", "Negative prime", "Fragment")

# Make the table
kable(wlist,row.names=FALSE, booktabs = T, escape = FALSE, caption = "Targets, negative primes, and word fragments from Smith and Tindell, 1997.", digits=3) %>%
kable_classic(full_width = F, html_font = "Cambria") 


# Make table of words and targets
kable(vitwords,row.names=FALSE, booktabs = T, escape = FALSE, caption = "Target and cue words from Vitevitch et al., 2012. Each cue is a string-edit distance of one from the target---meaning one phoneme difference.", digits=3) %>%
kable_classic(full_width = F, html_font = "Cambria") %>%
  add_header_above(c(" ", " ", "Cues" = 10) )

