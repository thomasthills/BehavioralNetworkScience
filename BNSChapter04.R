knitr::opts_chunk$set(echo = FALSE)
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(comment = NA)
knitr::opts_chunk$set(warning = FALSE)
options(knitr.table.format = "latex")

### Behavioral Network Science
### Language, Mind, and Society
# By Thomas T. Hills

# Code to reproduce figures and analyses for

#### Chapter 4: Zipf's Law of Meaning: The Degree Distribution of the Mind ####

library(igraph)
library(tidyverse)
library(tidytext)
library(gridExtra)# Load gridExtra package
library(geostats)# for Cantor Set
library(poweRlaw)
library(latex2exp)



#### Figure 4-1 ####

# Original data is downloadable from: https://0-www-science-org.pugwash.lib.warwick.ac.uk/doi/full/10.1126/science.1240064#supplementary-materials
# The data used is schichdata2_akl.xlsx  which I have here converted/saved to csv within Excel. 

akl <- read.csv("SampleDataFilesBNS/schichdatas2_akl.csv")

# Filter out only those labeled as FineArts
aklfa <- akl %>% filter(FineArts == 1)

# Get birth and death locations as edgelist
aklafa.edgelist <- aklfa %>% dplyr::select(BLocLabel, DLocLabel)
el <- as.matrix(aklafa.edgelist) # transform from data.frame to matrix
# Create graph from matrix edge list
gs <- graph_from_edgelist(el, directed=T) 
# Fix some names so they look more familiar
V(gs)$name[V(gs)$name == "Rom"] <- "Rome"
V(gs)$name[V(gs)$name == "Moskau"] <- "Moscow"
# Remove self loops and multi-edges
gss <- igraph::simplify(gs)
# Limit to n nodes, top 500 degree nodes
n = 500
set.seed(19)
# Randomly sample a subset of all but n nodes with degree less than 500 for deletion
toDelete <- sample(V(gss)[igraph::degree(gss)<500], length(V(gss))-n)
# Delete the list from above
newgraph <- igraph::delete_vertices(gss, toDelete)
# Delete all but giant component
newgraph <- igraph::delete_vertices(newgraph, igraph::components(newgraph)$membership!=1)
# The above produces a graph with very high degree nodes and a subsample of low degree nodes.
##plot(newgraph, vertex.size = degree(newgraph)^(1/1.5), edge.arrow.size = .1, vertex.label.cex =(ifelse(log(degree(newgraph))>2, 1, 0))+.001, layout=layout_nicely(newgraph, dim =2))

# Compute indegree and outdegree
gsid <- igraph::degree(gs, mode = "in")
gsout <- igraph::degree(gs, mode = "out")
# Make data frame from in and outdegree for plotting
gsdata <- data.frame("place" = names(gsid),"indegree"= gsid,"outdegree"= gsout)
# Make two frames for plotting below
par(mfrow=c(1,2))
par(mar=c(4,4,0.5,2))
x = c(1,3600)
with(gsdata, plot(outdegree, indegree,cex=.3,pch=16, log="xy", xlab = "Birth source (outdegree)", ylab="Death attractor (indegree)", cex.lab = .8, col = alpha("black", alpha=.3)))
lines(x,x)
# Get and adjust location names for plotting on figure 
locations <- with(gsdata[c("Paris", "Berlin", "Rome","London", "Moscow"),],data.frame(x=outdegree*.7,y=indegree*.9, place))
locations[locations$place=="Rome",2] <- 2200
locations[locations$place=="Rome",1] <- 350
locations[locations$place=="Berlin",1] <- 280
locations[locations$place=="London",1] <- 350
locations[locations$place=="Moscow",2] <- 850
# Place text
with(locations, text(x,y, place, cex = .7))

# Plot graph
par(mar=c(0,0,0,0))
E(newgraph)$color = alpha("gray", alpha = .5)
plot(newgraph, vertex.size = igraph::degree(newgraph)^(1/1.5), edge.arrow.size = .2, vertex.label.cex =(ifelse(log(igraph::degree(newgraph))>3.2, .8, 0))+.001, layout=layout_with_fr(newgraph), vertex.color = "white", vertex.label.color = "black", vertex.label.dist = 1.55,vertex.label.family="Helvetica")




#### Figure 4-2 ####

## Data is download War and Peace by Leo Tolstoy plain text from https://www.gutenberg.org/ebooks/2600

scannedWAP <- scan('SampleDataFilesBNS/warAndPeace.txt', what = " ", quote = NULL)
# Convert scanned text to a tibble to use tidytext features
WAP <- tibble(line=1:length(scannedWAP), text = scannedWAP)
# Clean up words
WAP <- WAP %>% unnest_tokens(word, text)
# Count words
wapcount <- WAP %>% dplyr::count(word, sort=TRUE)
# Put counts next to words in ranked data frame
wapcount <- wapcount %>% add_column(Rank = 1:nrow(wapcount))
# Only take words with count > 10
wapcut <- wapcount %>% filter(n > 10)
# Very basic linear regression of logged values
a1 <- with(wapcut, lm(log(n)~log(Rank))) 
##summary(a1)

# Set sizes for text scaled to rank
sizel <- 5/(wapcut$Rank^.6)
# Only keep the largest
sizel <- ifelse(sizel > 1.55, sizel+1, 0)
# Plot using ggplot (in tidyverse package)
g1 <- ggplot(data = wapcut, aes(x = Rank, y = n, label = word)) +
  geom_point() +
  theme_classic() + theme_classic() + geom_text(hjust = -.5, size = sizel, vjust= .3, srt = 0) + 
  labs(x = "Rank", y = "Frequency") 


g2 <- ggplot(data = wapcut, aes(x = Rank, y = n, label = word)) +
  geom_point() +
  scale_y_log10(limits=c(10,100000)) +
  annotation_logticks(sides = "lb") +
  scale_x_log10(limits=c(1,4000)) + theme_classic() + geom_text(hjust = -.7, size = sizel, vjust= .3, srt = 45) +
  labs(x = "Rank", y = "Frequency") 
 
gridExtra::grid.arrange(g1, g2, ncol=2)



#### Figure 4-3 ####

## Code from Martin Stefan, https://rpubs.com/mstefan-rpubs/fractals
## code controls line length, angle, and position, to specified branch length
tree <- function(line0, angle=30, reduce=.7, randomness=0) {
  
  # Angles and randomness
  angle1 <- angle+rnorm(1,0,randomness)  # left branch
  angle2 <- -angle+rnorm(1,0,randomness) # right branch
  
  # New branches
  line1 <- newLine(line0, angle=angle1, reduce=reduce)   
  line2 <- newLine(line0, angle=angle2, reduce=reduce)
  
  # Store in matrix and return
  mat <- matrix(c(line1,line2), byrow=T, ncol=4)
  return(mat)
  
}


iterate <- function(object, ifun, ...) {
  linesList <- vector("list",0)
  for(i in 1:nrow(object)) {
    old_line <- matrix(object[i,], nrow=1)
    new_line <- ifun(old_line, ...)
    linesList[[length(linesList)+1]] <- new_line
  }
  new_object <- do.call(rbind, linesList)
  return(new_object)
}


newLine <- function(line, angle, reduce=1) {
  
  x0 <- line[1]
  y0 <- line[2]
  x1 <- line[3]
  y1 <- line[4]
  
  dx <- unname(x1-x0)                      # change in x direction
  dy <- unname(y1-y0)                      # change in y direction
  l <- sqrt(dx^2 + dy^2)                   # length of the line
  
  theta <- atan(dy/dx) * 180 / pi          # angle between line and origin
  rad <- (angle+theta) * pi / 180          # (theta + new angle) in radians
  
  coeff <- sign(theta)*sign(dy)            # coefficient of direction
  if(coeff == 0) coeff <- -1
  
  x2 <- x0 + coeff*l*cos(rad)*reduce + dx  # new x location
  y2 <- y0 + coeff*l*sin(rad)*reduce + dy  # new y location
  return(c(x1,y1,x2,y2))
  
}
## Function to create empty canvas
emptyCanvas <- function(xlim, ylim, bg="gray20") {
  par(mar=rep(1,4), bg=bg)
  plot(1, 
       type="n", 
       bty="n",
       xlab="", ylab="", 
       xaxt="n", yaxt="n",
       xlim=xlim, ylim=ylim)
}

drawObject <- function(object, col="white", lwd=1) {
  invisible(apply(object, 1, drawLine, col=col, lwd=lwd))
}

drawLine <- function(line, col="white", lwd=1) {
  segments(x0=line[1], 
           y0=line[2], 
           x1=line[3], 
           y1=line[4], 
           col=col,
           lwd=lwd)
}


## Example: recursive tree (after ten iterations)
par(mfrow=c(1,2))
fractal <- matrix(c(0,0,0,10), nrow=1)
emptyCanvas(xlim=c(-30,30), ylim=c(0,35))
drawObject(fractal)
for(i in 1:10) {
  fractal <- iterate(fractal, ifun=tree, angle=23)
  drawObject(fractal)
}

# Plot Cantor set using geostats package
plot(c(0,1),y=c(0,1),type='n',bty='n',ann=FALSE,xaxt='n',yaxt='n',xpd=NA)
geostats::cantor(n=0,Y=1.00,plot=TRUE,add=TRUE, col="white")
geostats::cantor(n=1,Y=0.75,plot=TRUE,add=TRUE, col="white")
geostats::cantor(n=2,Y=0.50,plot=TRUE,add=TRUE, col="white")
geostats::cantor(n=3,Y=0.25,plot=TRUE,add=TRUE, col="white")
geostats::cantor(n=4,Y=0.00,plot=TRUE,add=TRUE, col="white")




#### Figure 4-4 ####

set.seed(1)
par(mfrow=c(1,1))
par(mar = c(0,0,0,0))

# To download data follow these instructions: 
# Go to https://smallworldofwords.org/en/project/research#download
# Download from 'English' data  SWOW-EN2008 assoc. strengths (R1) [3Mb]
# Unzip to the CSV file and place in the SampleDataFilesBNS folder
# There is a request for some information, but providing it is optional
# At the request of SWOW, I will not provide the free association data directly.

swow <- read.delim("SampleDataFilesBNS/strength.SWOW-EN.R1.csv", sep = "\t")

# Edge list of forward association strengths
swow <- tibble(swow)
# Require at least x people to produce associations, this can be changed
swowT <- swow %>% filter(R1 >=2)
# Make graph from two columns of edge list
sg <- graph_from_edgelist(as.matrix(swowT[,1:2]))

# Here is the weighted network, if needed
# sgw <- graph_from_data_frame(swowT[,c(1,2,5)], directed = FALSE)
# E(sgw)$weight <- E(sgw)$R1.Strength


# Remove loops
sg <- igraph::simplify(sg)
V(sg)$indegree <- igraph::degree(sg, mode = "in")

# University of South Florida Free Association Norms
usf <- read.delim("SampleDataFilesBNS/USF.txt", sep = ",")

# Edge list
usf <- tibble(usf)
# Make graph
usfg <- graph_from_edgelist(as.matrix(usf[,2:3]))

usfg <- igraph::simplify(usfg)
usfgdeg <- igraph::degree(usfg)
##table(usfgdeg)

# Plot subset of swow
# Remove nodes from swow with degree lower than or equal to ...
sgcut <- sg %>% igraph::delete.vertices(which(igraph::degree(sg, mode = "in") <= 60))
# Remove isolates
sgcut <- sgcut %>% igraph::delete.vertices(which(igraph::degree(sgcut, mode = "in")==0))
# Set network layout
l =  layout_with_drl(sgcut, options=list(simmer.attraction = 0, crunch.iterations = 0, simmer.attraction = 0 ))
# Remove labels with low indegree
cutlabels <- which(V(sgcut)$indegree <=130)
V(sgcut)$name[cutlabels] <- NA

#dev.off() # reset plot display from previous black background, if needed

plot(sgcut, vertex.label.cex = .43, edge.arrow.size = .1,  vertex.size = V(sgcut)$indegree/20, vertex.color = "white", layout = l, vertex.label.family="Helvetica", vertex.label.color="black")

## Accounting (uncomment to see it at work)

# # 5 highest degree nodes in full network:
# head(igraph::degree(sg, mode = "in")[order(igraph::degree(sg, mode = "in"), decreasing = TRUE)])
# # Nodes ordered by in/out degree
# igraph::degree(sg, mode = "in")[order(igraph::degree(sg, mode = "in"), decreasing = TRUE)]
# igraph::degree(sg, mode = "out")[order(igraph::degree(sg, mode = "out"), decreasing = TRUE)]
# # Nodes ordered by in/out degree for plotted subgraph
# igraph::degree(sgcut, mode = "in")[order(igraph::degree(sgcut, mode = "in"), decreasing = TRUE)]
# igraph::degree(sgcut, mode = "out")[order(igraph::degree(sgcut, mode = "out"), decreasing = TRUE)]

# How many words have non-zero outdegree
# sum(igraph::degree(sg, mode = "out")[order(igraph::degree(sg, mode = "out"), decreasing = TRUE)] != 0)
# How many cues
# length(unique(swow$cue)) # 12217
# How many words have non-zero indegree 
# sum(igraph::degree(sg, mode = "in")[order(igraph::degree(sg, mode = "in"), decreasing = TRUE)] != 0)
# How many cues
# length(unique(swow$response))


 
#### Figure 4-5 ####

# Three figures with total, in, and out degree
kind = c("total", "in", "out")
par(mfrow=c(2,3))
i = kind[1] 
# Compute degree
  gstot <- igraph::degree(sg, mode=i)#/sum(igraph::degree(sg, mode=i))
  # Reverse rank
  ranko <- rank(-gstot)
  # Make data frame
  gsd <- data.frame(gstot, ranko)
  # Uncomment to see the top items
  #head(gsd[order(ranko),])
  par(mar=c(4,4,0.5,2))
  plot(ranko, gstot, log="xy", xlab = "Rank", ylab = "P(Total degree)")

# (Commented as above )
i = kind[2] 
  gstot <- igraph::degree(sg, mode=i)#/sum(igraph::degree(sg, mode=i))
  ranko <- rank(-gstot)
  gsd <- data.frame(gstot, ranko)
  #head(gsd[order(ranko),])
  par(mar=c(4,4,0.5,2))
  plot(ranko, gstot, log="xy", xlab = "Rank", ylab = "P(Indegree)")
  # error is from attempting to plot log(0) items, which are removed

i = kind[3] 
  gstot <- igraph::degree(sg, mode=i)#/sum(igraph::degree(sg, mode=i))
  ranko <- rank(-gstot)
  gsd <- data.frame(gstot, ranko)
  #head(gsd[order(ranko),])
  par(mar=c(4,4,0.5,2))
  plot(ranko, gstot, log="xy", xlab = "Rank", ylab = "P(Outdegree)")

  
# South Florida free association norms
kind = c("total", "in", "out")
i = kind[1] 
  gstot <- igraph::degree(usfg, mode=i)#/sum(igraph::degree(usfg, mode=i))
  ranko <- rank(-gstot)
  gsd <- data.frame(gstot, ranko)
  #head(gsd[order(ranko),])
  par(mar=c(4,4,0.5,2))
  plot(ranko, gstot, log="xy", xlab = "Rank", ylab = "P(Total degree)")

i = kind[2] 
  gstot <- igraph::degree(usfg, mode=i)#/sum(igraph::degree(usfg, mode=i))
  ranko <- rank(-gstot)
  gsd <- data.frame(gstot, ranko)
  #head(gsd[order(ranko),])
  par(mar=c(4,4,0.5,2))
  plot(ranko, gstot, log="xy", xlab = "Rank", ylab = "P(Indegree)")

i = kind[3] 
  gstot <- igraph::degree(usfg, mode=i)#/sum(igraph::degree(usfg, mode=i))
  ranko <- rank(-gstot)
  gsd <- data.frame(gstot, ranko)
  #head(gsd[order(ranko),])
  par(mar=c(4,4,0.5,2))
  plot(ranko, gstot, log="xy", xlab = "Rank", ylab = "P(Outdegree)")

  
  


#### Figure 4-6 ####

# A useful introduction to the poweRlaw package can be found here:
# https://cran.r-project.org/web/packages/poweRlaw/vignettes/a_introduction.pdf

par(mar=c(5,5,2,2))
par(mfrow=c(1,2))
# Small World of Words
# Limit the network to nodes with positive outdegree
sgs <- subgraph(sg, which(igraph::degree(sg, mode="out")>=1 ))
gstot <- igraph::degree(sgs, mode="total") # 21555
gstot <- gstot[gstot !=0] # 20603
# igraphs method
fit1 <- fit_power_law(gstot, implementation='plfit')
# 
m_pl = displ$new(gstot)
est = estimate_xmin(m_pl)
m_pl$setXmin(est)

plot(m_pl, xlab = "<k>", ylab = TeX(r'(P(i \geq x))'), main = "SWOW FAN")
lines(m_pl, col = 2, lty=2, lwd=2)

# South Florida Norms 
# Limit the network to nodes with positive outdegree
usfgs <- subgraph(usfg, which(igraph::degree(usfg, mode="out")>=1 ))
gstot <- igraph::degree(usfgs, mode="total")
gstot <- gstot[gstot !=0] 

# igraphs method
fit1 <- fit_power_law(gstot, implementation='plfit')
# poweRlaw
m_pl = displ$new(gstot)
est = estimate_xmin(m_pl)
m_pl$setXmin(est)

 plot(m_pl, xlab = "<k>", ylab = TeX(r'(P(i \geq x))'), main = "USF FAN")
lines(m_pl, col = 2, lty=2, lwd=2)

# Correlation between gsin and gsout
## # ## cor.test(gsin, gsout)

 


#### Figure 4-7 ####

set.seed(1)
n <- length(V(sg))
p <- graph.density(sg)
## ER random
gfs <- sample_gnp(n,p)
## Preferential attachment
gpa <- sample_pa(n, m=3, power=1, out.pref=TRUE, zero.appeal=1, directed=FALSE, algorithm="bag")

## forest.fire.game
ffg <- forest.fire.game(n, fw.prob=.34)
##graph.density(ffg)
##graph.density(gfs)
##graph.density(gpa)

set.seed(1)
## grow_DD
n =  1 # length(V(sg))
pdd = .2 
gdd <- make_empty_graph(2, directed = FALSE)
gdd <- gdd %>% add_edges(c(1,2))
for(i in 3:n){
 dupli.list <- V(gdd) 
 node.to.duplicate <- sample(dupli.list, 1, prob = igraph::degree(gdd)+.0001)
 gddam <- as_adjacency_matrix(gdd, sparse=F)
 model <- gddam[node.to.duplicate,]
 edges.to.add <- which(model == 1)
 gdd <- gdd %>% add_vertices(1, color = "white", name = paste(i, "-", node.to.duplicate))
 for(j in 1:length(edges.to.add)){
   if(runif(1) <= pdd){
    gdd <- gdd %>% add_edges(c(edges.to.add[j],i))
   }
 }
}


### Figures and estimates

ger <- igraph::degree(gfs, mode="total")
gpa <- igraph::degree(gpa, mode="total")
gff <- igraph::degree(ffg, mode="total")

ger <- ger[which(ger>0)]
 m_pl = displ$new(ger)
est = estimate_xmin(m_pl)
m_pl$setXmin(est)
m_pl_ger <- m_pl

gpa <- gpa[which(gpa>0)]
 m_pl = displ$new(gpa)
est = estimate_xmin(m_pl)
m_pl$setXmin(est)
m_pl_gpa <- m_pl

gff <- gff[which(gff>0)]
 m_pl = displ$new(gff)
est = estimate_xmin(m_pl)
m_pl$setXmin(est)
m_pl_gff <- m_pl

par(mfrow=c(1,3))
 plot(m_pl_ger, xlab = "<k>", ylab = TeX(r'(P(i \geq x))'), main = "ER")
 plot(m_pl_gpa, xlab = "<k>", ylab = TeX(r'(P(i \geq x))'), main = "Preferential Attachment")
 plot(m_pl_gff, xlab = "<k>", ylab = TeX(r'(P(i \geq x))'), main = "Forest Fire", ylim = c(.00001,1))
 


