knitr::opts_chunk$set(echo = FALSE)
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(comment = NA)
knitr::opts_chunk$set(warning = FALSE)
options(knitr.table.format = "latex")

### Behavioral Network Science
### Language, Mind, and Society
# By Thomas T. Hills

# Code to reproduce figures and analyses for

#### Chapter 7 ####
rm(list=ls()) # to good health
library(igraph)
library(tidyverse)
library(latex2exp)
library(ggraph)


#### Figure 7-1 ####

# To make networks replicable
set.seed(20)
# Produces a multi-panel figure
layout(matrix(c(1,2,3,4,5,5,5,5,5,5,5,5), 3, 4, byrow = TRUE))
# List of probabilities for rewiring in small world
pv <- c(0,.01, .1, 1)
# Set margins in figure
par(mar=c(3,3,3,3))
# Number of nodes
n=50
# For loop through probabilities
for(i in pv){
  # Make small world
  gsw <- sample_smallworld(1,n,2, p=i)
  # Set layout
  l = layout_with_fr(gsw)
  # Plot
  plot(gsw, layout = l, vertex.color = "white", vertex.label = NA, vertex.size = 8, edge.width = .8 )
  # Compute local clustering coefficient
  mc <- round(mean(igraph::transitivity(gsw, type="localundirected", isolates = "Zero")),1)
  # Get average path length
  md <- round(mean_distance(gsw, directed = FALSE),1)
  #text(-1,1, paste("p=", i," \n CC=", mc, "\n ASPL = ",md))
  # Print the info
  mtext(paste("p=", i," \n C=", mc, "\n ASPL = ", md), side = 3, cex=.6)
  
}

set.seed(1)
par(mar=c(5,5,0,2))
## Create log scaled probabilities value
logsc <- 1:10
xx <- exp(logsc)
# To normalize the values
yy <- (xx - min(xx))/(max(xx)-min(xx))
# Sims per probability
S = 100
# Number of nodes
network_size = 400 
# Edge number
average_edge_number =4 
# Probabilitiy of rewiring vector for for-loop
prob_of_rewiring <- yy[2:10]
# Buffer for output
output_sims <- matrix(NA, nrow=length(prob_of_rewiring),ncol=3)
# Get initial values  
g0 <-  sample_smallworld(1, network_size, average_edge_number, 0)
C0 <- igraph::transitivity(g0, type="localaverage")
L0 <- mean_distance(g0, directed = FALSE)
# Run through probabilities
for(p in 1:length(prob_of_rewiring)){
  gL <- c() # initialize lengths
  gT <- c() # initialize transitivity
  outputg1 <- matrix(NA, nrow=S, ncol=3) # initialize infection data
  # For each sim repeat the following
  for(i in 1:S){
    # Build graph
    g1<- sample_smallworld(1, network_size, average_edge_number, prob_of_rewiring[p])
    # Compute values and concatenate them on to vector
    gL <- c(gL, mean_distance(g1, directed = FALSE))
    gT <- c(gT, igraph::transitivity(g1, type="localaverage"))
  }
  # Take mean of values for each p
  output_sims[p,] <- c(prob_of_rewiring[p], mean(gL)/L0, mean(gT)/C0) 
}

# Make it a data frame
output_sims <- data.frame(output_sims)
# Label the columns
names(output_sims) <- c("p", "ASPL", "C")
# Plot output
with(output_sims, plot(p, C, log="x", ylim = c(0, 1), xlim = c(0.0001, 1), xlab = "Probability (p)", ylab = "Relative size", pch=16, xaxt="n"))
# Set axis labels
axis(1, c(.0001, .001, .01, .1, 1), labels = c(".0001", ".001", ".01", ".1", "1")) 
# Make pretty lines and points
with(output_sims, lines(p, ASPL,  lwd = 2, lty = 2))
with(output_sims, points(p, ASPL, pch = 16,  lwd = 2, lty = 2))
with(output_sims, lines(p, C,  pch=3, lwd = 3, lty = 1))
# Add legend
legend(.0001, .4, legend=c("C(p)/C(0)", "L(p)/L(0)"), lty = c(1,2), lwd = 2)




## Load Data

# This data can be downloaded from WordBank here: http://wordbank.stanford.edu/data?name=item_data
# I provide a version here, but it should **not** be considered the up-to-date version from WordBank as this data was downloaded in 2018 and used in the article Jimenez and Hills 2022 (Jiménez, E., & Hills, T. T. (2022). Semantic maturation during the comprehension‐expression gap in late and typical talkers. Child Development, 93(6), 1727-1743.)

wb <- read.csv(file = "SampleDataFilesBNS/instrument_data_SENTENCES.csv")

# To download the SWOW data follow these instructions (also used in Chapter 4 and 5): 
# go to https://smallworldofwords.org/en/project/research#download
# Download from 'English' data  SWOW-EN2008 assoc. strengths (R1) [3Mb]
# Unzip to the CSV file and place in the SampleDataFilesBNS folder
# There is a request for some information, but providing it is optional
# At the request of SWOW, I will not provide the free association data directly.

swow <- read.delim("SampleDataFilesBNS/strength.SWOW-EN.R1.csv", sep = "\t")


#### Figure 7-2 ####

set.seed(1)
# These bounds determine which children will be compared (dotted lines in Figure 7.3)
lowerbound <- 150
upperbound <- 200

# How many children
num_children <- length(unique(wb$data_id)) # 5520 as in Jimenez - Hills
## List of categories to keep (most of nouns)
cat_keep <- c("animals", "vehicles", "toys", "food_drink", "clothing", "body_parts", "household", "furniture_rooms", "outside", "people")
## Keep only categories above
wbs <- subset(wb, category %in% cat_keep) 
## Limit to value == produces, only keep words kids produce
wbs <- subset(wbs, value == "produces")

vocabwords <- unique(wbs$definition)
num_words_wbs <- length(vocabwords) # 341 words
num_childrens <- length(unique(wbs$data_id)) # 5520 kids

### Import small world of words then reduce network to size of vocabulary

swow <- tibble(swow)
swowT <- swow %>% filter(R1 >=2)
sg <- graph_from_edgelist(as.matrix(swowT[,1:2]), directed = FALSE)
sg <- igraph::simplify(sg) # undirected and unweighted

### Get intersection of swow and wordbank cdi. This will remove polysemous words with parentheses, e.g., 'chicken (food)'

interlex <- intersect(swow$response, vocabwords)

## Make subgraph of swow with only cdi words
vids <- which(V(sg)$name %in% interlex)
sgcdi <- subgraph(sg, vids) ## childrens full lexical network with 303 nodes
par(mfrow=c(1,1))
par(mar=c(1,1,1,1))
#plot(sgcdi, edge.arrow.size = .1, vertex.label.cex = .8, vertex.size = .1, vertex.label.color = "black", layout = layout_with_lgl(sgcdi, maxiter=200))


V(sgcdi)$label = V(sgcdi)$name

# In the book I saved the figure and then uploaded it, so this will look different
ggraph(sgcdi, layout = "nicely") +
	 geom_edge_link0(edge_colour = alpha("#A8A8A8", .9),
    edge_width = 0.5, edge_alpha = 1) +
	 geom_node_point(fill = alpha("#000000", .3), colour = alpha("#000000", .5),
    size = 1, stroke = 0.3, shape = 21) +
	 geom_node_text(aes(label = name), colour = "#000000",
    size = 3, family = "serif", repel = TRUE) +
	 theme_graph() +
	 theme(legend.position = "none")

# Here is how to upload  saved figure
# knitr::include_graphics(path = "./images/Figure7-2.pdf")


#### Figure 7-3 ####

## Reduce wbs (child word data) to only words in swow
wbswow <- subset(wbs, definition %in% interlex)

## Need age and vocab count for each
out <- with(wbswow, tapply(definition, list(data_id, age), length)) # count vocab sizes for each child at each month
## Make into data from
out <- data.frame(out)
## Add rownamesand pivot to long form with months in one column
vocount <- out %>% tibble::rownames_to_column(var = "Row") %>%  pivot_longer(-Row,names_to="age", values_to="vocab_num")
## Remove na rows
vocount <- subset(vocount, !is.na(vocab_num))
## Cut out the month from the string
vocount$age <- as.numeric(substr(vocount$age, 2,3))

## Compute the top and bottom 10% for an age
months <- sort(unique(wbs$age)) # from 16 to 30 months
## Separate into below 20% and above 20% for each month
late <- c()
early <- c()
for(i in 1:length(months)){
  # Find quantiles for each month
 q <- quantile(subset(vocount, age == months[i])$vocab_num, probs = seq(0,.9, .1)) 
 # Assign quantiles for each, <= 20% or >=80% 
 late <- c(late, subset(vocount, age == months[i] & vocab_num <= q[3])$Row)
 early <- c(early, subset(vocount, age == months[i] & vocab_num >= q[9])$Row)
}
# Create labels for age groups
agegroups <- rep(1, nrow(vocount))
agegroups[vocount$Row %in% late] <-2 # late
agegroups[vocount$Row %in% early] <- 3 # early

# Figure margins
par(mar=c(5,5,4,4))
# Plot
with(vocount, plot(jitter(age), vocab_num, col = alpha("black", 0.3), ylab = "Vocabulary size", xlab = "Age (months)", pch = agegroups))
# Keep lines in bounds
par(xpd=FALSE)
# Add lines
abline(h = upperbound, lty = 2, lwd = 2)
abline(h = lowerbound, lty = 2, lwd = 2)



# Limit groups to those in the right vocabulary range 
lateGroup <- subset(vocount, Row %in% late & vocab_num >= lowerbound & vocab_num <= upperbound)
earlyGroup <- subset(vocount, Row %in% early & vocab_num >= lowerbound & vocab_num <= upperbound)

# Compute how many in each group
# nrow(lateGroup)
# nrow(earlyGroup)
## This is useful to check to make sure the distributions are sufficiently overlapping
## hist(lateGroup$vocab_num)
## hist(earlyGroup$vocab_num)


##  Function to produce network and stats 
##  for each child in lateGroup and earlyGroup
##  Small world function to compute metrics for each child's network
compute_smallworlds <- function(graph){
  g_density <- edge_density(graph)
  nodes <- length(V(graph))
  edges_count <- length(E(graph))
  # Observed metrics 
  Cobs <- mean(igraph::transitivity(graph, type="local"), na.rm=T)
  Lobs <- mean_distance(graph, directed = FALSE)
  # Simulations
  sims = 100
  # Output buffer
  output <- matrix(0, nrow=sims, ncol = 2)
  # Run simulations
  for(i in 1:sims){
    # Make random graph
      bufg <- erdos.renyi.game(nodes,g_density, type="gnp", directed = FALSE, loops = FALSE)
      # Get metrics
      output[i, 1] <- mean(igraph::transitivity(bufg, type="local"), na.rm=T)
      output[i, 2] <- mean_distance(bufg, directed = FALSE)
  }
 
  # Find mean for each measure from simulations
  crand <- mean(output[,1])
  lrand <- mean(output[,2])
  # Compute Small Wolrd Index
  SWI = (Cobs / Lobs) / ( crand / lrand)  
  # Make lattice networks:  the aim is to create a lattice network 
  # With the same density as observed child's network.
  # Compute edges per node rounded up
  edges_per_node <- ceiling(edges_count/nodes) 
  # Make it at least 2
  edges_per_node <- ifelse(edges_per_node < 2, 2, edges_per_node)
  # Make lattice with rounded up edges per node
  latmat <- sample_smallworld(1, nodes, edges_per_node, p=0) # this will overshoot slightly 
  # Get number of edges
  El <- E(latmat)
  # Difference in number of edges between observed and lattice
  difgraphEl <- length(El) - edges_count 
  # Make dummy vector with edge count
  x <- 1:length(El)
  # Data output buffer for metrics  
  outputlat <- matrix(0, nrow=sims, ncol = 2)
  # For each of simulations 
  for(i in 1:sims){
    # Delete random edges to make density the same
      buflat <- latmat %>% delete_edges(sample(x, difgraphEl)) # delete the difference
      # Compute metrics
      outputlat[i, 1] <- mean(igraph::transitivity(buflat, type="local"), na.rm=T)
      outputlat[i, 2] <- mean_distance(buflat, directed = FALSE)
  }
  
  # Get mean for lattice metrics
  clat <- mean(outputlat[,1])
  llat <- mean(outputlat[,2])
 # Compute small world propensity
  dC =  (clat - Cobs) / (clat-crand)
  dL = (Lobs - lrand ) / (llat - lrand)
  dC <- ifelse(dC > 1,  1, dC)
  dC <- ifelse(dC < 0,  0, dC)
  dL <- ifelse(dL < 0, 0, dL)
  dL <- ifelse(dL > 1, 1, dL)
  SWP = 1- sqrt((dC^2 + dL^2)/2)  
  # Compute Telesford's measure  
  Tswi = (lrand/Lobs) - (Cobs/clat) 
  # Return values
  return(c(Cobs, Lobs, crand, lrand, clat, llat, SWI, Tswi, SWP, dC, dL))
  
}

# Buffer for late group data
latedata <- c()
# For each child in lategroup
for(i in 1:nrow(lateGroup)){
  # Get lexicon for ith child
  idlexbuffer <- subset(wbswow, data_id == lateGroup$Row[i] & age == lateGroup$age[i])
  # Get list of nodes in child's vocabulary 
  idgraphvlist <- which(V(sgcdi)$name %in% idlexbuffer$definition)
  # Subset to list of nodes in child's vocabulary
  idg <- subgraph(sgcdi, idgraphvlist)
  # Compute stats
  databl <- c(lateGroup$age[i], nrow(idlexbuffer), edge_density(idg), compute_smallworlds(idg))
  # Add stats to data buffer
  latedata <- rbind(latedata, databl)
}

# Make data frame from above
latedataf <- data.frame(lateGroup$Row, latedata, lgroup = "late")

# Do the same as above for the early talkers
earlydata <- c()
for(i in 1:nrow(earlyGroup)){
 # Get lexicon
  idlexbuffer <- subset(wbswow, data_id == earlyGroup$Row[i] & age == earlyGroup$age[i])
 # Subgraph 
  idgraphvlist <- which(V(sgcdi)$name %in% idlexbuffer$definition)
  idg <- subgraph(sgcdi, idgraphvlist)
  #plot(idg,  edge.arrow.size = .1, vertex.label.cex = .6, vertex.size = .1, vertex.label.color = "black")
 # Compute stats
  databe <- c(earlyGroup$age[i], nrow(idlexbuffer), edge_density(idg), compute_smallworlds(idg))
  earlydata <- rbind(earlydata, databe)
}

earlydataf <- data.frame(earlyGroup$Row, earlydata, lgroup="early")

# Name the data frames
names(earlydataf) <- c("id", "age", "nodes", "density", "Cobs", "Lobs", "Crand", "Lrand", "Clat", "Llat", "SWI", "Tswi", "SWP" , "dC", "dL", "lgroup")

names(latedataf) <- c("id", "age", "nodes", "density", "Cobs", "Lobs", "Crand", "Lrand", "Clat", "Llat", "SWI", "Tswi", "SWP", "dC", "dL", "lgroup" )

# Bind the data together by row
alldata <- rbind(earlydataf, latedataf)
# Regressions with and without language group for small world index
a1 <- lm(SWI ~ nodes, data=alldata)
a2 <- lm(SWI ~ lgroup + nodes, data=alldata)
## summary(a2)
# Statistical value of adding groups, 
## anova(a1, a2)

# As above for small world propensity
a1 <- lm(SWP ~ nodes, data=alldata)
a2 <- lm(SWP ~ lgroup + nodes, data=alldata)
## summary(a2)
## anova(a1, a2)

# As above for Telesford's index
a1 <- lm(Tswi ~ nodes, data=alldata)
a2 <- lm(Tswi ~ lgroup + nodes, data=alldata)
## summary(a2)
## anova(a1, a2)

# As above for components of small world propensity
a1 <- lm(Cobs ~ nodes, data=alldata)
a2 <- lm(Cobs ~ lgroup + nodes, data=alldata)
## summary(a2)
## anova(a1, a2)

a1 <- lm(Lobs ~ nodes, data=alldata)
a2 <- lm(Lobs ~ lgroup + nodes, data=alldata)
## summary(a2)
## anova(a1, a2)

# T-tests tell same story but don't take into account vocab size
##with(alldata, t.test(SWI~lgroup))
##with(alldata, t.test(SWP~lgroup))
##with(alldata, t.test(Tswi~lgroup))


#### Figure 7-4 ####

# Make three plots
par(mfrow=c(1, 3))
# Stripchart shows each data point
stripchart(SWI~lgroup, method="jitter", data = alldata, vert = T, pch = 16, cex = .6, col = "gray50", ylab = "Small-world index", xlab = "Talker type")
# Compute meand and standard error = std/sqrt(n)
m <- with(alldata, tapply(SWI, lgroup, mean, na.rm=T))
s <- with(alldata, tapply(SWI, lgroup, sd, na.rm=T))
n <- with(alldata, tapply(SWI, lgroup, length))
se <- s/ sqrt(n)
# Add error bars
arrows(1:2, m + se, 1:2, m-se, angle=90, code = 3, lwd =2, len = .1)

# As above for SWP and Tswi
stripchart(SWP~lgroup, method="jitter", data = alldata, vert = T, pch = 16, cex = .6, col = "gray50", ylab = "Small-world propensity",  xlab = "Talker type")
m <- with(alldata, tapply(SWP, lgroup, mean, na.rm=T))
s <- with(alldata, tapply(SWP, lgroup, sd, na.rm=T))
n <- with(alldata, tapply(SWP, lgroup, length))
se <- s/ sqrt(n)
arrows(1:2, m + se, 1:2, m-se, angle=90, code = 3, lwd =2, len = .1)

stripchart(Tswi~lgroup, method="jitter", data = alldata, vert = T, pch = 16, cex = .6, col = "gray50", ylab = "Telesford's measure",  xlab = "Talker type")
m <- with(alldata, tapply(Tswi, lgroup, mean, na.rm=T))
s <- with(alldata, tapply(Tswi, lgroup, sd, na.rm=T))
n <- with(alldata, tapply(Tswi, lgroup, length))
se <- s/ sqrt(n)
arrows(1:2, m + se, 1:2, m-se, angle=90, code = 3, lwd =2, len = .1)


#### Figure 7-5 ####

# Four figures
par(mfrow=c(1,4))
# Set margins
par(mar=c(4,4,1,1))

# As above for the small world measures, so below
stripchart(Cobs~lgroup, method="jitter", data = alldata, vert = T, pch = 16, cex = .6, col = "gray50", ylab = "Clustering coefficient")
m <- with(alldata, tapply(Cobs, lgroup, mean, na.rm=T))
s <- with(alldata, tapply(Cobs, lgroup, sd, na.rm=T))
n <- with(alldata, tapply(Cobs, lgroup, length))
se <- s/ sqrt(n)
arrows(1:2, m + se, 1:2, m-se, angle=90, code = 3, lwd =2, len = .1)

stripchart(Lobs~lgroup, method="jitter", data = alldata, vert = T, pch = 16, cex = .6, col = "gray50", ylab = "Average shortest path length")
m <- with(alldata, tapply(Lobs, lgroup, mean, na.rm=T))
s <- with(alldata, tapply(Lobs, lgroup, sd, na.rm=T))
n <- with(alldata, tapply(Lobs, lgroup, length))
se <- s/ sqrt(n)
arrows(1:2, m + se, 1:2, m-se, angle=90, code = 3, lwd =2, len = .1)


stripchart(dC~lgroup, method="jitter", data = alldata, vert = T, pch = 16, cex = .6, col = "gray50", ylab = TeX(r'($\Delta C$)'))
m <- with(alldata, tapply(dC, lgroup, mean, na.rm=T))
s <- with(alldata, tapply(dC, lgroup, sd, na.rm=T))
n <- with(alldata, tapply(dC, lgroup, length))
se <- s/ sqrt(n)
arrows(1:2, m + se, 1:2, m-se, angle=90, code = 3, lwd =2, len = .1)


stripchart(dL~lgroup, method="jitter", data = alldata, vert = T, pch = 16, cex = .6, col = "gray50", ylab = TeX(r'($\Delta L$)'))
m <- with(alldata, tapply(dL, lgroup, mean, na.rm=T))
s <- with(alldata, tapply(dL, lgroup, sd, na.rm=T))
n <- with(alldata, tapply(dL, lgroup, length))
se <- s/ sqrt(n)
arrows(1:2, m + se, 1:2, m-se, angle=90, code = 3, lwd =2, len = .1)

