
### Behavioral Network Science
### Language, Mind, and Society
# By Thomas T. Hills

# Code to reproduce figures and analyses for

#### Chapter 17  The Conspiracy Frame ####


knitr::opts_chunk$set(echo = FALSE)
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(comment = NA)
knitr::opts_chunk$set(warning = FALSE)

rm(list=ls())

#library(osfr)
##library(statgraph)
#library(DiagrammeR)
library(tinytex)
library(igraph)
#library(gridExtra)
#library(grid)
library(tidyverse)
#library(igraphdata)
library(kableExtra)
#library(latex2exp)
#library(scales)
#library(cowplot)
#library(nlme)
#library(gridGraphics)
#library(fitdistrplus)
#library(RColorBrewer)
library(BayesFactor)
#library(ggpubr)
#library(stargazer)
library(Rmisc)
#library(corrplot)
library(ggraph)
library(quanteda)
#library(lsa)


# Code here is adapted from 
# Miani, A., Hills, T., & Bangerter, A. (2022b). LOCO: The 88-million-word language of conspiracy corpus. Behavior Research Methods, 54, 1794–1817.

#### Load data ####

LOCO <- as.data.frame(jsonlite::fromJSON('SampleDataFilesBNS/Conspiracy_miani/LOCO.json'))
## Resolve synonyms in seeds by substitution
LOCO$seeds <- gsub('elvis.presley', 'elvis.presley.death', LOCO$seeds, fixed = T)
LOCO$seeds <- gsub('elvis.death', 'elvis.presley.death', LOCO$seeds, fixed = T)
LOCO$seeds <- gsub('nwo', 'new.world.order', LOCO$seeds, fixed = T)
LOCO$seeds <- gsub('drug.companies', 'big.pharma', LOCO$seeds, fixed = T)
LOCO$seeds <- gsub('big.pharma', 'drug.companies', LOCO$seeds, fixed = T)
LOCO$seeds <- gsub('pharmaceutical.industry', 'drug.companies', LOCO$seeds, fixed = T)
LOCO$seeds <- gsub('covid.19', 'coronavirus', LOCO$seeds, fixed = T)
LOCO$seeds <- gsub('climate.change', 'global.warming', LOCO$seeds, fixed = T)
LOCO$seeds <- gsub('vaccine.autism', 'vaccine', LOCO$seeds, fixed = T)
LOCO$seeds <- gsub('vaccine.covid', 'vaccine', LOCO$seeds, fixed = T)
LOCO$seeds <- gsub('elvis.presley.death', 'elvis.presley', LOCO$seeds, fixed = T)
LOCO$seeds <- gsub('michael.jackson.death', 'michael.jackson', LOCO$seeds, fixed = T)
LOCO$seeds <- gsub('paul.mccartney.death', 'paul.mccartney', LOCO$seeds, fixed = T)
LOCO$seeds <- gsub('princess.diana.death', 'princess.diana', LOCO$seeds, fixed = T)
LOCO$seeds <- gsub('september.11.attack', 'september.11', LOCO$seeds, fixed = T)
LOCO$seeds <- gsub('.', '_', LOCO$seeds, fixed = T)

## Get the seed list
s <- quanteda::tokens(LOCO$seeds,
            remove_punct = T, remove_symbols = T)
mySeeds <- as.character(quanteda::featnames(dfm(s)))


##  Load functions with quanteda
get_fcm <- function(txt, doc_id) {
  require(quanteda)
  x <- txt
  names(x) <- doc_id
  # Functions to clean text
  x <- tolower(x)
  y <- tokens(x,
              remove_url = TRUE,
              remove_punct = TRUE, 
              remove_numbers = TRUE, 
              remove_separators = TRUE,
              split_hyphens = F,
              remove_symbols = TRUE)
  dfmc <- dfm(y, tolower = TRUE, verbose = TRUE ) 
  dfmc <- as.matrix(dfmc)
  dfmc <- ifelse(dfmc > 0, 1, 0)
  dfmc <- quanteda::as.dfm(x = dfmc)
  fcmm <- quanteda::fcm(dfmc)
  return(fcmm)
}

## Subset corpus ------------------------------------------------------
dc <- LOCO[LOCO$subcorpus=='conspiracy',]
dm <- LOCO[LOCO$subcorpus=='mainstream',]
# Mainstream corpus without mentios of conspiracy 
dmclean <- LOCO[LOCO$subcorpus=='mainstream' & LOCO$mention_conspiracy == 0,]

## Create FCM (feature co-occurrence matrix) --------------------------
fcm.c <- get_fcm(txt = dc$seeds, doc_id = dc$doc_id)
fcm.m <- get_fcm(txt = dm$seeds, doc_id = dm$doc_id)
fcm.clean <- get_fcm(txt = dmclean$seeds, doc_id = dmclean$doc_id)

## Prepare plots
col.C <- 'firebrick1'
col.M <- 'royalblue4'
cols <- c(col.C, col.M)
      
weighted = NULL
diag = T
mode = 'upper'
normalized = F

## Make networks
# Conspiracy
m.c <- igraph::graph_from_adjacency_matrix(adjmatrix = fcm.c, weighted = weighted, diag = diag, mode = mode)
bm.c <- igraph::degree(m.c, normalized = normalized)
bdf.c <- data.frame(n = names(bm.c), v = as.numeric(bm.c), d = 'C')
bdf.c <- bdf.c[order(bdf.c$v, decreasing = T),]
# Ordered by degree in conspiracy corpus
level_order <- bdf.c$n

# Mainstream: These can be used to produce corpus with 'conspiracy' tokens
## m.m <- igraph::graph_from_adjacency_matrix(adjmatrix = fcm.m, weighted = weighted, diag = diag, mode = mode)
## bm.m <- igraph::degree(m.m, normalized = normalized)
##bdf.m <- data.frame(n = names(bm.m), v = as.numeric(bm.m), d = 'M')


# Mainstream cleaned: without 'conspiracy'
m.m <- igraph::graph_from_adjacency_matrix(adjmatrix = fcm.clean, weighted = weighted, diag = diag, mode = mode)
bm.m <- igraph::degree(m.m, normalized = normalized)
bdf.m <- data.frame(n = names(bm.m), v = as.numeric(bm.m), d = 'M cleaned')

# Statistics
mean.m <- mean(igraph::degree(m.m))
mean.m.sd <- sd(igraph::degree(m.m))
mean.c <- mean(igraph::degree(m.c))
mean.c.sd <- sd(igraph::degree(m.c))

# T-test
testoutput <- t.test(igraph::degree(m.m), igraph::degree(m.c), paired=T)

# Bayesian t-test
btestoutpu <- ttestBF(
    x = as.vector(igraph::degree(m.m)),
    y = as.vector(igraph::degree(m.c)),
    paired = TRUE
 )

# Combine both
bdf <- rbind(bdf.c, bdf.m)
# Summary stats
bdf.se <- Rmisc::summarySE(data = bdf, measurevar = 'v', groupvars = 'd')
# Y-axis limits
ylimits <- c(0, max(bdf$v))

#### Explore data: To find which seeds have which values
t1 <- dc$seeds[grep('michael_jackson', dc$seeds)] # list seeds with michael_jackson
t2 <- dc$seeds[grep('5g', dc$seeds)] # list with 5g
t3 <- intersect(t1, t2) # show intersection of two lists


#### Table 17.1 ####

# List of seeds

# Remove underscores for table
mseeds <- gsub('_', ' ', mySeeds, fixed = T)
msee <- data.frame("seed"=mseeds)
names(mseeds) <- "seed"
# Make table of seed names
kable(msee, booktabs=T, escape=FALSE, caption="List of conspiracy topics.") # print list of seeds



#### Figure 17-1 ####

## The network plots

# Convert quanteda dfm to matrix
mm <- quanteda::convert(fcm.m, 'matrix')
mc <- quanteda::convert(fcm.c, 'matrix')

# Set seed value
s <- 5600
set.seed(s)

# Make graph
net1 <- graph_from_adjacency_matrix(mm, mode= "undirected", weighted = TRUE)
# Reorganize mc columns to match mm
mc <- mc[rownames(mm),rownames(mm)]
# Make graph
net2 <- graph_from_adjacency_matrix(mc, mode= "undirected", weighted = TRUE)

# Plot parameters
par(mfrow=c(1,2))
par(mar=c(3,3,3,3))
# Layout
l = layout_in_circle(net1)

# Plot network for mainstream without labels
plot(net1, vertex.size = 1, edge.width = E(net1)$weight/50, layout=l, edge.arrow.size=0, edge.color = alpha("black", alpha = E(net1)$weight/10), main = "Mainstream",  vertex.label=NA)
# Add labels with rotation
iangles = (1:39-1)*9.23 
iangles <- ifelse(iangles > 90 & iangles < 270, iangles-180, iangles)
scalef = 1.43
for (i in 1:dim(l)[1]) {
  text(x = l[i,1]*scalef, y = l[i,2]*scalef, labels = V(net1)$name[i], 
       col = "black", srt = iangles[i], xpd = T, cex = .45)
}

# Plot network conspiracy without labels
plot(net2, vertex.size = 1, edge.width = E(net2)$weight/50, layout=l, edge.arrow.size=0, edge.color = alpha("black", alpha = E(net2)$weight/10), main = "Conspiracy", vertex.label=NA)
# Add lables with rotation
scalef = 1.4
for (i in 1:dim(l)[1]) {
  text(x = l[i,1]*scalef, y = l[i,2]*scalef, labels = V(net2)$name[i], 
       col = "black", srt = iangles[i], xpd = T, cex = .45)
}


#### Figure 17-2 ####

# Figure of relative degrees across seeds

p_bdf <- ggplot(bdf, aes(x = factor(n, level = rev(level_order)), y = v, group = d, fill = rev(d))) + 
  geom_bar(stat = "identity",  position="dodge", width = .5) +
  scale_fill_manual(values = cols,
                    labels = c('non-conspiracy', 'conspiracy')
  ) +
  ylab('Degree') + xlab('') + 
  ylim(ylimits) +
  theme_bw() +
  guides(fill=guide_legend(title="Corpora", title.position="top", title.hjust =0.5)) +
  coord_flip() +
  theme(text = element_text(size=10),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = .5, vjust = .2),
        legend.direction = 'vertical', 
        legend.justification = c(1, 0), legend.position = c(1, 0)
  )
p_bdf


# Clean up seeds
dcy <- tokens(dc$seeds,
              remove_url = TRUE,
              remove_punct = TRUE, 
              remove_numbers = TRUE, 
              remove_separators = TRUE,
              split_hyphens = F,
              remove_symbols = TRUE)

# Count seeds
dc.counts <- table(unlist(dcy))

# Clean up seeds
dcm <- tokens(dm$seeds,
              remove_url = TRUE,
              remove_punct = TRUE, 
              remove_numbers = TRUE, 
              remove_separators = TRUE,
              split_hyphens = F,
              remove_symbols = TRUE)

# Count seeds
dm.counts <- table(unlist(dcm))



#### Figure 17-3 ####

# PMI edges plot

# Convert format
mm <- quanteda::convert(fcm.m, 'matrix')
mc <- quanteda::convert(fcm.c, 'matrix')

## Make symmetric
mm <- mm + t(mm)
mc <- mc + t(mc)

# Test for PMI
 xi <- which(colnames(mm) == "jonestown_suicide") # this has less than 100 samples. Small samples confuse metrics like pmi
 # Remove it from mainstream matrix
 mm <- mm[-xi, -xi]
 xi <- which(colnames(mc) == "jonestown_suicide")
 # Remove it from other matrix
 mc <- mc[-xi, -xi]

# Make same order of rows and columns
mm <- mm[colnames(mc),colnames(mc)]

# Get number of rows
c.docs = nrow(dc)
m.docs = nrow(dm)
# PMI function
pmi <- function(ab,a,b, count = 1){
  return(log((ab/count)/((a/count)*(b/count))) )  # need to divide by probability to get an interpretable number (i.e., greater than 1)
}


C = 0 #quantile(mc)[3] # threshold for counting a connection
pmi.m <- mm
pmi.m[,] <- 0
# for each edge, compute PMI
for(i in 1:ncol(pmi.m)){
  for(j in 1:nrow(pmi.m)){
    # If lower than C, set to 0
    if(mm[i,j] < C){
      pmii <- 0
    } else {
      # Compute PMI
      pmii <- pmi(mm[i,j], dm.counts[colnames(mm)[i]],dm.counts[colnames(mm)[j]],count=m.docs )
    }
    
    pmi.m[i,j] <- pmii
  }
}

pmi.c <- mc
pmi.c[,] <- 0
for(i in 1:ncol(pmi.c)){
  for(j in 1:nrow(pmi.c)){
    if(mc[i,j] < C){
      pmii <- 0
    } else {
      pmii <- pmi(mc[i,j], dc.counts[colnames(mc)[i]],dc.counts[colnames(mc)[j]],count=c.docs )
    }
    pmi.c[i,j] <- pmii
  }
}

# Set less than 0 to 0
pmi.m[pmi.m < 0] <- 0 
pmi.c[pmi.c < 0] <- 0

# Make networks from adjacency matrix pmi values
cpmi.mat <- igraph::graph_from_adjacency_matrix(adjmatrix = pmi.c, weighted = TRUE, mode="undirected") 
mpmi.mat <- igraph::graph_from_adjacency_matrix(adjmatrix = pmi.m, weighted = TRUE, mode="undirected")
# Plot parameters
set.seed(26)
par(mfrow=c(1,2))
par(mar=c(1,1,1,1))
# Layout
lpmi <- layout_with_fr(cpmi.mat)
# Adjust layout to fit
lpmi[,1] <- (lpmi[,1]-min(lpmi[,1]))
lpmi[,1] <- lpmi[,1]/max(lpmi[,1]/2)-1
lpmi[,2] <- (lpmi[,2]-min(lpmi[,2]))
lpmi[,2] <- lpmi[,2]/max(lpmi[,2]/2)-1
# Plot
x <- plot(cpmi.mat, vertex.size =igraph::degree(cpmi.mat, mode="all"), vertex.label.cex = .5, main = "Conspiracy", vertex.label.cex = .52,  vertex.color=alpha("red", .5),vertex.frame.color = "grey30", vertex.label.family="Helvetica",layout=lpmi, vertex.label = NA, rescale = FALSE)

# Add labels
scalef =c(runif(length(V(cpmi.mat)), .9, 1.1))
for (i in 1:dim(lpmi)[1]) {
  text(x = lpmi[i,1]*scalef[i], y = lpmi[i,2]*scalef[i]+.07, labels = V(cpmi.mat)$name[i], 
       col = "black", xpd = T, cex = .5)
}
# Plot
plot(mpmi.mat, vertex.size =igraph::degree(mpmi.mat, mode="all"), vertex.label.cex = .5, main = "Mainstream", vertex.label.cex = .52, vertex.label.dist = 1, vertex.label.family="Helvetica",vertex.frame.color = "grey30", vertex.color=alpha("violet", .5),layout=lpmi,vertex.label= NA)
# Add labels
for (i in 1:dim(lpmi)[1]) {
  text(x = lpmi[i,1]*scalef[i], y = lpmi[i,2]*scalef[i]+.07, labels = V(cpmi.mat)$name[i], 
       col = "black", xpd = T, cex = .5)
}
# T.test
testoutput <- t.test(pmi.m, pmi.c, paired=T)
# Bayesian HT
btestoutpu <- ttestBF(
    x = as.vector(pmi.m),
    y = as.vector(pmi.c),
    paired = TRUE
 )


# Compute networks statistics for Table 17.2

bet <- igraph::betweenness(cpmi.mat, directed = FALSE, normalized=TRUE )
str <- igraph::strength(cpmi.mat, mode="all")
deg <- igraph::degree(cpmi.mat, mode="all")
# Make data.frame
df <- data.frame(seed = names(bet), Betweenness = bet, Strength = str, Degree = deg)
# Make labels looks nice
df$seed <- gsub('_', ' ', df$seed, fixed = T)
# Order by Betweenness
df <- df[order(df$Betweenness, decreasing=TRUE),]


#### Figure 17-4 ####

## Make the seeds into something regex can read (remove spaces and replace ; with OR == |)
dc$seeds <- gsub(' ', '', dc$seeds, fixed = T)
dc$seeds <- gsub(';', '|', dc$seeds, fixed = T)

dmclean$seeds <- gsub(' ', '', dmclean$seeds, fixed = T)
dmclean$seeds <- gsub(';', '|', dmclean$seeds, fixed = T)

## Make seeds into a string of numbers so order doesn't matter
xtest <- apply(dc, 1, function(x) paste(mySeeds[which(str_detect(mySeeds, regex(x[4]))==TRUE)], collapse=";"))
dc$seeds <- xtest
xtest <- apply(dmclean, 1, function(x) paste(mySeeds[which(str_detect(mySeeds, regex(x[4]))==TRUE)], collapse=";"))
dmclean$seeds <- xtest


# Normalized entropy equation used for coherence entropy with seed combinations
Hentropy <- function(x){
  h <- 0
  for(i in 1:length(x)){
    h = h + x[i]*log(x[i])
  }
  hn <- h /log(length(x))
  return(hn)
}

entropydf.c <- c()
# For each seed
for(i in 1:length(mySeeds)){
## Subset seeds for each seed
  subcorp <- grep(mySeeds[i], dc$seeds) ## Find presence of seed across corpus
## Make table of seed combinations for the primary seed : order doesn't matter
  tabcorp <- table(dc$seeds[subcorp])  
  ps <- tabcorp/sum(tabcorp) # Probability of each seed combination
  hps <- -Hentropy(ps) # Find coherence entropy
  entropydf.c <- rbind(entropydf.c, hps)
  
}
# As above for mainstream corpus
entropydf.m <- c()
for(i in 1:length(mySeeds)){
## Subset seeds for each seed
  subcorp <- grep(mySeeds[i], dm$seeds) 
## Make table of seed combinations for the primary seed : order doesn't matter
  tabcorp <- table(dm$seeds[subcorp])  
  ps <- tabcorp/sum(tabcorp)
  hps <- -Hentropy(ps)
  entropydf.m <- rbind(entropydf.m, hps)
  
}


# Make data frame with values for coherence entropy
dfhps <- data.frame(seeds = c(mySeeds, mySeeds), hps = c(entropydf.c, entropydf.m), subcorpus = c(rep("C", length(mySeeds)), rep("M", length(mySeeds))))

# Summarize statistics for each subcorpus (mainstream, conspiracy)
sumse <- summarySE(dfhps, measurevar = "hps", groupvars = "subcorpus")

# Plot parameters
par(mfrow=c(1,2))
par(mar=c(5,5,1,1))

# Bar plot
x <- barplot(sumse$hps, ylim = c(0, .5), ylab="Incoherence entropy", xlab = "Subcorpus", col = "cornflowerblue", names = c("Conspiracy", "Mainstream"))
arrows(x, sumse$hps+sumse$se, x, sumse$hps-sumse$se, code = 3, angle=90, len=.2, lwd=1.5)

# Fix labels
df$seed <- gsub(' ', '_', df$seed, fixed = T)
# Make data frame
df2 <- data.frame(seed=mySeeds, hps = as.vector(entropydf.c))
df <- df %>% left_join(df2)
plot(df$Betweenness, df$hps, xlab = "Betweenness", ylab = "Incoherence entropy", ylim = c(0.1, .6), pch=16, cex = .9)

# Fix labels
df$seed <- gsub('_', ' ', df$seed, fixed = T)
# Relabel column as 'Entropy'
names(df)[5] <- "Entropy"
# Make Table
kable(df,row.names=FALSE, booktabs = T, escape = FALSE, caption = "Values of conspiracy seeds along various centrality metrics. Entropy is incoherence entropy.", digits=2) %>% kable_classic(full_width = F, html_font = "Cambria")



## 
## # Processing Van Gogh letters
## # Uncomment to explore the Van Gogh networks
## 
## ## Van Gogh artist mentions
##  library(quanteda)
##  library(tinytex)
##  library(tidyverse)
##  library(igraph)
## 
## ## get data
## rm(list=ls())
## rd <- read.csv("SampleDataFilesBNS/vangogh/formatted_letters_Aug2021_formattedForExcelEvenRows.csv", header = T)
## # Only letters from Van Gogh
## rd <- subset(rd, from == "Vincent van Gogh")
## ## nrow(rd) 819
## rdt <- rd$text
## # Tokenize letter text--break it up into words
## s <- tokens(rdt,
##             remove_punct = T, remove_symbols = T)
## 
## ## get artist names in correct time period--from WikiArt
## alist <- read.csv(file="SampleDataFilesBNS/vangogh/artistAndDate.csv")
## alist <- alist %>% filter(date <1889)
## alist <- alist %>% filter(date > 0)
## alist <- alist$artist
## ##alist <- data.frame(artist=alist)
## 
## rdt <- rd$text
## artisto <- unique(alist)
## # Adding some additional artists that arise from other sources
## artisto <- c(artisto, "Gustave Brion", "Velázquez", "Hugo_van_der_Goes")
## # Tokenize list
## a <- tokens(artisto, remove_punct = T, remove_symbols = T)
## # Breaks names apart
## atoks_nostop <- tokens_select(a, pattern = stopwords("en"), selection = "remove")
## # These are a list of names that are ambiguous (occur with more than one name or arise for other reasons). Not ommitting these seems to introduce many false positives.  Removing them introduces false negatives. The latter seems more conservative, but one can try it both ways.  Ideally, the list needs to be properly curated, disambiguating names when Van Gogh mentions them---which requires more professional domain knowledge.
## ambiguous_names_list <- c("Theo", "van", "Ma", "Jan", "the", "of",
##                           "and", "Jesus", "Michel", "May", "Anna", "do",
##                           "no", "Paul", "Sir", "am", "de", "am", "George",
##                           "Charles", "David", "Le", "Otto", "Jules", "Johan",
##                           "James", "Cesar", "Constant", "Frans", "Emile",
##                           "Camille", "T", "C", "Henri", "Brion Gysin",
##                           "Willem", "Piet", "R", "B", "J", "E", "H", LETTERS, "Alphonse", "Hugo", "Victor",
##                           "thomas", "jacob", "jean", "henry", "theodore", "gabriel", "andre", "john",
##                           "alfred", "marie", "william", "nicholas", "robert", "william", "richard", "samuel",
##                           "max", "joan", "henri", "jane", "karl", "fred", "louis",
##                           "howard", "gerard", "alexander", "gustave", "pierre", "edouard",
##                           "wilhelm", "albert", "constantin", "maria", "georges", "leon", "yves",
##                           "nicolas", "christian", "henri", "claude", "on", "august",
##                           "brothers", "felix","guy", "bernard buffet", "bernard schultze" ,"morgan", "white",
##                           "la", "jane frank", "frank johnston" , "guy", "roman cotosman", "roman opalka", "da", "Veronese",
##                           "tom", "martin", "Mary", "albrecht", "Jones", "Eduard", "joseph", "edwin", "Flemish", "Frederick")
## 
## # Remove ambiguous names
## atoks_nostop <- tokens_select(a, pattern = ambiguous_names_list, selection = "remove")
## 
## ##Find names when they occur in letters
## namesInLetters <- function(textdoc){
##   letterList <- list(NULL)
##   for(i in 1:length(textdoc)){
##    letterList[[i]] <-  textdoc[[i]][textdoc[[i]] %in% unlist(atoks_nostop)]
##   }
##   return(letterList)
## }
## 
## # Feed in the Van Gogh letters
## niL <- namesInLetters(s)
## 
## listOfNames <- unique(unlist(niL)) #  check each of these
## 
## ## checking on co-locations
## ## returns window around specific words
## 
## # The function below find collocations with the name and 3 words before and after
## # This was used to determine which of the names were genuine and who/what they referred to.
## # The results were used to determine the ambiguous names list above
## # I went through this list once, recognizing it could use additional sets of eyes.
## ## kwic(s, pattern = c("Bodmer"), window = 3)
## ## kwic(s, pattern = c("Weber"), window = 3)
## ## kwic(s, pattern = c("Diaz"), window = 3)
## ## kwic(s, pattern = c("Otto"), window = 3)
## ## kwic(s, pattern = c("Jules"), window = 3)
## ## kwic(s, pattern = c("Johan"), window = 3)
## ## kwic(s, pattern = c("Hendrik"), window = 3)
## ## kwic(s, pattern = c("Tissot"), window = 3)
## ## kwic(s, pattern = c("Cesar"), window = 3)
## ## kwic(s, pattern = c("Frans"), window = 3)
## ## kwic(s, pattern = c("Constant"), window = 3)
## ## kwic(s, pattern = c("Camille"), window = 3)
## ## kwic(s, pattern = c("C"), window = 3)
## ## kwic(s, pattern = c("T"), window = 3)
## ## kwic(s, pattern = c("Brion"), window = 3)
## ## kwic(s, pattern = c("Rousseau"), window = 3)
## ## kwic(s, pattern = c("Genk"), window = 3)
## ## kwic(s, pattern = c("Willem"), window = 3)
## ## kwic(s, pattern = c("Piet"), window = 3)
## ## kwic(s, pattern = c("Boldini"), window = 3)
## ## kwic(s, pattern = c("Alphonse"), window = 3)
## ## kwic(s, pattern = c("Hugo"), window = 3)
## ## kwic(s, pattern = c("Pietro"), window = 3)
## ## kwic(s, pattern = c("Breton"), window = 3)
## ## kwic(s, pattern = c("Bernard"), window = 3)
## ## kwic(s, pattern = c("Frederick"), window = 3) # referring to Frederick the Great
## ## kwic(s, pattern = c("Haan"), window = 3)
## ## kwic(s, pattern = c("Veronese"), window = 3) # Veronese green
## ## kwic(s, pattern = c("Zorn"), window = 3)
## ## kwic(s, pattern = c("Puvis"), window = 3)
## ## kwic(s, pattern = c("Armand"), window = 3)
## ## kwic(s, pattern = c("Guillaumin"), window = 3)
## ## kwic(s, pattern = c("Weissenbruch"), window = 3)
## ## kwic(s, pattern = c("Lord"), window = 3)
## ## kwic(s, pattern = c("Jones"), window = 10)
## ## kwic(s, pattern = c("Hill"), window = 3)
## ## kwic(s, pattern = c("Millet"), window = 3)
## ## kwic(s, pattern = c("arnold"), window = 3)
## ## kwic(s, pattern = c("stevens"), window = 3)
## ## kwic(s, pattern = c("Whistler"), window = 3)
## ## kwic(s, pattern = c("Pissarro"), window = 3)
## ## wic(s, pattern = c("Hill"), window = 3)
## ## kwic(s, pattern = c("russell"), window = 3) # russell painted van gogh
## ## kwic(s, pattern = c("edwin"), window = 3) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("renoir"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("monticelli"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Reynolds"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Goes"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Anker"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Bouguereau"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("North"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Cross"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Rubens"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Romney"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("crome"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Rosa"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Christus"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Leonardo"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Mantegna"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Isaac"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Israels"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Judith"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Rosenthal"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Jongkind"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Theophile"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Stuart"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Rose"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Frederick"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Ribot"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Sarto"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Lear"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Bingham"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Sydney"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Dante"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Gavarni"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Felicien"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Lefebvre"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Steen"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Bernhard"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Frank"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Ribera"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("ward"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("pyle"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("courbet"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Valentin"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Morris"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Gilbert"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Watson"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Dadd"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Robertson"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Hardy"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Boulanger"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Murillo"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Bonnat"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Moore"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Hubert"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Osman"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Bracquemond"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Lewis"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Laurens"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Lucas"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Leibl"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Hunt"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Hermans"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Dou"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Cabanel"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Poussin"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Brouwer"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Boucher"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("tour"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Barbara"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Greuze"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Prud'hon"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Raphael"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Cimabue"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Caillebotte"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Hooch"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Redon"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Crivelli"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Maurin"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Couture"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Homer"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Holman"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Rossetti"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Angelico"), window = 5) # edwin drood and edwin edwards
## ## kwic(s, pattern = c("Cheret"), window = 5) # edwin drood and edwin edwards
## ## # it's not Thomas monticelli, but adolphe monticelli
## 
## # This is the final list which I saved after doing the above
## vgc <- read.csv("SampleDataFilesBNS/vangogh/vanGoghChecked.csv", header = FALSE)
## ## Painters to keep
## vgc <- vgc[vgc[,2]=="y",]
## vgc[,1] <- tolower(vgc[,1])
## 
## # Tokenize names
## toks <- tokens(niL,
##             remove_punct = T, remove_symbols = T)
## 
## # Count occurrences of names
## freqNames <- table(unlist(toks))
## 
## # Here are the names
## mySeeds <- as.character(featnames(dfm(toks)))
## 
## ## Load functions with quanteda -- the same as used for conspiracy corpus
## get_fcm <- function(txt, doc_id) {
##   require(quanteda)
##   dfmc <- dfm(txt, tolower = TRUE, verbose = TRUE )
## 
##   dfmc <- as.matrix(dfmc)
##   dfmc <- ifelse(dfmc > 0, 1, 0)
##   dfmc <- quanteda::as.dfm(x = dfmc)
##   fcmm <- quanteda::fcm(dfmc)
##   return(fcmm)
## }
## 
## ## create FCM (feature co-occurrence matrix) ----------------------------
## # 'toks' is the list of names that occur in each letter, allowing us to produce a name x name matrix
## fcm.c <- get_fcm(txt = toks, doc_id = rd$letterid)
## # Make it symmetric
## fcm.c <- fcm.c + t(fcm.c)
## # Remove all the names not in the curated list of approved names
## fcm.c <- fcm.c[rownames(fcm.c) %in% vgc[,1], rownames(fcm.c) %in% vgc[,1]]
## #
## library(stringr)
## # Make first letter uppercase for plotting
## rownames(fcm.c) <- str_to_title(rownames(fcm.c))
## colnames(fcm.c) <- str_to_title(colnames(fcm.c))
## # Set netork parameters
## weighted = NULL
## diag = T
## mode = 'upper'
## normalized = F
## # Make network
## vgg <- igraph::graph_from_adjacency_matrix(adjmatrix = fcm.c, weighted = TRUE, diag = diag, mode = mode)
## # Simplify
## vgg <- igraph::simplify(vgg)
## 
## # Save network
## ##vggn <- as_adjacency_matrix(vgg, sparse=F)
## ##write.csv(vggn, file = "vanGoghNetwork.csv", quote=F)
## 
## 
## # Upload network
## vgtest <- read.csv("SampleDataFilesBNS/vangogh/vanGoghNetwork.csv", header = T, row.names=1 )
## # Make it a matrix
## vgtest <- as.matrix(vgtest)
## # Make network
## vgt <- graph_from_adjacency_matrix(vgtest, mode="undirected")
## 
## # Find weak edges
## deledge <- which(E(vgg)$weight < 2)
## # Threshold for visualization
## vgg <- igraph::delete.edges(vgg, deledge)
## # Find isolates
## delnode <- which(igraph::degree(vgg) <=0)
## # Remove isolates
## vgg <- igraph::delete.vertices(vgg, delnode)
## # Set random seed
## set.seed(4)
## # Plot -- this could be made more beautiful in R, but Gephi is worth exploring
## plot(vgg, vertex.size = 10*igraph::degree(vgg)/max(igraph::degree(vgg)),
##      vertex.label.cex = igraph::degree(vgg)/max(igraph::degree(vgg))+.2,
##      layout=igraph::layout_with_kk(vgg), edge.color=alpha("gray", alpha=.3),
##      vertex.label.dist=.5,  vertex.label.family="Helvetica")
## 
## # Let's export this for Gephi
## 
## # Get weighted adjacency matrix
## vmat <- as_adj(vgg, attr="weight")
## # Set column names
## colnames(vmat) <- rownames(vmat)
## # Set as matrix
## vmat <- as.matrix(vmat)
## # Export --- then upload this file in Gephi
## write.csv(vmat, file= "vangoghforGephi.csv")
