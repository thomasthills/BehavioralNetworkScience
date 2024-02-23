knitr::opts_chunk$set(echo = FALSE)
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(comment = NA)
knitr::opts_chunk$set(warning = FALSE)
options(knitr.table.format = "latex")


### Behavioral Network Science
### Language, Mind, and Society
# By Thomas T. Hills

# Code to reproduce figures and analyses for

#### Chapter 8 ####

rm(list=ls())
library(tidytext)
library(igraph)
library(tidyverse)
library(kableExtra)
library(cowplot)
library(nlme)



#### Figure 8-1 ####

knitr::include_graphics(path = "./images/Figure8-1.pdf")


# Read in Google ngrams, words from 1800 and 1900
md <- read.delim('SampleDataFilesBNS/mergedfile2.txt', header = FALSE)
# Label columns
names(md) <- c("word", "year", "count", "books")
# Make all words lowercase
md$word <- tolower(md$word)

# To download the SWOW data follow these instructions (also used in Chapter 4 and 5): 
# go to https://smallworldofwords.org/en/project/research#download
# Download from 'English' data  SWOW-EN2008 assoc. strengths (R1) [3Mb]
# Unzip to the CSV file and place in the SampleDataFilesBNS folder
# There is a request for some information, but providing it is optional
# At the request of SWOW, I will not provide the free association data directly.

swow <- read.delim("SampleDataFilesBNS/strength.SWOW-EN.R1.csv", sep = "\t")

# Make it a tibble for using tidyverse command
swow <- tibble(swow)
# Limit to words that receive more than 1 response
swowT <- swow %>% filter(R1 >=2)
# Make graph from SWOW edgelist
sg <- graph_from_edgelist(as.matrix(swowT[,1:2]))
# Remove self-loops and multi-edges
sg <- igraph::simplify(sg)

## Keep only Google ngram words in SWOW 
mds <- subset(md, word %in% V(sg)$name) 

## Combine rows with same values and sum across counts
mso <- mds %>%
  group_by(word, year) %>%
  dplyr::summarise(across(c(count, books), sum))

# Compute growth: word frequency change
mso1800 <- subset(mso, year == 1800)
mso1900 <- subset(mso, year == 1900)
# Join 1800 and 1900 counts by word
msowide <- full_join(mso1800, mso1900, by = "word")
names(msowide) <- c("word", "year1800", 'count1800', 'books1800', 'year1900', 'count1900', 'books1900')
# Replace NAs for counts with 0s
msowide[is.na(msowide)] <- 0
# Make a tibble
msowide <- tibble(msowide)
# Compute each words growth (change from 1800 to 1900)
msowidem <- msowide %>% mutate(growth = count1900-count1800)
# nrow(msowidem)   #### 16780

# Remove words in fewer than 2 books to avoid mispellings and other errors in the data
msowidem <- msowidem %>% filter(books1900 > 2)
# nrow(msowidem)   #### 16444 before removing stopwords


# Remove stop words (tidytext function)
# Get list of stopwords
stopwords <- get_stopwords( source = "snowball")
# Remove all stopwords
msostops <- msowidem %>% anti_join(stopwords, by = "word") # 16313 total words in sample after removing stop words 

# Do any stop words originate in the 1800s
# Reduce list to stopwords only
msoStopsOnly <- msowidem %>% inner_join(stopwords, by = "word")
# What are the stopwords
uniq_msostops <- unique(msoStopsOnly$word) ##
# Limit to words at 0 frequency in 1800
newStops <- subset(msoStopsOnly, count1800 == 0)
# What are they?
unis <- unique(newStops$word) # contractions!

msowidem <- msostops %>% dplyr::arrange(desc(growth))

# nrow(msostops) # 16313
# nrow(subset(msostops, count1800 ==0)) #  1539 new words
# nrow(subset(msostops, count1800 > 0)) # count that were not present 14720

# We need a subgraph with only words from 1900
# Why? This removes words that might be added since 1900.
# Also, we want to compare words present in 1800 and 1900 with words not present in 1800 but present in 1900.

# Reduce sg graph to subgraph of words in ngram selection to compare new vs old
vids <- which(V(sg)$name %in% msowidem$word)
sggng <- subgraph(sg, vids)
## Compute degree, betweenness, and closeness for all words
degv <- data.frame(word = V(sggng)$name, k = igraph::degree(sggng, mode = "in"), b = igraph::betweenness(sggng, directed = TRUE), c = igraph::closeness(sggng, mode = "in", normalized = TRUE), cc = igraph::transitivity(sggng, type = "local"), ev = igraph::eigen_centrality(sggng, directed = TRUE)$vector )
## Join growth stats with degree and age (assign Old and New)
growdeg <- msowidem %>% left_join(degv, by = "word") %>% mutate(age = ifelse(count1800==0, "New", "Old"))

# See the new words
newwords <-  subset(growdeg, age == "New")
oldwords <-  subset(growdeg, age == "Old")

# Select top most words for each metric
topgrowth <- newwords %>% dplyr::arrange(desc(growth)) %>% dplyr::select(word) %>% head()
topk <- newwords %>% dplyr::arrange(desc(k)) %>% dplyr::select(word) %>% head()
topb <- newwords %>% dplyr::arrange(desc(b)) %>% dplyr::select(word) %>% head()
topc <- newwords %>% dplyr::arrange(desc(c)) %>% dplyr::select(word) %>% head()
topcc <- newwords %>% dplyr::arrange(desc(cc)) %>% dplyr::select(word) %>% head() # a sample, lots of ties
topev <- newwords %>% dplyr::arrange(desc(ev)) %>% dplyr::select(word) %>% head()
# Select bottom most words for each metric
botgrowth <- newwords %>% dplyr::arrange(desc(growth)) %>% dplyr::select(word) %>% tail()
botk <- newwords %>% dplyr::arrange(desc(k)) %>% dplyr::select(word) %>% tail()
botb <- newwords %>% dplyr::arrange(desc(b)) %>% dplyr::select(word) %>% tail()
botc <- newwords %>% dplyr::arrange(desc(c)) %>% dplyr::select(word) %>% tail()
botcc <- newwords %>% dplyr::arrange(desc(cc)) %>% dplyr::select(word) %>% tail() # a sample, lots of ties
botev <- newwords %>% dplyr::arrange(desc(ev)) %>% dplyr::select(word) %>% tail() # a sample, lots of ties

# Combine tops
topw <- cbind(topgrowth, topk, topb, topc, topcc, topev)
# Add names
colnames(topw) <- c("Growth", "k", "b", "c", "C", "x")
# Make table
knitr::kable(topw, caption = "The top ranked new words entering the lexicon in the 1800s along each measure: growth = frequency change, k = indegree, b = betweenness, c = closeness, C = clustering coefficient, and x = eigenvector centrality. For C, many words tie for the highest clustering coefficient with a value of 1, so those shown are a random sample from the top words. ") %>% 
    kable_paper(full_width = F, position="center") %>% 
    #column_spec(1, bold = T) %>%
    row_spec(0, bold=T)

# As above, but for Old words
topgrowth <- oldwords %>% dplyr::arrange(desc(growth)) %>% dplyr::select(word) %>% head()
topk <- oldwords %>% dplyr::arrange(desc(k)) %>% dplyr::select(word) %>% head()
topb <- oldwords %>% dplyr::arrange(desc(b)) %>% dplyr::select(word) %>% head()
topc <- oldwords %>% dplyr::arrange(desc(c)) %>% dplyr::select(word) %>% head()
topcc <- oldwords %>% dplyr::arrange(desc(cc)) %>% dplyr::select(word) %>% head() # a sample, lots of ties
topev <- oldwords %>% dplyr::arrange(desc(ev)) %>% dplyr::select(word) %>% head()

topw <- cbind(topgrowth, topk, topb, topc, topcc, topev)
colnames(topw) <- c("Growth", "k", "b", "c", "C", "x")
knitr::kable(topw, caption = "The top ranked old words in the lexicon in the 1800s along each measure. For C, many words tie for the highest clustering coefficient with a value of 1, so those shown are a random sample from the top words. ") %>% 
    kable_paper(full_width = F, position="center") %>% 
    row_spec(0, bold=T)


# Bottom ranked new words
botw <- cbind(botgrowth, botk, botb, botc, botcc, botev)
colnames(botw) <- c("Growth", "k", "b", "c", "C", "x")
knitr::kable(botw, caption = "The lowest ranked new words entering the lexicon in the 1800s ranked along each measure. For C, many words tie for the lowest clustering coefficient with a value of 0.") %>% 
    kable_paper(full_width = F, position="center") %>% 
    row_spec(0, bold=T)

# As above, but for Old bottom ranked words
botgrowth <- oldwords %>% dplyr::arrange(desc(growth)) %>% dplyr::select(word) %>% tail()
botk <- oldwords %>% dplyr::arrange(desc(k)) %>% dplyr::select(word) %>% tail()
botb <- oldwords %>% dplyr::arrange(desc(b)) %>% dplyr::select(word) %>% tail()
botc <- oldwords %>% dplyr::arrange(desc(c)) %>% dplyr::select(word) %>% tail()
botcc <- oldwords %>% dplyr::arrange(desc(cc)) %>% dplyr::select(word) %>% tail() # a sample, lots of ties
botev <- oldwords %>% dplyr::arrange(desc(ev)) %>% dplyr::select(word) %>% tail() # a sample, lots of ties


botw <- cbind(botgrowth, botk, botb, botc, botcc, botev)
colnames(botw) <- c("Growth", "k", "b", "c", "C", "x")
knitr::kable(botw, caption = "The lowest ranked old words in the lexicon in the 1800s ranked along each measure. For C, many words tie for the lowest clustering coefficient with a value of 0. ") %>% 
    kable_paper(full_width = F, position="center") %>% 
    row_spec(0, bold=T)


## # This figure was in an early draft and shows networks for some of the Old and New words.
## #
## # set.seed(1)
## # par(mfrow=c(2,2))
## # par(mar=c(1,1,1,1))
## # nname <- "biological"
## # vids <-unlist(ego(sggng, 1, nodes = nname,  mode = "all"))
## # n_subgraph <- subgraph(sggng, vids)
## # vsize <- rep(5, length(V(n_subgraph)))
## # x <- which(V(n_subgraph)$name == nname)
## # vsize[x] <-10
## # vname <- rep(NA, length(V(n_subgraph)))
## # vname[x] <- nname
## # vcol <-rep("black", length(V(n_subgraph)))
## # vcol[x] <- "white"
## # plot(n_subgraph, vertex.size = vsize, vertex.label.color = "black", vertex.label.dist = 1, vertex.color = vcol, vertex.label.cex= vsize/10, edge.arrow.size = .1)
## #
## # nname <- "movie"
## # vids <-unlist(ego(sggng, 1, nodes = nname,  mode = "all"))
## # n_subgraph <- subgraph(sggng, vids)
## # vsize <- rep(5, length(V(n_subgraph)))
## # x <- which(V(n_subgraph)$name == nname)
## # vsize[x] <-10
## # vname <- rep(NA, length(V(n_subgraph)))
## # vname[x] <- nname
## # vcol <-rep("black", length(V(n_subgraph)))
## # vcol[x] <- "white"
## # plot(n_subgraph, vertex.size = vsize, vertex.label.color = "black", vertex.label.dist = 1, vertex.color = vcol, vertex.label.cex= vsize/10, edge.arrow.size = .1)
## #
## #
## # nname <- "slapstick" # isolate
## # altname <- "corny"
## # vids <-c(unlist(ego(sggng, 1, nodes = altname,  mode = "all")))
## # vids <- c(vids, which(V(sggng)$name == nname))
## # n_subgraph <- subgraph(sggng, vids)
## # vsize <- rep(5, length(V(n_subgraph)))
## # x <- which(V(n_subgraph)$name == nname)
## # vsize[x] <-10
## # vname <- rep(NA, length(V(n_subgraph)))
## # vname[x] <- nname
## # vcol <-rep("black", length(V(n_subgraph)))
## # vcol[x] <- "white"
## # plot(n_subgraph, vertex.size = vsize, vertex.label.color = "black", vertex.label.dist = 1, vertex.color = vcol, vertex.label.cex= vsize/10, edge.arrow.size = .1)
## #
## #
## # nname <- "unplug" # isolate
## # altname <- "disable"
## # vids <-c(unlist(ego(sggng, 1, nodes = altname,  mode = "all")))
## # vids <- c(vids, which(V(sggng)$name == nname))
## # n_subgraph <- subgraph(sggng, vids)
## # vsize <- rep(5, length(V(n_subgraph)))
## # x <- which(V(n_subgraph)$name == nname)
## # vsize[x] <-10
## # vname <- rep(NA, length(V(n_subgraph)))
## # vname[x] <- nname
## # vcol <-rep("black", length(V(n_subgraph)))
## # vcol[x] <- "white"
## # plot(n_subgraph, vertex.size = vsize, vertex.label.color = "black", vertex.label.dist = 1, vertex.color = vcol, vertex.label.cex= vsize/10, edge.arrow.size = .1)
## 


#### Figure 8-2 ####

# Two panels for figure
par(mfrow=c(1,2))
# Are new words more likely to be isolates than old words?
# Isolates have k == 0
# New words are about twice as likely to be isolates as old words
# Count words in each group
x1<-with(growdeg, tapply(as.numeric(k==0), age, sum))
x2<-with(growdeg, tapply(as.numeric(k>=1), age, sum))
# This is total as it counts all, for proportion test below
x3<-with(growdeg, tapply(as.numeric(k>=1), age, length))

data1 <- cbind(x1,x2,x3)
# Proportion test
# prop.test(x = data1[,1], n = data1[,3])

library(plyr) 
# Summarise for each age group and get confidence intervals
age.prop<-ddply(growdeg,.(age),summarise,
      prop=sum(as.numeric(k==0))/length(as.numeric(k==0)),
      low=prop.test(sum(I(k==0)),length(I(k==0)))$conf.int[1],
      upper=prop.test(sum(I(k==0)),length(I(k==0)))$conf.int[2])

# Plot above with error bars
g1 <- ggplot(age.prop, aes(as.factor(age),y=prop,ymin=low,ymax=upper))+
  geom_bar(stat="identity")+
  geom_errorbar(width = .5)+labs( y = "Proportion of isolates", x = "Age") +
  theme_classic()

## Density plot across degree values with log axis
g2 <- ggplot(subset(growdeg,k>0), aes(x=k, linetype = age)) +
  geom_density(aes(color=age)) +
  coord_trans(x="log") +
  theme_classic()+labs( y = "Density", x = "Degree (k)") +
   scale_x_continuous(breaks=c(1,5,10,50, 100, 500))+ scale_colour_grey()

plot_grid(g1, g2, rel_widths = c(1,2), labels="AUTO")

# Tests of the degree by age 
# (given the long-tail, non-parametric is better)
# with(growdeg, t.test(log(k+1)~age))
# with(growdeg, t.test(k~age))
# with(growdeg, wilcox.test(k~age))
# with(growdeg, wilcox.test(log(k+1)~age))




#### Figure 8-3 ####

### Building a multilevel model :::: BETWEENNESS

## b is long-tailed and log(b+1) reduces the error substantially (compare without log())
## k is long-tailed and log(k+1) reduces the error substantially 
## We add 1 because sometimes b or k == 0.
interceptOnlywithoutLog <- gls(b~1, data = growdeg, method = "ML")
# summary(interceptOnlywithoutLog)
interceptOnly <- gls(log(b+1)~1, data = growdeg, method = "ML")
# summary(interceptOnly)
randomInterceptOnly <- lme(log(b+1)~1, data = growdeg, random = ~1|log(k+1), method = "ML")
# summary(randomInterceptOnly)
# Comparing the two above, we can see the random intercepts are warranted
# anova(interceptOnly, randomInterceptOnly)
# Adding Fixed Effects
randomInterceptAge <- lme(log(b+1)~age, data = growdeg, random = ~1|log(k+1), method = "ML")
# summary(randomInterceptAge)
# anova(randomInterceptOnly,randomInterceptAge)
# randomInterceptAgeK <- lme(log(b+1)~age+log(k+1), data = growdeg, random = ~1|log(k+1), method = "ML")
# # summary(randomInterceptAgeK)
# # anova(randomInterceptAge,randomInterceptAgeK)
# randomInterceptAgeKGrowth <- lme(log(b+1)~age+log(k+1)+growth, data = growdeg, random = ~1|log(k+1), method = "ML")
# # summary(randomInterceptAgeKGrowth)
# ## anova(randomInterceptOnly, randomInterceptAge, randomInterceptAgeK, randomInterceptAgeKGrowth)
# #
# Add random slopes
# randomInterceptAgeKGrowthSlopeAge <- lme(log(b+1)~age+log(k+1)+growth, data = growdeg, random = ~age|k, method = "ML")
# summary(randomInterceptAgeKGrowthSlopeAge)
# anova(randomInterceptAgeKGrowth, randomInterceptAgeKGrowthSlopeAge)
# Summary: old words have higher betweenness, suggesting that newer words are less likely to be on the paths between words
growdeg$bfit <- predict(randomInterceptAge) 

g1g<- ggplot(subset(growdeg, k>0 & b>0),aes(log(k+1), log(b+1), col=age, shape = age))  + 
  geom_jitter(width =.020,alpha=0.3, size=0.8)  +
  geom_line(aes(y=bfit, lty=age), size=0.8)+
  #coord_trans(x="log", y = "log") +
  scale_colour_grey() + ylim(1,15)+labs( y = "Log betweenness", x = "Log degree")+ theme_classic()+ 
  theme(legend.position = c(.8, .3)) + scale_color_manual(values=c("palevioletred4", "darkseagreen2"))
        
        

### Building a multilevel model ::: CLOSENESS (as above)
## c is bimodally distributed -- so there is room for some classification in the future
## k is long-tailed and log(k+1) reduces the error substantially 
## We add 1 because betweeness sometimes b or k == 0.

# with(subset(growdeg, !is.na(c)), hist(c))
interceptOnly <- gls(c~1, data = subset(growdeg, !is.na(c)), method = "ML")
# summary(interceptOnly)
randomInterceptOnly <- lme(c~1, data = subset(growdeg, !is.na(c)), random = ~1|log(k+1), method = "ML")
# summary(randomInterceptOnly)
# Comparing the two above, we can see the random intercepts are warranted
# anova(interceptOnly, randomInterceptOnly)
# Adding Fixed Effects
randomInterceptAge <- lme(c~age, data = subset(growdeg, !is.na(c)), random = ~1|log(k+1), method = "ML")
# anova(randomInterceptOnly,randomInterceptAge)
# # summary(randomInterceptAge)
# randomInterceptAgeK <- lme(c~age+log(k+1), data = subset(growdeg, !is.na(c)), random = ~1|log(k+1), method = "ML")
# # summary(randomInterceptAgeK)
# randomInterceptAgeKGrowth <- lme(c~age+log(k+1)+growth, data = subset(growdeg, !is.na(c)), random = ~1|log(k+1), method = "ML")
# # summary(randomInterceptAgeKGrowth)
# # anova(randomInterceptOnly, randomInterceptAge, randomInterceptAgeK, randomInterceptAgeKGrowth)
# # 
# Summary: old words have lower closeness after controlling for degree, suggesting that newer words start near the center
nagrowdeg <- subset(growdeg, !is.na(c))
nagrowdeg$cfit <- predict(randomInterceptAge) 

g2g<- ggplot(nagrowdeg,aes(log(k+1), c, col=age, shape = age))  + 
  geom_point(alpha=0.3, size=0.8) +
  geom_line(aes(y=cfit, lty=age), size=0.8)+
  theme_classic()+ scale_colour_grey() +labs( y = "Closeness", x = "Log degree")+ theme(legend.position = "none")+ scale_color_manual(values=c("palevioletred4", "darkseagreen2"))

### Building a multilevel model ::: CC
## k is long-tailed and log(k+1) reduces the error substantially 
## We add 1 because betweeness sometimes b or k == 0.

# with(subset(growdeg, !is.na(cc)), hist(cc))
interceptOnly <- gls(cc~1, data = subset(growdeg, !is.na(cc)), method = "ML")
# summary(interceptOnly) # 
randomInterceptOnly <- lme(cc~1, data = subset(growdeg, !is.na(cc)), random = ~1|log(k+1), method = "ML")
# summary(randomInterceptOnly)
# comparing the two above, we can see the random intercepts are warranted
# anova(interceptOnly, randomInterceptOnly)
# Adding Fixed Effects
randomInterceptAge <- lme(cc~age, data = subset(growdeg, !is.na(cc)), random = ~1|log(k+1), method = "ML")
# anova(randomInterceptOnly, randomInterceptAge)
# summary(randomInterceptAge)
# randomInterceptAgeK <- lme(cc~age+log(k+1), data = subset(growdeg, !is.na(cc)), random = ~1|log(k+1), method = "ML")
# # summary(randomInterceptAgeK)
# randomInterceptAgeKGrowth <- lme(cc~age+log(k+1)+growth, data = subset(growdeg, !is.na(cc)), random = ~1|log(k+1), method = "ML")
# summary(randomInterceptAgeKGrowth)
# anova(randomInterceptOnly, randomInterceptAge, randomInterceptAgeK, randomInterceptAgeKGrowth)
#  Best model: randomInterceptAgeK
# Summary: old words have lower cc after controlling for degree, suggesting that newer words start nearer cluster centers

# We get the same pattern of results if we limit the data set to words produced more than 100 times each year in the Google Ngrams data.  
naccgrowdeg <- subset(growdeg, !is.na(cc))
naccgrowdeg$ccfit <- predict(randomInterceptAge) 

g3g <- 
  ggplot(naccgrowdeg,aes(log(k+1), cc, col=age, shape = age))  + 
  geom_jitter(width=.01, alpha=0.3, size=0.8) +
  geom_line(aes(y=ccfit, lty=age), size=0.8)+
  ylim(c(0, 1))+theme_classic()+ 
  scale_colour_grey() +labs( y = "Clustering coefficient", x = "Log degree")+ theme(legend.position = "none")+ scale_color_manual(values=c("palevioletred4", "darkseagreen2"))

### Building a multilevel model ::: Eigvenvector 
## k is long-tailed and log(k+1) reduces the error substantially 
## We add 1 because betweeness sometimes b or k == 0.

# with(subset(growdeg, !is.na(cc)), hist(cc))
interceptOnly <- gls(ev~1, data = growdeg, method = "ML")
# summary(interceptOnly) # 
randomInterceptOnly <- lme(ev~1, data = growdeg, random = ~1|log(k+1), method = "ML")
# summary(randomInterceptOnly)
# comparing the two above, we can see the random intercepts are warranted
# anova(interceptOnly, randomInterceptOnly)
# Adding Fixed Effects
randomInterceptAge <- lme(ev~age, data = growdeg, random = ~1|log(k+1), method = "ML")
# summary(randomInterceptAge)
# anova(randomInterceptOnly,randomInterceptAge)
# randomInterceptAgeK <- lme(ev~age+log(k+1), data = growdeg, random = ~1|log(k+1), method = "ML")
# # summary(randomInterceptAgeK)
# randomInterceptAgeKGrowth <- lme(ev~age+log(k+1)+growth, data = growdeg, random = ~1|log(k+1), method = "ML")
# summary(randomInterceptAgeKGrowth)
# anova(randomInterceptOnly, randomInterceptAge, randomInterceptAgeK, randomInterceptAgeKGrowth)
#  Best model: randomInterceptAgeK
# Summary: old words have lower ev after controlling for degree, suggesting that newer words start nearer cluster centers

growdeg$evfit <- predict(randomInterceptAge) 

g4g <- 
  ggplot(growdeg,aes(log(k+1), ev, col=age, shape = age))  + 
  geom_jitter(width=.01, alpha=0.3, size=0.8) +
  geom_line(aes(y=evfit, lty=age), size=0.8)+
  ylim(c(0, .25))+theme_classic()+ 
  scale_colour_grey() +labs( y = "Eigenvector centrality", x = "Log degree")+ theme(legend.position = "none")+ scale_color_manual(values=c("palevioletred4", "darkseagreen2"))


plot_grid(g1g, g2g, g3g,g4g, labels="AUTO")
