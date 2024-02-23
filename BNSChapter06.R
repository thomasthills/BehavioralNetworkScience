knitr::opts_chunk$set(echo = FALSE)
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(comment = NA)
knitr::opts_chunk$set(warning = FALSE)
options(knitr.table.format = "latex")

### Behavioral Network Science
### Language, Mind, and Society
# By Thomas T. Hills

# Code to reproduce figures and analyses for

#### Chapter 6 ####

rm(list=ls())
library(igraph)
library(tidyverse)
library(knitr)
library(kableExtra)
library('RVAideMemoire')



#### Figure 6-1 ####
# Get list of files in Painters folder
myimages<-list.files("/Users/thomashills/Dropbox/Life_3.0_db/Books/BNS_Hills/BNS_Bookdown/testbook/images/Painters/", pattern = ".pdf", full.names = TRUE)
# Put figures in pdf
include_graphics(myimages)


#### Table 6-1 ####

# Make painter x feature matrix
# List painters and features
features <- c('French',  'Landscape', 'Portrait', 'Ear', 'Impressionist')
painters <- c('Manet', 'Monet','Van Gogh')
# Prepare matrix
bimat <- matrix(0, ncol = length(features), nrow=length(painters))
# Fill in matrix
colnames(bimat) <- features
rownames(bimat) <- painters
bimat["Manet",] <- c(1,.1,1,0, .5)
bimat["Monet",] <- c(1,1,0,0, 1)
bimat["Van Gogh",] <- c(0,1,.1,1, 1)
# Make table
bimat %>% knitr::kable(caption="A bipartite adjacency matrix linking two node types: painters and features.") %>% 
  kable_styling(bootstrap_options = c("striped", "condensed"), full_width = F)



#### Figure 6-2 ####

## Convert matrix to bipartite graph 
gbn<- graph.incidence(bimat)
## is.bipartite(gbn) # sanity check 
V(gbn)$color <- c(rep('gray80', length(painters)), rep('white', length(features))) # individuals = black, groups = white

# Figure parameters
set.seed(3)
par(mfrow=c(1,1))
par(mar=c(1,1,1,1))
# Set vertex size
c=20 
# Plot graph
plot(gbn, layout=layout_as_bipartite, vertex.label.dist=c(-3,-3, -3,3, 3,3,3, 3), vertex.label.color = "black", vertex.size = c, vertex.label.cex= .8, vertex.label.degree = pi/2, vertex.label.family="Helvetica")


#### Figure 6-3 ####

set.seed(1)
# Make bipartite projections
g.bp <- bipartite.projection(gbn)
par(mfrow=c(1,2))
# Plot painter projection unweighted edges
plot(g.bp[[1]], vertex.label.dist = c(-3,3,-3),  vertex.label.degree = pi/2, vertex.size = c, vertex.label.cex= 1, edge.label = c(1,1,1), vertex.label.family="Helvetica",edge.label.family="Helvetica",vertex.label.color = "black",edge.label.color = "black")

# Plot painter projection weighted edges
plot(g.bp[[1]],  layout = layout_with_fr(g.bp[[1]], weights = (1/E(g.bp[[1]])$weight)^2),  edge.label = signif(1/E(g.bp[[1]])$weight,2),  vertex.label.dist = c(-3.5,3,-3), vertex.label.degree = pi/2, vertex.label.family="Helvetica",edge.label.family="Helvetica",vertex.label.color = "black",edge.label.color = "black")



#### Figure 6-4 ####

### Symmetric difference : non-shared feature distance
### Network based on a count of non-shared features
pnet2 <- matrix(0, nrow=nrow(bimat), ncol=nrow(bimat))
rownames(pnet2)<-rownames(bimat)
colnames(pnet2)<-rownames(bimat)
# Threshold function
thresh<-function(v){as.numeric(v>0)}
# Non-shared-feature
non_shared_count<-function(v,x){sum(x+v==1)}
# Fill out matrix with counts
for(i in 1:nrow(pnet2)){
  for(j in 1:nrow(pnet2)){
    pnet2[i,j] <- non_shared_count(thresh(bimat[i,]),thresh(bimat[j,]))
  }
}

# These aren't necessary here, but they will be below.
# They make 0 weight edges still visible and remove self-loops
pnet2[pnet2==0]<-.01
diag(pnet2) <- 0

# Make graph from pnet2 with weights
nspnet <- graph_from_adjacency_matrix(pnet2, weighted = TRUE, mode = "undirected")

### Jaccard 
pnet <- matrix(0, nrow=nrow(bimat), ncol=nrow(bimat))
rownames(pnet)<-rownames(bimat)
colnames(pnet)<-rownames(bimat)
thresh<-function(v){as.numeric(v>0)}
# Jaccard function
#jaccard_thresh<-function(v,x){sum(thresh(v)*thresh(x))/sum(thresh(thresh(v)+thresh(x)))}
#jaccard<-function(v,x){sum(v*x)/sum(v+x)} ## incorporates reporting %
# 1-sum(shared)/sum(total)
jaccard_dist<-function(v,x){1-sum(thresh(v)*thresh(x))/sum(thresh(thresh(v)+thresh(x)))}

## Jaccard Index
for(i in 1:nrow(pnet)){
  for(j in 1:nrow(pnet)){
    #!CORRECTION: Now uses jaccard distance instead of similarity
    pnet[i,j] <- (jaccard_dist(bimat[i,],bimat[j,]))
  }
}
diag(pnet) <- 0
jpnet <- graph_from_adjacency_matrix(pnet, weighted = TRUE, mode = "undirected")

## Manhattan
mpnet <- matrix(0, nrow=nrow(bimat), ncol=nrow(bimat))
rownames(mpnet)<-rownames(bimat)
colnames(mpnet)<-rownames(bimat)
# Manhattan function: sum of differences across all features
manhattan <- function(v,x){sum(abs(v-x))}

for(i in 1:nrow(mpnet)){
  for(j in 1:nrow(mpnet)){
    mpnet[i,j] <- manhattan(bimat[i,],bimat[j,])
  }
}

diag(mpnet) <- 0
mpnet <- graph_from_adjacency_matrix(mpnet, weighted = TRUE, mode = "undirected")

# Plot network for each of three different distance metrics
set.seed(1)
par(mfrow=c(1,3))
labs <- ifelse(E(nspnet)$weight>.5, E(nspnet)$weight, 0)
plot(nspnet , layout=layout_with_fr(nspnet, weights = 1/E(nspnet)$weight), vertex.label.dist = c(-4,4, 4), edge.label.dist = 3, edge.label = labs,  vertex.color = "white", vertex.label.family="Helvetica",edge.label.family="Helvetica",vertex.label.color = "black",edge.label.color = "black")
cl = -5
# Add title
title("Symmetric difference", line = cl)
plot(jpnet , layout=layout_with_fr(jpnet, weights = (1/E(jpnet)$weight)^3), edge.label = E(jpnet)$weight ,  vertex.color = "white", vertex.label.dist = c(4, -4, -4), vertex.label.family="Helvetica",edge.label.family="Helvetica",vertex.label.color = "black",edge.label.color = "black")
title("Jaccard distance", line = cl)
plot(mpnet , layout=layout_with_fr(mpnet, weights = (1/E(mpnet)$weight)^3) , edge.label = E(mpnet)$weight, vertex.color = "white", vertex.label.dist = c(-5, -5,4), ,vertex.label.color = "black",edge.label.color = "black", vertex.label.family="Helvetica",edge.label.family="Helvetica")
title("Manhattan distance", line = cl)


 
#### Figure 6-5 ####

knitr::include_graphics(path = "./images/Figure6-5.pdf")



#### Table 6.3 ####

# McRae feature norms available from here: https://link.springer.com/article/10.3758/BF03192726#SecESM1
# Download supplementary materials and use CONCS_FEATS_concstats_brm.txt
# Put into SampleDataFilesBNS folder

norms <- read.csv('SampleDataFilesBNS/CONCS_FEATS_concstats_brm.txt', sep="\t", header = T)
# Make adjacency/bipartite matrix with features
concepts <- unique(norms$Concept)
features <- unique(norms$Feature)
wordconc <- matrix(0, nrow=length(concepts), ncol=length(features))
rownames(wordconc) <- concepts
colnames(wordconc) <- features
# Populate the adjacency matrix with feature weights = number saying feature divided by number asked (n=30)
for(i in 1:nrow(norms)){
  wordconc[norms[i,"Concept"], norms[i,"Feature"]] <- norms[i,"Prod_Freq"]/30
}

# Example table
mini_wordconc <- wordconc[c("accordion", "piano", "balloon"),
                          c("a_musical_instrument",
                            "has_keys",
                            "requires_air",
                            "associated_with_polkas",
                            "has_buttons")]
mini_wordconc %>% knitr::kable(digits =2, caption="A sample of the McRae Feature Norms. There are 541 nouns (rows) and 7259 unique features (columns).") %>%
  kable_styling(bootstrap_options = c("striped", "condensed"), full_width = F)

# Kuperman age of acquisition norms (see Chapter 5)
aoab <- read.table('SampleDataFilesBNS/AoA_ratings_Kuperman_et_al_BRM.txt', header = T)


## Raw distinctiveness based on number of nouns that have that feature in feature norms (sum of Distinct = 1/(#of concepts in which feature occurs))
# 'Distinct' is already 1 / # of concepts with that feature, so we'll sum that up for each Concepts features
rawdist<-with(norms, tapply(Distinct, list(Concept), sum))
rawdist <- data.frame(word=rownames(rawdist), rawdist)
# Lowercase 'word' so we can join  
names(aoab)<-c('word',names(aoab[,-1]))
aoab <- aoab[,c(1,5)]
dt <- inner_join(rawdist, aoab) # 492 words

## Prepare dataframes for jaccard index 

dnet <- matrix(0, nrow=nrow(wordconc), ncol=nrow(wordconc))
rownames(dnet)<-rownames(wordconc)
colnames(dnet)<-rownames(wordconc)
thresh<-function(v){as.numeric(v>0)}
jaccard_dist<-function(v,x){1-sum(thresh(v)*thresh(x))/sum(thresh(thresh(v)+thresh(x)))}
## Jaccard Index
for(i in 1:nrow(dnet)){
  for(j in 1:nrow(dnet)){
    dnet[i,j] <- (jaccard_dist(wordconc[i,],wordconc[j,]))
  }
}

dnett<-dnet[rownames(dnet)%in%dt$word,rownames(dnet)%in%dt$word]
jacdist <- data.frame(word = rownames(dnet), jdist = rowSums(dnet))
dt <- inner_join(dt, jacdist) # 492 words

## Non-shared feature network
dnet2 <- matrix(0, nrow=nrow(wordconc), ncol=nrow(wordconc))
rownames(dnet2)<-rownames(wordconc)
colnames(dnet2)<-rownames(wordconc)
thresh<-function(v){as.numeric(v>0)}
non_shared_count<-function(v,x){sum(thresh(x)+thresh(v)==1)}

for(i in 1:nrow(dnet2)){
  for(j in 1:nrow(dnet2)){
    dnet2[i,j] <- non_shared_count(wordconc[i,],wordconc[j,])
  }
}
dnett2<-dnet2[rownames(dnet2)%in%dt$word,rownames(dnet2)%in%dt$word]
nsdist <- data.frame(word = rownames(dnett2), nonshareddist = rowSums(dnett2))
dt <- inner_join(dt, nsdist) # 492 words

## Manhattan
dnet3 <- matrix(0, nrow=nrow(wordconc), ncol=nrow(wordconc))
rownames(dnet3)<-rownames(wordconc)
colnames(dnet3)<-rownames(wordconc)

for(i in 1:nrow(dnet3)){
  for(j in 1:nrow(dnet3)){
    dnet3[i,j] <- sum(abs(wordconc[i,]-wordconc[j,]))
  }
}
dnett3<-dnet3[rownames(dnet3)%in%dt$word,rownames(dnet3)%in%dt$word]
mhdist <- data.frame(word=rownames(dnett3), mdist = rowSums(dnett3))
dt <- inner_join(dt, mhdist)


## Two bipartite projections
bp <- graph.incidence(wordconc, weighted = TRUE)
gbp <- bipartite.projection(bp, multiplicity = FALSE)
# multiplicity = TRUE makes multiple edges -> weight
gbpm <- bipartite.projection(bp, multiplicity = TRUE)
# Inverse to transform to distance
E(gbpm[[1]])$weight <- 1/E(gbpm[[1]])$weight
# Make data.frame 
degdf <- data.frame(word = names(igraph::degree(gbp[[1]])), totdegree = igraph::degree(gbp[[1]], mode = 'total'), weightdeg = igraph::strength(gbpm[[1]]) )
# Combine with other measures using join
dt <- inner_join(dt, degdf)
# Stats
r1 <- with(dt, cor.test(Rating.Mean, totdegree))
r2 <- with(dt, cor.test(Rating.Mean, weightdeg))
r3 <- with(dt, cor.test(Rating.Mean, rawdist))
r4 <- with(dt, cor.test(Rating.Mean, jdist))
r5 <- with(dt, cor.test(Rating.Mean, nonshareddist))
r6 <- with(dt, cor.test(Rating.Mean, mdist))

rs <- c(signif(r1$estimate,2),
       signif(r2$estimate,2), 
       signif(r3$estimate,2),
       signif(r4$estimate,2),
       signif(r5$estimate,2),
       signif(r6$estimate,2))

cis <-  c(
  paste("[",signif(r1$conf.int[1],2),", ",signif(r1$conf.int[2],2),"]", sep = ""),
  paste("[",signif(r2$conf.int[1],2),", ",signif(r2$conf.int[2],2),"]", sep = ""),
  paste("[",signif(r3$conf.int[1],2),", ",signif(r3$conf.int[2],2),"]", sep = ""),
  paste("[",signif(r4$conf.int[1],2),", ",signif(r4$conf.int[2],2),"]", sep = ""),
  paste("[",signif(r5$conf.int[1],2),", ",signif(r5$conf.int[2],2),"]", sep = ""),
  paste("[",signif(r6$conf.int[1],2),", ",signif(r6$conf.int[2],2),"]", sep = ""))


tab <- data.frame( "Model" = c("Simple degree", "Inverse multiplicity", "Raw distinctiveness", "Jaccard distance", "Symmetric difference", "Manhattan distance"), "r"=rs, "CI" = cis)
tab <- tab[order(tab[,2], decreasing = FALSE),]
tab %>% as.matrix() %>% knitr::kable(digits=2,  caption = "Correlations between age of acquisition and the different degree and distinctiveness measures. There are $n=492$ words and each measure is computed as described above. CI indicates the  95\\% confidence interval.") %>% 
   kable_styling(bootstrap_options = c("striped", "condensed"), full_width = F)


### Calculate Manhattan distance within each feature type and correlate with Kuperman's AoA.

output <- matrix(0, ncol=10)

for (n in 1: length(unique(norms$BR_Label)) )
{
  norms_s <- subset(norms, norms$BR_Label == unique(norms$BR_Label)[n])  
  
  uniq_feats_s <- unique(norms_s$Feature)
  uniq_words_s <- unique(norms_s$Concept)
  
  wordconc_s <- matrix(0, nrow=length(uniq_words_s), ncol=length(uniq_feats_s))
  
  rownames(wordconc_s) <- uniq_words_s
  colnames(wordconc_s) <- uniq_feats_s
  
  for(i in 1:nrow(norms_s)){
    wordconc_s[toString(norms_s$Concept[i]), toString(norms_s$Feature[i])] <- norms_s$Prod_Freq[i]/30
  }
  
  dnets <- matrix(0, nrow=nrow(wordconc_s), ncol=nrow(wordconc_s))
  rownames(dnets)<-rownames(wordconc_s)
  colnames(dnets)<-rownames(wordconc_s)
  
  for(i in 1:nrow(dnets)){
    for(j in 1:nrow(dnets)){
      dnets[i,j] <- sum(abs(wordconc_s[i,]-wordconc_s[j,]))
    }
  }
  
  dnetts <- as.data.frame(cbind(rownames(dnets),rowSums(dnets)))
  names(dnetts) <- c("word","Manhattan")
  dnetts <- inner_join(as.data.frame(dnetts),as.data.frame(aoab))
  bufff <- cor.test(dnetts$Rating.Mean,as.numeric(dnetts$Manhattan))
  
  outputs <- as.data.frame (cbind( toString(unique(norms$BR_Label)[n]) , nrow(norms_s) , length(uniq_feats_s) , round(length(uniq_feats_s) / nrow(norms_s), digits = 2) , round(as.numeric(cor.test(dnetts$Rating.Mean,as.numeric(dnetts$Manhattan), nrep = 1000)$estimate), digits = 2) , round(as.numeric(cor.test(dnetts$Rating.Mean,as.numeric(dnetts$Manhattan), method = "spearman")$p.value) , digits = 3) , p.adjust(as.numeric(cor.test(dnetts$Rating.Mean,as.numeric(dnetts$Manhattan), method = "spearman")$p.value), method = "bonferroni", n = length(unique(norms$BR_Label))) ,paste("[", formatC(round(bufff$conf.int[1],2),2,format="f"), ", ", formatC(round(bufff$conf.int[2],2),2,format="f"), "]", sep="")  , as.numeric(cor.test(dnetts$Rating.Mean,as.numeric(dnetts$Manhattan))[2]) , nrow(dnetts)))
  output <- rbind (output, outputs)
}

output <- output[-1,]
names(output) <- c("Feature type", "n_of_features" , "n_of_unique features"  , "proportion_of_unique_features" , "r", "p" , "p_bonf" , "CI" , "df" , "n")
result9 <- output[, c(1, 5,6, 8, 10)]
result9 <- result9[order(result9$p),]
result9 <- result9[,-3]
rownames(result9) <- NULL
result9[2,1] <- "encyclopedic"

knitr::kable(result9, digits=2,  caption = "Correlations between age of acquisition and individual feature types. Each correlation is limited to the $n$ words that have that feature. CI indicates the  95\\% confidence interval.") %>% 
  kable_styling(bootstrap_options = c("striped", "condensed"), full_width = F)




### Calculate Manhattan distance within each for each sub-type of visual-form_and_surface and correlate with Kuperman's AoA.

output2 <- matrix(0, ncol=10)

norms_s1 <- subset(norms, norms$BR_Label == "visual-form_and_surface")

for (n in 1: length(unique(norms_s1$WB_Label)) )
{
  norms_s2 <- subset(norms_s1, norms_s1$WB_Label == unique(norms_s1$WB_Label)[n])
  if (nrow(norms_s2) > 9)
  {
    uniq_feats_s2 <- unique(norms_s2$Feature)
    uniq_words_s2 <- unique(norms_s2$Concept)
    
    wordconc_s2 <- matrix(0, nrow=length(uniq_words_s2), ncol=length(uniq_feats_s2))
    
    rownames(wordconc_s2) <- uniq_words_s2
    colnames(wordconc_s2) <- uniq_feats_s2
    
    for(i in 1:nrow(norms_s2)){
      wordconc_s2[toString(norms_s2$Concept[i]), toString(norms_s2$Feature[i])] <- norms_s2$Prod_Freq[i]/30
    }
    
    dnets2 <- matrix(0, nrow=nrow(wordconc_s2), ncol=nrow(wordconc_s2))
    rownames(dnets2)<-rownames(wordconc_s2)
    colnames(dnets2)<-rownames(wordconc_s2)
    
    for(i in 1:nrow(dnets2)){
      for(j in 1:nrow(dnets2)){
        dnets2[i,j] <- sum(abs(wordconc_s2[i,]-wordconc_s2[j,]))
      }
    }
    
    dnetts2 <- as.data.frame(cbind(rownames(dnets2),rowSums(dnets2)))
    names(dnetts2) <- c("word","Manhattan")
    dnetts2 <- inner_join(as.data.frame(dnetts2),as.data.frame(aoab))
    bufff2 <- spearman.ci(dnetts2$Rating.Mean,as.numeric(dnetts2$Manhattan), nrep = 1000)
    
    outputs2 <- as.data.frame (cbind( toString(unique(norms_s1$WB_Label)[n]) , nrow(norms_s2) , length(uniq_feats_s2) , round(length(uniq_feats_s2) / nrow(norms_s2), digits = 2) , round(as.numeric(spearman.ci(dnetts2$Rating.Mean,as.numeric(dnetts2$Manhattan), nrep = 1000)$estimate), digits = 2) , round(as.numeric(cor.test(dnetts2$Rating.Mean,as.numeric(dnetts2$Manhattan), method = "spearman")$p.value) , digits = 3) , p.adjust(as.numeric(cor.test(dnetts2$Rating.Mean,as.numeric(dnetts2$Manhattan), method = "spearman")$p.value), method = "bonferroni", n = 5) ,paste("[", formatC(round(bufff2$conf.int[1],2),2,format="f"), ", ", formatC(round(bufff2$conf.int[2],2),2,format="f"), "]", sep="")  , as.numeric(cor.test(dnetts2$Rating.Mean,as.numeric(dnetts2$Manhattan))[2]) , nrow(dnetts2)))
    output2 <- rbind (output2, outputs2)
  }
  else
  {
  #  outputs2 <- as.data.frame (cbind( toString(unique(norms_s1$WB_Label)[n]) , nrow(norms_s2) , "NA" , "NA" , "NA" , "NA" , "NA"))
   # output2 <- rbind (output2, outputs2)
  }
}

op2 <- output2[-1,]
names(op2) <- c("Feature type", "n" , "Unique features"  , "proportion_of_unique_features" , "r" , "p" , "p_bonf" , "CI" , "df" , "n" )

op2 <- op2[,c(1,5,6,8, 10)]
result10 <- op2
result10 <- result10[order(result10$p),]
result10 <- result10[,-3]
result10[,1] <- c("Made of (material)", "External component", "External surface property", "Internal component", "Internal surface property")
rownames(result10) <- NULL
knitr::kable(result10, digits = 2,  caption = "Correlations between age of acquisition and individual feature types within visual form and surface. n indicates the number of words with this feature type on which the analysis is based.") %>% 
  kable_styling(bootstrap_options = c("striped", "condensed"), full_width = F)
