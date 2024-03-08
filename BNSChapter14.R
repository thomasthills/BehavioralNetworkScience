
### Behavioral Network Science
### Language, Mind, and Society
# By Thomas T. Hills

# Code to reproduce figures and analyses for

#### Chapter 14  Network Illusions ####

knitr::opts_chunk$set(echo = FALSE)
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(comment = NA)
knitr::opts_chunk$set(warning = FALSE)
options(knitr.table.format = "latex")

library(tinytex)
library(igraph)
library(tidyverse)
library(kableExtra)


#### Figure 14-1 ####

set.seed(1)

# Make random graph
friends <- erdos.renyi.game(8,10, type = "gnm")
# Plot it
plot(friends, layout=layout_in_circle, vertex.label = V(friends)$name, vertex.color = "white")

# Function to compute average friends of neighbors for all nodes in network
friendshipParadox <- function(g){
  # Compute node degrees 
  degrees <- igraph::degree(g)
  # Find neighbors
  neighborlist <- igraph::neighborhood(g, 1, V(g))
  # Get average of neighbors
  avgfriends <- lapply(neighborlist, FUN = function(x) mean(degrees[x[-1]])) 
  # Return neighbors
  return(avgfriends)
}
# Make table using above function
fp <- data.frame(myFriends = igraph::degree(friends), theirFriends = unlist(friendshipParadox(friends)) )

# Add column means
fp <-rbind(fp, colMeans(fp)) 
# Name table
rownames(fp) <- c(1:8, "Average")
# Round the numbers so they look nice
fp <- round(fp, 2)
# Add names
fp <- cbind(node=rownames(fp), fp)

# Print table
kable(fp,row.names=FALSE, booktabs = T, escape = FALSE, caption = "The friendship paradox.") %>%
kable_classic(full_width = F, html_font = "Cambria")


#### Figure 14-2 ####

# Set random seed
set.seed(1)

## Make an edge list with individuals and clubs
n = 10 # People
c = 4 # Clubs
p = .4 # Probability of being in a club

# Build matrix
clubadj <-  matrix(0, nrow = n, ncol = c)
# Name matrix rows and columns
rownames(clubadj) <- 1:10
colnames(clubadj) <-letters[1:c]
# Make random edges
for(i in 1:n){
 for(j in 1:c){
   if(runif(1) < p){
     clubadj[i,j] <- 1
   }
 }
}

# Make graph
gbn<- graph.incidence(clubadj)
# Sanity check 
## is.bipartite(gbn) # sanity check 

# Color nodes
V(gbn)$color <- c(rep('gray80', n), rep('white', c)) # individuals = black, groups = white 

# Class size paradox function (like friendship paradox above)
classSizeParadox <- function(g){
  degrees <- igraph::degree(g)
  classlist <- igraph::neighborhood(g, 1, V(g))
  classsize <- lapply(classlist, FUN = function(x) mean(degrees[x[-1]])) 
  return(classsize)
}

# Make layout
lb <- layout_as_bipartite(gbn)
# Plot graph
plot(gbn, layout=lb[,2:1])

# Compue values and round
avgclasssize <- round(unlist(classSizeParadox(gbn)[1:n]),2)
# Get degree for classes -- class sizes
classSize <- igraph::degree(gbn)[(n+1):(n+c)]
# Make table
fp <- data.frame(Node = 1:n, 
                 "Avg_Class_Sizes" = round(avgclasssize,2), 
                 Class = c(names(classSize), rep("",n-length(names(classSize)))), 
                           c(classSize, rep("",n-length(names(classSize)))))
         
# Make bottom row values        
bottomrow <- c("Average", round(mean(avgclasssize),2), "", round(mean(classSize),2))

# Add bottom row
fp <- rbind(fp, bottomrow)

# Print above table
kable(fp,row.names=FALSE, booktabs = T, escape = FALSE, caption = "The class size paradox.", col.names = c("Node", "Avg. Class Sizes", "Class", "Size")) %>%
kable_classic(full_width = F, html_font = "Cambria") %>% 
  add_header_above(c("Students" = 2, "Classes" = 2), bold = TRUE) %>% 
  column_spec(3, border_left = TRUE) %>%
  row_spec(10,  extra_css = "border-bottom: 1px solid") 


#### Figure 14-3 ####

# Plot parameters
par(mfrow=c(2,2))
par(mar=c(1,1,1,1))
set.seed(1)
# Nodes
n = 10
# Make network
gmi <- sample_fitness(n, sample(c(.01,.01,.02), n, replace=TRUE))
# Assign colors
V(gmi)$color = "gray20"
V(gmi)[V(gmi)[order(igraph::degree(gmi), decreasing=TRUE)][1:3]]$color <- "coral1"
# Plot network
plot(gmi, layout=layout_in_circle(gmi), vertex.label.dist = 2)

# Plot points
plot(1,1,type = "n", axes = FALSE, xlim = c(0,max(igraph::degree(gmi))+2), ylim =c(0, n))
for(i in 1:n){
  text(1,11-i, i)
  for(j in 1:igraph::degree(gmi)[i]){
    points(1+j,11-i, pch=16, cex = 2, col = V(gmi)[neighbors(gmi, i)]$color[j])
  }
}
points(c(1,1,1),11-which(V(gmi)$color=="coral1"), pch = 0, cex = 2.5)

# Do same for second network
set.seed(13)
n = 10
gmi <- sample_fitness(n, sample(c(.01,.01,.02), n, replace=TRUE))
V(gmi)$color = "gray20"
V(gmi)[sample(1:n,3)]$color <- "coral1"
plot(gmi, layout=layout_in_circle(gmi), vertex.label.dist = 2)

plot(1,1,type = "n", axes = FALSE, xlim = c(0,max(igraph::degree(gmi))+2), ylim =c(0, n))
for(i in 1:n){
  text(1,11-i, i)
  for(j in 1:igraph::degree(gmi)[i]){
    points(1+j,11-i, pch=16, cex = 2, col = V(gmi)[neighbors(gmi, i)]$color[j])
  }
}
points(c(1,1,1),11-which(V(gmi)$color=="coral1"), pch = 0, cex = 2.5)





#### Figure 14-4 ####

# Plot parameters
par(mfrow=c(1,1))
set.seed(8)  
# Nodes
n = 100
# Assign types
typeS <- rep(c(0,1), each = n/2)
cs = 2 # Number of places to apply (number of prey)
c = (n+1):(n+cs) # Number of places to apply
ctype = rep(c("h","s"), each = cs/2) # Assign types to places to apply
p0 = rep(c(.8,.2), each = cs/2) # p of 0 favors type 'h'
p1 = rep(c(.2,.8), each = cs/2) # p of 1 favors type 's'
c0 = .8 # Probability of acceptance
c1 = .2 # Probability of acceptance 
appS <- rep(NA, length(n))
acceptedS <- rep(NA, length(n))
## Where do they apply
for(k in 1:n){ # For each student
  if(typeS[k]==0){ # if type 0 set p to p0 
    # sample  h or s based on probabilities
    appS[k] = sample(c, 1, prob =p0 )
  } else { # otherwise set to p1
    appS[k] = sample(c, 1, prob =p1 )
  }      
}
## Who gets accepted
for(k in 1:n){
  # If h or s
 if(appS[k] %in% (n+1):(n+cs/2)){
   # Compute success or not based on c0
   acceptedS[k] = ifelse(runif(1)<c0, 1,0)
 } else {
   # Or c1
   acceptedS[k] = ifelse(runif(1)<c1, 1,0)
 } 
}
## Make data frame with students applications and acceptances (people, prey, success)
appsac <- data.frame(stud = 1:n, appS, acceptedS)
appsac2 <- as.matrix(appsac)
# Make edgelist from successful
edgelst <- rbind(appsac2[,c(1,2)],appsac2[appsac2[,3]==1,c(2,1)])
# Make network
sp <- graph_from_edgelist(edgelst)
# Assign values to network nodes and edges
V(sp)$name = V(sp)
# Set size for applicants/hunters
V(sp)$size = ifelse(V(sp)$name <= n, 3, 10)
# Set color for males and females
V(sp)$color = ifelse(V(sp)$name <= n/2,"black", "gray80")
# Set color for prey
V(sp)$color[V(sp)$name %in% c[1:cs/2]] <- "salmon2"
V(sp)$color[V(sp)$name %in% c[(cs/2+1):cs]] <- "navyblue"
# Remove labels
V(sp)$name<- NA
# Plotting margins
par(mar=c(1,1,1,1))
# Is applicant successful
m <-  is.mutual(sp)
# Set border colors 
# Indegree is positive, then success
V(sp)$frame.color[igraph::degree(sp, mode ="in")>0 ] <- "red"
# set edge weights based on successes (they're equal here, but they don't have to be)
E(sp)$weight <- ifelse(m, .1,.1)
# Set color based on success
E(sp)$color <- ifelse(m, "black", "gray50")
# Plot network
plot(sp, edge.arrow.size =.3, edge.curved = 0 , layout = layout_with_fr, vertex.label = NA)


## Count up successes
appsac <- tibble(appsac)
appsaco <- appsac %>% mutate(typeS0=ifelse(stud %in% 1:(n/2), TRUE, FALSE)) %>% mutate(typeC0=ifelse(appS %in% c[1:(cs/2)], TRUE, FALSE))
accepts <- with(appsaco, table(typeS0, acceptedS))
applic <- with(appsaco, table(typeS0, typeC0, acceptedS))
applic2 <- with(appsaco, table(typeS0, typeC0))
accepts2 <- with(appsaco, table(typeC0, acceptedS))
## typeS0 apply to typeC0

## Accounting for different applicants/hunters and depts/prey

totals <- c(sum(applic),sum(applic[,,2]),sum(applic[,,1]), sum(applic[,,2])/sum(applic), sum(applic[,,1])/sum(applic))

mtotal <- c(n/2, accepts[2,2], accepts[2,1],accepts[2,2]/(n/2), accepts[2,1]/(n/2) )
ftotal <- c(n/2, accepts[1,2], accepts[1,1], accepts[1,2]/(n/2), accepts[1,1]/(n/2) )
mtotal0 <-c(applic2[2,2], applic[2,2,2], applic[2,2,1], applic[2,2,2]/applic2[2,2], applic[2,2,1]/applic2[2,2]) 

mtotal1 <-c(applic2[2,1],applic[2,1,2], applic[2,1,1], applic[2,1,2]/applic2[2,1], applic[2,1,1]/applic2[2,1])

ftotal0 <-c(applic2[1,2],applic[1,2,2], applic[1,2,1], applic[1,2,2]/applic2[1,2], applic[1,2,1]/applic2[1,2])

ftotal1 <-c(applic2[1,1], applic[1,1,2], applic[1,1,1], applic[1,1,2]/applic2[1,1], applic[1,1,1]/applic2[1,1])

typeC0 <- c(sum(applic2[,2]), accepts2[2,2], accepts2[2,1], accepts2[2,2]/sum(applic2[,2]), accepts2[2,1]/sum(applic2[,2]))

typeC1 <- c(sum(applic2[,1]),accepts2[1,2], accepts2[1,1], accepts2[1,2]/sum(applic2[,1]), accepts2[1,1]/sum(applic2[,1]))

spresults <- rbind(totals, mtotal, ftotal, mtotal0,ftotal0, mtotal1,  ftotal1, typeC0, typeC1)

# Make and produce table
spresults[,4:5] <- round(spresults[,4:5], 2)
colnames(spresults) <- c("Total", "Successes", "Failures", "P(Success)", "P(Failure)")
rownames(spresults) <- c("Total foraging attempts", "Women", "Men","Lizards--Women", "Lizards--Men","Kangaroo--Women", "Kangaroo--Men", "Lizards", "Kangaroo")
kable(spresults, caption = "Simpson's Paradox among hunters and gatherers.", booktabs = T
) %>%
kable_classic(full_width = F, html_font = "Cambria")



#### Figure 14-5 ####

set.seed(1) # 
# Number of people
n = 100
# Assign gender
typeS <- rep(c(0,1), each = n/2)
# How many strategy types
cs = 10 
# Number them
c = (n+1):(n+cs)
# High or low risk strategies
ctype = rep(c("h","s"), each = cs/2)
# Assign 'application' probabilities for men and women 
p0 = rep(c(.8,.2), each = cs/2) # p of 0 favors type 's'
p1 = rep(c(.2,.8), each = cs/2) # p of 0 favors type 's'
# Assign success probabilities from beta distribution
csucc <- c(rbeta(cs/2,8,2), rbeta(cs/2, 2, 8)) # probability of success 
# Data buffer
appS <- c()
## Where do they apply
for(k in 1:n){ # For each person 
  for(ci in 1:cs){ # Assign prey they hunt 
    if(typeS[k]==0){ # If type 0 set p to p0 
      appS <- rbind(appS, c(k, ifelse(runif(1)<p0[ci], c[ci], 0)))
    } else { # Otherwise set to p1
      appS <- rbind(appS, c(k, ifelse(runif(1)<p1[ci], c[ci], 0)))
    }  
  }
}
## The above decides which prey to hunt
## Now see which prey they are successful with
## Limit to attempts 
appS <- appS[appS[,2]!=0,] # remove prey not hunted for each (==0)
# Make vector for success
acceptedS <- rep(NA, nrow(appS))
#Compute successes for each person
for(at in 1:nrow(appS)){
   # We only do this once per prey kind, but could do this many times
   acceptedS[at] = ifelse(runif(1)<csucc[appS[at,2]-100], 1,0)
}
## Create data frame of 'application/attempts' and 'successes'
appsac <- data.frame(appS[,1], appS[,2], acceptedS)
#Make it a matrix
appsac2 <- as.matrix(appsac)
# Make edgelist from agents to prey and reciprocate successes
edgelst <- rbind(appsac2[,c(1,2)],appsac2[appsac2[,3]==1,c(2,1)])
# Make graph
sp <- graph_from_edgelist(edgelst)
# Assign colors
V(sp)$name = V(sp)
V(sp)$size = ifelse(V(sp)$name <= n, 3, 10)
V(sp)$color = ifelse(V(sp)$name <= n/2,"black", "gray80")
V(sp)$color[V(sp)$name %in% c[1:cs/2]] <- "salmon2"
V(sp)$color[V(sp)$name %in% c[(cs/2+1):cs]] <- "navyblue"
V(sp)$name <- NA
  par(mar=c(1,1,1,1))
m <-  is.mutual(sp)
V(sp)$frame.color[igraph::degree(sp, mode ="in")>0 ] <- "red"
V(sp)$frame.color[V(sp)$name %in% c] <- "black"
E(sp)$weight <- ifelse(m, .5,.1)
E(sp)$color <- ifelse(m, "black", "gray50")
edge_col <- function(x, alpha)  ifelse(x > .2, rgb(0,0,0,alpha = .5), rgb(0,0,0,alpha = .1))
par(mar=c(0,0,0,0))
plot(sp, edge.arrow.size =.3, edge.curved = 0 , layout = layout_with_fr, vertex.label = NA, edge.color = edge_col(E(sp)$weight, alpha = .2))


## Count up successes
appsac <- tibble(appsac)
colnames(appsac) <- c("ID", "Strategy", "Success")
# Assign gender and prey difficulty
appsaco <- appsac %>% mutate(typeS0=ifelse(ID %in% 1:(n/2), TRUE, FALSE)) %>% mutate(typeC0=ifelse(Strategy %in% c[1:(cs/2)], TRUE, FALSE))
# Count successes by gender (S0 = female)
accepts <- with(appsaco, table(typeS0, acceptedS))
## Count success by gender and prey type
applic <- with(appsaco, table(typeS0, typeC0, acceptedS))
# Count prey by gender
applic2 <- with(appsaco, table(typeS0, typeC0))
# Count prey by successes
accepts2 <- with(appsaco, table(typeC0, acceptedS))

# Count total successes and failures
totals <- c(sum(applic),sum(applic[,,2]),sum(applic[,,1]), sum(applic[,,2])/sum(applic), sum(applic[,,1])/sum(applic))
# Do the accounting
mtotal <- c(sum(accepts[2,]), accepts[2,2],
            accepts[2,1],accepts[2,2]/sum(accepts[2,]),
            accepts[2,1]/sum(accepts[2,]) )
ftotal <- c(sum(accepts[1,]), accepts[1,2],
            accepts[1,1], accepts[1,2]/sum(accepts[1,]),
            accepts[1,1]/sum(accepts[1,]) )
mtotal0 <-c(applic2[2,2], applic[2,2,2],
            applic[2,2,1], applic[2,2,2]/applic2[2,2],
            applic[2,2,1]/applic2[2,2]) 

mtotal1 <-c(applic2[2,1],applic[2,1,2], applic[2,1,1], applic[2,1,2]/applic2[2,1], applic[2,1,1]/applic2[2,1])

ftotal0 <-c(applic2[1,2],applic[1,2,2], applic[1,2,1], applic[1,2,2]/applic2[1,2], applic[1,2,1]/applic2[1,2])

ftotal1 <-c(applic2[1,1], applic[1,1,2], applic[1,1,1], applic[1,1,2]/applic2[1,1], applic[1,1,1]/applic2[1,1])

typeC0 <- c(sum(applic2[,2]), accepts2[2,2], accepts2[2,1], accepts2[2,2]/sum(applic2[,2]), accepts2[2,1]/sum(applic2[,2]))

typeC1 <- c(sum(applic2[,1]),accepts2[1,2], accepts2[1,1], accepts2[1,2]/sum(applic2[,1]), accepts2[1,1]/sum(applic2[,1]))

spresults <- rbind(totals, mtotal, ftotal, mtotal0,ftotal0, mtotal1,  ftotal1, typeC0, typeC1)

spresults[,4:5] <- round(spresults[,4:5], 2)
colnames(spresults) <- c("Total", "Successes", "Failures", "P(Success)", "P(Failure)")
rownames(spresults) <- c("Total foraging attempts", "Women", "Men","Low risk--Women", "Low risk--Men","High risk--Women", "High risk--Men", "Low risk", "High risk")

kable(spresults, booktabs = T, caption = "Simpson's Paradox among hunters and gatherers with multiple strategies per individual.") %>%
kable_classic(full_width = F, html_font = "Cambria")


# Gerrymandering figure not in the book

## library(redist)
## iowa_map <- redist_map(iowa, existing_plan = cd_2010)

## plot(iowa_map, adj=T) + plot(iowa_map)


##iowa <- redist_map(iowa, existing_plan = cd_2010, pop_tol = 0.05, total_pop = pop)
##plans <- redist_smc(iowa, nsims = 100, silent = FALSE)
##redist.plot.plans(plans, c(1, 2, 3, 7), iowa)



#### Figure 14-6 ####

# Figure parameters
par(mar=c(2,2,2,2))

# Make tree network
trip <- make_tree(40, 3, mode = "undirected")
# Assign colors
V(trip)$color <- "white"
# Assign grays
V(trip)$color[c(4, 5, 8,11, 12, 13, 14,15,16,17,23,24,25,26, 32,33,34,35,36,37,38,39,40)] <- "gray50"
# Plot network 
plot(trip, layout = layout_as_tree(trip), vertex.size = 4, vertex.label = NA)
# Add labels
text(1.2, 1, "State \n (100%)", cex = .9)
text(1.2, .33, "District\n (66%)", cex = .9)
text(1.2, -.33, "Parish\n (44%)", cex = .9)
text(1.2, -1, "People \n (37%)", cex = .9)


#### Figure 14-7 ####

# Plot parameters
par(mar=c(5,5,5,5))
par(mfrow=c(1,2))

# Set random seed
set.seed(1)
# Make graph
gig <- graph_from_literal(1-2-3, 6-4-5-6)
# Assign colors
V(gig)$color[4:6] <- "red"
V(gig)$color[1:3] <- "blue"

# Plot network
plot(gig, vertex.label = NA, vertex.size = 30)

# Make new network
gig <- graph_from_literal(1-2-3, 6-4-5-6,6-3,4-3,5-3)
# Assign colors
V(gig)$color[4:6] <- "red"
V(gig)$color[1:3] <- "blue"
# Plot network
plot(gig, vertex.label = NA,  vertex.size = 30)
