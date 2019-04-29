# LAB 1 Association Rules

# install.packages("arules")
library(arules)

# # loading the groceries data
# data(Groceries)
# 
# # from lecture notes "DataMining2d2019.pdf"
# fit <- apriori(Groceries,parameter=list(confidence=0.5,support=0.01))
# fit <- sort(fit,by="lift")
# inspect(fit)
# 
# # interestMeasure() gives a measure called the hyperLift for each rule.
# interestMeasure(fit,"hyperLift",Groceries)


######  LAB 1  ######
dat <- read.table("http://mathsci.ucd.ie/~brendan/data/Old/nltcs.txt",header=TRUE)

#look at the first few rows of the data and the last few
head(dat)
tail(dat)

#looking at how many people are in the data
sum(dat$COUNT)

# Count how many patterns occur in the data and call it M.
M <- nrow(dat)
# Make a vector of numbers from 1 to M.
indices <- 1:M
# Extract the count for how often each pattern happens.
counts <- dat$COUNT

# Construct a vector where the row number of each pattern
# records the number of times that the pattern arises
# in the data. The rep() command is used for this.
rowindices <- rep(indices,counts)

# Now, let's create the matrix nltcsmat.
nltcsmat <- dat[rowindices,]

# Let's drop the last two columns because they're not needed
nltcsmat <- nltcsmat[,-(17:18)]
# Let's reorder the columns, so that they give disabilities
# from 1 to 16 instead of 16 to 1.
nltcsmat <- nltcsmat[,16:1]

# colnames(nltcsmat) <- c("eating",
# "getting in/out of bed",
# "getting around inside",
# "dressing",
# "bathing",
# "getting to the bathroom or using toilet",
# "doing heavy house work",
# "doing light house work",
# "doing laundry",
# "cooking",
# "grocery shopping",
# "getting about outside",
# "traveling",
# "managing money",
# "taking medicine",
# "telephoning")

# Let's coerce the data.frame into a matrix.
nltcsmat <- as.matrix(nltcsmat)

# to turn the data into the transaction format for the arules command
nltcs <- as(nltcsmat,"transactions")

# Checking The Data
table(nltcsmat[,7])  # doing heavy house work
table(nltcsmat[,8])  # doing light house work
table(nltcsmat[,7], nltcsmat[,8])

# Get a table of the outcome for each disability
aaa = apply(nltcsmat,2,table)

# Let's look at the distribution of the number of disabilities per person.
# First, store the number of disabilties per person & compute some summaries.
disabilitycount <- apply(nltcsmat,1,sum)
hist(disabilitycount)
summary(disabilitycount)



###############  ###############
# Get a table of the disability numbers
table(disabilitycount)

fit<-apriori(nltcs,parameter=list(support=0.35, confidence=0.5))
fit<-sort(fit,by="confidence")
inspect(fit)


# Add P(A) and P(B) to the output
qual <- quality(fit)
PA <- qual$support/qual$confidence
PB <- qual$confidence/qual$lift
quality(fit) <- data.frame(qual,PA,PB)
inspect(fit)


# Add the Lift bounds to the output
UB <- cbind(1/PA,1/PB)
UB <- apply(UB,1,min)
LB <- cbind(0,1/PA+1/PB-1/(PA*PB))
LB <- apply(LB,1,max)
quality(fit) <- data.frame(qual,PA,PB,LB,UB)
inspect(fit)

# Add standardized lift
sLift <- (qual$lift-LB)/(UB-LB)
quality(fit) <- data.frame(qual, PA, PB, LB, UB, sLift)
fit   <- sort(fit, by="support")
inspect(fit)


fit   <- sort(fit, by="lift")
inspect(fit)

# Try removing redundant rules
# Firstly, let's just inspect the redundant rules
inspect(fit[is.redundant(fit)])

# Secondly, if you want to remove these rules entirely...
fit <- fit[!is.redundant(fit)]
inspect(fit)




# # Instead use tighter lift bounds which depend on the support and confidence thresholds
# # Recompute the standardized lift
# supplb <- 0.45
# conflb <- 0.8
# LB2 <- cbind(0,1/PA+1/PB-1/(PA*PB),supplb/(PA*PB),conflb/PB)
# LB2 <- apply(LB2,1,max)
# sLift2 <- (qual$lift-LB2)/(UB-LB2)
# quality(fit) <- data.frame(qual, PA, PB, LB2, UB, sLift2)
# fit    <- sort(fit, by="sLift2")
# inspect(fit)


#############################################
# vignette("arulesViz")
# installing visualization package as a way of exploring your association rule analysis visually
# install.packages("arulesViz")
library(arulesViz)
#############################################
fit = fit[1:29]
inspect(fit)

## graphs only work well with very few rules
subrules2 <- sample(fit, 15)
plot(fit, method="graph")
## igraph layout generators can be used (see ? igraph::layout_)
plot(subrules2, method="graph", control=list(layout=igraph::in_circle()))
plot(subrules2, method="graph", control=list(
  layout=igraph::with_graphopt(spring.const=5, mass=50)))

plot(subrules2, method="graph", control=list(type="itemsets"))
## try: plot(subrules2, method="graph", interactive=TRUE)
## try: plot(subrules2, method="graph", control=list(engine="graphviz"))


## parallel coordinates plot
plot(subrules2, method="paracoord")
plot(subrules2, method="paracoord", control=list(reorder=TRUE))

## Doubledecker plot only works for a single rule
oneRule <- sample(fit, 1)
plot(oneRule, method="doubledecker", data = nltcsmat)

## use iplots (experimental)
## try: sel <- plot(rules, method="iplots", interactive=TRUE)


## for itemsets
itemsets <- eclat(Groceries, parameter = list(support = 0.02, minlen=2))
plot(itemsets)
plot(itemsets, method="graph")
plot(itemsets, method="paracoord", control=list(alpha=.5, reorder=TRUE))

## add more quality measures to use for the scatterplot
quality(itemsets) <- interestMeasure(itemsets, trans=Groceries)
head(quality(itemsets))
plot(itemsets, measure=c("support", "allConfidence"), shading="lift")

###############   PLOTTING   ###################



plot(fit, measure = c("support", "lift"), shading = "confidence")

subrules <- head(fit, n = 10, by = "lift")
plot(subrules, method = "graph")

plot(fit, method = "grouped")

# subrules <- fit[quality(fit)$confidence > 0.8]
plot(subrules, method = "matrix", measure = "lift")


cormat = cor(dat)
library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)


library(ggplot2)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()