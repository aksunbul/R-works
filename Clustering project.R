# Read in data from the web
dat <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/voting-records/house-votes-84.data",sep=",")

# Give sensible column names to data.
colnames(dat) <- c("Party",paste("Vote",1:16,sep=""))

# Look at the data
dat

# Note that the ?'s are people being absent from the vote.
# We recode the ?'s as n's
dat[dat=="?"] <- "n"

# Removing "party" from the data so that we only analyze votes.
X<-dat[,-1]

# storing the indices of democrats as true
rep_dem = c(dat[1]=="democrat")

# Let's make a binary version of the data (1="y" and 0="n")
X <- 1L*(X=="y")
X

X_dem = X[rep_dem,]
X_rep = X[!rep_dem,]

# num_of_vot represents sum of number of yes votes of each person 
num_of_vot = apply(X,2,table)
num_of_dem = apply(X_dem,2,table)
num_of_rep = apply(X_rep,2,table)
yes_votes = rbind(num_of_vot[1,],num_of_dem[1,],num_of_rep[1,])
yes_votes

par(mfrow = c(1, 2))
# plot(1:16,yes_votes[1,],col="red")
plot(1:16, yes_votes[2,],col="red",yaxt='n',xaxt='n', ann=FALSE, pch=19)
par(new=TRUE)
plot(1:16, yes_votes[3,],col="blue",  pch=19, main = "Red->Democrat, Blue->Republican", xlab="votes", ylab="number of yes votes")
#legend(12,135, legend=c("dem", "rep"), col=c("red", "blue"), lty=19, cex = 0.9, pt.cex = 0.15)
par(new=FALSE)
#axis(1, t = seq(1, 16, by = 1), las=2)

vot_of_each = apply(X,1,sum)
hist(vot_of_each, main = "ALL")

par(mfrow = c(1, 3))
# vot_of_each represents sum of number of yes votes of each person 
vot_of_each = apply(X,1,sum)
hist(vot_of_each, main = "ALL")
summary(vot_of_each)
table(vot_of_each)

vot_of_dem = apply(X_dem,1,sum)
vot_of_rep = apply(X_rep,1,sum)
hist(vot_of_dem, main = "Democrat")
hist(vot_of_rep, main = "Republican")

mtext("Sum of number of yes votes of", side = 3, line =-1.5,outer = TRUE)
par(mfrow = c(1, 1))


#Create a scaled version of X and call it Xs.
Xs<-scale(X)

par(mfrow = c(1, 2))

# finding the within sum of squares values for k 1:10 in k-means
tot.withinss = c()
for (i in 1:10) {
  fitkm <- kmeans(X, centers=i)
  tot.withinss = c(tot.withinss,fitkm$tot.withinss)
}
plot(1:10, tot.withinss, xlab = "k", ylab = "Total Within SS", main = "the within sum of squares values versus k")


# finding the within sum of squares values for k 1:10 in k-means
tot.withinss_scaled = c()
for (i in 1:10) {
  fitkm <- kmeans(Xs, centers=i)
  tot.withinss_scaled = c(tot.withinss_scaled,fitkm$tot.withinss)
}
plot(1:10, tot.withinss_scaled, xlab = "k", ylab = "Total Within SS", main = "k means with scaled data")

par(mfrow = c(1, 1))

# Run k-means (with K=2) and store the resulting fit in fitkm.
fitkm <- kmeans(X, centers=3)

# Cluster centres for k=2
fitkm$centers

# Cluster sizes for k=2
fitkm$size

############  FCLUST  ###############
# install.packages("fclust")
library(fclust)

# value: vector containing the loss function values for the RS starts
fuzz_value = c()
for (i in 1:10) {
  a = FKM(X, k = i)
  fuzz_value = c(fuzz_value,a$value)
}
plot(1:10, tot.withinss, xlab = "k", main = "loss function values versus k")

a = FKM(X, k = 3)
b=a$clus
# number of elements in each cluster
table(b[,1])

############  K-MEDOIDS  ###############
# Load the cluster library
library(cluster)

# The pam() function needs the data to be in a distance matrix form.
# We make a distance matrix called d.
d<-dist(X,method="euclidean")
par(mfrow = c(1, 2))
# finding swap values for k 1:10 in k-medoids
swap = c()
for (i in 1:10) {
  fitpam<-pam(d,k=i)
  swap = c(swap, fitpam$objective[2])
}
plot(1:10, swap, xlab = "k", ylab = "swap", main = "the swap values versus k using euclidean distance ")

d<-dist(X,method="manhattan")
swap = c()
for (i in 1:10) {
  fitpam<-pam(d,k=i)
  swap = c(swap, fitpam$objective[2])
}
plot(1:10, swap, xlab = "k", ylab = "swap", main = "the swap values versus k using manhattan distance")
par(mfrow = c(1, 1))



fitpam<-pam(d,k=2)
fitpam

# We can construct a table to compare the k-means and PAM results
table(fitkm$cluster,fitpam$clustering)



############  silhoutte  ###############

#library(cluster)
par(mfrow = c(2, 2))

dist_slht <- dist(X, method="euclidean")^2

for (i in c(2,3,4,5)){
  fitkm <- kmeans(X, centers=i)
  sil <- silhouette(fitkm$cluster,dist_slht)
  silvals<-sil[,3]
  avg = round(mean(silvals), digits = 2)
  hist(silvals, main = paste("silhoutte values with k =",  i,"with mean " ,  avg, sep=" "))
}

par(mfrow = c(1, 1))

fitkm <- kmeans(X, centers=3)
sil <- silhouette(fitkm$cluster,dist_slht)
plot(sil)


par(mfrow = c(2, 2))

# dist_slht <- dist(X, method="euclidean")^2
# 
# for (i in c(2,3,4,5)){
#   fitkm <- kmeans(X, centers=i)
#   sil <- silhouette(fitkm$cluster,dist_slht)
#   plot(sil)
# }

par(mfrow = c(1,1))

# 
# 
# # To construct the silhouette plot, we do the following:
# # Construct a distance matrix using squared Euclidean distance
# dist_slht <- dist(X, method="euclidean")^2
# # Compute the silhouette for each observation
# sil <- silhouette(fitkm$cluster,dist_slht)
# # Plot the silhouette plot
# plot(sil)
# 

# 
# silvals<-sil[,3]
# hist(silvals, main = "silhoutte values")



############  rand  ###############
# 
# fitkm <- kmeans(X, centers=3)
# fitpam<-pam(d,k=3)
# 
# # Tabulate the results
# tab<-table(fitkm$cluster,fitpam$clustering)
# tab
# Compute the Rand and adjusted Rand indices
# install.packages('e1071')
library(e1071)
# classAgreement(tab)
# matchClasses(tab)

# test for whether results agree or not with different k values
par(mfrow = c(1,3))
diag = c()
for (i in 1:10) {
  fitkm <- kmeans(X, centers=i)
  fitpam<-pam(d,k=i)
  tab<-table(fitkm$cluster,fitpam$clustering)
  diag = c(diag, 1 - classAgreement(tab)$diag)
}
plot(1:10, diag, xlab = "k", ylab = "swap", main = "k-means versus k-medoids")

diag = c()
for (i in 1:10) {
  fitkm <- kmeans(X, centers=i)
  a = FKM(X, k = 3)
  b=a$clus
  cc=b[,1]
  tab<-table(fitkm$cluster,cc)
  diag = c(diag, 1 - classAgreement(tab)$diag)
}
plot(1:10, diag, xlab = "k", ylab = "swap", main = "k-means versus fuzzy k-means")

diag = c()
for (i in 1:10) {
  a = FKM(X, k = 3)
  b=a$clus
  cc=b[,1]
  fitpam<-pam(d,k=i)
  tab<-table(cc,fitpam$clustering)
  diag = c(diag, 1 - classAgreement(tab)$diag)
}
plot(1:10, diag, xlab = "k", ylab = "swap", main = "fuzzy k-means versus k-medoids")

par(mfrow = c(1,1))

