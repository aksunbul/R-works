# Read in online copy of the data 
titanic<-read.table("http://biostat.mc.vanderbilt.edu/wiki/pub/Main/DataSets/titanic.txt", sep=",", header=TRUE)

head(titanic)
titanic2 = titanic[,2:ncol(titanic)]

# Ensure that any categorical variables are coded as factor
titanic2$pclass<-as.factor(titanic2$pclass)
titanic2$survived<-as.factor(titanic2$survived)
titanic2$embarked<-as.factor(titanic2$embarked)
titanic2$sex<-as.factor(titanic2$sex)
head(titanic2)


# Fit the logistic regression model
fit2<-glm(survived~pclass+age+sex+embarked,data=titanic2,family="binomial")
# Look at the output
summary(fit2)


# Fit the logistic regression model
fit<-glm(survived~pclass+age+sex,data=titanic2,family="binomial")
# Look at the output
summary(fit)

# Odds ratio
beta<-coef(fit)
odd_ratios = exp(beta)

summ<-summary(fit)
betaLB<-summ$coef[,1]-qt(0.975,summ$df.residual)*summ$coef[,2]
betaUB<-summ$coef[,1]+qt(0.975,summ$df.residual)*summ$coef[,2]
BETA<-cbind(betaLB,beta,betaUB)
BETA

# Compute odds & confidence limits for odds
exp(BETA)






# Look at predicted probabilities
pred <- predict(fit,type="response")
# pred

# Plot the predicted values versus the deviance residuals.
plot(pred,residuals(fit,type="deviance"))
resid = residuals(fit,type="deviance")

# Plot the predicted values versus the deviance residuals.
plot(pred,residuals(fit,type="pearson"))

# Fit a local regression and draw a confidence interval
plot(pred, resid)
abline(h=0, lty=2, col="grey")
rl <- loess(resid ~ pred)
lines(x=pred[order(pred)], y=rl$fitted[order(pred)], col="black", lwd=2)
y  <- predict(rl, se=TRUE)
segments(pred,y$fit + qnorm(0.975) * y$se.fit, pred , y$fit - qnorm(0.975) * y$se.fit, col="green")


## ~/Desktop/MSc Statistics/Statistical ML/Labs/Lab 3/assignment3statisticalml/
#install.packages("~/Desktop/MSc Statistics/Statistical ML/Labs/Lab 3/assignment3statisticalml/binomTools_1.0-1.tar.gz", repos = NULL, type="source")

#install.packages("C:/Users/aksunbul/Desktop/MSc Statistics/Statistical ML/project/binomTools_1.0-1.tar.gz", repos = NULL, type="source")
library(binomTools)

# The HLtest() command needs the logistic regression output
# to be put into Rsq format. The Rsq() command does this.
HLtest(Rsq(fit))

# The X2GOFtest() command needs the logistic regression output
# to be put into Rsq format. The Rsq() command does this.
X2GOFtest(Rsq(fit))


# Load the ROCR library
# install.packages("ROCR")
library(ROCR)

# complete.cases(titanic2)
titanic3 <- titanic2[complete.cases(titanic2), ]

plot(titanic3$sex,titanic3$survived)


# Take the predicted probabilities from earlier and the observed response
# patterns and pass them through the prediction() command
predobj<-prediction(pred,titanic3$survived)
# Complete the TPR and FPR for the model (as the threshold varies).
# Many other performance measures can be computed using the performance() command.
# Read the help command for the function to look at these.
# Use performance() to help you construct a precision-recall curve.
perf <- performance(predobj,"tpr","fpr")
# perf1 <- performance(predobj, "sens")

# install.packages("pROC")
library(pROC)
#apply roc function
analysis <- roc(response=titanic3$survived, predictor=pred)

#Find t that minimizes error
e <- cbind(analysis$thresholds,analysis$sensitivities+analysis$specificities)
opt_t <- subset(e,e[,2]==max(e[,2]))[,1]


# Plot the ROC curve.
# Can you find the ‘optimal threshold’ and highlight the corresponding point on your ROC curve?
plot(perf, colorize=TRUE,print.cutoffs.at=opt_t, text.adj=c(-0.2,1.7))

# Can you add the 45 degree line?
abline(a = 0,b = 1,col = "red")

opt_t #print optimal threshold

# Can you quantify performance using this single threshold?
pred2 = predict(fit, type = "response", newdata = titanic3)
a = table(titanic3$survived,pred2 >= opt_t)

# Performance measure with optimal threshold
threshold = opt_t
# Sensitivity
sens = a[4]/(a[2]+a[4])
# Specificity
spec = a[1]/(a[1]+a[3])
# Precision (PPV)
prec = a[4]/(a[4]+a[3])
# NPV
npv = a[1]/(a[1]+a[2])
# Accuracy
acc = (a[1]+a[4])/(a[1]+a[3]+a[2]+a[4])
# FDR
fdr = a[3]/(a[4]+a[3])
# FPR
fpr = a[1]/(a[1]+a[3])
# F1 Measure
F1 = 2*prec*sens/(prec+sens)
#AUC 
#auc = fpr/sens
#auc
vec_opt = c(threshold,sens,spec,prec,npv,acc,fdr,fpr, F1)
vec_opt = round(vec_opt,2)
vec_opt


# Performance measures table with different threshold values
vec_all = c() 
for (i in seq(0,1,0.1)){
  a = table(titanic3$survived,pred2 >= i)
  threshold = i
  # Sensitivity with threshold 0.3
  sens = a[4]/(a[2]+a[4])
  # Specificity with threshold 0.3
  spec = a[1]/(a[1]+a[3])
  # Precision (PPV)
  prec = a[4]/(a[4]+a[3])
  # NPV
  npv = a[1]/(a[1]+a[2])
  # Accuracy
  acc = (a[1]+a[4])/(a[1]+a[3]+a[2]+a[4])
  # FDR
  fdr = a[3]/(a[4]+a[3])
  # FPR
  fpr = a[1]/(a[1]+a[3])
  # F1 Measure
  F1 = 2*prec*sens/(prec+sens)
  #AUC 
  #auc = fpr/sens
  #auc
  vec = c(threshold,sens,spec,prec,npv,acc,fdr,fpr,F1)
  vec = round(vec,2)
  vec_all = rbind(vec_all,vec)
  rownames(vec_all)[(i+0.1)*10] = i
}
colnames(vec_all) = c("threshold","sens","spec","prec","npv","acc","fdr","fpr","F1")
vec_all


# Find the AUC value, and F-scores for each cutoff.
auc(analysis)

# Cutoff graph
plot(unlist(performance(predobj, "sens")@x.values), unlist(performance(predobj, "sens")@y.values), 
     type="l", lwd=2, ylab="Sensitivity", xlab="Cutoff")
par(new=TRUE)
plot(unlist(performance(predobj, "spec")@x.values), unlist(performance(predobj, "spec")@y.values), 
     type="l", lwd=2, col='red', ylab="", xlab="")
axis(4, at=seq(0,1,0.2), col='red')
mtext("Specificity",side=4, padj=-2, col='red')















# # https://medium.com/analytics-vidhya/a-guide-to-machine-learning-in-r-for-beginners-part-5-4c00f2366b90
# plot(perf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
# #Plot ROC Curve
# plot(1-analysis$specificities,analysis$sensitivities,type="l",
#      ylab="Sensitiviy",xlab="1-Specificity",col="black",lwd=2,
#      main = "ROC Curve")
# abline(v = opt_t,col = "blue") #add optimal t to ROC curve


# 
# str(perf)
# cutoffs <- data.frame(cut=perf@alpha.values[[1]], fpr=perf@x.values[[1]], 
#                       tpr=perf@y.values[[1]])
# head(cutoffs)
# 
# measureACC(pred,titanic2$survived)
# SpEqualSe(perf)
# 
# data(elas)
# head(elas)
# 
# optimal.cutpoint.Youden<-optimal.cutpoints(X = "pclass", status = "survived", tag.healthy = 0,
#                                            methods = "Youden", data = titanic2, pop.prev = NULL, categorical.cov =
#                                              "pclass", control = control.cutpoints(), ci.fit = TRUE, conf.level = 0.95, trace = FALSE)
# 



# install.packages("measures")
#library(measures)

# TP(pred,titanic3$survived, 1)
# performance(predobj, "sens")@x.values

# 
# 
# 
# bb=as.numeric(unlist(performance(predobj, "sens")@y.values))+ as.numeric(unlist(performance(predobj, "spec")@y.values))
# 
# plot(unlist(performance(predobj, "ppv")@x.values), unlist(performance(predobj, "ppv")@y.values), 
#      type="l", lwd=2, ylab="precision", xlab="Cutoff")
# 
# #plot(unlist(performance(predobj, "npv")@x.values), unlist(performance(predobj, "npv")@y.values), 
# #     type="l", lwd=2, ylab="npv", xlab="Cutoff")
# 
# plot(unlist(performance(predobj, "acc")@x.values), unlist(performance(predobj, "acc")@y.values), 
#      type="l", lwd=2, ylab="accuracy", xlab="Cutoff")
# 
# #plot(unlist(performance(predobj, "tpr")@x.values), unlist(performance(predobj, "tpr")@y.values), 
# #     type="l", lwd=2, ylab="tpr", xlab="Cutoff")
# 
# performance(predobj, "sens")@x.values
# performance(predobj, "spec")@x.values
# performance(predobj, "ppv")@x.values
# performance(predobj, "npv")@x.values
# performance(predobj, "acc")@x.values
# performance(predobj, "auc")@y.values
# performance(predobj, "tpr")@x.values
# 
# 
# 
# # install.packages("pROC")
# library(pROC)
# my_roc <- roc(pred, titanic3$survived)
# coords(my_roc, "best", ret = "threshold")
