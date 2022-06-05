library(gRim)
library(dplyr)

options(max.print=1000000)
setwd("/Users/karen2/latex/papers/dust/steph_diss/")

fdat <- read.csv("data/dust.data.as-of-5-24-22_SAMPLE_INFO.csv", header=T) # Sample info
dat  <- read.csv("data/dust.data.as-of-5-24-22.csv", header=T)             # Processed data
dim(dat)

# Categires (actual node names), but they will reference/IDed by their row numbers
categs <- dat[,1:3]

# Data, n-samples by p-categories
X <- t(dat[,4:ncol(dat)])
dim(X)

# Sample ID labels
lbl <- fdat$lbl.loc    # Label by location
#lbl <- fdat$lbl.room  # Label by room


hif <- harmonize.QtoKs(t(X[1,]),X[6:10,])
categs[hif$kept.category.idxs,]

#KK <- rbind(hif$Q.harmonized,hif$K.harmonized)
KK <- hif$K.harmonized
KK

#For the simulation:
prob1.mat<-groupwise.category.freq.mat2(KK, c(1,1,1,1,1), est.type = "MLE")
prob1.mat
prob1.mat<-groupwise.category.freq.mat2(KK, c(1,1,1,1,1), est.type = "Bayes")
prob1.mat
prob1.mat[prob1.mat==1] <- 0.87
prob1.mat[prob1.mat==0] <- 0.005
prob1.mat
simulate.dust.sample.old(KK[1,], num.sims=25, prob1.mat)


colnames(KK) <- paste0("C",hif$kept.category.idxs)
rownames(KK) <- 1:nrow(KK)
data.frame(KK)

KK.tab <- xtabs(~ ., data = data.frame(KK))
dim(KK.tab)
KK.tab
