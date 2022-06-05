library(dustproj)
library(CRFutil)
library(gRapHD)
#library(gRim)
#library(MASS)

options(max.print=1000000)
setwd("/Users/karen2/latex/papers/dust/steph_diss/")

fdat <- read.csv("data/dust.data.as-of-5-24-22_SAMPLE_INFO.csv", header=T) # Sample info
dat  <- read.csv("data/dust.data.as-of-5-24-22.csv", header=T)             # Processed data
dim(dat)

# Categires (actual node names), but they will reference/IDed by their row numbers
categs <- data.frame(1:nrow(dat), dat[,1:3])
colnames(categs) <- c("CategoryID",colnames(categs)[2:4]) # Put in an extra column to gaurantee OG category numbers don't get lost. We'll need them

# Entire Data, n-samples by p-categories WITH repeats-per-location
X.all <- t(dat[,4:ncol(dat)]) # Keep all data in this one

# Drop categories that never occurred:
X           <- t(dat[,4:ncol(dat)])
dim(X)
rownames(X) <- 1:nrow(X)

never.occur.categ.idxs <- which(colSums(X) == 0)
X                      <- X[,-never.occur.categ.idxs]
# Re-index the category IDs:
colnames(X) <- paste0("X",1:ncol(X))
head(X)
dim(X)

# In principle, this should keep the OG categ indices attached to the categories that occurred,
# but they are safely stored in categs as a back-up:
occur.categ.idxs <- (1:nrow(categs))[-never.occur.categ.idxs] # OG category IDs that occured
#colnames(X)      <- paste0("X", occur.categ.idxs)
#head(X)

categs.occured <- categs[occur.categ.idxs,]
rownames(categs.occured) <- NULL
head(categs.occured,20)
dim(categs.occured)

# Drop the repeats-per-location to form the "population" data:
lbl       <- fdat$lbl.loc
lbl.names <- unique(lbl)

X.pop   <- NULL
lbl.pop <- NULL
for(i in 1:length(lbl.names)){
  #print(i)
  grp.mat.info <- pick.out.groups(X, lbl, c(lbl.names[i]))
  grp.mat      <- grp.mat.info[[1]]

  if(length(grp.mat.info[[2]]) == 1){
    X.pop   <- rbind(X.pop, grp.mat) # Only one aliquot at the location, so just tack it in
    lbl.pop <- c(lbl.pop, lbl.names[i])

    print("**********************************")
    print(paste("Location:", lbl.names[i], "has", length(grp.mat.info[[2]]), "total aliquots."))
    print(paste("Just tacked in 1 dust vector of length", length(grp.mat), ". Dim blank?:", dim(grp.mat),"from location:", lbl.names[i] ))


  } else {
    grp.mat.uniq <- unique(grp.mat)      # If more than one aliquot at location, keep only the unique configurations
    X.pop        <- rbind(X.pop,grp.mat.uniq)
    lbl.pop      <- c(lbl.pop, rep(lbl.names[i], nrow(grp.mat.uniq)) )

    print("----------------------------------")
    print(paste("Location:", lbl.names[i], "has", length(grp.mat.info[[2]]), "total aliquots."))
    print(paste("Dropped", nrow(grp.mat) - nrow(grp.mat.uniq), "from location:", lbl.names[i] ))

  }

}
rownames(X.pop) <- NULL
head(X.pop)
dim(X.pop)
#lbl.pop

pop.adj.mat <- read.csv("analysis_prototypes/5-24-22_processed_pop_adj_mat.csv")
sum(pop.adj.mat)/2 # Check. Should be edges in the population

#
dim(X)     # All data with non-occuring categories dropped
dim(X.pop) # "Population" with matching replicates and non-occuring categories dropped

lbl.names
Q.idx <- 9  # 1:828 dust vectors
K.lbl <- 61 # 1:198 locations

# True location name of the Questioned
lblQ.true <- lbl[Q.idx]
lblQ.true

Q  <- t(X[Q.idx,])            # Questioned dust vector
Ks <- X[which(lbl == K.lbl),] # Known(s) dust vector(s)
Q
dim(Ks)

hif <- harmonize.QtoKs(Q, Ks)
hif$Q.harmonized
hif$K.harmonized
hif$QK.Category.IDs
hif$Q.only.category.IDs
hif$Q.only.harmonized.idxs
hif$K.only.category.IDs
hif$K.only.harmonized.idxs
Ks[,hif$K.only.category.IDs]
ncol(Ks[,hif$K.only.category.IDs])
24*23/2

prob1.mat<-groupwise.category.freq.mat2(hif$K.harmonized, c(1,1,1,1,1), est.type = "MLE", prob.adj = 0.005)
prob1.mat

sim.grp.junk <- simulate.dust.sample.simple(629, prob.vec = prob1.mat)
sim.grp.junk


table(
  factor(sim.grp.junk[, 1], levels = c(1,0)),
  factor(sim.grp.junk[, 2], levels = c(1,0))
)

table(
  factor(sim.grp.junk[, 32], levels = c(1,0)),
  factor(sim.grp.junk[, 35], levels = c(1,0))
)

table(
  factor(sim.grp.junk[, 1], levels = c(1,0)),
  factor(sim.grp.junk[, 32], levels = c(1,0))
)

table(
  factor(sim.grp.junk[, 32], levels = c(1,0)),
  factor(sim.grp.junk[, 2], levels = c(1,0))
)

table(
  factor(sim.grp.junk[, 2], levels = c(1,0)),
  factor(sim.grp.junk[, 32], levels = c(1,0))
)

