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

# ****** Make a function to compute X.pop !!!!!!!!!!!!
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

# ****** Make a function to compute pop.adj.mat in the same file as X.pop !!!!!!!!!!!!
pop.adj.mat <- read.csv("analysis_prototypes/5-24-22_processed_pop_adj_mat.csv", header=F) # see build_population_graph.R
sum(pop.adj.mat)/2 # Check. Should be edges in the population


dim(X)     # All data with non-occuring categories dropped
dim(X.pop) # "Population" with matching replicates and non-occuring categories dropped

# Tests: -----------------------------------------------------------------
# Small:
# Q.idx <- 9  # 1:828 dust vectors
# lbl[Q.idx]  # True location name of the Questioned
# K.lbl <- 61 # 1:198 locations

# BIG:
Q.idx <- 746  # 1:828 dust vectors
lbl[Q.idx]  # True location name of the Questioned
K.lbl <- 167 # 1:198 locations

Q  <- t(X[Q.idx,])            # Questioned dust vector
Ks <- X[which(lbl == K.lbl),] # Known(s) dust vector(s)

mdi <- local.model.prep(Q, Ks, pop.adj.mat, model.type = "model2", printQ = T)
mdi$model.edge.mat
dim(mdi$model.edge.mat)
sum(mdi$model.adj.mat)/2

junk.harmonized.info <- harmonize.QtoKs(Q,Ks)

# Return sims incase we need to bug check
make.QK.harmonized.affinities(junk.harmonized.info, mdi$model.edge.mat,
                              population.datamat = X.pop, num.local.sims = 629-5, normalizeQ = F,
                              printQ=T)
#





junk.harmonized.info$Q.only.harmonized.idxs
junk.harmonized.info$Q.only.category.IDs
X.pop[,295]
junk.harmonized.info$K.only.harmonized.idxs
junk.harmonized.info$K.only.category.IDs
junk.harmonized.info$K.harmonized[,52]
junk.harmonized.info$QK.Category.IDs
#





tt <- rbind(
  c(111, 143),
  c(0, 0)
)
rownames(tt) <- c(1,0)
colnames(tt) <- c(1,0)
tt <- as.table(tt)
class(tt)
names(attributes(tt)$dimnames) <- c(18,4)
tt
nrow(which(tt == 0, arr.ind = T))
zi <- which(tt == 0, arr.ind = T)
tt[zi] <- 1
tt
ceiling(tt/sum(tt) * 100)

tt2 <- table(
  factor(X.pop[,1], levels = c(1,0)),
  factor(X.pop[,1], levels = c(1,0))
)
