# Test to see what local probabilities look like with grapHD models instead of the fully connected local models

source("/Users/karen2/latex/papers/dust/steph_diss/analysis_prototypes/test_load_data_for_prob_models.R")

# Smaller test:
Q.idx <- 9  # 1:828 dust vectors
lbl[Q.idx]  # True location name of the Questioned
K.lbl <- 61 # 1:198 locations

# BIGGER test:
# Q.idx <- 746  # 1:828 dust vectors
# lbl[Q.idx]  # True location name of the Questioned
# K.lbl <- 167 # 1:198 locations

Q  <- t(X[Q.idx,])            # Questioned dust vector
Ks <- X[which(lbl == K.lbl),] # Known(s) dust vector(s)

lprep <- local.model.prep2(a.Q.vec = Q, a.K.mat = Ks, population.datamat = X.pop, printQ = F, plotQ=T, Category.IDs.plotQ = F)
junk  <- make.QK.local.harmonized.affinities2(a.QK.local.prep.info.list = lprep, normalizeQ=T, scale.factor = 100, printQ=T)
junk$node.affinities
junk$edge.affinities


# CHECKS:
#factor(lprep$QK.harmonized.local[,12], levels=c(1,0))
#factor(lprep$QK.harmonized.local[,29], levels=c(1,0))
#lprep$QK.harmonized.info$K.harmonized
#

#
sum(lprep$QK.harmonized.local[,18])
lprep$QK.harmonized.info$Q.only.harmonized.idxs
lprep$QK.harmonized.info$K.only.harmonized.idxs
sum(X.pop[,104])
categs.occured[104,]


lprep$QK.harmonized.local[,4]
lprep$QK.harmonized.local[,17]
table(
  factor(lprep$QK.harmonized.local[,4], levels=c(1,0)),
  factor(lprep$QK.harmonized.local[,17], levels=c(1,0))
)

jm <- cbind(lprep$QK.harmonized.local[,4], lprep$QK.harmonized.local[,17])
length(which((jm[,1] == 0) & (jm[,2] == 0)))
table(
  lprep$QK.harmonized.local[,12],
  lprep$QK.harmonized.local[,29]
)
