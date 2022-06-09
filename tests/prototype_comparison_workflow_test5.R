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

lprep <- local.model.prep2(a.Q.vec = Q, a.K.mat = Ks, population.datamat = X.pop, printQ = F, plotQ=T)
lprep$edge.mat
make.QK.local.harmonized.affinities2(a.QK.local.prep.info.list = lprep, normalizeQ=F, printQ=F)
