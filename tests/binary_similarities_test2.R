library(proxy)

source("/Users/karen2/latex/papers/dust/steph_diss/analysis_prototypes/test_load_data_for_prob_models.R")

which(lbl == 154)

# KM
# Q dust vector
Q.idx         <- 6           # 1:828 dust vectors
true.Q.lbl    <- lbl[Q.idx]    # True location name of the Questioned
true.Q.lbl                     # True label of the Q
#true.lbl.idxs <- which(lbl == 154)
true.lbl.idxs <- which(lbl == true.Q.lbl)
true.lbl.idxs
Q             <- t(X[Q.idx,])  # Questioned dust vector
fdat[Q.idx,]                   # Info on the Q

# Location A to compare the Q to
KA.lbl      <- 2                         # 1:198 locations
KA.lbl.idxs <- which(lbl == KA.lbl)
KA.lbl.idxs
KA.idx.drp  <- which(KA.lbl.idxs == Q.idx) # If the Q is in KA drop it out
# If the Q is in KA drop it out
if(length(KA.idx.drp) != 0){KA.lbl.idxs <- KA.lbl.idxs[-KA.idx.drp]}
KA          <- X[KA.lbl.idxs,]             # Location A dust vector(s)
fdat[KA.lbl.idxs,]                         # Info on KA
# What stuff is in KA vs the Q
QKA <- QK.harmonized.summary(Q, KA, X.pop, categs.occured, type="all", printQ = T)    # All Categories
QKA <- QK.harmonized.summary(Q, KA, X.pop, categs.occured, type="K.only", printQ = T) # Categories common to both Q and K
QKA <- QK.harmonized.summary(Q, KA, X.pop, categs.occured, type="Q.only", printQ = T)    # Categories only in Q


daugman.score(Q, KA[1,], 40)
hij <- harmonize.QtoKs(Q,KA)
daugman.score(hij$Q.harmonized, hij$K.harmonized[1,], 40)
#summary(pr_DB)

# Send in as row vectors
dist(t(hij$Q.harmonized), t(hij$K.harmonized[1,]), method = "Jaccard")
dist(t(hij$Q.harmonized), t(hij$K.harmonized[1,]), method = "Dice")
dist(list(t(hij$Q.harmonized)), list(t(hij$K.harmonized[1,])), method = "Levenshtein")

simil(t(hij$Q.harmonized), t(hij$K.harmonized[1,]), method = "Jaccard")
simil(t(hij$Q.harmonized), t(hij$K.harmonized[1,]), method = "Dice")
simil(list(t(hij$Q.harmonized)), list(t(hij$K.harmonized[1,])), method = "Levenshtein")


# KNM
# Q dust vector
Q.idx         <- 2          # 1:828 dust vectors
true.Q.lbl    <- lbl[Q.idx]    # True location name of the Questioned
true.Q.lbl                     # True label of the Q
#true.lbl.idxs <- which(lbl == 154)
true.lbl.idxs <- which(lbl == true.Q.lbl)
true.lbl.idxs
Q             <- t(X[Q.idx,])  # Questioned dust vector
fdat[Q.idx,]                   # Info on the Q

# Location A to compare the Q to
KA.lbl      <- 10                         # 1:198 locations
KA.lbl.idxs <- which(lbl == KA.lbl)
KA.lbl.idxs
KA.idx.drp  <- which(KA.lbl.idxs == Q.idx) # If the Q is in KA drop it out
# If the Q is in KA drop it out
if(length(KA.idx.drp) != 0){KA.lbl.idxs <- KA.lbl.idxs[-KA.idx.drp]}
KA          <- X[KA.lbl.idxs,]             # Location A dust vector(s)
fdat[KA.lbl.idxs,]                         # Info on KA
# What stuff is in KA vs the Q
QKA <- QK.harmonized.summary(Q, KA, X.pop, categs.occured, type="all", printQ = T)    # All Categories
QKA <- QK.harmonized.summary(Q, KA, X.pop, categs.occured, type="K.only", printQ = T) # Categories common to both Q and K
QKA <- QK.harmonized.summary(Q, KA, X.pop, categs.occured, type="Q.only", printQ = T)    # Categories only in Q


daugman.score(Q, KA[1,], 40)
hij <- harmonize.QtoKs(Q,KA)
daugman.score(hij$Q.harmonized, hij$K.harmonized[1,], 40)
#summary(pr_DB)

# Send in as row vectors
dist(t(hij$Q.harmonized), t(hij$K.harmonized[1,]), method = "Jaccard")
dist(t(hij$Q.harmonized), t(hij$K.harmonized[1,]), method = "Dice")
dist(list(t(hij$Q.harmonized)), list(t(hij$K.harmonized[1,])), method = "Levenshtein")

simil(t(hij$Q.harmonized), t(hij$K.harmonized[1,]), method = "Jaccard")
simil(t(hij$Q.harmonized), t(hij$K.harmonized[1,]), method = "Dice")
simil(list(t(hij$Q.harmonized)), list(t(hij$K.harmonized[1,])), method = "Levenshtein")


