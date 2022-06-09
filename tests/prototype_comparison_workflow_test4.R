
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

# Prep local model. For now, we just need harmonized info
#loc.prep <- local.model.prep(Q, Ks, pop.adj.mat, model.type = "model1", printQ = T)
QK.harmonized.info <- harmonize.QtoKs(Q, Ks)
QK.harmonized.info$K.harmonized

# Simulate QK for use with making local potentials.
# Use pop freqs for Qonlys?
pvec <- groupwise.category.freq.mat2(QK.harmonized.info$K.harmonized,
                                     rep(1,nrow(QK.harmonized.info$K.harmonized)),
                                     est.type = "MLE", prob.adj = 0.005)
pvec
# # Estimate Q.only probs from Population:
# QK.harmonized.info$Q.only.harmonized.idxs
# QK.harmonized.info$Q.only.category.IDs
# X.pop.Q.only <- X.pop[,QK.harmonized.info$Q.only.category.IDs]
# pvecQ.only <- groupwise.category.freq.mat2(X.pop.Q.only,
#                                            rep(1,nrow(X.pop.Q.only)),
#                                            est.type = "Bayes", prob.adj = 0.005)
# pvecQ.only
# pvec[QK.harmonized.info$Q.only.harmonized.idxs]
# pvec[QK.harmonized.info$Q.only.harmonized.idxs] <- pvecQ.only

# Local simulation
QK.harmonized.local <- simulate.dust.sample.simple(629-5, prob.vec = pvec)
QK.harmonized.local <- rbind(     # This is reals AND sims
  QK.harmonized.info$K.harmonized,
  QK.harmonized.local
)
dim(QK.harmonized.info$K.harmonized)
dim(QK.harmonized.local)
# Use these with grapHD to get a "local" graph
# Get local graph with Konly? or all QK?
# Thinking K only and treat Q as independent since they DID NOT occur in  the K

# NOTE: We won't use the Qonly sims columns.
QK.harmonized.info$K.only.harmonized.idxs
Konly.sims <- QK.harmonized.local[,QK.harmonized.info$K.only.harmonized.idxs] # K nodes only
#Konly.sims <- QK.harmonized.local                                             # All QK nodes
head(Konly.sims)

Konly.simsf <- sapply(1:ncol(Konly.sims), function(xx){as.factor(Konly.sims[,xx])})
Konly.simsf <- data.frame(Konly.simsf, stringsAsFactors = T )
rownames(Konly.simsf) <- NULL
colnames(Konly.simsf) <- NULL
#colnames(Konly.simsf) <- QK.harmonized.info$K.only.harmonized.idxs
head(Konly.simsf)
Konly.simsf[,1]
dim(Konly.simsf) # NOTE Reduced because Konly

# Build graph for Konly, assuming Qonlys are independent because they appeared in NONE of the K samples
KForest <- minForest(Konly.simsf, stat = "AIC")
KForest
data.frame(KForest$edges, KForest$statSeq)
KForest$error # ** NOTE: CHECK these with fisher test??


Degree(KForest)
v <- which(Degree(KForest)>0)
v
dev.off()
plot(KForest,vert=v,numIter=2000,vert.label=T)

# Move froward with edges found by grapHD
KForest$edges # These are in terms of Konly reduced indices. We want to put them back into QK harmonized indices
length(QK.harmonized.info$K.only.harmonized.idxs)
legs <- cbind(
  QK.harmonized.info$K.only.harmonized.idxs[ KForest$edges[,1] ],
  QK.harmonized.info$K.only.harmonized.idxs[ KForest$edges[,2] ]
)
rownames(legs) <- NULL
#
loc.adj.mat <- edges2adj(legs, n.nodes = 37)
gph.loc <- graph_from_adjacency_matrix(loc.adj.mat, mode="undirected")
# graphNEL format:
gph.loc <- as_graphnel(gph.loc)
gph.loc # Check
dev.off()
plot(gph.loc)


gphHD.loc <- as.gRapHD(legs, p=37)
gphHD.loc # Check
dev.off()
plot(gphHD.loc, numIter=1000, vert.label=T)
