source("/Users/karen2/latex/papers/dust/steph_diss/analysis_prototypes/test_load_data_for_prob_models.R")

# Smaller test:
# Q.idx <- 9  # 1:828 dust vectors
# lbl[Q.idx]  # True location name of the Questioned
# K.lbl <- 61 # 1:198 locations

# BIGGER test:
Q.idx <- 746  # 1:828 dust vectors
lbl[Q.idx]  # True location name of the Questioned
K.lbl <- 167 # 1:198 locations

Q  <- t(X[Q.idx,])            # Questioned dust vector
Ks <- X[which(lbl == K.lbl),] # Known(s) dust vector(s)

# Local: Set up connections and associated node/edge affinities
lprep        <- local.model.prep2(a.Q.vec = Q, a.K.mat = Ks, population.datamat = X.pop, printQ = F, plotQ=T, Category.IDs.plotQ = F)
lprep.pruned <- prune.local.model(lprep, plotQ = T, printQ = T, Category.IDs.plotQ = F)

laff.pruned.info    <- make.QK.local.harmonized.affinities2(a.QK.local.prep.info.list = lprep.pruned, normalizeQ=T, scale.factor = 1, printQ=F)
laff.pruned.info$node.affinities
laff.pruned.info$edge.affinities

# Graph workflow:
# Step 1 separate graph components:
gph.loc      <- graph_from_adjacency_matrix(lprep.pruned$model.adjacency.mat, mode="undirected")
gph.loc      <- as_graphnel(gph.loc)
ccp.list.loc <- connComp(gph.loc)           # Eventually loop over this list
ccp.list.loc

# Step 2 get probs for each graph component and the overall prob:
d.vec <- lprep$QK.harmonized.info$K.harmonized[5 , ]
#d.vec <- lprep$QK.harmonized.info$Q.harmonized
compute.dust.config.prob.info(d.vec, ccp.list.loc, lprep.pruned, laff.pruned.info, printQ = T)

# For lots of K variation, keep only nodes that occur alot (> 80%) in the Known set and toss all categories
# that occur a lot in the population??
cbind(
  lprep$QK.harmonized.info$Q.harmonized,
  t(lprep$QK.harmonized.info$K.harmonized)
)
# Tack pop counts onto above
