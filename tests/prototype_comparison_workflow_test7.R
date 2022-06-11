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

# Local: Set up connections and associated node/edge affinities
lprep      <- local.model.prep2(a.Q.vec = Q, a.K.mat = Ks, population.datamat = X.pop, printQ = F, plotQ=T, Category.IDs.plotQ = F)
laff.info  <- make.QK.local.harmonized.affinities2(a.QK.local.prep.info.list = lprep, normalizeQ=F, scale.factor = 100, printQ=F)

# Graph workflow:
# Step 1 separate graph components:
gph.loc      <- graph_from_adjacency_matrix(lprep$model.adjacency.mat, mode="undirected")
gph.loc      <- as_graphnel(gph.loc)
ccp.list.loc <- connComp(gph.loc)           # Eventually loop over this list
#ccp.list.loc

# Step 2 get probs for each graph component and the overall prob:
d.vec <- lprep$QK.harmonized.info$K.harmonized[1 , ]
#d.vec <- lprep$QK.harmonized.info$Q.harmonized
compute.dust.config.prob.info(d.vec, ccp.list.loc, lprep, laff.info, printQ = T)

plot.graph(lprep, Category.IDs.plotQ = T)
