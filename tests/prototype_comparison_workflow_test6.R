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
# Step 0 separate graph components:
gph.loc      <- graph_from_adjacency_matrix(lprep$model.adjacency.mat, mode="undirected")
gph.loc      <- as_graphnel(gph.loc)
ccp.list.loc <- connComp(gph.loc)           # Eventually loop over this list
ccp.list.loc

# Do this for each multinode graph component:
# Step 1a If graph component is multinode, get required node and edge info, make CRF object and insert graph component affinities
gcomp.idx      <- 1
gcomp.mrf.info <- make.component.mrf(ccp.list.loc[[gcomp.idx]], lprep, laff.info)

# Step 2 extract nodes corresponding to the component graph
# and compute energy of configuration for nodes in component and normalize
comp.nd.idxs <- gcomp.mrf.info$harmonized.node.idxs.for.mrf            # ******* CRITICAL!!!!!!!!!!!
XKs          <- lprep$QK.harmonized.info$K.harmonized[ , comp.nd.idxs]
XQ           <- lprep$QK.harmonized.info$Q.harmonized[comp.nd.idxs]
t(rbind(XQ, XKs))
#dim(XKs)
#length(ccp.list.loc[[gcomp.idx]])

# Define states and feature function:
s1 <- 1
s2 <- 0
f  <- function(y){ as.numeric(c((y==s1),(y==s2))) }

compute.component.graph.dust.config.prob.info(XKs[1,], gcomp.mrf.info, f, printQ = T)
compute.component.graph.dust.config.prob.info(XKs[2,], gcomp.mrf.info, f, printQ = T)
compute.component.graph.dust.config.prob.info(XKs[3,], gcomp.mrf.info, f, printQ = T)
compute.component.graph.dust.config.prob.info(XKs[4,], gcomp.mrf.info, f, printQ = T)
compute.component.graph.dust.config.prob.info(XKs[5,], gcomp.mrf.info, f, printQ = T)
compute.component.graph.dust.config.prob.info(XQ,      gcomp.mrf.info, f, printQ = T)


# Step 1b If graph component is a single node
gcomp.idx      <- 1
