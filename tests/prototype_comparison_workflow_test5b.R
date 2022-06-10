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

# Local: Set up connections and associated node/edge affinities
lprep      <- local.model.prep2(a.Q.vec = Q, a.K.mat = Ks, population.datamat = X.pop, printQ = F, plotQ=T, Category.IDs.plotQ = F)
laff.info  <- make.QK.local.harmonized.affinities2(a.QK.local.prep.info.list = lprep, normalizeQ=T, scale.factor = 100, printQ=T)
#laff.info$node.affinities
#laff.info$edge.affinities

# Graph workflow:
# Step 0 separate graph components:
gph.loc <- graph_from_adjacency_matrix(lprep$model.adjacency.mat, mode="undirected")
gph.loc <- as_graphnel(gph.loc)
#plot(gph.loc)
plot.graph(lprep, Category.IDs.plotQ = F)
ccp.list.loc <- connComp(gph.loc)           # Eventually loop over this list
ccp.list.loc

# Do this for each multinode graph component:
# Step 1 If graph component is multinode, get required node and edge info, make CRF object and insert graph component affinities
gcomp.idx <- 2
ccp.list.loc[[gcomp.idx]]
gcomp.mrf.info <- make.component.mrf(ccp.list.loc[[gcomp.idx]], lprep, laff.info)

# Check node beliefs compared to observations on graph component nodes that should all be Ks and connected
data.frame(
  gcomp.mrf.info$harmonized.node.names.for.mrf,
  t(rbind(
    gcomp.mrf.info$bp.info$node.bel[,1],
    lprep$QK.harmonized.info$K.harmonized[, as.numeric(ccp.list.loc[[gcomp.idx]])]
  ))
)

# Check edge beliefs compared to un-normalized affinities
ebs <- gcomp.mrf.info$bp.info$edge.bel
names(ebs) <- gcomp.mrf.info$harmonized.edge.names.for.mrf
ebs
edge.affs.for.compare  <- make.QK.local.harmonized.affinities2(a.QK.local.prep.info.list = lprep, normalizeQ=F, printQ=T)$edge.affinities

names(ebs)
names(edge.affs.for.compare)

rearr.idxs <- sapply(1:length(ebs), function(xx){which(names(edge.affs.for.compare) == names(ebs)[xx])})
edge.affs.for.compare <- edge.affs.for.compare[rearr.idxs]

edidx <- 3
edge.affs.for.compare[edidx]
ebs[edidx]

# Step 2 compute energy of configuration for nodes in component and normalize



gcomp.mrf.info$harmonized.node.idxs.for.mrf
ccp.list.loc[[gcomp.idx]]
lprep$QK.harmonized.info$K.harmonized[,gcomp.mrf.info$harmonized.node.idxs.for.mrf]
lprep$QK.harmonized.info$QK.Category.IDs[gcomp.mrf.info$harmonized.node.idxs.for.mrf]
colnames(lprep$QK.harmonized.info$K.harmonized[,gcomp.mrf.info$harmonized.node.idxs.for.mrf])

Ks.comp.nds <- lprep$QK.harmonized.info$K.harmonized[,gcomp.mrf.info$harmonized.node.idxs.for.mrf]
Ks.comp.nds
XKs <- Ks.comp.nds

XQ <- t(as.matrix(lprep$QK.harmonized.info$Q.harmonized[gcomp.mrf.info$harmonized.node.idxs.for.mrf]))
XQ

# Get the Q and Ks to test prob calcs on this chosen graph component
#comp.categ.idxs.hz <- cil$idx.translation.mat[,"harmonized.idxs"]
#XQ <- t(as.matrix(loc.prep$harmonized.info$Q.harmonized[comp.categ.idxs.hz])) # Q dust vector chunk for this component
#XK <- loc.prep$harmonized.info$K.harmonized[5,comp.categ.idxs.hz]             # dust vector chunk of a K for this component

gcomp.mrf.info
enc <- config.energy(config    = XQ,
                     edges.mat = cil$component.edge.mat,
                     one.lgp   = lnp,
                     two.lgp   = lep, # use same order as edges!
                     ff        = f)
enc
li$logZ
enc - li$logZ
exp(enc - li$logZ)
