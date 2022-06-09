
# Test to see if we can get local config probabilities.

# We can, but they are TIGHT. 1 category flip in a K and they plummet. That said, KNM Qs tested are even much smaller which is good.
# Overall however the funcky prob magnitudes look unrealistic (<e-16).
# We could just use the log probs as similarity scores if need be.

# I think the "tightness" in the probababilities is stemming from the VERY connected local models were using

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


# Get local model and local affinities:
loc.prep <- local.model.prep(Q, Ks, pop.adj.mat, model.type = "model1", printQ = T)

loc.affs <- make.QK.local.harmonized.affinities(
  a.harmonized.info.list = loc.prep$harmonized.info,
  an.edge.mat            = loc.prep$model.edge.mat,
  population.datamat     = X.pop, #num.local.sims = 629-5,
  normalizeQ             = F,
  printQ                 = F)

# Local component separations:
gph.loc <- graph_from_adjacency_matrix(loc.prep$model.adj.mat, mode="undirected")
# graphNEL format:
gph.loc <- as_graphnel(gph.loc)
gph.loc # Check
ccp.list.loc <- connComp(gph.loc)
ccp.list.loc

comp.idx <- 1
ccp.list.loc[[comp.idx]]
cil <- get.component.graph.info(
  component.graph.nodes = ccp.list.loc[[comp.idx]],
  a.model.adj.mat       = loc.prep$model.adj.mat,
  affinities.info       = loc.affs,
  a.harmonized.info     = loc.prep$harmonized.info)

# Belief propagation tests:
gs.min          <- make.crf(cil$component.adj.mat, 2)
gs.min$edge.pot <- cil$edge.affinities
naf             <- t(sapply(1:length(cil$node.affinities), function(xx){cil$node.affinities[[xx]]}))
naf # Should be no 0
gs.min$node.pot <- naf
li <- infer.lbp(gs.min,max.iter = 10000)
li$logZ

# Get the Q and Ks to test prob calcs on this chosen graph component
comp.categ.idxs.hz <- cil$idx.translation.mat[,"harmonized.idxs"]
XQ <- t(as.matrix(loc.prep$harmonized.info$Q.harmonized[comp.categ.idxs.hz])) # Q dust vector chunk for this component
XK <- loc.prep$harmonized.info$K.harmonized[1,comp.categ.idxs.hz]             # dust vector chunk of a K for this component

cbind(
  round(li$node.bel[,1],2),
  #t(XQ)
  XK
)

# Log affinities needed
lnp <- lapply(1:length(cil$node.affinities), function(xx){log(cil$node.affinities[[xx]])})
lep <- lapply(1:length(gs.min$edge.pot), function(xx){log(gs.min$edge.pot[[xx]])})


# Define states and feature function:
s1 <- 1
s2 <- 0
f  <- function(y){ as.numeric(c((y==s1),(y==s2))) }

data.frame(cil$harmonized.edge.mat, names(gs.min$edge.pot), cil$component.edge.mat)

# Get the Q and Ks to test prob calcs on this chosen graph component
comp.categ.idxs.hz <- cil$idx.translation.mat[,"harmonized.idxs"]
XQ <- t(as.matrix(loc.prep$harmonized.info$Q.harmonized[comp.categ.idxs.hz])) # Q dust vector chunk for this component
XK <- loc.prep$harmonized.info$K.harmonized[5,comp.categ.idxs.hz]             # dust vector chunk of a K for this component

enc <- config.energy(config    = XQ,
                     edges.mat = cil$component.edge.mat,
                     one.lgp   = lnp,
                     two.lgp   = lep, # use same order as edges!
                     ff        = f)
enc
li$logZ
enc - li$logZ
exp(enc - li$logZ)

# NOTES:
# Some Ks have high prob as expected (~0.9) but some Ks with one item different from a high prob K, the
# prob PLUMMETS (~e-16).
# That said, the (KNM) Q probs (e-60) are indeed much lower than the K probs. Overall however, these
# PLUMMETING probs look ridiculous.

# Maybe why the probs PLUMMET is because the local model is too dependent (ie too connected)??
# What do graphs of the local data that pop out of grapHD look like?
dim(loc.affs$K.harmonized.local)
Konly.sims <- loc.affs$K.harmonized.local[,loc.prep$harmonized.info$K.only.harmonized.idxs]
#Konly.sims <- loc.affs$K.harmonized.local
#Konly.sims <- loc.prep$harmonized.info$K.harmonized

Konly.simsf <- sapply(1:ncol(Konly.sims), function(xx){as.factor(Konly.sims[,xx])})
Konly.simsf <- data.frame(Konly.simsf, stringsAsFactors = T )
head(Konly.simsf)
Konly.simsf[,1]

KForest <- minForest(Konly.sims, stat = "AIC")
KForest
KForest$edges
KForest$error

Degree(KForest)
v <- which(Degree(KForest)>0)
v
dev.off()
plot(KForest,vert=v,numIter=2000,vert.label=T)

