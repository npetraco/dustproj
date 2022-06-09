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
loc.prep <- local.model.prep(Q, Ks, pop.adj.mat, model.type = "model3", printQ = T)

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
li$node.bel

# *********** This is why we made this script: Check node affinities are consistent with alg in make.local.QK
nd.idx.check <- 6
#loc.prep$harmonized.info$K.harmonized
loc.prep$harmonized.info$K.harmonized[,nd.idx.check] # The real data
loc.affs$Category.Probs.for.KSims[nd.idx.check]      # sim prob used

loc.affs$node.affinities[[nd.idx.check]]             # Node affinity
sum(loc.affs$K.harmonized.local[,nd.idx.check])      # Check 1 counts
sum(loc.affs$K.harmonized.local[,nd.idx.check]==0)   # Check 0 counts

# Now check counts of this categ in the pop. See ll. 243-244. in make.local.QK. Takes from pop data for Qonly
nd.idx.pop <- loc.prep$harmonized.info$QK.Category.IDs[nd.idx.check]  # Pop name check
nd.idx.pop
colnames(loc.prep$harmonized.info$K.harmonized)[nd.idx.check]         # Pop name check confirm
sum(X.pop[,nd.idx.pop]==1)
sum(X.pop[,nd.idx.pop]==0)
loc.affs$node.affinities[[nd.idx.check]]



