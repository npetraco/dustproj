source("/Users/karen2/latex/papers/dust/steph_diss/analysis_prototypes/test_load_data_for_prob_models.R")

samp.loc.sizes <- table(lbl)
samp.loc.sizes[samp.loc.sizes >= 4]

# Smaller test:
Q.idx <- 9  # 1:828 dust vectors
lbl[Q.idx]  # True location name of the Questioned
K.lbl <- 61 # 1:198 locations

Q  <- t(X[Q.idx,])            # Questioned dust vector
Ks <- X[which(lbl == K.lbl),] # Known(s) dust vector(s)

qksma <- QK.harmonized.summary(Q, Ks, X.pop, categs.occured, type="all", printQ = T)    # All QK harmonized info summary
qksmk <- QK.harmonized.summary(Q, Ks, X.pop, categs.occured, type="K.only", printQ = T) # Info shared by Q and K summary
qksmq <- QK.harmonized.summary(Q, Ks, X.pop, categs.occured, type="Q.only", printQ = T) # Info occuring only for Q summary
harmonize.QtoKs(Q,Ks)

#-------------------------------
# Local: Set up connections and associated node/edge affinities
lprep            <- local.model.prep2(a.Q.vec = Q, a.K.mat = Ks, population.datamat = X.pop, printQ = F, plotQ=T, Category.IDs.plotQ = F)
lprep.pruned     <- prune.local.model(lprep, plotQ = T, printQ = T, Category.IDs.plotQ = F)
laff.pruned.info <- make.QK.local.harmonized.affinities2(a.QK.local.prep.info.list = lprep.pruned, normalizeQ=T, scale.factor = 1, printQ=F)

# Graph workflow:
# Step 1 separate graph components:
gph.loc      <- graph_from_adjacency_matrix(lprep.pruned$model.adjacency.mat, mode="undirected")
gph.loc      <- as_graphnel(gph.loc)
ccp.list.loc <- connComp(gph.loc)           # Eventually loop over this list

# Step 2 get probs for each graph component and the overall prob:
#d.vec <- lprep$QK.harmonized.info$K.harmonized[5 , ]
d.vec <- lprep$QK.harmonized.info$Q.harmonized
compute.dust.config.prob.info(d.vec, ccp.list.loc, lprep.pruned, laff.pruned.info, printQ = F)


#---------------------------
# Population model setup
pprep     <- population.model.prep2(Q, Ks, pop.adj.mat, plotQ = T, Category.IDs.plotQ = T)
paff.info <- make.QK.population.harmonized.affinities2(a.QK.population.prep.info.list = pprep, population.datamat = X.pop, normalizeQ = F, scale.factor = 100, printQ=F)

# Graph workflow:
# Step 1 separate graph components:
gph.pop      <- graph_from_adjacency_matrix(pprep$model.adjacency.mat, mode="undirected")
gph.pop      <- as_graphnel(gph.pop)
ccp.list.pop <- connComp(gph.pop)           # Eventually loop over this list

# Step 2 get probs for each graph component and the overall prob:
compute.dust.config.prob.info(d.vec, ccp.list.pop, pprep, paff.info, printQ = T)

# Model plots
plot.graph(lprep, Category.IDs.plotQ = T)
plot.graph(lprep.pruned, Category.IDs.plotQ = T)
plot.graph(pprep, Category.IDs.plotQ = T)
