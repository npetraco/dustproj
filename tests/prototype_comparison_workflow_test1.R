library(dustproj)
library(CRFutil)
library(gRapHD)
#library(gRim)
#library(MASS)

options(max.print=1000000)
setwd("/Users/karen2/latex/papers/dust/steph_diss/")

fdat <- read.csv("data/dust.data.as-of-5-24-22_SAMPLE_INFO.csv", header=T) # Sample info
dat  <- read.csv("data/dust.data.as-of-5-24-22.csv", header=T)             # Processed data
dim(dat)

# Categires (actual node names), but they will reference/IDed by their row numbers
categs <- data.frame(1:nrow(dat), dat[,1:3])
colnames(categs) <- c("CategoryID",colnames(categs)[2:4]) # Put in an extra column to gaurantee OG category numbers don't get lost. We'll need them

# Entire Data, n-samples by p-categories WITH repeats-per-location
X.all <- t(dat[,4:ncol(dat)]) # Keep all data in this one

# Drop categories that never occurred:
X           <- t(dat[,4:ncol(dat)])
dim(X)
rownames(X) <- 1:nrow(X)

never.occur.categ.idxs <- which(colSums(X) == 0)
X                      <- X[,-never.occur.categ.idxs]
# Re-index the category IDs:
colnames(X) <- paste0("X",1:ncol(X))
head(X)
dim(X)

# In principle, this should keep the OG categ indices attached to the categories that occurred,
# but they are safely stored in categs as a back-up:
occur.categ.idxs <- (1:nrow(categs))[-never.occur.categ.idxs] # OG category IDs that occured
#colnames(X)      <- paste0("X", occur.categ.idxs)
#head(X)

categs.occured <- categs[occur.categ.idxs,]
rownames(categs.occured) <- NULL
head(categs.occured,20)
dim(categs.occured)

# ****** Make a function to compute X.pop !!!!!!!!!!!!
# Drop the repeats-per-location to form the "population" data:
lbl       <- fdat$lbl.loc
lbl.names <- unique(lbl)

X.pop   <- NULL
lbl.pop <- NULL
for(i in 1:length(lbl.names)){
  #print(i)
  grp.mat.info <- pick.out.groups(X, lbl, c(lbl.names[i]))
  grp.mat      <- grp.mat.info[[1]]

  if(length(grp.mat.info[[2]]) == 1){
    X.pop   <- rbind(X.pop, grp.mat) # Only one aliquot at the location, so just tack it in
    lbl.pop <- c(lbl.pop, lbl.names[i])

    print("**********************************")
    print(paste("Location:", lbl.names[i], "has", length(grp.mat.info[[2]]), "total aliquots."))
    print(paste("Just tacked in 1 dust vector of length", length(grp.mat), ". Dim blank?:", dim(grp.mat),"from location:", lbl.names[i] ))


  } else {
    grp.mat.uniq <- unique(grp.mat)      # If more than one aliquot at location, keep only the unique configurations
    X.pop        <- rbind(X.pop,grp.mat.uniq)
    lbl.pop      <- c(lbl.pop, rep(lbl.names[i], nrow(grp.mat.uniq)) )

    print("----------------------------------")
    print(paste("Location:", lbl.names[i], "has", length(grp.mat.info[[2]]), "total aliquots."))
    print(paste("Dropped", nrow(grp.mat) - nrow(grp.mat.uniq), "from location:", lbl.names[i] ))

  }

}
rownames(X.pop) <- NULL
head(X.pop)
dim(X.pop)
#lbl.pop

# ****** Make a function to compute pop.adj.mat in the same file as X.pop !!!!!!!!!!!!
pop.adj.mat <- read.csv("analysis_prototypes/5-24-22_processed_pop_adj_mat.csv", header=F) # see build_population_graph.R
sum(pop.adj.mat)/2 # Check. Should be edges in the population


dim(X)     # All data with non-occuring categories dropped
dim(X.pop) # "Population" with matching replicates and non-occuring categories dropped

# Tests: -----------------------------------------------------------------
# Small:
Q.idx <- 9  # 1:828 dust vectors
lbl[Q.idx]  # True location name of the Questioned
K.lbl <- 61 # 1:198 locations

# BIG:
# Q.idx <- 746  # 1:828 dust vectors
# lbl[Q.idx]  # True location name of the Questioned
# K.lbl <- 167 # 1:198 locations

Q  <- t(X[Q.idx,])            # Questioned dust vector
Ks <- X[which(lbl == K.lbl),] # Known(s) dust vector(s)

# Get local model and local affinities:
loc.prep <- local.model.prep(Q, Ks, pop.adj.mat, model.type = "model3", printQ = T)
loc.affs <- make.QK.local.harmonized.affinities(
  loc.prep$harmonized.info, loc.prep$model.edge.mat,
  population.datamat = X.pop, #num.local.sims = 629-5,
  normalizeQ = T, printQ=F)

# Get Population model and population affinities
pop.prep <- population.model.prep(Q, Ks, pop.adj.mat)
pop.affs <- make.QK.population.harmonized.affinities(
  pop.prep$harmonized.info, pop.prep$model.edge.mat,
  population.datamat = X.pop,
  normalizeQ = T, printQ=F)


# Plots
# Local:
gph.loc <- graph_from_adjacency_matrix(loc.prep$model.adj.mat, mode="undirected")
# graphNEL format:
gph.loc <- as_graphnel(gph.loc)
gph.loc # Check
dev.off()
plot(gph.loc)
# grapHD format:
gphHD.loc <- as.gRapHD(loc.prep$model.edge.mat, p=length(loc.affs$node.affinities))
gphHD.loc # Check
dev.off()
plot(gphHD.loc, numIter=1000, vert.label=T)

# Population:
gph.pop <- graph_from_adjacency_matrix(pop.prep$model.adj.mat, mode="undirected")
# graphNEL format:
gph.pop <- as_graphnel(gph.pop)
gph.pop # Check
dev.off()
plot(gph.pop)
# grapHD format:
gphHD.pop <- as.gRapHD(pop.prep$model.edge.mat, p=length(pop.affs$node.affinities))
gphHD.pop # Check
dev.off()
plot(gphHD.pop, numIter=1000, vert.label=T)


# Component separations
# Population
ccp.list.pop <- connComp(gph.pop)
ccp.list.pop
as.numeric(ccp.list.pop[[2]]) # for each multinode component
# Get adj mat
# Store "harmonized" node ID so we don't loose them.
# From adj mat, construct edge mat (with "harmonized" IDs or new reduced for the second time IDs??)
# match edge to a model.edge.mat row and pull the corresponding affinity
cii <- get.component.graph.info(
  component.graph.nodes = ccp.list.pop[[6]],
  a.model.adj.mat       = pop.prep$model.adj.mat,
  affinities.info       = pop.affs,
  a.harmonized.info     = pop.prep$harmonized.info)

cii$component.adj.mat
cii$harmonized.edge.mat
cii$component.edge.mat
cii$idx.translation.mat
cii$node.affinities
cii$edge.affinities

