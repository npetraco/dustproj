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
lbl.names
Q.idx <- 9  # 1:828 dust vectors
K.lbl <- 61 # 1:198 locations

# BIG:
# Q.idx <- 746  # 1:828 dust vectors
# lbl[Q.idx]  # True location name of the Questioned
# K.lbl <- 167 # 1:198 locations


# True location name of the Questioned
lblQ.true <- lbl[Q.idx]
lblQ.true

Q  <- t(X[Q.idx,])            # Questioned dust vector
Ks <- X[which(lbl == K.lbl),] # Known(s) dust vector(s)
Q
dim(Ks)

hif <- harmonize.QtoKs(Q, Ks)
hif$QK.Category.IDs

# Connect all of these. Make as edge mat
hif$K.only.category.IDs
hif$K.only.harmonized.idxs

# Model 1
# Treat all Qonly nodes as independent
#make.model.rep(hif$K.only.harmonized.idxs, hif$Q.only.harmonized.idxs, model.type = "model1")
mdi <- make.model.rep(Q, Ks, model.type = "model1")
dim(mdi$model.edge.mat)
sum(mdi$model.adj.mat)/2

# graph eq, split and plot
grphf.m1   <- adj2formula(mdi$model.adj.mat)
grphf.m1
gp.m1 <- ug(grphf.m1, result = "graph")
gp.m1
ccp.list <- connComp(gp.m1)
ccp.list
# Should be the known only indices:
as.numeric(ccp.list[[1]])
hif$K.only.category.IDs
hif$K.only.harmonized.idxs

dev.off()
plot(gp.m1)

gpHD.m1 <- as.gRapHD(mdi$model.edge.mat, p=length(hif$QK.Category.IDs))
dev.off()
plot(gpHD.m1, numIter=500, vert.label=T)




# Model 2
# Connect all Qonly nodes that are dependent in the population, but dont connect them to K nodes
Q.idx <- 9  # 1:828 dust vectors
lbl[Q.idx]  # True location name of the Questioned
K.lbl <- 61 # 1:198 locations

# BIG:
# Q.idx <- 746  # 1:828 dust vectors
# lbl[Q.idx]  # True location name of the Questioned
# K.lbl <- 167 # 1:198 locations

Q  <- t(X[Q.idx,])            # Questioned dust vector
Ks <- X[which(lbl == K.lbl),] # Known(s) dust vector(s)

hif <- harmonize.QtoKs(Q, Ks)
hif$K.only.category.IDs
hif$Q.only.category.IDs
kl <- length(hif$K.only.category.IDs)
ql <- length(hif$Q.only.category.IDs)
kl*(kl-1)/2
ql*(ql-1)/2

mdi2 <- make.model.rep(Q, Ks, pop.adj.mat, model.type = "model2", print=T)
dim(mdi2$model.edge.mat)
sum(mdi2$model.adj.mat)/2

# graph eq, split and plot
grphf.m2   <- adj2formula(mdi2$model.adj.mat)
grphf.m2
gp.m2 <- ug(grphf.m2, result = "graph")
gp.m2
ccp.list <- connComp(gp.m2)
ccp.list
# Should also be the known only indices:
as.numeric(ccp.list[[1]])
hif$K.only.category.IDs
hif$K.only.harmonized.idxs

dev.off()
plot(gp.m2)

gpHD.m2 <- as.gRapHD(mdi2$model.edge.mat, p=length(hif$QK.Category.IDs))
dev.off()
plot(gpHD.m2, numIter=1000, vert.label=T)


# Model 3
# Adding Qonly (Q) edges to K nodes and Qnodes:
# Connect Q nodes K nodes only if there is a population connection between categories
# If a Q node is connected to a K node
# To any Q nodes added check to see if they connect to each other
# To any Q nodes added see if they connect to any Q nodes not added
#
# Can we do this as:
# Loop over Qonly categories and see if they connect to any category in the union QK.Category.IDs (make sure not eq)

# Q.idx <- 9  # 1:828 dust vectors
# lbl[Q.idx]  # True location name of the Questioned
# K.lbl <- 61 # 1:198 locations

# BIG:
Q.idx <- 746  # 1:828 dust vectors
lbl[Q.idx]  # True location name of the Questioned
K.lbl <- 167 # 1:198 locations

Q  <- t(X[Q.idx,])            # Questioned dust vector
Ks <- X[which(lbl == K.lbl),] # Known(s) dust vector(s)

hif <- harmonize.QtoKs(Q, Ks)
hif$QK.Category.IDs
hif$Q.only.category.IDs
hif$K.only.category.IDs
length(hif$Q.only.category.IDs)*length(hif$QK.Category.IDs) - length(hif$Q.only.category.IDs)

mdi3 <- make.model.rep(Q, Ks, pop.adj.mat, model.type = "model3", printQ = T)
mdi3$model.edge.mat
dim(mdi3$model.edge.mat)
sum(mdi3$model.adj.mat)/2

# graph eq, split and plot
grphf.m3   <- adj2formula(mdi3$model.adj.mat) # This won't work for BIG graphs (LOTS of edges....)
grphf.m3
gp.m3 <- ug(grphf.m3, result = "graph")
gp.m3

# Works on BIG graphs but only returns connected components
# Must be messing up numbering too because of this......... UGH!!!!!
#gp.m3 <- ftM2graphNEL(mdi3$model.edge.mat, edgemode="undirected")
#
# Doesn't work either......
#sparseM2Graph(mdi3$model.adj.mat, nodeNames = as.character(1:ncol(mdi3$model.adj.mat)), edgemode="undirected")
#
# This needs work .....
# cl <- sapply(1:ncol(mdi3$model.adj.mat), function(xx){which(mdi3$model.adj.mat[,xx] == 1)})
# cl[[1]]
#
# Try igraph 3 functionality
igp.m3 <- graph_from_adjacency_matrix(mdi3$model.adj.mat, mode="undirected")
#plot(igp.m3)
gp.m3 <- as_graphnel(igp.m3)
gp.m3

ccp.list <- connComp(gp.m3)
ccp.list
# Some Q-nodess may now have been connected to the K-nodes:
as.numeric(ccp.list[[1]])
hif$K.only.category.IDs
hif$K.only.harmonized.idxs

dev.off()
plot(gp.m3)

gpHD.m3 <- as.gRapHD(mdi3$model.edge.mat, p=length(hif$QK.Category.IDs))
gpHD.m3
dev.off()
plot(gpHD.m3, numIter=1000, vert.label=T)
