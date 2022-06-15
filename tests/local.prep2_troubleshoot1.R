source("/Users/karen2/latex/papers/dust/steph_diss/analysis_prototypes/test_load_data_for_prob_models.R")

# local.prep2 is break if no edges are found by minForest. This script is to find problem and fix

samp.loc.sizes <- table(lbl)
samp.loc.sizes[samp.loc.sizes >= 4]

locs.4p <- as.numeric(names(samp.loc.sizes[samp.loc.sizes >= 4]))
locs.4p

# Test: Q same location as as Ks, so all Categories will be shared in common
true.Q.lbl      <- 122 # True location name of the Questioned
true.Q.lbl.idxs <- which(lbl == true.Q.lbl)
true.Q.lbl.idxs
Q.idx           <- 461  # 1:828 dust vectors
KA.lbl          <- true.Q.lbl # 1:198 locations

Q  <- t(X[Q.idx,])             # Questioned dust vector
KA <- X[which(lbl == KA.lbl),] # KM Known(s) dust vector(s)
#KA
KA <- KA[-which(true.Q.lbl.idxs == Q.idx),] # Hold-out the Questioned
#KA

Aa <- ombibus.standard.analysis(
  a.Q.idx            = Q.idx,
  a.Q.lbl            = true.Q.lbl,
  a.Q                = Q,
  a.K.lbl            = KA.lbl,
  a.Kmat             = KA,
  population.datamat = X.pop,
  population.adj.mat = pop.adj.mat)
Aa
oa.mat <- Aa # omnibus analysis mat


locs.4p.tmp <- locs.4p
locs.4p.tmp <- locs.4p.tmp[-which(locs.4p.tmp == true.Q.lbl)] # Drop the true loc lbl and loop over the remaining KNM

for(i in 1:length(locs.4p.tmp)) {
  KB.lbl <- locs.4p.tmp[i]           # KNM dust vector(s)
  # KB.lbl == KA.lbl
  KB     <- X[which(lbl == KB.lbl),] # KNM dust vector(s)
  # #KB

  Ba <- ombibus.standard.analysis(
    a.Q.idx            = Q.idx,
    a.Q.lbl            = true.Q.lbl,
    a.Q                = Q,
    a.K.lbl            = KB.lbl,
    a.Kmat             = KB,
    population.datamat = X.pop,
    population.adj.mat = pop.adj.mat)
  oa.mat <- rbind(oa.mat, Ba)

  print(paste0("Loc: ", KA.lbl, " vs. Loc: ", KB.lbl, " done."))
}

locs.4p
KB
KB.lbl

ombibus.standard.analysis(
  a.Q.idx            = Q.idx,
  a.Q.lbl            = true.Q.lbl,
  a.Q                = Q,
  a.K.lbl            = KB.lbl,
  a.Kmat             = KB,
  population.datamat = X.pop,
  population.adj.mat = pop.adj.mat)

local.model.prep2(a.Q.vec = Q, a.K.mat = KB, population.datamat = X.pop,
                  printQ = T, plotQ = T, Category.IDs.plotQ = F)
harmonize.QtoKs(Q, KB)
QKB <- QK.harmonized.summary(Q, KB, X.pop, categs.occured, type="all", printQ = T)

# local.model.prep2_TESTING(a.Q.vec = Q, a.K.mat = KB, population.datamat = X.pop,
#                           printQ = T, plotQ = T, Category.IDs.plotQ = F)
#

junk.prep <- local.model.prep2(a.Q.vec = Q, a.K.mat = KB, population.datamat = X.pop,
                          printQ = T, plotQ = T, Category.IDs.plotQ = F)

junk.prep$edge.mat
junk.aff.info <- make.QK.local.harmonized.affinities2(a.QK.local.prep.info.list = junk.prep,
                                                         normalizeQ             = F,
                                                         scale.factor           = 1,
                                                         printQ                 = T)
junk.aff.info$node.affinities
junk.aff.info$edge.affinities

local.analysis(a.Q.vec = Q, a.K.mat = KB, ex.vec.num = 1, population.datamat = X.pop, seed=NULL, normalizeQ=F, scale.factor=1,
               pruneQ = T, printQ = F, plotQ=F, Category.IDs.plotQ = F)
