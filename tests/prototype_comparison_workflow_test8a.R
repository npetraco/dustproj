source("/Users/karen2/latex/papers/dust/steph_diss/analysis_prototypes/test_load_data_for_prob_models.R")

samp.loc.sizes <- table(lbl)
samp.loc.sizes[samp.loc.sizes >= 4]

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

QK.harmonized.summary(Q, Ks, X.pop, categs.occured, type="all")    # All QK harmonized info summary
QK.harmonized.summary(Q, Ks, X.pop, categs.occured, type="K.only") # Info shared by Q and K summary
QK.harmonized.summary(Q, Ks, X.pop, categs.occured, type="Q.only") # Info occuring only for Q summary

# ============ Analysis ========
dv.num <- 6 # Dust vector number for analysis. Q is 1. Ks start with 2.

#-------------------------------
# Local
loc.pi <- local.analysis(
  a.Q.vec            = Q,      #
  a.K.mat            = Ks,     #
  ex.vec.num         = dv.num, # Dust vec to get probs for. 1 is Q. Ks start at 2
  population.datamat = X.pop,  #
  seed               = 1,      # Comment out for random local sims for each execution
  normalizeQ         = F,      #
  scale.factor       = 1,      #
  pruneQ             = T,      # Prune minForest edges with fisher tests?
  printQ             = F,      #
  plotQ              = T,      #
  Category.IDs.plotQ = F)
loc.pi$config.prob

#-------------------------------
# Population
pop.pi <- population.analysis(
  a.Q.vec            = Q,      #
  a.K.mat            = Ks,     #
  ex.vec.num         = dv.num, # Dust vec to get probs for. 1 is Q. Ks start at 2
  population.datamat = X.pop,  #
  population.adj.mat = pop.adj.mat,
  normalizeQ         = F,      #
  scale.factor       = 1,      #
  printQ             = F,      #
  plotQ              = T,      #
  Category.IDs.plotQ = F)
pop.pi$config.prob

# log LR local vs population
loc.pi$config.prob
pop.pi$config.prob
loc.pi$config.log.prob - pop.pi$config.log.prob

# log LR local-A vs local-B

# log LR Q local-KM vs a/all local-KNM
