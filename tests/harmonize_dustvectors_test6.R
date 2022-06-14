# No common categories between Q and Ks test. What does this break??

K.lbl <- 61 # 1:198 locations
Ks <- X[which(lbl == K.lbl),] # Known(s) dust vector(s)
Ks

Q.test.junk <- array(0, c(1,ncol(Ks)))
Q.test.junk[which(colSums(Ks) == 0)[1:20]] <- 1
Q.test.junk

junk <- harmonize.QtoKs(Q.test.junk, Ks)
junk

QK.sma <- QK.harmonized.summary(Q.test.junk, Ks, X.pop, categs.occured, type="all", printQ = T)    # All QK harmonized info summary
QK.smk <- QK.harmonized.summary(Q.test.junk, Ks, X.pop, categs.occured, type="K.only", printQ = T) # Info shared by Q and K summary
QK.smq <- QK.harmonized.summary(Q.test.junk, Ks, X.pop, categs.occured, type="Q.only", printQ = T) # Info occurring only for Q summary


# ============ Analysis ========
dv.num <- 1 # Dust vector number for analysis. Q is 1. Ks start with 2.

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
