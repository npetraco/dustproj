# PRUNE function. Input edges mat and datamat. Do like we did for population graph
lprep$edge.mat
fp.vec  <- array(-1, nrow(lprep$edge.mat))
fps.vec <- array(-1, nrow(lprep$edge.mat))
cp.vec  <- array(-1, nrow(lprep$edge.mat))
cps.vec <- array(-1, nrow(lprep$edge.mat))

for(i in 1:nrow(lprep$edge.mat)) {
  iidx1 <- lprep$edge.mat[i,1]
  iidx2 <- lprep$edge.mat[i,2]

  AAs <- lprep$QK.harmonized.local[,iidx1]
  BBs <- lprep$QK.harmonized.local[,iidx2]
  AA  <- lprep$QK.harmonized.info$K.harmonized[,iidx1]
  BB  <- lprep$QK.harmonized.info$K.harmonized[,iidx2]

  ctabC <- table(
    factor(AA, levels = c(1,0)),
    factor(BB, levels = c(1,0))
  )

  ctabCs <- table(
    factor(AAs, levels = c(1,0)),
    factor(BBs, levels = c(1,0))
  )

  fpC <- fisher.test(ctabC, alternative = "two.sided", simulate.p.value=T)$p.value
  cpC <- chisq.test(ctabC, simulate.p.value = T)$p.value

  fpCs <- fisher.test(ctabCs, alternative = "two.sided", simulate.p.value=T)$p.value
  cpCs <- chisq.test(ctabCs, simulate.p.value = T)$p.value

  fp.vec[i]  <- fpC
  fps.vec[i] <- fpCs
  cp.vec[i]  <- cpC
  cps.vec[i] <- cpCs

}

fp.vec
fps.vec
cp.vec
cps.vec

num.comp <- nrow(lprep$edge.mat)
fps.corrected <- p.adjust(fps.vec, method = "bonferroni", n = num.comp)
cps.corrected <- p.adjust(cps.vec, method = "bonferroni", n = num.comp)

fps.corrected
cps.corrected
#

plot.graph(lprep, Category.IDs.plotQ = F)

lprep$QK.harmonized.info$QK.Category.IDs
lprep$QK.harmonized.local
lprep$model.adjacency.mat
lprep$edge.mat
lprep$minForest.edges
lprep$minForest.edge.scores
lprep$KForest

junk <- prune.local.model(lprep)
junk$QK.harmonized.info
junk$QK.harmonized.local
dim(junk$model.adjacency.mat)
sum(junk$model.adjacency.mat)
junk$edge.mat
junk$minForest.edges
junk$minForest.edge.scores

# Pairwise check function to check all possible pairs. Alternative to graphHD for local graph.

