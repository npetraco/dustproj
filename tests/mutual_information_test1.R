library(gRbase)
library(entropy)
#library(bioDist)

source("/Users/karen2/latex/papers/dust/steph_diss/analysis_prototypes/test_load_data_for_prob_models.R")

ns1 <- X.pop[1,]
ns2 <- X.pop[2,]

atab <- table(
  factor(ns1, levels = c(1,0)),
  factor(ns2, levels = c(1,0))
)

names(attributes(atab)$dimnames) <- c("A", "B")

atab
pAB <- tabNormalize(atab, "all")
pA  <- tabMarg(pAB, marg = "A")
pB  <- tabMarg(pAB, marg = "B")

# Mutual information:
# A measure of the average amount of information that two random varites, X and Y share
# If X and Y are independent, MI(X,Y) = 0
sum(tabMult(pAB, log(pAB %a/% (pA %a*% pB), base = 2)))
mi.plugin(pAB, unit = "log2") # Check with entropy library function


# Point-wise mutual information:
# Quantifies the discrepancy between the probability of a pair of outcomes coincidence given their
# joint distribution and their individual distributions assuming independence:
ppmi  <- log(pAB %a/% (pA %a*% pB), base = 2) # Point-wise mutual information
nppmi <- tabDiv0(ppmi, -log(pAB, base = 2)) # "normalized" pont-wise mutual information, -1 to 1: -1 for never occurring together, 0 for independence, and +1 for complete co-occurrence
ppmi
nppmi


