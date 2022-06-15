source("/Users/karen2/latex/papers/dust/steph_diss/analysis_prototypes/test_load_data_for_prob_models.R")

samp.loc.sizes <- table(lbl)
samp.loc.sizes[samp.loc.sizes >= 4]
samp.locs.4p   <- as.numeric(names(samp.loc.sizes[samp.loc.sizes >= 4]))
samp.locs.4p

count <- 1
#for(i in 1:length(samp.locs.4p)) {
for(i in 1:1) { # length(samp.locs.4p)
  Q.true.loc <- samp.locs.4p[i]
  samp.idxs <- which(lbl == samp.locs.4p[i]) # Pick out samples with more than 4 aliquots
  for(j in 1:length(samp.idxs)) {
    Q.idx   <- samp.idxs[j]            # Pick out one dv at a time from the KM location to serve as the Q
    KM.idxs <- samp.idxs[-j]           # The remaining dvs from the KM location will serve as the (KM) Ks
    KNM.samp.names <- samp.locs.4p[-i] # All other locations are KNM

    # One KM calc here

    # print(paste0("Questioned idx: ", Q.idx))
    # print("KM idxs:")
    # print(KM.idxs)

    for(k in 1:length(KNM.samp.names)) { # Loop over the KNM locations and do the calculations
      KNM.idxs <- which(lbl == KNM.samp.names[k])

      # KNM calcs here
      count <- count + 1

      # print(paste0("KNM# ", k, " = location: ", KNM.samp.names[k], ". KNM idxs:"))
      # print(KNM.idxs)
      # print("-------------------------")

    }

  }
}
count
#


which(lbl == 135)
Q.idx <- 509  # 1:828 dust vectors
lbl[Q.idx]  # True location name of the Questioned
K.lbl <- 191 # 1:198 locations

Q  <- t(X[Q.idx,])            # Questioned dust vector
Ks <- X[which(lbl == K.lbl),] # Known(s) dust vector(s)
