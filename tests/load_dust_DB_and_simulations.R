source("/Users/npetraco/latex/papers/dust/DUST PROJECT/program_codes/sourceme.R")
source("/Users/npetraco/codes/R/chemometric_utilities/sourceme.R")
rootd<-"/Users/npetraco/math/Data/database/dust/"

dat <- read.csv(paste0(rootd,"dust_data.csv"), header = T)
alt.lbl.names <- read.csv(paste0(rootd,"dust_data_alt_labels.csv"), header = T)
catg.names <- read.csv(paste0(rootd,"dust_data_categories.csv"), header = T)
colnames(dat)
X <- dat[,-c(1,2)]
#colnames(X) <- NULL
#rownames(X) <- NULL
lbl <- dat[,2]

# Separate groups with replicates from groups with only 1 replicate
grp.reps <- count.group.replicates(lbl)
grp.reps
#lbl[-grps.with.reps.idxs]
#alt.lbl.names[-grps.with.reps.idxs,]
#
grps.with.reps.idxs <- which(unlist(sapply(1:length(grp.reps), function(x){rep(grp.reps[x],grp.reps[x])}))>1)
lbl.with.reps <- lbl[grps.with.reps.idxs]
X.with.reps <- X[grps.with.reps.idxs,]
colnames(X.with.reps)
colnames(X)

length(lbl.with.reps)
dim(X.with.reps)

#For the simulation, use the reps set:
# Group-wise marginal probabilities for each category:
prob1.mat <- groupwise.category.freq.mat(X.with.reps, lbl.with.reps)
dim(prob1.mat)

#Simulate 5 each from the reps
dim(X.with.reps)
X.with.reps.sim<-NULL
lbl.with.reps.sim <- NULL
for(i in 1:nrow(X.with.reps)) {
  print(i)
  sim<-simulate.sample(X.with.reps[i,], num.sims=5, prob1.mat)
  colnames(sim) <- colnames(X.with.reps)

  X.with.reps.sim<-rbind(X.with.reps.sim, X.with.reps[i,], sim)

  lbl.with.reps.sim <- c(lbl.with.reps.sim, lbl.with.reps[i], rep(lbl.with.reps[i], nrow(sim)))
}

dim(X.with.reps.sim)
length(lbl.with.reps.sim)
dim(X.with.reps)
lbl.with.reps.sim
count.group.replicates(lbl.with.reps.sim)
lbl.with.reps


#Simulate 29 from the non reps.
lbl.without.reps <- lbl[-grps.with.reps.idxs]
X.without.reps <- X[-grps.with.reps.idxs,]

length(lbl.without.reps)
dim(X.without.reps)

dim(X.without.reps)
X.without.reps.sim<-NULL
lbl.without.reps.sim<-NULL
for(i in 1:nrow(X.without.reps)) {
  print(i)
  sim<-simulate.sample(X.without.reps[i,], num.sims=29, prob1.mat)
  colnames(sim) <- colnames(X.without.reps)

  #Xb.sim<-rbind(Xb.sim,Xb[i,],sim)
  X.without.reps.sim<-rbind(X.without.reps.sim, X.without.reps[i,], sim)
  lbl.without.reps.sim <- c(lbl.without.reps.sim, lbl.without.reps[i], rep(lbl.without.reps[i], nrow(sim)))

}

dim(X.without.reps.sim)
length(lbl.without.reps.sim)
dim(X.without.reps)
lbl.without.reps.sim
count.group.replicates(lbl.without.reps.sim)
lbl.without.reps


# Re-order and Check real-simulated data set:
lbl.real.and.sim <- c(lbl.with.reps.sim, lbl.without.reps.sim)
X.real.and.sim <- rbind(X.with.reps.sim, X.without.reps.sim)
dim(X.real.and.sim)

reorder.idxs <- order(lbl.real.and.sim)
X.real.and.sim <- X.real.and.sim[reorder.idxs,]
lbl.real.and.sim <- lbl.real.and.sim[reorder.idxs]

#Remove categories that never are encountered in the data set:
X.real.and.sim.red <- X.real.and.sim
# drop.idxs<-which(colSums(X.real.and.sim.red)==0)
# length(drop.idxs)
# dropped.catg.names <- catg.names[drop.idxs,]
# remaining.catg.names <- catg.names[-drop.idxs,]
# remaining.catg.names

#Before we jitter, look at the real/sim-occurances of the encountered categories:
plot(colSums(X.real.and.sim.red[,-drop.idxs]),typ="h", main="Encountered Categories Only, Real and Sim")
split.screen(c(2,1))
screen(1)
plot(colSums(X.real.and.sim.red),typ="h", main="All Categories, Real and Sim Data")
screen(2)
plot(colSums(X),typ="h", main="All Categories, Real Data Only")
dev.off()

#X.real.and.sim.jit <- X.real.and.sim.red[,-drop.idxs] # Note: unencountered categories have already been removed
#Jitter the vects:
#Real and simulated:
# X.sim.jit<-NULL
#X.real.and.sim.jit <- NULL
#X.real.and.sim.red <- X.real.and.sim
dim(X.real.and.sim.red)
X.real.and.sim.jit <- array(0,dim(X.real.and.sim.red))
dim(X.real.and.sim.jit)

for(i in 1:nrow(X.real.and.sim.red)) {
  tmp <- unlist(jitter.non.rep.vec(X.real.and.sim.red[i,], 1, 0.1))
  print(i)
  #colnames(tmp) <- colnames(X.real.and.sim.red)
  #X.real.and.sim.jit <- rbind(X.real.and.sim.jit, tmp)
  X.real.and.sim.jit[i,] <- tmp
}

drop.idxs <- which(colSums(X.real.and.sim.jit)==0)
X.real.and.sim.jit.red <- X.real.and.sim.jit[,-drop.idxs]


# Quick 3D PCA plot to see how things look:
Xcheck<-scale(X.real.and.sim.jit.red,center=TRUE,scale=FALSE)[,]
lbl.check <- lbl.real.and.sim
pca.model<-prcomp(Xcheck,scale=FALSE)
plot(pca.model)
summary(pca.model)

#Do a 3D PCA "scores" plot:
M<-3                                              #Pick dimension
Z<-predict(pca.model)[,1:M]                       #Grab PCA scores
plot3d(Z[,1],Z[,2],Z[,3],type="s",radius=0.01,col=as.numeric(lbl.check),aspect="iso",xlab="PC1",ylab="PC2",zlab="PC3")
text3d(Z[,1],Z[,2],Z[,3],text=lbl.check,font=1,adj=1.5) #Group lables
