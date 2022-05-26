source("/Users/npetraco/latex/papers/dust/DUST PROJECT/program_codes/sourceme.R")
source("/Users/npetraco/codes/R/chemometric_utilities/sourceme.R")
rootd<-"/Users/npetraco/latex/papers/dust/DUST PROJECT/Completed Data Sheets/"

file.root<-"dust_table_"
path.to.files<-paste(rootd,file.root,sep="")
num.of.samples<-79
num.of.reps<-c(rep(5,36),rep(1,23),rep(5,20))

X<-NULL
for(i in 1:num.of.samples) {
  for(j in 1:num.of.reps[i]) {
    dfpath<-paste(path.to.files,i,".",j,".xlsx",sep="")
    dinfo<-read.dust.file(dfpath)
    data.vec<-dinfo[[1]]
    #data.vec<-t(info[,4])
    X<-rbind(X,data.vec)
    print(paste("Done sample",i,"replicate",j))
  }
}
catg.names<-dinfo[[2]]
catg.counts<-colSums(X)
catg.percent<-colSums(X)/nrow(X)*100

catg.names
catg.counts

plot(catg.counts,typ="h",ylab="counts",xlab="Debris Category", main="Dust Component Distribution Across Database")

lbl.orig<-generate.label.vec(num.of.reps)
lbl.orig<-as.numeric(lbl.orig)

#Separate reps and non-reps
Xa<-X[c(1:180,204:303),]
lbla<-lbl.orig[c(1:180,204:303)]
Xb<-X[181:203,]
lblb<-lbl.orig[181:203]

#For the simulation:
prob1.mat<-groupwise.category.freq.mat(Xa,lbla)

#Simulate 5 each from the reps
dim(Xa)
Xa.sim<-NULL
for(i in 1:nrow(Xa)) {
  sim<-simulate.sample(Xa[i,], num.sims=5, prob1.mat)
  Xa.sim<-rbind(Xa.sim,Xa[i,],sim)
}
dim(Xa.sim)
dim(Xa)
#plot(sim[5,],typ="h")
#plot(Xa[1,],typ="h")

#Simulate 29 from the non reps. use a burn in?????
dim(Xb)
Xb.sim<-NULL
for(i in 1:nrow(Xb)) {
  sim<-simulate.sample(Xb[i,], num.sims=29, prob1.mat)
  Xb.sim<-rbind(Xb.sim,Xb[i,],sim)
}
dim(Xb.sim)

#Now Load in the new dust samples from Florida. They're are a few extra things in section 7 (cf. read.dust.file.EXPANDED)
#Since these samples have only one rep each, drop them for now. There are not enough to build a prob mat for the
#extra things and none of the other data sheets have these categires.
new.samp1 <- read.dust.file.EXPANDED("/Users/npetraco/latex/papers/dust/DUST PROJECT/Dust Specimens_5-19-15/DB14/DB14_mod.xlsx")
new.samp2 <- read.dust.file.EXPANDED("/Users/npetraco/latex/papers/dust/DUST PROJECT/Dust Specimens_5-19-15/DB28/DB28_mod.xlsx")
new.samp3 <- read.dust.file.EXPANDED("/Users/npetraco/latex/papers/dust/DUST PROJECT/Dust Specimens_5-19-15/DB48/DB48.xlsx")
new.samp4 <- read.dust.file.EXPANDED("/Users/npetraco/latex/papers/dust/DUST PROJECT/Dust Specimens_5-19-15/DB49/DB49_mod.xlsx")

Xc <- rbind(new.samp1[[1]],new.samp2[[1]], new.samp3[[1]], new.samp4[[1]])
dim(Xc)
Xc <- Xc[,-(540:587)] #Drop the extra things addes in this new data.
dim(Xc)

#Simulate 29 from for these new samples. They non reps.
dim(Xc)
Xc.sim<-NULL
for(i in 1:nrow(Xc)) {
  sim<-simulate.sample(Xc[i,], num.sims=29, prob1.mat)
  Xc.sim<-rbind(Xc.sim,Xc[i,],sim)
}
dim(Xc.sim)


# Load WTC dust sample
new.samp5 <- read.dust.file("/Users/npetraco/latex/papers/dust/DUST PROJECT/WTC_dust_samples/WTC_Site_Ground_Zero_911_mod_samp1.xlsx")
new.samp6 <- read.dust.file("/Users/npetraco/latex/papers/dust/DUST PROJECT/WTC_dust_samples/WTC_Site_Ground_Zero_911_mod_samp2.xlsx")
new.samp7 <- read.dust.file("/Users/npetraco/latex/papers/dust/DUST PROJECT/WTC_dust_samples/WTC_Site_Ground_Zero_911_mod_samp3.xlsx")
new.samp8 <- read.dust.file("/Users/npetraco/latex/papers/dust/DUST PROJECT/WTC_dust_samples/WTC_Site_Ground_Zero_911_mod_samp4.xlsx")
new.samp9 <- read.dust.file("/Users/npetraco/latex/papers/dust/DUST PROJECT/WTC_dust_samples/WTC_Site_Ground_Zero_911_mod_samp5.xlsx")

Xd <- rbind(new.samp5[[1]],new.samp6[[1]], new.samp7[[1]], new.samp8[[1]], new.samp9[[1]])
dim(Xd)

#Simulate 5 each from the reps
dim(Xd)
Xd.sim<-NULL
for(i in 1:nrow(Xd)) {
  sim<-simulate.sample(Xd[i,], num.sims=5, prob1.mat)
  Xd.sim<-rbind(Xd.sim,Xd[i,],sim)
}
dim(Xd.sim)
dim(Xd)


lbl.sim<-as.numeric(generate.label.vec(rep(30,84)))
X.sim<-rbind(Xa.sim, Xb.sim, Xc.sim, Xd.sim)
dim(X.sim)

#Jitter the vects:
#Real and simulated:
X.sim.jit<-NULL
for(i in 1:nrow(X.sim)) {
  tmp<-jitter.non.rep.vec(X.sim[i,], 1, 0.1)
  X.sim.jit<-rbind(X.sim.jit,tmp)
}

#Remove categories that never are encountered in the data set:
drop.idxs<-which(colSums(X.sim.jit)==0)
dropped.catg.names <- catg.names[drop.idxs,]
remaining.catg.names <- catg.names[-drop.idxs,]
X2.sim.jit<-X.sim.jit[,-drop.idxs]

dim(X2.sim.jit)
#These are the observation #s for the 4 real new samples:
which(lbl.sim==80)[1]
which(lbl.sim==81)[1]
which(lbl.sim==82)[1]
which(lbl.sim==83)[1]



# Quick 3D PCA plot to see how things look:
Xcheck<-scale(X2.sim.jit,center=TRUE,scale=FALSE)[,]
lbl.check <- lbl.sim
pca.model<-prcomp(Xcheck,scale=FALSE)
plot(pca.model)
summary(pca.model)

#Do a 3D PCA "scores" plot:
M<-3                                              #Pick dimension
Z<-predict(pca.model)[,1:M]                       #Grab PCA scores
plot3d(Z[,1],Z[,2],Z[,3],type="s",radius=0.01,col=as.numeric(lbl.check),aspect="iso",xlab="PC1",ylab="PC2",zlab="PC3")
text3d(Z[,1],Z[,2],Z[,3],text=lbl.check,font=1,adj=1.5) #Group lables
