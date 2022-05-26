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

#What percentage our samples have human hair in them?
cbind(catg.names[1:38,],catg.percent[1:38])
dim(X)
length(num.of.reps)
lbl <- generate.label.vec(num.of.reps)
X.hair <- X[,1:38]
cbind(lbl, X.hair)
count <- 0
for(i in 1:79) {
  if(1 %in% pick.out.groups(X.hair,lbl,c(i))[[1]]){
    count <- count + 1
  }
}
count/79


# which(catg.names=="Human")
# junk <- strsplit(catg.names[,1]," ")
# 
# human.hair.idxs <- NULL
# for(i in 1:length(junk))  
# 
# junk[[i]]
# sum(catg.counts[1:38])
# cbind(catg.names, catg.counts, catg.percent)


plot(catg.counts,typ="h",ylab="counts",xlab="Debris Category", main="Dust Component Distribution Across Database")
#Most numerous categories:
#idxs<-which(catg.percent<=2 & catg.percent>0)
idxs<-which(catg.percent>10)
coms<-cbind(idxs,catg.names[idxs,],catg.counts[idxs],catg.percent[idxs])
coms
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

lbl.sim<-as.numeric(generate.label.vec(rep(30,79)))
X.sim<-rbind(Xa.sim,Xb.sim)
dim(X.sim)

#Jitter the vects:
#Real and simulated:
X.sim.jit<-NULL
for(i in 1:nrow(X.sim)) {
  tmp<-jitter.non.rep.vec(X.sim[i,], 1, 0.01)
  X.sim.jit<-rbind(X.sim.jit,tmp)
}
#Real only:
X.jit<-NULL
for(i in 1:nrow(X)) {
  tmp<-jitter.non.rep.vec(X[i,], 1, 0.01)
  X.jit<-rbind(X.jit,tmp)
}


#Remove categories that never are encountered in the data set:
drop.idxs<-which(colSums(X.sim.jit)==0)
catg.names[drop.idxs,]
X2.sim.jit<-X.sim.jit[,-drop.idxs]

#Remove categories that never are encountered in the data set:
drop.idxs<-which(colSums(X.jit)==0)
catg.names[drop.idxs,]
X2.jit<-X.jit[,-drop.idxs]