#There are only 1 replicate per sample of these new dust samples. Also there are a few new fiber categories added. 
#Need a new simulation strategy for them since there are no replicates for that new chunk.
new.samp1 <- read.dust.file.EXPANDED("/Users/npetraco/latex/papers/dust/DUST PROJECT/Dust Specimens_5-19-15/DB14/DB14_mod.xlsx")
new.samp2 <- read.dust.file.EXPANDED("/Users/npetraco/latex/papers/dust/DUST PROJECT/Dust Specimens_5-19-15/DB28/DB28_mod.xlsx")
new.samp3 <- read.dust.file.EXPANDED("/Users/npetraco/latex/papers/dust/DUST PROJECT/Dust Specimens_5-19-15/DB48/DB48.xlsx")
new.samp4 <- read.dust.file.EXPANDED("/Users/npetraco/latex/papers/dust/DUST PROJECT/Dust Specimens_5-19-15/DB49/DB49_mod.xlsx")

ups <- unique(as.numeric(prob1.mat))
count.vec <- numeric(length(ups))
for(i in 1:length(ups)){
  count.vec[i] <- length(which(as.numeric(prob1.mat) == ups[i]))
}
names(count.vec) <- as.character(ups)
count.vec
sum(count.vec)

hist(prob1.mat)
one.condition.0.probs <- count.vec/sum(count.vec)
one.condition.1.probs <- count.vec[-1]/sum(count.vec[-1])

#Old categires/subctegories:
catg.names

#New (additional) Categories/subcategories:
new.samp1[[2]][540:587,]

#Data on new categories:
new.samp1[[1]][540:587]
new.samp2[[1]][540:587]
new.samp3[[1]][540:587]
new.samp4[[1]][540:587]




working.vec <- new.samp2[[1]][540:587]
working.vec

num.sims <- 29
tmp.sims.array <- array(-1, c(29,length(working.vec)))
for(i in 1:length(working.vec)){
  obs <- working.vec[i]
  p.obs <- -1
  if(obs==0) {
    pobs <- sample(as.numeric(names(one.condition.0.probs)), size=1, prob=one.condition.0.probs)
    print(paste("Pr(1 | 0)=",pobs))
    sub.categ.sim.vec <- sample(c(1,0), size=num.sims, replace=T, prob=c(pobs, 1-pobs))
    tmp.sims.array[,i] <- sub.categ.sim.vec
  } else {
    pobs <- sample(as.numeric(names(one.condition.1.probs)), size=1, prob=one.condition.1.probs)
    print(paste("Pr(1 | 1)=",pobs))
    sub.categ.sim.vec <- sample(c(1,0), size=num.sims, replace=T, prob=c(pobs, 1-pobs))
    tmp.sims.array[,i] <- sub.categ.sim.vec
  }
}
rbind(tmp.sims.array,working.vec)

sample(ups, size = 1, prob= one.condition.0.probs)
sample(ups[-1], size = 1, prob = one.condition.1.probs)

prob.occ.info <- cbind(ups[-1], count.vec[-1], count.vec[-1]/sum(count.vec[-1]))
ber.prob <- sample(prob.occ.info[,1], size = 1, prob=prob.occ.info[,3])
sample(c(1,0), size = 1, prob=c(ber.prob,1-ber.prob))



cts <- hist(as.numeric(prob1.mat), plot=F)$counts[-1]
cts2 <- cts[-c(which(cts==0))]
cts2
hist(as.numeric(prob1.mat), plot=F)$breaks[-c(1,which(cts==0))]
