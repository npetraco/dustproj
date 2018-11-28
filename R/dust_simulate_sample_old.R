#' Est prob-of-one-matrix
#'
#' Elements give est of prob that that the jth category will be observed in the ith group
#'
#' The function will XXXX
#'
#' @param XX The XX
#' @return The function will XX
#'
#'
#' @export
groupwise.category.freq.mat<-function(dat,lbls){
  grp.nms<-levels(factor(lbls))
  #prob-of-one-mat is groups by categories. Elems give est of prob that that the jth category will be observed in the ith group
  p1.mat<-NULL
  for(i in 1:length(grp.nms)) {
    datg<-pick.out.groups(dat,lbls,c(grp.nms[i]))[[1]]
    p1.vec<-colSums(datg)/nrow(datg) #MLE estimated probs
    p1.mat<-rbind(p1.mat,p1.vec)
  }

  return(p1.mat)

}


#' New est prob-of-one-matrix
#'
#' Elements give est of prob that that the jth category will be observed in the ith group. This one can do MLE or Bayes-uniform prior estimation
#'
#' The function will XXXX
#'
#' @param XX The XX
#' @return The function will XX
#'
#'
#' @export
groupwise.category.freq.mat2<-function(dat, lbls, est.type="MLE"){
  grp.nms<-levels(factor(lbls))
  #prob-of-one-mat is groups by categories. Elems give est of prob that that the jth category will be observed in the ith group
  p1.mat<-NULL
  for(i in 1:length(grp.nms)) {
    datg<-pick.out.groups(dat,lbls,c(grp.nms[i]))[[1]]
    if(est.type=="MLE") {
      p1.vec<-colSums(datg)/nrow(datg) #MLE estimated probs
    } else if(est.type=="Bayes") {
      nn <- nrow(datg)
      sm <- colSums(datg)
      p1.vec <- (sm+1)/(nn+1+1) #Posterior mean asuming uniform prior
    } else {
      stop("Specify an estimation type. Choices: MLE or Bayes")
    }
    p1.mat<-rbind(p1.mat,p1.vec)
  }

  return(p1.mat)

}

#' Simulate a dust sample, old version assuming cell independence
#'
#' Was called: simulate.sample
#'
#' The function will XXXX
#'
#' @param XX The XX
#' @return The function will XX
#'
#'
#' @export
simulate.dust.sample.old<-function(cat.ind.vec, num.sims, prob.of.1.mat) {

  sim.grp<-array(NA,c(num.sims,length(cat.ind.vec)))
  #print(dim(sim.grp))
  #Naieve 0/1 prob in the cat vector fed in.
  naieve.prob1<-sum(cat.ind.vec)/length(cat.ind.vec)

  for(i in 1:length(cat.ind.vec)) {

    if(cat.ind.vec[i]==0) {
      #For 0 categories:
      #in the prob vec for that category, randomly select an element, this will be p1 for that category in the simulation

      prob.vec.for.cat<-prob.of.1.mat[,i]
      prob1<-sample(prob.vec.for.cat,replace=T,size=1)

      #Temper 100% selections from prob-vec-for-cat a bit???? They seem to occur alot
      if(prob1==1) {
        #print(paste(i,prob1*100))
        prob1<-sample(c(prob1,naieve.prob1,0),replace=F,size=1,prob=c(1/3,1/3,1/3))
        #print(paste("Now:",i,prob1*100))
      }
      sim.cat.elem.ind.vec<-sample(c(1,0),size=num.sims,replace=T,prob=c(prob1,1-prob1))
      #print(sim.cat.elem.ind.vec)
    }

    if(cat.ind.vec[i]==1) {
      #For 1 categories:
      #go to that category in the prob-of-1-mat
      #keep only non zero percentages:
      prob.vec.for.cat<-prob.of.1.mat[,i]
      zero.idxs<-which(prob.vec.for.cat==0)

      #CAUTION AND NOTE: This routene weights 1 categories heavily in favor of coming up 1 again. This is in keeping of what we have observed empirically so far. 02/08/2013
      #if all probs in category are 0, make vector c(0,100,runif(1)*100) and sample from it. this will be p1
      if(length(zero.idxs) == length(prob.vec.for.cat)) {
        #Set, 95% of the time prob1 will come up 100% Can change later.
        prob1<-sample(c(0,1,runif(1)),replace=F,size=1,prob=c(0.025,0.95,0.025))
      } else { #otherwise collect non-zero vals. make vector c(non-zero-vals, 0,100, runif(1)*100)
        prob.vec.for.cat<-prob.vec.for.cat[-zero.idxs]
        #and sample form it; this will possibly be p1
        prob1<-sample(c(prob.vec.for.cat,0,1,runif(1)),replace=F,size=1)
        #Strongly weight prob1 in favor of 100% since we observe that when a category comes up 1 it is usually always 1
        prob1<-sample(c(1,prob1),replace=F,size=1,prob=c(0.95,0.05))
      }

      sim.cat.elem.ind.vec<-sample(c(1,0),size=num.sims,replace=T,prob=c(prob1,1-prob1))

    }

    sim.grp[,i]<-sim.cat.elem.ind.vec

  }

  return(sim.grp)
}
