#' Take a indicator vec and (positively) jitter the 1s with positive uniform random noise
#'
#' XXXX
#'
#' The function will XXXX
#'
#' @param XX The XX
#' @return The function will XX
#'
#'
#' @export
jitter.non.rep.vec<-function(dvec, num.reps.req, amt) {

  #Indices of the 1s
  id.idxs<-which(dvec!=0)
  num.id<-length(id.idxs)

  jittered.grp<-array(NA,c(num.reps.req,length(dvec)))

  count<-1
  while(count<=num.reps.req) {
    jittered.dvec<-dvec
    jittered.dvec[id.idxs]<-(jittered.dvec[id.idxs] + runif(num.id,0,amt))

    jittered.grp[count,]<-jittered.dvec

    count <- count + 1
  }

  return(jittered.grp)

}
