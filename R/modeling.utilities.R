#' "Harmonize" one or more questioned dust vector to a set of known dust vectors
#'
#' XXXX
#'
#' The function will put the two sets of dust vectors into the same category space
#'
#' @param XX The XX
#' @details The function will put the two sets of dust vectors into the same category space.
#' It keeps categories which appear in at leat one of the vectors and drops categories wich appear
#' in neither set.
#' @return The function will XX
#'
#'
#' @export
harmonize.QtoKs<-function(Q.mat, K.mat) {

  if(!is.matrix(Q.mat)){
    stop("Q input must be a matrix!")
  }
  if(!is.matrix(K.mat)){
    stop("K input must be a matrix!")
  }

  harmonized.vects <- rbind(Q.mat, K.mat)
  non.zeroQ        <- colSums(harmonized.vects) != 0
  harmonized.vects <- harmonized.vects[,non.zeroQ]

  Q.harmonized <- harmonized.vects[1:nrow(Q.mat),]
  K.harmonized <- harmonized.vects[(nrow(Q.mat)+1):nrow(harmonized.vects),]

  kept.category.idxs <- which(non.zeroQ==T)

  harmonized.info <- list(Q.harmonized, K.harmonized, kept.category.idxs)
  names(harmonized.info) <- c("Q.harmonized", "K.harmonized", "kept.category.idxs")

  return(harmonized.info)

}
