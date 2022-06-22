#' Margenalize a data matrix  NOT WORKING YET. WE MAY TOSS THIS!!!!!!!!!
#'
#' XXXX
#'
#' The function
#'
#' @param XX The XX
#' @details The function
#'
#' @return The function will XX
#'
#'
#' @export
margenalize_to<-function(a.class=NULL, a.subclass=NULL, an.attribute=NULL, categmat, datamat) {

  categmat.colnames <- colnames(categmat)
  #print(categmat.colnames)

  # Determine which column of the categmat we need:
  col.idx.vec <- NULL
  if(!is.null(a.class)){
    class.col.idx <- which(categmat.colnames == "Class") # Do it this way in case we switched things around in categmat
    col.idx.vec <- c(col.idx.vec, class.col.idx)         # At the Class check should be nothing in here at this point.....
  }
  if(!is.null(a.subclass)){
    subclass.col.idx <- which(categmat.colnames == "Subclass")   # Do it this way in case we switched things around in categmat
    col.idx.vec <- c(col.idx.vec, subclass.col.idx)
  }
  if(!is.null(an.attribute)){
    attribute.col.idx <- which(categmat.colnames == "Attribute") # Do it this way in case we switched things around in categmat
    col.idx.vec <- c(col.idx.vec, attribute.col.idx)
  }
  #print(col.idx.vec)

  search.categ.mat           <- data.frame(categmat[,col.idx.vec])
  #print("HERE")
  #print(colnames(categmat)[col.idx.vec])
  rownames(search.categ.mat) <- NULL # Need to use the row nums as indices because mach_df does not return them. Reset the rownames in case they are not contiguous.
  #print(search.categ.mat)

  # Specified Class, Subclass, Attribute vector to match in categmat
  csa.vec <- c(a.class, a.subclass, an.attribute)
  csa.vec <- data.frame(array(csa.vec, c(1, length(csa.vec))))
  if(length(col.idx.vec) == 1) {
    colnames(csa.vec) <- colnames(categmat)[col.idx.vec]
    #print("HERE")
    #print(names(search.categ.mat))
    #print(colnames(search.categ.mat))
  } else {
    colnames(csa.vec) <- colnames(search.categ.mat)
  }
  #print(csa.vec)

  # FIX HERE FOR SINGLE CLASS. SOMETHING NOT RIGHT..........
  c.idxs <- as.numeric(rownames(match_df(search.categ.mat, csa.vec)))
  print(c.idxs)

  if(length(c.idxs) == 1) {
    marg.vec <- datamat[, c.idxs]
  } else {
    marg.vec <- as.numeric(rowSums(datamat[, c.idxs]) != 0)
  }
  #print(marg.vec)

  marg.info <- list(
    csa.vec,
    marg.vec
  )

  return(marg.info)

}
