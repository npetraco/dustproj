#' Clean out spaces and set all characters to lowercase in a character vector
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
clean.chars<-function(char.vec, char.to.rm){

  cleaned.char.vec <- sapply(1:length(char.vec), function(xx){gsub(char.to.rm, "", tolower(char.vec[xx]), fixed = TRUE)})
  return(cleaned.char.vec)

}


#' Code from prodlim library to match a row in a matrix
#'
#' Code also used in CRFutil as row.match
#'
#' The function will XXXX
#'
#' @param XX The XX
#' @return The function will XX
#'
#'
#' @export
get.row.idx <- function (x, table, nomatch = NA)
{
  if (class(table) == "matrix")
    table <- as.data.frame(table)
  if (is.null(dim(x)))
    x <- as.data.frame(matrix(x, nrow = 1))
  cx <- do.call("paste", c(x[, , drop = FALSE], sep = "\r"))
  ct <- do.call("paste", c(table[, , drop = FALSE], sep = "\r"))
  match(cx, ct, nomatch = nomatch)
}


#' Pick out groups of observations and form a new X matrix
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
pick.out.groups<-function(X.mat,all.lbls,grp.picks) {

  pick.out.rows<-NULL
  new.grp.lbls<-NULL
  for(i in 1:length(grp.picks))
  {
    grp.idxs<-which(as.numeric(all.lbls)==as.numeric(grp.picks[i]))
    pick.out.rows<-c(pick.out.rows,grp.idxs)
    #print(grp.idxs)
    new.grp.lbl<-rep(i,length(grp.idxs))
    new.grp.lbls<-c(new.grp.lbls,new.grp.lbl)
  }

  new.grp.lbls<-factor(new.grp.lbls)
  new.X.mat<-X.mat[pick.out.rows,]

  return(list(new.X.mat,new.grp.lbls))

}


#' Take the log of stuff (vectors, matrices, etc) in a list
#'
#' Handy for going from node/edge potentials in list form to node/edge energies (log potentials)
#'
#' The function will XXXX
#'
#' @param XX The XX
#' @return The function will XX
#'
#'
#' @export
log.list <- function(a.list.for.logging) {

  a.logged.list <- lapply(1:length(a.list.for.logging), function(xx){log(a.list.for.logging[[xx]])})

  if(!is.null(names(a.list.for.logging))) {
    names(a.logged.list) <- names(a.list.for.logging)
  }

  return(a.logged.list)

}


#' Exp a bunch of stuff (vectors, matrices, etc) in a list
#'
#' Handy for going from node/edge energies (log potentials) in list form to node/edge potentials
#'
#' The function will XXXX
#'
#' @param XX The XX
#' @return The function will XX
#'
#'
#' @export
exp.list <- function(a.list.for.eing) {

  an.exp.list <- lapply(1:length(an.exp.list), function(xx){log(an.exp.list[[xx]])})

  if(!is.null(names(a.list.for.eing))) {
    names(an.exp.list) <- names(a.list.for.eing)
  }

  return(an.exp.list)

}
