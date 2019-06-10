#' Collect together classes and then alphabetize within class subclass and attribute
#'
#' XXXX
#'
#' The function will XXXX
#'
#' @param class.order      Use this class order instead of alphabetizing class names if specified
#' @return The function will XX
#'
#'
#' @export
order.master.info<-function(a.master.info, class.order=NULL){

  master.category.mat  <- a.master.info$master.category.mat  # ASSUMES: no spaces and all lowercase!
  master.indicator.mat <- a.master.info$master.indicator.mat # NOTE: pxn NOT nxp
  master.note.mat      <- a.master.info$master.note.mat      # NOTE: pxn NOT nxp

  if(is.null(class.order)) {
    class.names <- sort(unique(master.category.mat[,1]))
  } else {
    class.names <- class.order
  }

  # Loop over the classes and alphabetize within (first) subclass and (second) attribute
  master.category.mat.alp.ord  <- NULL
  master.indicator.mat.alp.ord <- NULL
  master.note.mat.alp.ord      <- NULL
  count                        <- 0
  for(i in 1:length(class.names)) {

    class.idxs            <- which(master.category.mat[,1] == class.names[i])
    class.category.block  <- master.category.mat[class.idxs,]
    class.indicator.block <- master.indicator.mat[class.idxs,]
    class.note.block      <- master.note.mat[class.idxs,]

    # Container to hold the alphabetically ordered indices
    class.alp.ord.idxs <- NULL

    # The subclass names of a class
    subclass.names.of.class <- sort(unique(class.category.block[,2]))
    # The attribute names are the same for each subclass, so just grab them all now and "unique" them
    attrib.names.of.subclass <- sort(unique(class.category.block[,3]))

    for(j in 1:length(subclass.names.of.class)) {
      for(k in 1:length(attrib.names.of.subclass)) {
        #print(paste(class.names[i], subclass.names.of.class[j], attrib.names.of.subclass[k]))
        alp.categ.vec      <- c(class.names[i], subclass.names.of.class[j], attrib.names.of.subclass[k])
        alp.categ.vec.idx  <- get.row.idx(alp.categ.vec, table = class.category.block)
        class.alp.ord.idxs <- c(class.alp.ord.idxs, alp.categ.vec.idx)
        count <- count + 1
      } # attrib loop
    } # subclass loop

    #print(class.alp.ord.idxs)
    #print(sort(class.alp.ord.idxs) == (1:length(class.alp.ord.idxs)))
    #print("=====================")
    master.category.mat.alp.ord  <- rbind(master.category.mat.alp.ord,  class.category.block[class.alp.ord.idxs, ])
    master.indicator.mat.alp.ord <- rbind(master.indicator.mat.alp.ord, class.indicator.block[class.alp.ord.idxs, ])
    master.note.mat.alp.ord      <- rbind(master.note.mat.alp.ord,      class.note.block[class.alp.ord.idxs, ])

  } # class loop

  if(count != nrow(master.category.mat)) {
    stop("Something is wrong! Length of alphabetized categories is not the same as the number of original categories!")
  }

  master.info.alp.ord <- list(
    master.category.mat.alp.ord,
    master.indicator.mat.alp.ord,
    master.note.mat.alp.ord
  )
  names(master.info.alp.ord) <- c(
    "master.category.mat",
    "master.indicator.mat",
    "master.note.mat"
  )

  return(master.info.alp.ord)

}
