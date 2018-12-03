#' Read in the generalized, expandable data sheet
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
read.datasheet<-function(fpath, out.format="matrix", printQ = FALSE, snum.tmp){

  raw.dat     <- read.xlsx(fpath, 1, header = FALSE)
  sect.starts <- which(is.na(raw.dat[,1]) == FALSE)
  sect.stops  <- sect.starts - 1
  sect.stops  <- c(sect.stops[-1], nrow(raw.dat))
  sect.idxs   <- cbind(raw.dat[sect.starts,1], sect.starts, sect.stops)
  num.sects   <- nrow(sect.idxs)
  if(printQ==TRUE){
    print(sect.idxs)
  }

  sect.num  <- snum.tmp
  sect.info <- raw.dat[sect.starts[sect.num]:sect.stops[sect.num],]

  na.col.idxs <- which(is.na(sect.info[1,]) == TRUE) # col labels should be in the first row
  ao.col.idx  <- which(sect.info[1,] == "ADD-Other") # Fixed label for now. GENERALIZE FOR OTHER NAMES????

  na.row.idxs <- which(is.na(sect.info[,2]) == TRUE) # row labels should be in the SECOND column
  ao.row.idx  <- which(sect.info[,2] == "ADD-Other") # Fixed label for now. GENERALIZE FOR OTHER NAMES????

  sect.attr.names <- sect.info[1,]
  sect.attr.names <- sect.attr.names[-c(1,2,ao.col.idx, na.col.idxs)] # column names
  sect.attr.names <- as.character(as.matrix(sect.attr.names))         # fix for type conversion snafus
  sect.attr.names <- gsub(" ", "", sect.attr.names, fixed = TRUE)     # Remove any whitespace

  sect.cl.name <- sect.info[1,2]                                      # section class name
  sect.cl.name <- gsub(" ", "", sect.cl.name, fixed = TRUE)           # Remove any whitespace

  sect.scl.names <- sect.info[,2]
  sect.scl.names <- sect.scl.names[-c(1,ao.row.idx, na.row.idxs)]     # section subclass names
  sect.scl.names <- gsub(" ", "", sect.scl.names, fixed = TRUE)       # Remove any whitespace

  data.mat <- sect.info                                  # Actual data matrix for section
  data.mat <- data.mat[-c(1,ao.row.idx, na.row.idxs),]   # Drop the non data rows first
  data.mat <- data.mat[,-c(1,2,ao.col.idx, na.col.idxs)] # Drop the non data cols second

  # Not necessary, just for error checking.
  rownames(data.mat) <- sect.scl.names
  colnames(data.mat) <- sect.attr.names

  # Loop over all the emelents of the section and determine their category (0,1,note). Is there a faster way to do this??
  indic.mat <- array(0, dim(data.mat))
  note.mat  <- array("", dim(data.mat))

  for(i in 1:nrow(data.mat)) {
    for(j in 1:ncol(data.mat)) {

      elem <- as.character(data.mat[i,j])       # convert any data to a character string
      elem.orig <- elem                         # use this if elem is determined to be a note
      elem <- gsub(" ", "", elem, fixed = TRUE) # remove (possible stray) whitespace

      if(is.na(elem)){
        indic.mat[i,j] <- 0
      } else if(elem == "1") {
        indic.mat[i,j] <- 1
      } else if(elem == "0") {
        indic.mat[i,j] <- 0
      } else if(elem == "") {
        warning(paste0("Stray whitespace encountered in: ", sect.cl.name, " ", sect.scl.names[i], " ", sect.attr.names[j]), ". Recording indicator cell element as 0.\n")
        indic.mat[i,j] <- 0 # empty cell, but stray white space orignially
      } else {
        note.mat[i,j] <- elem.orig # if we get here, cell must be a note
        warning(paste0("Note encountered in: ", sect.cl.name, " ", sect.scl.names[i], " ", sect.attr.names[j]),".\nNOTE: ", elem.orig, "\nRecording indicator cell element as 1.\n")
        indic.mat[i,j] <- 1
      }
    }
  }

  rownames(indic.mat) <- sect.scl.names
  colnames(indic.mat) <- sect.attr.names
  rownames(note.mat)  <- sect.scl.names
  colnames(note.mat)  <- sect.attr.names

  if(out.format=="vector") {
    # Flatten out all cpmninations of subclass and attribute categories into a vector and then split into a two column matrix
    sect.categs.mat <- as.character(t(outer(sect.scl.names,sect.attr.names,FUN=paste,sep=",")))
    sect.categs.mat <- strsplit(sect.categs.mat,",")
    sect.categs.mat <- matrix(unlist(sect.categs.mat), ncol = 2, byrow = TRUE)

    # 3 Tack on the class name
    sect.categs.mat <- cbind(rep(sect.cl.name, nrow(sect.categs.mat)), sect.categs.mat)

    indic.vec <- as.numeric(t(indic.mat))
    note.vec <- as.numeric(t(note.mat))

    #tmpj <- cbind(sect.categs.mat, indic.vec, note.vec)
    #print(tmpj)
  }

  if(out.format=="vector"){
    out.sect.info.list <- list(
      sect.categs.mat,
      indic.vec,
      note.vec
    )
    names(out.sect.info.list) <- c("category.mat","indicator.vec","note.vec")
  } else {
    out.sect.info.list <- list(
      indic.mat,
      note.mat
    )
    names(out.sec.info.list) <- c("indicator.mat","note.mat")
  }

  return(out.sect.info.list)

}
