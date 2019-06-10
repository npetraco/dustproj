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
read.datasheet<-function(fpath, out.format="matrix", add.other.rm=TRUE, printQ=FALSE){

  raw.dat     <- read.xlsx(fpath, 1, header = FALSE)
  sect.starts <- which(is.na(raw.dat[,1]) == FALSE)
  sect.stops  <- sect.starts - 1
  sect.stops  <- c(sect.stops[-1], nrow(raw.dat))
  sect.idxs   <- cbind(raw.dat[sect.starts,1], sect.starts, sect.stops)
  num.sects   <- nrow(sect.idxs)
  if(printQ==TRUE){
    print(sect.idxs)
  }

  if(out.format=="matrix"){
    proc.dat.sht <- rep(list(NULL), num.sects)
  } else if(out.format=="vector"){
    proc.indic.vec <-NULL
    proc.note.vec  <-NULL
    proc.categ.mat <-NULL
  } else {
    stop("Choose matrix or vector for out.format!")
  }

  for(i in 1:num.sects){
    sect.num      <- i
    sect.info     <- raw.dat[sect.starts[sect.num]:sect.stops[sect.num],]
    sect.out.info <- process.data.sheet.section(sect.info, out.format=out.format)

    if(out.format=="vector") {
      proc.indic.vec <-     c(proc.indic.vec, sect.out.info$indicator.vec)
      proc.note.vec  <-     c(proc.note.vec,  sect.out.info$note.vec)
      proc.categ.mat <- rbind(proc.categ.mat, sect.out.info$category.mat)
    } else {
      proc.dat.sht[[i]] <- list(sect.out.info$indicator.mat, sect.out.info$note.mat)
    }
  }

  if(out.format=="vector") {
    colnames(proc.categ.mat) <- c("Class","Subclass","Attribute")
    all.proc.info <- list(
      proc.indic.vec,
      proc.note.vec,
      proc.categ.mat
    )
    names(all.proc.info) <- c("indicator.vec","note.vec","category.mat")
  } else { # for matrix partitionrd output:
    all.proc.info <- proc.dat.sht
    names(all.proc.info) <- paste0("Section",1:num.sects)
  }

  if(add.other.rm==TRUE) {
    all.proc.info <- remove.all.add.other(all.proc.info, out.format = out.format)
  }

  return(all.proc.info)
}

#' Read in a section for the generalized, expandable data sheet
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
process.data.sheet.section<-function(sect.info.dat, out.format="matrix"){

  na.col.idxs <- which(is.na(sect.info.dat[1,]) == TRUE) # col labels should be in the first row
  ao.col.idx  <- which(sect.info.dat[1,] == "ADD-Other") # Fixed label for now. GENERALIZE FOR OTHER NAMES????

  na.row.idxs <- which(is.na(sect.info.dat[,2]) == TRUE) # row labels should be in the SECOND column
  ao.row.idx  <- which(sect.info.dat[,2] == "ADD-Other") # Fixed label for now. GENERALIZE FOR OTHER NAMES????

  sect.attr.names <- sect.info.dat[1,]
  sect.attr.names <- sect.attr.names[-c(1,2,ao.col.idx, na.col.idxs)] # column names
  sect.attr.names <- as.character(as.matrix(sect.attr.names))         # fix for type conversion snafus
  sect.attr.names <- gsub(" ", "", sect.attr.names, fixed = TRUE)     # Remove any whitespace

  sect.cl.name <- sect.info.dat[1,2]                                  # section class name
  sect.cl.name <- gsub(" ", "", sect.cl.name, fixed = TRUE)           # Remove any whitespace

  sect.scl.names <- sect.info.dat[,2]
  sect.scl.names <- sect.scl.names[-c(1,ao.row.idx, na.row.idxs)]     # section subclass names
  sect.scl.names <- gsub(" ", "", sect.scl.names, fixed = TRUE)       # Remove any whitespace

  sect.data.mat <- sect.info.dat                                      # Actual data matrix for section
  sect.data.mat <- sect.data.mat[-c(1,ao.row.idx, na.row.idxs),]      # Drop the non data rows first
  sect.data.mat <- sect.data.mat[,-c(1,2,ao.col.idx, na.col.idxs)]    # Drop the non data cols second
  # For one column sections, we get a factor or a vector. Make it into a one column matirx:
  if(is.null(dim(sect.data.mat))) {
    sect.data.mat <- as.matrix(sect.data.mat, c(length(sect.data.mat),1))
  }

  # Not necessary, just for error checking.
  rownames(sect.data.mat) <- sect.scl.names
  colnames(sect.data.mat) <- sect.attr.names

  # Loop over all the emelents of the section and determine their category (0,1,note). Is there a faster way to do this??
  indic.mat <- array(0, dim(sect.data.mat))
  note.mat  <- array("", dim(sect.data.mat))

  for(i in 1:nrow(sect.data.mat)) {
    for(j in 1:ncol(sect.data.mat)) {

      elem <- as.character(sect.data.mat[i,j])  # convert any data to a character string
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
        indic.mat[i,j] <- 0 # empty cell, but stray white space orignially. This can happen when we write an excel sheet fro R too.
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
    note.vec <- as.character(t(note.mat))

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
    names(out.sect.info.list) <- c("indicator.mat","note.mat")
  }

  return(out.sect.info.list)

}


#' Remove add others in expandable data sheet
#'
#' Least traumatic to do it this way....
#'
#' The function will XXXX
#'
#' @param XX The XX
#' @return The function will XX
#'
#'
#' @export
remove.all.add.other<-function(dsheet.info, out.format="matrix"){

  dsheet.info.new <- dsheet.info

  if(out.format=="matrix"){
    for(i in 1:length(dsheet.info)) {
      # Assumes add-others are at the last row and column, which they should be
      drp.idxs <- dim(dsheet.info[[i]][[1]])

      dsheet.info.new[[i]][[1]] <- as.matrix(dsheet.info.new[[i]][[1]][-drp.idxs[1], -drp.idxs[2]])
      dsheet.info.new[[i]][[2]] <- as.matrix(dsheet.info.new[[i]][[2]][-drp.idxs[1], -drp.idxs[2]])

      colnames(dsheet.info.new[[i]][[1]]) <- colnames(dsheet.info[[i]][[1]])[-drp.idxs[2]]
      rownames(dsheet.info.new[[i]][[1]]) <- rownames(dsheet.info[[i]][[1]])[-drp.idxs[1]]
      colnames(dsheet.info.new[[i]][[2]]) <- colnames(dsheet.info[[i]][[1]])[-drp.idxs[2]]
      rownames(dsheet.info.new[[i]][[2]]) <- rownames(dsheet.info[[i]][[1]])[-drp.idxs[1]]

    }
  } else if(out.format=="vector"){

    drp.idxs <- unique(
      c(
        which(tolower(dsheet.info$category.mat[,2]) == "add-other"),
        which(tolower(dsheet.info$category.mat[,3]) == "add-other")
      )
    )

    dsheet.info.new$category.mat  <- dsheet.info.new$category.mat[-drp.idxs,]
    dsheet.info.new$indicator.vec <- dsheet.info.new$indicator.vec[-drp.idxs]
    dsheet.info.new$note.vec      <- dsheet.info.new$note.vec[-drp.idxs]

  } else {
    stop("Choose matrix or vector for out.format!")
  }

  return(dsheet.info.new)

}
