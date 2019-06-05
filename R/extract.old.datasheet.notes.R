#' Extract any text in cells of old datasheets
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
extract.old.datasheet.notes<-function(fpath.datasheet, study.name, print.level=0){

  #dsheet <- read_excel(path = fpath.datasheet, col_names=F)
  dsheet <- read.xlsx(fpath.datasheet,1,header=FALSE)

  sect1.head.idx <- which(tolower(gsub(" ", "", dsheet[,1], fixed = TRUE)) == "humanhairnatural")
  sect2.head.idx <- which(tolower(gsub(" ", "", dsheet[,1], fixed = TRUE)) == "humanhairtreated")
  sect3.head.idx <- which(tolower(gsub(" ", "", dsheet[,1], fixed = TRUE)) == "syntheticfibers")
  sect4.head.idx <- which(tolower(gsub(" ", "", dsheet[,1], fixed = TRUE)) == "animalhair")
  sect5.head.idx <- which(tolower(gsub(" ", "", dsheet[,1], fixed = TRUE)) == "naturalfibers")
  sect6.head.idx <- which(tolower(gsub(" ", "", dsheet[,1], fixed = TRUE)) == "mineralfibers")
  sect7.head.idx <- which(tolower(gsub(" ", "", dsheet[,1], fixed = TRUE)) == "glass/mineralgrains")

  if(study.name =="Original"){
    sect8.head.idx <- which(tolower(gsub(" ", "", dsheet[,1], fixed = TRUE)) == "feathers()") # Starts the Various category
  } else if((study.name =="FloridaPhase1") | (study.name =="FloridaPhase2") | (study.name =="WTCk") | (study.name =="WTCq")){
    sect8.head.idx <- which(tolower(gsub(" ", "", dsheet[,1], fixed = TRUE)) == "feathers") # Starts the Various category
  } else {
    stop("Choose Original, FloridaPhase1, FloridaPhase2, WTCk or WTCq for study name!")
  }

  sect.header.idxs <- c(
    sect1.head.idx,
    sect2.head.idx,
    sect3.head.idx,
    sect4.head.idx,
    sect5.head.idx,
    sect6.head.idx,
    sect7.head.idx,
    sect8.head.idx
  )
  #print(sect.header.idxs)

  # Loop over the first 7 classes and extract the class cells:
  dsheet.notes <- NULL
  for(i in 1:7) {
    sect.header.row <- dsheet[sect.header.idxs[i],]
    #print(sect.header.row)
    # Get all the columns of the section.
    # ** NOTE ** Assumes no stray characters to the right of the last subclass names
    # ** NOTE ** Also keeps Other() subclass name. Later however it should be dropped in convert.datasheet()
    sect.ncol <- max(which( !is.na(sect.header.row) == TRUE))

    # If there are NA cols before the last col, drop them:
    if(sect.ncol > 1){
      if(print.level >=2){
        print(paste("Num cols of class:", sect.ncol))
      }
      na.col.idxs   <- which(is.na(sect.header.row[1:sect.ncol]) == TRUE)
      sect.col.idxs <- 1:sect.ncol
      if(length(na.col.idxs) > 0) {
        #print("Here!")
        sect.col.idxs <- sect.col.idxs[-na.col.idxs]
      }
    } else {
      # This is the exception for the Mineral Fiber class which only ever has one column and may or may not have a subclass name for it.
      #print(paste("******** Mineral Fibers ?? Num cols of class: ******:", sect.ncol))
      #na.col.idxs   <- which(is.na(sect.header.row[1:sect.ncol]) == TRUE)
      #print(paste("#NA cols:", na.col.idxs))
      #print(na.col.idxs)
      sect.col.idxs <- c(1,2)
    }

    sect.mat  <- as.matrix(dsheet[(sect.header.idxs[i]+1):(sect.header.idxs[i+1]-1), sect.col.idxs[-1]])
    colnames(sect.mat) <- sect.header.row[sect.col.idxs[-1]]

    if(print.level >= 1) {
      print(paste("Sec Name: ", sect.header.row[1]))
      print(paste("Sec Start:", sect.header.idxs[i]))
      print(paste("Row Start:", sect.header.idxs[i]+1))
      print(paste("Row Stop: ", sect.header.idxs[i+1]-1))
    }
    if(print.level >= 2) {
      print(sect.mat)
    }
    if(print.level >= 1) {
      print("======================================")
    }

    # Should column stact the cells:
    dsheet.notes <- c(dsheet.notes, as.character(sect.mat))
  }

  # Now do class 8. It does not have a header name, that's why we do it separately
  # First make sure we don't run off the bottom od the datasheet due to stray space:
  if(is.na(dsheet[nrow(dsheet),1])){
    stop("Problem at bottom of Various class on this datasheet. Stray space?? Check!")
  }

  sect.mat  <- as.matrix(dsheet[(sect.header.idxs[i]+1):(nrow(dsheet)), 2])
  colnames(sect.mat) <- "ColorNR"
  if(print.level >= 1) {
    print(paste("Sec Name: ", "Various"))
    print(paste("Sec Start:", sect.header.idxs[i]))
    print(paste("Row Start:", sect.header.idxs[i]))
    print(paste("Row Stop: ", nrow(dsheet)))
  }
  if(print.level >= 2) {
    print(sect.mat)
  }
  if(print.level >= 1) {
    print("======================================")
  }

  # Should column stack the cells:
  dsheet.notes <- c(dsheet.notes, as.character(sect.mat))

  return(dsheet.notes)
}
