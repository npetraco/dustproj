#' Expts to chop conversion process into managable and de-buggable pieces
#'
#' Just extract a study's unique classes, subclasses and attributes. Make lower-case and extract extraeous characters
#'
#' The function will XXXX
#'
#' @param XX The XX
#' @return The function will XX
#'
#'
#' @export
parse.study.datasheet<-function(fpath.datasheet, study.name){

  dsheet.info <- NULL
  if(study.name =="Original"){
    dsheet.info <- read.dust.file(fpath = fpath.datasheet)
  } else if(study.name =="FloridaPhase1"){
    dsheet.info <- read.dust.file.EXPANDED(fpath = fpath.datasheet)
  } else if(study.name =="FloridaPhase2") {
    dsheet.info <- read.dust.file_ballantyne_study(fpath = fpath.datasheet)
  } else if(study.name =="WTCk") {
    dsheet.info <- read.dust.file_ballantyne_study(fpath = fpath.datasheet)
  } else if(study.name =="WTCq") {
    dsheet.info <- read.dust.file(fpath = fpath.datasheet)
  } else {
    stop("Choose Original, FloridaPhase1, FloridaPhase2, WTCk or WTCq for study name!")
  }

  # Classes, subclasses and attributes used in the loaded datasheet
  dsheet.categ.mat     <- dsheet.info[[2]]

  #Remove all capitalization and spaces
  for(i in 1:nrow(dsheet.categ.mat)){
    # Remove spaces, stray characters and set to lower case
    clean.drow <- sapply(1:ncol(dsheet.categ.mat), function(xx){gsub(" ", "", tolower(dsheet.categ.mat[i,xx]), fixed = TRUE)})
    clean.drow <- sapply(1:ncol(dsheet.categ.mat), function(xx){gsub("“", "", clean.drow[xx], fixed = TRUE)})
    dsheet.categ.mat[i,] <- clean.drow
  }
  colnames(dsheet.categ.mat) <- c("class", "subclass", "attribute")
  #print(dsheet.categ.mat)
  #print(dim(dsheet.categ.mat))

  studys.unique.classes       <- unique(dsheet.categ.mat[,1])
  studys.unique.subclasses    <- unique(dsheet.categ.mat[,2])
  studys.unique.attributes    <- unique(dsheet.categ.mat[,3])

  # Add in actual data from study datasheet 0/1
  dsheet.data <- dsheet.info[[1]]

  studys.unique.info <- list(
    dsheet.categ.mat,
    dsheet.data,
    studys.unique.classes,
    studys.unique.subclasses,
    studys.unique.attributes
  )


  names(studys.unique.info) <- c(
    "study.flattened.datasheet",
    "study.flattened.data",
    "studys.unique.classes",
    "studys.unique.subclasses",
    "studys.unique.attributes"
  )

  return(studys.unique.info)

}
