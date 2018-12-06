#' Convert old datasheet to the generalized format
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
convert.datasheet<-function(fpath.datasheet, study.name, fpath.convertion.table){

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
  dsheet.categ.mat  <- dsheet.info[[2]]
  unique.classes    <- unique(dsheet.categ.mat[,1])
  unique.subclasses <- unique(dsheet.categ.mat[,2])
  unique.attributes <- unique(dsheet.categ.mat[,3])

  # Study dependendent classes, subclasses and attributes from conversion table
  conversion.tabl <- read_excel(path = fpath.convertion.table, col_names=F)

  cl.scl.idx   <- which(conversion.tabl[2,]==study.name) # Column index for class and subclass names in a study
  study.cl.scl <- conversion.tabl[cl.scl.idx]
  #study.cl.scl <- unique(conversion.tabl[cl.scl.idx])
  #study.cl.scl <- study.cl.scl[-c(1,2,which(is.na(study.cl.scl) == TRUE)),]
  #study.cl.scl <- study.cl.scl[-c(1,2),] # Don't drop the NAs. It will mess up the indexing
  #print(study.cl.scl)

  ref.cl.scl.idx <- which(conversion.tabl[2,]=="NEW") # Column index for reference class and subclass names in a study
  ref.cl.scl <- conversion.tabl[ref.cl.scl.idx]
  #ref.cl.scl <- unique(conversion.tabl[ref.cl.scl.idx])
  #ref.cl.scl <- ref.cl.scl[-c(1,2,which(is.na(ref.cl.scl) == TRUE)),]
  #ref.cl.scl <- ref.cl.scl[-c(1,2),]
  #print(ref.cl.scl)
  print(cbind(ref.cl.scl,study.cl.scl))

  attribs.idx        <- which(conversion.tabl[1,]=="Attributes")           # Column index where the attribute infor begins
  study.attribs.idxs <- which(conversion.tabl[,attribs.idx] == study.name) # Row numbers for the study's recorded attribute names
  study.attribs      <- unique(as.character(as.matrix(conversion.tabl[study.attribs.idxs,(attribs.idx+1):ncol(conversion.tabl)])))
  study.attribs      <- study.attribs[-which(is.na(study.attribs) == TRUE)]
  #print(study.attribs)

  # Reference classes, subclasses and attributes from conversion table
  ref.attribs.idxs <- which(conversion.tabl[,attribs.idx] == "NEW") # Row numbers for the Reference attribute names
  ref.attribs      <- unique(as.character(as.matrix(conversion.tabl[ref.attribs.idxs,(attribs.idx+1):ncol(conversion.tabl)])))
  ref.attribs      <- ref.attribs[-which(is.na(ref.attribs) == TRUE)]
  #print(ref.attribs)

  # look up idx of unq in stdy stdy col. use that idx to look up ref name in stdy ref col
  for(i in l:length)
}
