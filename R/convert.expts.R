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
parse.study.datasheet.expt<-function(fpath.datasheet, study.name){

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

  #dsheet.categ.mat.new <- array(NA,dim(dsheet.categ.mat))
  studys.unique.classes       <- unique(dsheet.categ.mat[,1])
  studys.unique.subclasses    <- unique(dsheet.categ.mat[,2])
  studys.unique.attributes    <- unique(dsheet.categ.mat[,3])

  studys.unique.info <- list(
    dsheet.categ.mat,
    studys.unique.classes,
    studys.unique.subclasses,
    studys.unique.attributes
  )

  names(studys.unique.info) <- c(
    "study.flattened.datasheet",
    "studys.unique.classes",
    "studys.unique.subclasses",
    "studys.unique.attributes"
  )

  return(studys.unique.info)

}


#' Expts to chop conversion process into managable and de-buggable pieces
#'
#' Parse conversion sheet according to the study to be converted
#'
#' The function will XXXX
#'
#' @param XX The XX
#' @return The function will XX
#'
#'
#' @export
parse.conversion.table.expt<-function(fpath.convertion.table, study.name){

  # Study dependendent classes, subclasses and attributes from conversion table
  #conversion.tabl <- read_excel(path = fpath.convertion.table, col_names=F)
  conversion.tabl <- read.xlsx(fpath.convertion.table, 1, header = FALSE)

  cl.scl.idx   <- which(conversion.tabl[2,]==study.name) # Column index for class and subclass names in a study
  study.cl.scl <- conversion.tabl[,cl.scl.idx]
  study.cl.scl <- as.character(study.cl.scl)
  study.cl.scl <- clean.chars(study.cl.scl, " ")         # Set to lowercase and get rid of spaces
  study.cl.scl <- clean.chars(study.cl.scl, "“")         # Get rid of any wierd continuation quotes too
  #print(study.cl.scl)

  ref.cl.scl.idx <- which(conversion.tabl[2,]=="NEW") # Column index for NEW reference class and subclass names in a study
  ref.cl.scl     <- conversion.tabl[,ref.cl.scl.idx]
  ref.cl.scl     <- as.character(ref.cl.scl)
  ref.cl.scl     <- clean.chars(ref.cl.scl, " ")      # Set to lowercase and get rid of spaces
  #print(ref.cl.scl)

  cl.namechgs.col.idx <- which(conversion.tabl[1,]=="Class Changes") # Column index for stipulated class name changes
  cl.namechgs         <- conversion.tabl[,cl.namechgs.col.idx]       # Drop the first row because of the format of conversion.tbl
  cl.namechgs         <- as.character(cl.namechgs)
  cl.namechgs         <- clean.chars(cl.namechgs, " ")               # Set to lowercase and get rid of spaces
  #print(cl.namechgs)

  scl.namechgs.col.idx <- which(conversion.tabl[1,]=="Subclass Changes") # Column index for stipulated subclass name changes
  scl.namechgs         <- conversion.tabl[,scl.namechgs.col.idx]         # Drop the first row because of the format of conversion.tbl
  scl.namechgs         <- as.character(scl.namechgs)
  scl.namechgs         <- clean.chars(scl.namechgs, " ")                 # Set to lowercase and get rid of spaces
  #print(scl.namechgs)

  study2ref.cl.scl <- cbind(ref.cl.scl, study.cl.scl, cl.namechgs, scl.namechgs) # Class/Subclass name conversions collected together
  #print(study2ref.cl.scl)

  # Study Reference attributes from conversion table
  attribs.idx        <- which(conversion.tabl[1,]=="Attributes")           # Column index where the attribute information begins
  study.attribs.idxs <- which(conversion.tabl[,attribs.idx] == study.name) # Row numbers for the study's recorded attribute names
  study.attribs      <- as.character(as.matrix(conversion.tabl[study.attribs.idxs,(attribs.idx+1):ncol(conversion.tabl)]))
  study.attribs      <- clean.chars(study.attribs, " ")                    # Set to lowercase and get rid of spaces
  #print(study.attribs.idxs)
  #
  study.class.of.attribs.idxs <- which(conversion.tabl[,attribs.idx] == "NEW")
  study.class.of.attribs      <- as.character(conversion.tabl[study.class.of.attribs.idxs,2]) # Class names for the attributes
  study.class.of.attribs      <- clean.chars(study.class.of.attribs, " ")
  rep.leng <- length((attribs.idx+1):ncol(conversion.tabl))
  study.attribute.class.col <- as.vector(sapply(1:length(study.class.of.attribs), function(xx){rep(study.class.of.attribs[xx],rep.leng)}))
  study.attribs <- cbind(study.attribute.class.col, study.attribs)
  colnames(study.attribs) <- c("study.class.of.attribute","study.attribute")
  #
  #print(study.class.of.attribs)
  #print(study.name)

  # NEW Reference attributes from conversion table
  ref.attribs.idxs <- which(conversion.tabl[,attribs.idx] == "NEW") # Row numbers for the Reference attribute names
  ref.attribs      <- as.character(as.matrix(conversion.tabl[ref.attribs.idxs,(attribs.idx+1):ncol(conversion.tabl)]))
  ref.attribs      <- clean.chars(ref.attribs, " ")

  #ref.class.of.attribs.idxs <- which(conversion.tabl[,attribs.idx] == "NEW")
  ref.class.of.attribs.idxs <- ref.attribs.idxs
  ref.class.of.attribs      <- as.character(conversion.tabl[ref.class.of.attribs.idxs,2]) # Class names for the attributes
  ref.class.of.attribs      <- clean.chars(ref.class.of.attribs, " ")
  #rep.leng <- length((attribs.idx+1):ncol(conversion.tabl))
  #ref.class.of.attribs[which(ref.class.of.attribs == "feathers")] <- "various" # A workaround so I don't have to change conversion sheet.
  ref.attribute.class.col <- as.vector(sapply(1:length(ref.class.of.attribs), function(xx){rep(ref.class.of.attribs[xx], rep.leng)}))
  ref.attribs <- cbind(ref.attribute.class.col, ref.attribs)
  colnames(ref.attribs) <- c("ref.class.of.attribute","ref.attribute")

  #print(ref.attribs)

  if(nrow(ref.attribs) != nrow(study.attribs)){
    stop("Length of NEW attributs not the same as length of specified study attributes. Check for stray spaces or other problems.")
  }
  study2ref.attribs <- cbind(ref.attribs,study.attribs)
  #print(study2ref.attribs)
  #print(study.name)

  study2ref.conversion.info <- list(
    study2ref.cl.scl,
    study2ref.attribs
  )

  names(study2ref.conversion.info) <- c(
    "cl.scl.conversions",
    "attribs.conversions"
  )

  return(study2ref.conversion.info)

}


#' Parse conversion sheet according to the study to be converted
#'
#' The function will XXXX
#'
#' @param XX The XX
#' @return The function will XX
#'
#'
#' @export
parse.conversion.table.expt2<-function(fpath.convertion.table, study.name){

  # Study dependendent classes, subclasses and attributes from conversion table
  #conversion.tabl <- read_excel(path = fpath.convertion.table, col_names=F)
  conversion.tabl <- read.xlsx(fpath.convertion.table, 1, header = FALSE)

  cl.scl.idx   <- which(conversion.tabl[2,]==study.name) # Column index for class and subclass names in a study
  study.cl.scl <- conversion.tabl[,cl.scl.idx]
  study.cl.scl <- as.character(study.cl.scl)
  study.cl.scl <- clean.chars(study.cl.scl, " ")         # Set to lowercase and get rid of spaces
  study.cl.scl <- clean.chars(study.cl.scl, "“")         # Get rid of any wierd continuation quotes too
  #print(study.cl.scl)

  ref.cl.scl.idx <- which(conversion.tabl[2,]=="NEW") # Column index for NEW reference class and subclass names in a study
  ref.cl.scl     <- conversion.tabl[,ref.cl.scl.idx]
  ref.cl.scl     <- as.character(ref.cl.scl)
  ref.cl.scl     <- clean.chars(ref.cl.scl, " ")      # Set to lowercase and get rid of spaces
  #print(ref.cl.scl)

  cl.namechgs.col.idx <- which(conversion.tabl[1,]=="Class Changes") # Column index for stipulated class name changes
  cl.namechgs         <- conversion.tabl[,cl.namechgs.col.idx]       # Drop the first row because of the format of conversion.tbl
  cl.namechgs         <- as.character(cl.namechgs)
  cl.namechgs         <- clean.chars(cl.namechgs, " ")               # Set to lowercase and get rid of spaces
  #print(cl.namechgs)

  scl.namechgs.col.idx <- which(conversion.tabl[1,]=="Subclass Changes") # Column index for stipulated subclass name changes
  scl.namechgs         <- conversion.tabl[,scl.namechgs.col.idx]         # Drop the first row because of the format of conversion.tbl
  scl.namechgs         <- as.character(scl.namechgs)
  scl.namechgs         <- clean.chars(scl.namechgs, " ")                 # Set to lowercase and get rid of spaces
  #print(scl.namechgs)

  study2ref.cl.scl <- cbind(ref.cl.scl, study.cl.scl, cl.namechgs, scl.namechgs) # Class/Subclass name conversions collected together
  #print(study2ref.cl.scl)

  # Study Reference attributes from conversion table
  attribs.idx        <- which(conversion.tabl[1,]=="Attributes")           # Column index where the attribute information begins
  study.attribs.idxs <- which(conversion.tabl[,attribs.idx] == study.name) # Row numbers for the study's recorded attribute names
  #Lets do this as a loop instead.
  print(study.attribs.idxs)

  # study.attribs      <- as.character(as.matrix(conversion.tabl[study.attribs.idxs,(attribs.idx+1):ncol(conversion.tabl)]))
  # study.attribs      <- clean.chars(study.attribs, " ")                    # Set to lowercase and get rid of spaces
  # #print(study.attribs.idxs)
  # #
  # study.class.of.attribs.idxs <- which(conversion.tabl[,attribs.idx] == "NEW")
  # study.class.of.attribs      <- as.character(conversion.tabl[study.class.of.attribs.idxs,2]) # Class names for the attributes
  # study.class.of.attribs      <- clean.chars(study.class.of.attribs, " ")
  # rep.leng <- length((attribs.idx+1):ncol(conversion.tabl))
  # study.attribute.class.col <- as.vector(sapply(1:length(study.class.of.attribs), function(xx){rep(study.class.of.attribs[xx],rep.leng)}))
  # study.attribs <- cbind(study.attribute.class.col, study.attribs)
  # colnames(study.attribs) <- c("study.class.of.attribute","study.attribute")
  #
  #print(study.class.of.attribs)
  #print(study.name)

  # # NEW Reference attributes from conversion table
  # ref.attribs.idxs <- which(conversion.tabl[,attribs.idx] == "NEW") # Row numbers for the Reference attribute names
  # ref.attribs      <- as.character(as.matrix(conversion.tabl[ref.attribs.idxs,(attribs.idx+1):ncol(conversion.tabl)]))
  # ref.attribs      <- clean.chars(ref.attribs, " ")
  #
  # #ref.class.of.attribs.idxs <- which(conversion.tabl[,attribs.idx] == "NEW")
  # ref.class.of.attribs.idxs <- ref.attribs.idxs
  # ref.class.of.attribs      <- as.character(conversion.tabl[ref.class.of.attribs.idxs,2]) # Class names for the attributes
  # ref.class.of.attribs      <- clean.chars(ref.class.of.attribs, " ")
  # #rep.leng <- length((attribs.idx+1):ncol(conversion.tabl))
  # #ref.class.of.attribs[which(ref.class.of.attribs == "feathers")] <- "various" # A workaround so I don't have to change conversion sheet.
  # ref.attribute.class.col <- as.vector(sapply(1:length(ref.class.of.attribs), function(xx){rep(ref.class.of.attribs[xx], rep.leng)}))
  # ref.attribs <- cbind(ref.attribute.class.col, ref.attribs)
  # colnames(ref.attribs) <- c("ref.class.of.attribute","ref.attribute")
  #
  # #print(ref.attribs)
  #
  # if(nrow(ref.attribs) != nrow(study.attribs)){
  #   stop("Length of NEW attributs not the same as length of specified study attributes. Check for stray spaces or other problems.")
  # }
  # study2ref.attribs <- cbind(ref.attribs,study.attribs)
  # #print(study2ref.attribs)
  # #print(study.name)
  #
  # study2ref.conversion.info <- list(
  #   study2ref.cl.scl,
  #   study2ref.attribs
  # )
  #
  # names(study2ref.conversion.info) <- c(
  #   "cl.scl.conversions",
  #   "attribs.conversions"
  # )
  #
  # return(study2ref.conversion.info)

}


#' Expts to chop conversion process into managable and de-buggable pieces
#'
#' Bring together conversion sub-routenes
#'
#' The function will XXXX
#'
#' @param XX The XX
#' @return The function will XX
#'
#'
#' @export
tog.expt<-function(fpath.datasheet, study.name, fpath.convertion.table, print.level=0){

  study.unique.info         <- parse.study.datasheet.expt(fpath.datasheet, study.name = study.name)
  study2ref.conversion.info <- parse.conversion.table.expt(fpath.convertion.table, study.name = study.name)

  # Info extracted from a study data sheet which is to be converted
  # Should be lowercase and clean of extraneous charaters at this point
  study.flattened.datasheet <- study.info$study.flattened.datasheet
  study.unique.classes      <- study.info$studys.unique.classes
  study.unique.subclasses   <- study.info$studys.unique.subclasses
  study.unique.attributes   <- study.info$studys.unique.attributes

  # Conversion keys from STUDY classes, subclasses and attributes to REFERENCE classes, subclasses and attributes
  cl.scl.conversions        <- study2ref.conversion.info$cl.scl.conversions
  attribs.conversions       <- study2ref.conversion.info$attribs.conversions

  # This is temporary. We will embed it in an instance of the reference datasheet
  tmp.converted.datasheet   <- array(NA,dim(study.flattened.datasheet)) # This was: dsheet.categ.mat.new <- array(NA,dim(dsheet.categ.mat))


}
