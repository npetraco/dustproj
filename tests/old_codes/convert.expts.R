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

  # Read study dependendent classes, subclasses and attributes from conversion table
  conversion.tabl <- read.xlsx(fpath.convertion.table, 1, header = FALSE)

  # First parse the class and subclass information.
  # Study class/sublass info:
  cl.scl.idx   <- which(conversion.tabl[2,]==study.name) # Column index for class and subclass names in a study
  study.cl.scl <- conversion.tabl[,cl.scl.idx]
  study.cl.scl <- as.character(study.cl.scl)
  study.cl.scl <- clean.chars(study.cl.scl, " ")         # Set to lowercase and get rid of spaces
  study.cl.scl <- clean.chars(study.cl.scl, "“")         # Get rid of any wierd continuation quotes too
  #print(study.cl.scl)

  # Reference class/subclass info:
  ref.cl.scl.idx <- which(conversion.tabl[2,]=="NEW") # Column index for NEW reference class and subclass names in a study
  ref.cl.scl     <- conversion.tabl[,ref.cl.scl.idx]
  ref.cl.scl     <- as.character(ref.cl.scl)
  ref.cl.scl     <- clean.chars(ref.cl.scl, " ")      # Set to lowercase and get rid of spaces
  #print(ref.cl.scl)

  # Now process columns containing class and subclass name changes for the studies
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

  # Now get started parsing attributes:
  attribs.idx <- which(conversion.tabl[1,]=="Attributes")           # Column index where the attribute information begins

  # Study attributes from conversion table
  study.attribs.idxs <- which(conversion.tabl[,attribs.idx] == study.name) # Row numbers for the study's recorded attribute names
  study.attribs      <- NULL
  study.name.idx     <- which(conversion.tabl[2,] == study.name)

  for(i in 1:length(study.attribs.idxs)) {
    # Grab the attributes for A CLASS:
    study.attribs.tmp <- as.matrix(conversion.tabl[study.attribs.idxs[i], (attribs.idx+1):ncol(conversion.tabl)])
    study.attribs.tmp <- clean.chars(study.attribs.tmp, " ")

    # Grab the class name of the attributes for the given study:
    study.class.of.attribs.idxs <- which(conversion.tabl[,attribs.idx] == "NEW")                 # The class of the attributes will be a name in this row
    attrib.class <- as.character(conversion.tabl[study.class.of.attribs.idxs[i],study.name.idx]) # The class of the attributes for the given study is specifically this
    attrib.class <- clean.chars(attrib.class, " ")
    #print(attrib.class)

    # Tack the class name and its corresponding attributes together:
    study.attribs.tmp <- cbind(rep(attrib.class, length(study.attribs.tmp)), study.attribs.tmp)
    study.attribs     <- rbind(study.attribs, study.attribs.tmp)
  }
  #print(study.attribs)

  # Reference attributes from conversion table
  ref.attribs.idxs <- which(conversion.tabl[,attribs.idx] == "NEW") # Row numbers for the Reference attribute names
  ref.attribs      <- NULL
  ref.name.idx     <- which(conversion.tabl[2,] == "NEW")

  for(i in 1:length(ref.attribs.idxs)) {
    # Grab the attributes for A CLASS:
    ref.attribs.tmp <- as.matrix(conversion.tabl[ref.attribs.idxs[i], (attribs.idx+1):ncol(conversion.tabl)])
    ref.attribs.tmp <- clean.chars(ref.attribs.tmp, " ")
    #print(ref.attribs.tmp)

    # Grab the class name of the attributes for the reference:
    ref.class.of.attribs.idxs <- which(conversion.tabl[,attribs.idx] == "NEW")                # The class of the attributes will be a name in this row
    attrib.class <- as.character(conversion.tabl[ref.class.of.attribs.idxs[i], ref.name.idx]) # The class of the attributes for the reference is specifically this
    attrib.class <- clean.chars(attrib.class, " ")
    #print(attrib.class)

    # Tack the class name and its corresponding attributes together:
    ref.attribs.tmp <- cbind(rep(attrib.class, length(ref.attribs.tmp)), ref.attribs.tmp)
    ref.attribs     <- rbind(ref.attribs, ref.attribs.tmp)
  }
  #print(ref.attribs)

  if(nrow(ref.attribs) != nrow(study.attribs)){
    stop("Length of NEW (reference) attributs not the same as length of specified study attributes.
         Check for stray spaces or other problems.")
  }
  study2ref.attribs <- cbind(ref.attribs, study.attribs)
  colnames(study2ref.attribs) <- c("ref.class", "ref.attrib", "study.class", "study.attrib")
  # #print(study2ref.attribs)
  # #print(study.name)

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
parse.conversion.table.expt3<-function(fpath.convertion.table, study.name){

  # Read study dependendent classes, subclasses and attributes from conversion table
  conversion.tabl <- read.xlsx(fpath.convertion.table, 1, header = FALSE)

  # First parse the CLASS information
  class.name.row.idxs <- which(!is.na(as.matrix(conversion.tabl[,1])) == TRUE)[-1] # First column labels SectionX. That is where the class names are, just drop the row header.
  section.labels      <- as.character(conversion.tabl[class.name.row.idxs, 1])
  section.labels      <- clean.chars(section.labels, " ")

  # Study class info:
  study.col.idx <- which(conversion.tabl[2,]==study.name) # Column index for class and subclass names in a study. For now we just want to grab the class names
  study.classes <- as.character(conversion.tabl[class.name.row.idxs, study.col.idx])
  study.classes <- clean.chars(study.classes, " ")

  # Reference class info:
  ref.col.idx <- which(conversion.tabl[2,]=="NEW") # Column index for NEW reference class and subclass names in a study
  ref.classes <- as.character(conversion.tabl[class.name.row.idxs, ref.col.idx])
  ref.classes <- clean.chars(ref.classes, " ")
  study2ref.class.info <- cbind(section.labels, ref.classes, study.classes)
  colnames(study2ref.class.info) <- c("section.labels", "ref.classes", "study.classes")
  #print(study2ref.class.info)

  # Next parse the SUBCLASS info:

  # Study and reference subclass info:
  study.subclasses    <- NULL
  ref.subclasses      <- NULL
  subclass.start.idxs <- class.name.row.idxs+1
  subclass.stop.idxs  <- c((class.name.row.idxs-1)[-1], nrow(conversion.tabl))

  # Loop over the class blocks:
  for(i in 1:length(class.name.row.idxs)) {
    # Grab the sublasses for A GIVEN CLASS:
    subclass.idxs.of.class <- subclass.start.idxs[i]:subclass.stop.idxs[i]

    study.subclasses.tmp <- as.matrix(conversion.tabl[subclass.idxs.of.class, study.col.idx])
    study.subclasses.tmp <- clean.chars(study.subclasses.tmp, " ")
    study.subclasses.tmp <- clean.chars(study.subclasses.tmp, "“")

    ref.subclasses.tmp <- as.matrix(conversion.tabl[subclass.idxs.of.class, ref.col.idx])
    ref.subclasses.tmp <- clean.chars(ref.subclasses.tmp, " ")
    ref.subclasses.tmp <- clean.chars(ref.subclasses.tmp, "“")

    # Tack on the class name to each subclass that corresponds to it:
    study.subclasses.tmp <- cbind(rep(study.classes[i], length(study.subclasses.tmp)), study.subclasses.tmp)
    study.subclasses    <- rbind(study.subclasses, study.subclasses.tmp)

    ref.subclasses.tmp <- cbind(rep(ref.classes[i], length(ref.subclasses.tmp)), ref.subclasses.tmp)
    ref.subclasses     <- rbind(ref.subclasses, ref.subclasses.tmp)

  }
  study2ref.subclass.info           <- cbind(ref.subclasses, study.subclasses)
  colnames(study2ref.subclass.info) <- c("ref.class", "ref.subclass", "study.class", "study.subclass")
  #print(study2ref.subclass.info)

  # Now get started parsing attributes:
  attribs.idx <- which(conversion.tabl[1,]=="Attributes")           # Column index where the attribute information begins

  # Study attributes from conversion table
  study.attribs.idxs <- which(conversion.tabl[,attribs.idx] == study.name) # Row numbers for the study's recorded attribute names
  study.attribs      <- NULL
  study.name.idx     <- which(conversion.tabl[2,] == study.name)

  for(i in 1:length(study.attribs.idxs)) {
    # Grab the attributes for A CLASS:
    study.attribs.tmp <- as.matrix(conversion.tabl[study.attribs.idxs[i], (attribs.idx+1):ncol(conversion.tabl)])
    study.attribs.tmp <- clean.chars(study.attribs.tmp, " ")

    # Grab the class name of the attributes for the given study:
    study.class.of.attribs.idxs <- which(conversion.tabl[,attribs.idx] == "NEW")                 # The class of the attributes will be a name in this row
    attrib.class <- as.character(conversion.tabl[study.class.of.attribs.idxs[i],study.name.idx]) # The class of the attributes for the given study is specifically this
    attrib.class <- clean.chars(attrib.class, " ")
    #print(attrib.class)

    # Tack the class name and its corresponding attributes together:
    study.attribs.tmp <- cbind(rep(attrib.class, length(study.attribs.tmp)), study.attribs.tmp)
    study.attribs     <- rbind(study.attribs, study.attribs.tmp)
  }
  #print(study.attribs)

  # Reference attributes from conversion table
  ref.attribs.idxs <- which(conversion.tabl[,attribs.idx] == "NEW") # Row numbers for the Reference attribute names
  ref.attribs      <- NULL
  ref.name.idx     <- which(conversion.tabl[2,] == "NEW")

  for(i in 1:length(ref.attribs.idxs)) {
    # Grab the attributes for A CLASS:
    ref.attribs.tmp <- as.matrix(conversion.tabl[ref.attribs.idxs[i], (attribs.idx+1):ncol(conversion.tabl)])
    ref.attribs.tmp <- clean.chars(ref.attribs.tmp, " ")
    #print(ref.attribs.tmp)

    # Grab the class name of the attributes for the reference:
    ref.class.of.attribs.idxs <- which(conversion.tabl[,attribs.idx] == "NEW")                # The class of the attributes will be a name in this row
    attrib.class <- as.character(conversion.tabl[ref.class.of.attribs.idxs[i], ref.name.idx]) # The class of the attributes for the reference is specifically this
    attrib.class <- clean.chars(attrib.class, " ")
    #print(attrib.class)

    # Tack the class name and its corresponding attributes together:
    ref.attribs.tmp <- cbind(rep(attrib.class, length(ref.attribs.tmp)), ref.attribs.tmp)
    ref.attribs     <- rbind(ref.attribs, ref.attribs.tmp)
  }
  #print(ref.attribs)

  if(nrow(ref.attribs) != nrow(study.attribs)){
    stop("Length of NEW (reference) attributs not the same as length of specified study attributes.
         Check for stray spaces or other problems.")
  }
  study2ref.attribs.info           <- cbind(ref.attribs, study.attribs)
  colnames(study2ref.attribs.info) <- c("ref.class", "ref.attrib", "study.class", "study.attrib")
  # #print(study2ref.attribs)
  # #print(study.name)

  study2ref.conversion.info <- list(
    study2ref.class.info,
    study2ref.subclass.info,
    study2ref.attribs.info
  )

  names(study2ref.conversion.info) <- c(
    "class.conversions",
    "subclass.conversions",
    "attribute.conversions"
  )

  return(study2ref.conversion.info)

}


#' Convert a row of a read in (flattened) study to the reference class,subclass and attribute
#'
#' The function will XXXX
#'
#' @param XX The XX
#' @return The function will XX
#'
#'
#' @export
convert.study.row.expt<-function(study.row, conversion.info){

  #print(study.row)
  study.class    <- study.row[1]
  study.subclass <- study.row[2]
  study.attrib   <- study.row[3]

  # Get reference class name corresponding to study class name
  class.row.idx <- which(conversion.info$cl.scl.conversions[,2] == study.class)
  # ********* CLASS NAME CHANGE NEEDED???????? *********************
  ref.class     <- conversion.info$cl.scl.conversions[class.row.idx,1]
  #print(ref.class)

  # Get reference subclass name corresponding to study subclass name
  subclass.row.idx <- which(conversion.info$cl.scl.conversions[,2] == study.subclass)
  ref.subclass     <- conversion.info$cl.scl.conversions[subclass.row.idx,1]
  #print(ref.subclass)

  # Get reference attribute name corresponding to study attribute name GIVEN the study class name
  attrib.class.row.idxs <- which(conversion.info$attribs.conversions[,3] == study.class) # Pick out the rows of the class in the attributes conversion table
  attribs.of.class      <- conversion.info$attribs.conversions[attrib.class.row.idxs, ]  # Use these to grab the block of class-attribute info
  attrib.row.idx        <- which(attribs.of.class[,4] == study.attrib)                   # Pluck out the row with the study attribute given the class
  ref.attrib            <- attribs.of.class[attrib.row.idx, 2]
  #print(ref.attrib)

  ref.row <- matrix(c(ref.class, ref.subclass, ref.attrib), c(1,3))
  #colnames(ref.row) <- c("class","subclass","attribute")

  return(ref.row)

}

#' Convert a row of a read in (flattened) study to the reference class,subclass and attribute
#'
#' The function will XXXX
#'
#' @param XX The XX
#' @return The function will XX
#'
#'
#' @export
convert.study.row.expt2<-function(study.row, conversion.info){

  #print(study.row)
  study.class    <- study.row[1]
  study.subclass <- study.row[2]
  study.attrib   <- study.row[3]

  # Some classes repeat in the studies because their names haven't changed yet (As of 6/1/19 it's just glass/mineralgrains).
  # Because there can be more than 1 index returned, just pick the 1st since both class names should be the same
  # If this does happen we'll sort out the actual reference class name to change to when we change the subclass name below.
  # class.row.idx <- which(conversion.info$class.conversions[,3] == study.class)[1]
  #print(class.row.idx)

  # Grab the whole subclass block. It contains the class names info as well. They are need to differentiate study classes
  # that have the same name.
  subclass.block.row.idxs <- which(conversion.info$subclass.conversions[,3] == study.class)
  subclass.block          <- conversion.info$subclass.conversions[subclass.block.row.idxs,]
  #print(subclass.block)
  subclass.row.idx <- which(subclass.block[,4] == study.subclass) # This may return more than two indices it the subclass is other()

  # This shouldn't happen, but if the study.subclass is not found, through an error and manually check to see why.
  if(length(subclass.row.idx) == 0) {
    print(paste("Study class:", study.class))
    print(paste("*Study subclass:", study.subclass))
    stop("Above study.subclass is not found in the conversion table! Check the study datasheet and conversion table to see why. It probably needs to be added to the conversion table.")
  }

  if(length(subclass.row.idx)>1){ # See if subclass name gets repeated in the subclass block...
   if(study.subclass == "other()") { # If other() is the repeating subclass, just indicate to just skip the row and return

     ref.class         <- "skip"         # ****ADD MAKE A RECORD OF WHAT THE ROW WAS
     ref.subclass      <- "skip"
     ref.attrib        <- "skip"
     ref.row           <- matrix(c(ref.class, ref.subclass, ref.attrib), c(1,3))
     colnames(ref.row) <- c("class","subclass","attribute")
     return(ref.row)

   } else { # If the repeat is not an other() throw an error and manually see what is going on in the study datasheet.

     print("============ PROBLEM SUBCLASS BLOCK!!!!!!!!! =====================")
     print(subclass.block)
     print("vvvvvvvvvvvv PROBLEM SUBCLASS BLOCK!!!!!!!!! vvvvvvvvvvvvvvvvvvvvv")
     print(subclass.block[subclass.row.idx,])
     stop("Repeated subclass names found in this subclass block. Check study datasheet and see why!")

   }
  } else { # Final case. If here, no issue with the subclass at least. Below we check for issues with the attribute
    #print(subclass.block[subclass.row.idx,])
    ref.class         <- subclass.block[subclass.row.idx, 1]
    ref.subclass      <- subclass.block[subclass.row.idx, 2]
    ref.row           <- matrix(c(ref.class, ref.subclass, NA), c(1,3))
    colnames(ref.row) <- c("class","subclass","attribute")
  }

  #print("Row was:")
  #print(study.row)
  #print("Row now:")
  #print(ref.row)

  # Get reference attribute name corresponding to study attribute name GIVEN the study class name

  # Since we should by now know the reference class name corresponding to the study class name use ref.class
  # to pick out the rows of the study attributes conversion table.
  # NOTE we use ref.class instead of study.class here because some of the study class names are the same.
  # This is not true of the reference class names.
  attrib.class.row.idxs <- which(conversion.info$attribute.conversions[,1] == ref.class)

  # Use these indices to grab the block of class-attribute info
  attribs.of.class      <- conversion.info$attribute.conversions[attrib.class.row.idxs, ]
  #print(attribs.of.class)
  #print("Row was:")
  #print(study.row)
  #print("Row now:")
  #print(ref.row)

  # Pluck out the row with the study attribute given the (reference) class
  attrib.row.idx        <- which(attribs.of.class[,4] == study.attrib)
  # This shouldn't happen, but if the study.attribute is not found, through an error and manually check to see why.
  if(length(attrib.row.idx) == 0) {
    print(paste("Study class:", study.class))
    print(paste("Reference class:", ref.class))
    print(paste("Study subclass:", study.subclass))
    print(paste("Reference subclass:", ref.subclass))
    print(paste("*Study attribute:", study.attrib))
    stop("Above study.attrib is not found in the conversion table! Check the study datasheet and conversion table to see why. It probably needs to be added to the conversion table.")
  }

  # print(attrib.row.idx)
  ref.attrib            <- attribs.of.class[attrib.row.idx, 2]
  #print(ref.attrib)
  #
  ref.row[3] <- ref.attrib
  # print("Row was:")
  # print(study.row)
  # print("Row now:")

  return(ref.row)

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
