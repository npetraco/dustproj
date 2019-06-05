#' Parse conversion sheet according to the study to be converted
#'
#' The function will XXXX
#'
#' @param XX The XX
#' @return The function will XX
#'
#'
#' @export
parse.conversion.table<-function(fpath.convertion.table, study.name){

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
