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
  dsheet.categ.mat     <- dsheet.info[[2]]
  dsheet.categ.mat.new <- array(NA,dim(dsheet.categ.mat))
  unique.classes       <- unique(dsheet.categ.mat[,1])
  unique.subclasses    <- unique(dsheet.categ.mat[,2])
  unique.attributes    <- unique(dsheet.categ.mat[,3])

  # Study dependendent classes, subclasses and attributes from conversion table
  conversion.tabl <- read_excel(path = fpath.convertion.table, col_names=F)

  cl.scl.idx   <- which(conversion.tabl[2,]==study.name) # Column index for class and subclass names in a study
  study.cl.scl <- conversion.tabl[,cl.scl.idx]

  ref.cl.scl.idx   <- which(conversion.tabl[2,]=="NEW") # Column index for reference class and subclass names in a study
  ref.cl.scl       <- conversion.tabl[,ref.cl.scl.idx]
  study2ref.cl.scl <- cbind(ref.cl.scl,study.cl.scl)    # Class/Subclass name conversions collected together

  attribs.idx        <- which(conversion.tabl[1,]=="Attributes")           # Column index where the attribute infor begins
  study.attribs.idxs <- which(conversion.tabl[,attribs.idx] == study.name) # Row numbers for the study's recorded attribute names
  study.attribs      <- unique(as.character(as.matrix(conversion.tabl[study.attribs.idxs,(attribs.idx+1):ncol(conversion.tabl)])))
  study.attribs      <- study.attribs[-which(is.na(study.attribs) == TRUE)]

  cl.changes.idx <- which(conversion.tabl[1,]=="Class Changes")
  cl.changes     <- conversion.tabl[,cl.changes.idx]
  #print(cl.changes)

  # Reference classes, subclasses and attributes from conversion table
  ref.attribs.idxs <- which(conversion.tabl[,attribs.idx] == "NEW") # Row numbers for the Reference attribute names
  ref.attribs      <- unique(as.character(as.matrix(conversion.tabl[ref.attribs.idxs,(attribs.idx+1):ncol(conversion.tabl)])))
  ref.attribs      <- ref.attribs[-which(is.na(ref.attribs) == TRUE)]

  # look up idx of unq in stdy stdy col. use that idx to look up ref name in stdy ref col
  # Convert class names:
  for(i in 1:length(unique.classes)){
    dsheet.cl.idxs <- which(dsheet.categ.mat[,1] == unique.classes[i])
    # Many Glass/Mineral Grains cells should now be classed as Various! The class name will be
    # changed however in the subclass loop. For now they will be labeled as Inorganic Grains.
    cl.chg.idx <- which(study2ref.cl.scl[,2] == unique.classes[i])
    dsheet.categ.mat.new[dsheet.cl.idxs,1] <- as.character(as.matrix(study2ref.cl.scl[cl.chg.idx,1]))
  }
  #print(dsheet.categ.mat.new[,1])

  # Convert subclass names, and make any stipulated class name adjustments
  for(i in 1:length(unique.subclasses)){
    dsheet.scl.idxs <- which(dsheet.categ.mat[,2] == unique.subclasses[i])
    scl.chg.idx     <- which(study2ref.cl.scl[,2] == unique.subclasses[i])
    if(length(scl.chg.idx) != 1){
      print("Issue with old datasheet at: *****************************")
      print(paste0("===== ", unique.subclasses[i]," ====="))
      print(paste0("The above subclass name occurs: ", length(scl.chg.idx)," times."))
      if(length(scl.chg.idx)==0){
        # Probably best to thow an error if we run across a subclass we don't recognize in the reference datasheet:
        stop("Can't find the above subclass name in study2ref.cl.scl[,2]. Open up the old datasheet and see what's going on...")
      } else {
        for(j in 1:length(scl.chg.idx)) {
          #print(study2ref.cl.scl[scl.chg.idx,1])
          skipQ <- unique(study2ref.cl.scl[scl.chg.idx,1])
          if(length(skipQ) != 1) {
            print(paste("xxxxxxx",study2ref.cl.scl[scl.chg.idx,1], "xxxxxxxx"))
            stop("Problem! These should be uniquely SKIP!")
          } else {
            if(toupper(skipQ) != "SKIP") {
              stop("Problem! This should be SKIP! Check the old datasheet!")
            } else {
              # Skip his subclass category name
              print("Skipping")
            }
          }
        }
      }
    } else {
      # Do the name conversion
      print("Changing")
    }
    #print(as.character(as.matrix(study2ref.cl.scl[scl.chg.idx,2])))
    #print(as.character(as.matrix(study2ref.cl.scl[scl.chg.idx,1])))
  }
}
