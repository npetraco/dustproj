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
convert.datasheet<-function(fpath.datasheet, study.name, fpath.convertion.table, print.level=0){

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

  ref.cl.scl.idx   <- which(conversion.tabl[2,]=="NEW") # Column index for NEW reference class and subclass names in a study
  ref.cl.scl       <- conversion.tabl[,ref.cl.scl.idx]

  cl.namechgs.col.idx <- which(conversion.tabl[1,]=="Class Changes") # Column index for stipulated class name changes
  cl.namechgs         <- conversion.tabl[,cl.namechgs.col.idx]     # Drop the first row because of the format of conversion.tbl

  scl.namechgs.col.idx <- which(conversion.tabl[1,]=="Subclass Changes") # Column index for stipulated subclass name changes
  scl.namechgs         <- conversion.tabl[,scl.namechgs.col.idx]       # Drop the first row because of the format of conversion.tbl

  study2ref.cl.scl <- cbind(ref.cl.scl, study.cl.scl, cl.namechgs, scl.namechgs) # Class/Subclass name conversions collected together

  # Study Reference attributes from conversion table
  attribs.idx        <- which(conversion.tabl[1,]=="Attributes")           # Column index where the attribute infor begins
  study.attribs.idxs <- which(conversion.tabl[,attribs.idx] == study.name) # Row numbers for the study's recorded attribute names
  study.attribs      <- as.character(as.matrix(conversion.tabl[study.attribs.idxs,(attribs.idx+1):ncol(conversion.tabl)]))

  # NEW Reference attributes from conversion table
  ref.attribs.idxs <- which(conversion.tabl[,attribs.idx] == "NEW") # Row numbers for the Reference attribute names
  ref.attribs      <- as.character(as.matrix(conversion.tabl[ref.attribs.idxs,(attribs.idx+1):ncol(conversion.tabl)]))

  if(length(ref.attribs) != length(study.attribs)){
    stop("Length of NEW attributs not the same as length of specified study attributes. Check for stray spaces or other problems.")
  }
  study2ref.attribs <- cbind(ref.attribs,study.attribs)

  # Look up idx of unq in stdy stdy col. use that idx to look up ref name in stdy ref col
  # Convert class names:
  for(i in 1:length(unique.classes)){
    dsheet.cl.idxs <- which(dsheet.categ.mat[,1] == unique.classes[i])
    # Many Glass/Mineral Grains cells should now be classed as Various! The class name will be
    # changed however in the subclass loop. ** NOTE: ** For now they will be labeled as Inorganic Grains.
    cl.chg.idx <- which(study2ref.cl.scl[,2] == unique.classes[i])
    #print(unique.classes[i])
    #print(cl.chg.idx)
    #print(study2ref.cl.scl[,2])
    dsheet.categ.mat.new[dsheet.cl.idxs,1] <- as.character(as.matrix(study2ref.cl.scl[cl.chg.idx,1]))
  }

  # Convert subclass names, AND make any stipulated class name adjustments
  for(i in 1:length(unique.subclasses)){

    dsheet.scl.idxs <- which(dsheet.categ.mat[,2] == unique.subclasses[i])
    scl.chg.idx     <- which(study2ref.cl.scl[,2] == unique.subclasses[i])

    # If spotted an issue with the loaded datasheet subclass name. Diagnose:
    if(length(scl.chg.idx) != 1){

      if(print.level>=1){
        print("Issue with old datasheet at: *****************************")
        print(paste0("===== ", unique.subclasses[i]," ====="))
        print(paste0("The above subclass name occurs: ", length(scl.chg.idx)," times."))
      }

      if(length(scl.chg.idx)==0){

        # So we didn't find the subclass name in the reference study subclass names.
        # Look and see if it's in the reference new subclass names:
        poss.ref.scl.mch.idx <- which(study2ref.cl.scl[,1] == unique.subclasses[i])
        if(length(poss.ref.scl.mch.idx) == 1) {

          if(print.level>=1){
            # Found the subclass name in the reference study subclass names:
            cat("Subclass name:", unique.subclasses[i], "is not in the reference study subclass names.\nIt is in the new datasheet reference subclass names, so we'll use this name.\n\n" )
          } else {
            condl.trash <- NULL # Just to have something to do if we don't print at this level
          }

        } else {

          # Probably best to thow an error if we run across a subclass we don't recognize in the reference datasheets:
          stop("Can't find the above subclass name in study2ref.cl.scl[,2]. Open up the old datasheet and see what's going on...")

        }
      } else {

        # Handel subclass names we will toss:
        for(j in 1:length(scl.chg.idx)) {

          skipQ <- unique(study2ref.cl.scl[scl.chg.idx,1])

          if(length(skipQ) != 1) {
            print(paste("xxxxxxx",study2ref.cl.scl[scl.chg.idx,1], "xxxxxxxx"))
            stop("Problem! These should be uniquely SKIP!")
          } else {
            if(toupper(skipQ) != "SKIP") {
              stop("Problem! This should be SKIP! Check the old datasheet!")
            } else {
              # ******************Skip this subclass category name
              if(print.level >=1) {
                print(paste("Skipping subclass name:", unique.subclasses[i], "--->", skipQ))
              }
              dsheet.categ.mat.new[dsheet.scl.idxs,2] <- as.character(as.matrix(study2ref.cl.scl[scl.chg.idx,1]))
              # No need to change the class names for these since these will be tossed.
            }
          }
        }
      }
    } else {
      # ****************Do the name conversion
      # Look first for the subclassname change in the NEW column.
      # If not found look in the Subclass Change column:
      if(is.na(study2ref.cl.scl[scl.chg.idx,1])){
        new.scl.nme <- as.character(as.matrix(study2ref.cl.scl[scl.chg.idx,4]))
        old.scl.nme <- as.character(as.matrix(study2ref.cl.scl[scl.chg.idx,2]))

        if(print.level >= 2) {
          print(paste("Subclass name change from:", old.scl.nme, "to", new.scl.nme))
        }

        dsheet.categ.mat.new[dsheet.scl.idxs,2] <- new.scl.nme

        # Check if there is a class name change too:
        if(!is.na(study2ref.cl.scl[scl.chg.idx,3])) {
          old.cl.nme <- dsheet.categ.mat.new[dsheet.scl.idxs,1][1] # Just grab the first occurance.
          new.cl.nme <- as.character(as.matrix(study2ref.cl.scl[scl.chg.idx,3]))
          dsheet.categ.mat.new[dsheet.scl.idxs,1] <- new.cl.nme

          if(print.level >= 2){
            print(paste("Class name change from:", old.cl.nme, "to", new.cl.nme))
          }

        }

      } else {
        dsheet.categ.mat.new[dsheet.scl.idxs,2] <- as.character(as.matrix(study2ref.cl.scl[scl.chg.idx,1]))

        # Check for a class name change in the Class Change column:
        if(!is.na(study2ref.cl.scl[scl.chg.idx,3])) {
          old.cl.nme <- dsheet.categ.mat.new[dsheet.scl.idxs,1][1] # Just grab the first occurance.
          new.cl.nme <- as.character(as.matrix(study2ref.cl.scl[scl.chg.idx,3]))
          dsheet.categ.mat.new[dsheet.scl.idxs,1] <- new.cl.nme

          if(print.level >= 2){
            print(paste("Class name change from:", old.cl.nme, "to", new.cl.nme))
          }

        }
        # Check for a an extra subclass name change in the Subclass Change column:
        if(!is.na(study2ref.cl.scl[scl.chg.idx,4])) {
          old.scl.nme <- dsheet.categ.mat.new[dsheet.scl.idxs,2]
          new.scl.nme <- as.character(as.matrix(study2ref.cl.scl[scl.chg.idx,4]))
          dsheet.categ.mat.new[dsheet.scl.idxs,2] <- new.scl.nme

          if(print.level >= 2){
            print(paste("Subclass name change from:", old.scl.nme, "to", new.scl.nme))
          }

        }
      }

    }
  }

  for(i in 1:length(unique.attributes)){

    dsheet.attribs.idxs <- which(dsheet.categ.mat[,3] == unique.attributes[i])

    if(is.na(unique.attributes[i])){
      stop("NA unique attribute encountered!")
    }

    attribs.chg.idx <- which(study2ref.attribs[,2] == unique.attributes[i])
    dsheet.categ.mat.new[dsheet.attribs.idxs,3] <- as.character(as.matrix(study2ref.attribs[attribs.chg.idx[1],1]))
  }


  # Clean up:
  # Add in 0/1
  dsheet.categ.mat.new <- data.frame(dsheet.categ.mat.new, dsheet.info[[1]])
  colnames(dsheet.categ.mat.new) <- c("Class","Subclass","Attribute","Indicator")

  # Eliminate SKIPs wrt col 2,3:
  skip.idxs <- c(which(dsheet.categ.mat.new[,2] == "SKIP"),which(dsheet.categ.mat.new[,3] == "SKIP"))

  # Check for 1s in proposed eliminated rows and print a warning if there are any:
  if(1 %in% dsheet.categ.mat.new[skip.idxs,4]){
    print("xxxxxxxxxxxxx WARNING: SKIPPING THESE CELLS THOUGH THEY HAVE A 1 xxxxxxxxxx")
    print(dsheet.categ.mat.new[skip.idxs[which(dsheet.categ.mat.new[skip.idxs,4] == 1)],])
    if(print.level >= 2){
      print("This is what the original class, subclass and attribute names were:")
      print(dsheet.categ.mat[skip.idxs[which(dsheet.categ.mat.new[skip.idxs,4] == 1)],])
    }
    warning("Proposed SKIP rows contain 1s!")
    print("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
  }
  dsheet.categ.mat.new <- dsheet.categ.mat.new[-skip.idxs,]

  # Change Various attrib clear to ColorNR
  vclear.idxs <- which((dsheet.categ.mat.new[,1]=="Various") & (dsheet.categ.mat.new[,3]=="Clear"))
  dsheet.categ.mat.new[vclear.idxs,3] <- "ColorNR"

  # Elim all Various not ColorNR
  skip.idxs2 <- which((dsheet.categ.mat.new[,1]=="Various") & (dsheet.categ.mat.new[,3]!="ColorNR"))
  #print(dsheet.categ.mat.new[skip.idxs2,])
  # Check for 1s in the second round of proposed eliminated rows and throw error if there are any:
  if(1 %in% dsheet.categ.mat.new[skip.idxs2,4]){
    print("x-x-x-x-x-x-x WARNING: SKIPPING THESE CELLS THOUGH THEY HAVE A 1 x-x-x-x-x-x")
    print(dsheet.categ.mat.new[skip.idxs[which(dsheet.categ.mat.new[skip.idxs2,4] == 1)],])
    if(print.level >= 2){
      print("This is what the original class, subclass and attribute names were:")
      print(dsheet.categ.mat[skip.idxs2[which(dsheet.categ.mat.new[skip.idxs2,4] == 1)],])
    }
    stop("Proposed VARIOUS rows to eliminate contain 1s!")
    print("x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x")
  } else {
    dsheet.categ.mat.new <- dsheet.categ.mat.new[-skip.idxs2,]
  }

  return(dsheet.categ.mat.new)

}
