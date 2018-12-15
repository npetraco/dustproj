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

  #print(length(ref.cl.scl))
  #print(length(study.cl.scl))
  #print(length(cl.namechgs))
  #print(length(scl.namechgs))
  #study2ref.cl.scl <- cbind(ref.cl.scl, study.cl.scl) # Class/Subclass name conversions collected together
  study2ref.cl.scl <- cbind(ref.cl.scl, study.cl.scl, cl.namechgs, scl.namechgs) # Class/Subclass name conversions collected together
  #print(study2ref.cl.scl)

  # Study Reference attributes from conversion table
  attribs.idx        <- which(conversion.tabl[1,]=="Attributes")           # Column index where the attribute infor begins
  study.attribs.idxs <- which(conversion.tabl[,attribs.idx] == study.name) # Row numbers for the study's recorded attribute names
  study.attribs      <- as.character(as.matrix(conversion.tabl[study.attribs.idxs,(attribs.idx+1):ncol(conversion.tabl)]))

  # NEW Reference attributes from conversion table
  ref.attribs.idxs <- which(conversion.tabl[,attribs.idx] == "NEW") # Row numbers for the Reference attribute names
  ref.attribs      <- as.character(as.matrix(conversion.tabl[ref.attribs.idxs,(attribs.idx+1):ncol(conversion.tabl)]))

  #print(study.attribs)
  #print(ref.attribs)
  print(cbind(ref.attribs,study.attribs))
  print(length(study.attribs))
  print(length(ref.attribs))


  # # Study Reference attributes from conversion table
  # attribs.idx        <- which(conversion.tabl[1,]=="Attributes")           # Column index where the attribute infor begins
  # study.attribs.idxs <- which(conversion.tabl[,attribs.idx] == study.name) # Row numbers for the study's recorded attribute names
  # study.attribs      <- unique(as.character(as.matrix(conversion.tabl[study.attribs.idxs,(attribs.idx+1):ncol(conversion.tabl)])))
  # #study.attribs      <- study.attribs[-which(is.na(study.attribs) == TRUE)]
  # print(study.attribs)
  #
  # # NEW Reference attributes from conversion table
  # ref.attribs.idxs <- which(conversion.tabl[,attribs.idx] == "NEW") # Row numbers for the Reference attribute names
  # ref.attribs      <- unique(as.character(as.matrix(conversion.tabl[ref.attribs.idxs,(attribs.idx+1):ncol(conversion.tabl)])))
  # #ref.attribs      <- ref.attribs[-which(is.na(ref.attribs) == TRUE)]
  # ref.attribs      <- ref.attribs[-which(ref.attribs == "ADD-Other")] # Drop any "ADD-Other" due to the way the conversion table is formatted.
  # #ref.attribs[which(ref.attribs == "ADD-Other")] <- NA
  # print(ref.attribs)
  # print(length(study.attribs))
  # print(length(ref.attribs))
  # # The ref.attribs and study.attribs are likely not the same length.
  # # Figure out which is longer and pad the other to the same length.
  # # ** NOTE: ** THIS ASSUMES THE DISCREPANCY IS AT THE END OF THE VECTORS AND
  # # THAT ONCE PADDED EVERYTHING WILL LINE UP CORRECTLY !!!!!!
  # if(length(study.attribs) > length(ref.attribs)) {
  #   pad.leng <- length(study.attribs) - length(ref.attribs)
  #   ref.attribs <- c(ref.attribs, rep(NA,pad.leng))
  # } else if(length(ref.attribs) > length(study.attribs)){
  #   pad.leng <- length(ref.attribs) - length(study.attribs)
  #   study.attribs <- c(study.attribs, rep(NA,pad.leng))
  # } else {
  #   condl.trash <- NULL # Just to have something to do if vectors are the same length.
  # }
  # print(cbind(ref.attribs, study.attribs))

  # look up idx of unq in stdy stdy col. use that idx to look up ref name in stdy ref col
  # Convert class names:
  for(i in 1:length(unique.classes)){
    dsheet.cl.idxs <- which(dsheet.categ.mat[,1] == unique.classes[i])
    # Many Glass/Mineral Grains cells should now be classed as Various! The class name will be
    # changed however in the subclass loop. ** NOTE: ** For now they will be labeled as Inorganic Grains.
    cl.chg.idx <- which(study2ref.cl.scl[,2] == unique.classes[i])
    dsheet.categ.mat.new[dsheet.cl.idxs,1] <- as.character(as.matrix(study2ref.cl.scl[cl.chg.idx,1]))
  }
  #print(dsheet.categ.mat.new[,1])

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
            #print(paste("**Found in**: New ref scl nme:", study2ref.cl.scl[poss.ref.scl.mch.idx,1], "Loaded scl nme", unique.subclasses[i]))
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
          #print(study2ref.cl.scl[scl.chg.idx,1])
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
      #print("Changing")
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
    #print(as.character(as.matrix(study2ref.cl.scl[scl.chg.idx,2])))
    #print(as.character(as.matrix(study2ref.cl.scl[scl.chg.idx,1])))
  }

  # for(i in 1:length(unique.attributes)){
  #   dsheet.attribs.idxs <- which(dsheet.categ.mat[,3] == unique.attributes[i])
  #   attribs.chg.idx     <- which(study2ref.cl.scl[,3] == unique.attributes[i])
  #   # dsheet.categ.mat.new[dsheet.cl.idxs,1] <- as.character(as.matrix(study2ref.cl.scl[cl.chg.idx,1]))
  # }

  #print(study2ref.cl.scl)
  #print(dsheet.categ.mat.new)
}
