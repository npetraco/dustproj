#' Write the generalized, expandable data sheet
#'
#' XXXX
#'
#' The function will XXXX
#'
#' @param datasheet.info A refencence format data sheet in VECTOR form
#' @return The function will XX
#'
#'
#' @export
write.datasheet<-function(datasheet.info, out.fpath, section.class.order=NULL){

  indicator.vec <- datasheet.info$indicator.vec
  note.vec      <- datasheet.info$note.vec
  category.mat  <- datasheet.info$category.mat

  # Get a section number for each class
  if(is.null(section.class.order)) { # Just use the order in the datasheet
    ref.classes   <- unique(category.mat[,1])
    sect.nums     <- paste0("section",1:length(ref.classes))
    section2class <- cbind(sect.nums,ref.classes)
  } else {                           # Use a custom order
    ref.classes <- section.class.order
    section2class <- cbind(sect.nums,ref.classes)
  }

  # Compute block dims. We need these to flesh out the section (block with the SectionX and ADD-other-s)
  subcls     <- rep(list(NULL), length(ref.classes))
  attribs    <- rep(list(NULL), length(ref.classes))
  sect.idxs  <- rep(list(NULL), length(ref.classes))
  sect.dims  <- array(NA, c(length(ref.classes), 2))
  for(i in 1:length(ref.classes)){
    block.idxs        <- which(category.mat[,1] == ref.classes[i]) # Grab a class block
    sect.idxs[[i]]    <- block.idxs                                # Store to help put in data to section blocks in next loop
    csa.block         <- category.mat[block.idxs,]                 # Class subclass and attribute block

    unique.subclasses <- unique(csa.block[,2])                     # Might as well grab these now
    unique.attributes <- unique(csa.block[,3])                     # Might as well grab these now

    subcls[[i]]  <- unique.subclasses
    attribs[[i]] <- unique.attributes

    num.sect.rows <- length(unique.subclasses) + 2 # Add 2 for attributes header and ADD-other subclass
    num.sect.cols <- length(unique.attributes) + 3 # Add 3 for SectionX, class/subclasses and ADD-other attribute

    sect.dims[i,] <- c(num.sect.rows, num.sect.cols)

  }

  # Now decide which section is widest. That will dictate the number of columns for all the sections
  max.cols <- max(sect.dims[,2])

  # Now build the section blocks
  writeable.datasheet <- NULL # This is what we will write in Excel format
  for(i in 1:length(ref.classes)) {
    #sect.mat.tmp <- array("", c(sect.dims[i,1], max.cols))
    sect.mat.tmp <- array(NA, c(sect.dims[i,1], max.cols))

    # Insert column headers (sectionx class attributes add-other):
    row1.nmes.vec <- c(section2class[i,1], section2class[i,2], attribs[[i]], "add-other")
    sect.mat.tmp[1, 1:length(row1.nmes.vec)] <- row1.nmes.vec

    # Insert row names (subclasses)
    col2.nmes.vec <- c(subcls[[i]], "add-other")
    sect.mat.tmp[2:nrow(sect.mat.tmp) ,2] <- col2.nmes.vec

    # Now lets try to insert the input section data into the section matrix just constructed
    dsheet.sect.row.idxs <- sect.idxs[[i]] # These are the row indices for all the same class
    for(j in 1:length(dsheet.sect.row.idxs)) {

      row.idx       <- dsheet.sect.row.idxs[j]                   # Index of the data item of the original data sheet
      sect.subclass <- category.mat[row.idx,2]                   # Subclass of the data item (i.e. what row it should be in)
      sect.attrib   <- category.mat[row.idx,3]                   # Attribute of the data item (i.e. what column it should be in)
      itm.col.idx   <- which(sect.mat.tmp[1,] == sect.attrib)    # Section Column number of the data item of the datasheet row
      itm.row.idx   <- which(sect.mat.tmp[,2] == sect.subclass)  # Section Row number of the data item of the datasheet row

      indic.resp <- indicator.vec[row.idx]
      # Be a little careful here to check that indicator is 1 or 0 only
      if(indic.resp == 1) {
        indic.resp <- 1
      } else if(indic.resp == 0) {
        #indic.resp <- ""
        indic.resp <- NA
      } else {
        print(category.mat[row.idx,])
        print(paste("Indicator response:", indic.resp))
        stop(paste("Problem at row", row.idx, "of input datasheet! Indicator is not 0 or 1!!"))
      }

      # If everything seemd to be ok with the above check, put the response into the section matrix:

      # Check to make sure we aren't writing into a cell with something in it. That indicates a problem
      # finding the row and col index of the data item in the section matrix
      #if(sect.mat.tmp[itm.row.idx,itm.col.idx] != "" ){
      if(!is.na(sect.mat.tmp[itm.row.idx,itm.col.idx]) ){
        print(paste("Sect row num           :", itm.row.idx, "Subclass:", sect.subclass))
        print(paste("Sect col num           :", itm.col.idx, "Attrib  :", sect.attrib))
        print(paste("Indicator data to write:", indic.resp))
        print(paste("Actual input indicator :", indicator.vec[row.idx]))
        print(as.vector(category.mat[row.idx,]))
        print("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^")
        stop("Something is weird. We are trying to write to a column/row header... See above info. ")
      } else { # If nothing is word in the cell, write the response value (should be a character 0 or 1)
        sect.mat.tmp[itm.row.idx,itm.col.idx] <- indic.resp
      }

    }

    writeable.datasheet <- rbind(writeable.datasheet, sect.mat.tmp)

  }

  # Write the result to an excel file
  write.xlsx(writeable.datasheet, file = out.fpath, col.names = F, row.names = F, showNA = F)
  print(paste("Wrote:", out.fpath))

  #return(writeable.datasheet)

}
