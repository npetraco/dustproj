#' Convert a row of a read in (flattened) study to the reference class,subclass and attribute
#'
#' The function will XXXX
#'
#' @param XX The XX
#' @return The function will XX
#'
#'
#' @export
convert.study.row<-function(study.row, conversion.info){

  #print(study.row)
  study.class    <- study.row[1]
  study.subclass <- study.row[2]
  study.attrib   <- study.row[3]

  # Some classes repeat in the studies because their names haven't changed yet (As of 6/1/19 it's just glass/mineralgrains).
  # Because there can be more than 1 index returned, just pick the 1st since both class names should be the same
  # If this does happen we'll sort out the actual reference class name to change to when we change the subclass name below.

  # Deal with subclasses first

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

      ref.class         <- "skip"         # ****ADD MAKE A RECORD OF WHAT THE ROW WAS ??
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

    ref.class         <- subclass.block[subclass.row.idx, 1]
    ref.subclass      <- subclass.block[subclass.row.idx, 2]
    ref.row           <- matrix(c(ref.class, ref.subclass, NA), c(1,3))
    colnames(ref.row) <- c("class","subclass","attribute")

  }

  # Get reference attribute name corresponding to study attribute name GIVEN the study class name

  # Since we should by now know the reference class name corresponding to the study class name use ref.class
  # to pick out the rows of the study attributes conversion table.
  #
  # NOTE: we use ref.class instead of study.class here because some of the study class names are the same (e.g. Glass/Mineral Grains).
  # This is not true of the reference class names.
  attrib.class.row.idxs <- which(conversion.info$attribute.conversions[,1] == ref.class)

  # Use these indices to grab the block of class-attribute info
  attribs.of.class      <- conversion.info$attribute.conversions[attrib.class.row.idxs, ]

  # Pluck out the row with the study attribute given the (reference) class
  attrib.row.idx        <- which(attribs.of.class[,4] == study.attrib)
  # This shouldn't happen, but if the study.attribute is not found, throw an error and manually check to see why.
  if(length(attrib.row.idx) == 0) {
    print(paste("Study class:", study.class))
    print(paste("Reference class:", ref.class))
    print(paste("Study subclass:", study.subclass))
    print(paste("Reference subclass:", ref.subclass))
    print(paste("*Study attribute:", study.attrib))
    stop("Above study.attrib is not found in the conversion table! Check the study datasheet and conversion table to see why. It probably needs to be added to the conversion table.")
  }

  # Found the attribute is we made it here. Put it in the new row vector containing the converted names:
  ref.attrib            <- attribs.of.class[attrib.row.idx, 2]
  ref.row[3] <- ref.attrib

  return(ref.row)

}


#' Convert study classes, subclasses and attributes to the reference classes, subclasses and attributes
#'
#' The function will XXXX
#'
#' @param XX The XX
#' @return The function will XX
#'
#'
#' @export
convert.study.datasheet<-function(study.datasheet.file.path, study.name, study2ref.conversion.info, print.lvl=0){

  parsed.dsht.info <- parse.study.datasheet(study.datasheet.file.path, study.name = study.name)
  flat.dsht        <- parsed.dsht.info$study.flattened.datasheet
  flat.dsht.new    <- array("", dim(flat.dsht))

  skip.idxs <- NULL
  for(i in 1:nrow(flat.dsht)) {

    if(print.lvl>0){
      print(paste("Row:",i, "Study class:", flat.dsht[i,1], "Study subclass:", flat.dsht[i,2], "Study attrib:", flat.dsht[i,3] ))
    }

    row.new <- convert.study.row(flat.dsht[i, ], study2ref.conversion.info)

    # Use these to drop the skips later:
    if("skip" %in% row.new){
      skip.idxs <- c(skip.idxs, i)
    }

    flat.dsht.new[i,] <- row.new

    if(print.lvl>1){
      compre <- cbind(
        as.matrix(flat.dsht[i, ]),
        t(as.matrix(row.new))
      )
      print(compre)
      print("++++++++++++++++++++++++++++++++++++++++++++++")
    }

  }

  flat.dsht.new.full            <- data.frame(flat.dsht.new, parsed.dsht.info$study.flattened.data)
  colnames(flat.dsht.new.full)  <- c("class", "subclass", "attribute", "response")
  flat.dsht.new.red             <- flat.dsht.new.full[-skip.idxs,]
  row.names(flat.dsht.new.full) <- NULL
  row.names(flat.dsht.new.red)  <- NULL

  new.dsht.info <- list(
    skip.idxs,
    flat.dsht.new.full,
    flat.dsht.new.red
  )

  names(new.dsht.info) <- c(
    "skip.idxs",
    "full.converted.df",
    "reduced.converted.df"
  )

  return(new.dsht.info)

}
