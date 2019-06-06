#' Convert study datasheet to refence datasheet
#'
#' The function will XXXX
#'
#' @param reference.template.info The reference datasheet in VECTOR form
#' @return The function will XX
#'
#'
#' @export
study2ref.datasheet<-function(study.datasheet.file.path, study.name, study2ref.conversion.info, reference.template.info, print.lvl=0){

  # First convert a study datasheet to have reference classes, subclasses and attributes
  conv.dsheet.info <- convert.study.datasheet(
    study.datasheet.file.path = study.datasheet.file.path,
    study.name                = study.name,
    study2ref.conversion.info = study2ref.conversion.info,
    print.lvl                 = print.lvl)

  # Grab the study datasheet version that drops the skipped categories.
  # NOTE: This contains the categories AND the indicatior responses
  study.conv.dsheet <- conv.dsheet.info$reduced.converted.df

  # These will embed the harvested study info in the reference datasheet format
  ref.categ.mat     <- tolower(reference.template.info$category.mat)
  #ref.categ.mat.CAP <- reference.template.info$category.mat           # Includes capitalization. Make more efficient.........
  ref.indic.vec     <- reference.template.info$indicator.vec

  for(i in 1:nrow(study.conv.dsheet)){

    study.row.categs   <- study.conv.dsheet[i,1:3]
    #print(as.matrix(study.row.categs[1]))
    ref.categ.mat.idx <- get.row.idx(study.row.categs, ref.categ.mat)
    #print(ref.categ.mat.idx)

    # If the study row is not found, add it.
    if(is.na(ref.categ.mat.idx)) {

      warning(paste("********* Study row:", i,
                    as.matrix(study.row.categs[1]),
                    as.matrix(study.row.categs[2]),
                    as.matrix(study.row.categs[3]),
                    "not found. Adding... BUT DOUBLE CHECK!! ************")) # THINK CARFULLY HOW TO IMPLEMENT THIS

      # Just tack on the new categories at the bottom for now
      ref.categ.mat <- rbind(ref.categ.mat, study.row.categs)
      ref.indic.vec <- c(ref.indic.vec, study.conv.dsheet[i,4])

    } else { # The study row was found, so just add the data into the reference indicator vector

      # print(paste("Study row:", i,
      #             as.matrix(study.row.categs[1]),
      #             as.matrix(study.row.categs[2]),
      #             as.matrix(study.row.categs[3])))
      # print(paste("Ref row:  ", i,
      #             as.matrix(ref.categ.mat.CAP[ref.categ.mat.idx,1]),
      #             as.matrix(ref.categ.mat.CAP[ref.categ.mat.idx,2]),
      #             as.matrix(ref.categ.mat.CAP[ref.categ.mat.idx,3])))  # ADD NOTES TOO????

      ref.indic.vec[ref.categ.mat.idx] <-  study.conv.dsheet[i,4]
    }

  }

  #print(data.frame(ref.categ.mat, ref.indic.vec))
  ref.note.vec      <- reference.template.info$note.vec
  ref.embedded.info <- list(
    ref.indic.vec,
    ref.note.vec,
    ref.categ.mat)

  return(ref.embedded.info)

}
