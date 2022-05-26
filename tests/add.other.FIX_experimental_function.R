remove.all.add.other.FIX<-function(dsheet.info, out.format="matrix", printQ=F){

  dsheet.info.new <- dsheet.info

  if(out.format=="matrix"){
    for(i in 1:length(dsheet.info)) {
      # Assumes add-others are at the last row and column, which they should be if they are there
      drp.idxs <- dim(dsheet.info[[i]][[1]])

      # Check and see IF there are any "add-other" categories first:
      #print( tolower(colnames(dsheet.info[[i]][[1]])) )
      check.drp.idxs <- which(tolower(colnames(dsheet.info[[i]][[1]]))  == "add-other" )
      #print(check.drp.idxs)
      if(length(check.drp.idxs) != 0) {
        dsheet.info.new[[i]][[1]] <- as.matrix(dsheet.info.new[[i]][[1]][-drp.idxs[1], -drp.idxs[2]])
        dsheet.info.new[[i]][[2]] <- as.matrix(dsheet.info.new[[i]][[2]][-drp.idxs[1], -drp.idxs[2]])

        colnames(dsheet.info.new[[i]][[1]]) <- colnames(dsheet.info[[i]][[1]])[-drp.idxs[2]]
        rownames(dsheet.info.new[[i]][[1]]) <- rownames(dsheet.info[[i]][[1]])[-drp.idxs[1]]
        colnames(dsheet.info.new[[i]][[2]]) <- colnames(dsheet.info[[i]][[1]])[-drp.idxs[2]]
        rownames(dsheet.info.new[[i]][[2]]) <- rownames(dsheet.info[[i]][[1]])[-drp.idxs[1]]
      }

      if(printQ==T){
        if(length(check.drp.idxs) == 0){
          print(paste0("Section ",i,": No add-other categories to drop."))
        } else {
          print(paste0("Section ",i, ": ", length(check.drp.idxs), " add-other categories dropped."))
        }
      }


    }
  } else if(out.format=="vector"){

    drp.idxs <- unique(
      c(
        which(tolower(dsheet.info$category.mat[,2]) == "add-other"),
        which(tolower(dsheet.info$category.mat[,3]) == "add-other")
      )
    )
    #print(drp.idxs)
    #print(length(drp.idxs))

    if(length(drp.idxs) != 0){
      dsheet.info.new$category.mat  <- dsheet.info.new$category.mat[-drp.idxs,]
      dsheet.info.new$indicator.vec <- dsheet.info.new$indicator.vec[-drp.idxs]
      dsheet.info.new$note.vec      <- dsheet.info.new$note.vec[-drp.idxs]
    }

    if(printQ==T){
      if(length(drp.idxs) == 0){
        print("No add-other categories to drop.")
      } else {
        print(paste0(length(drp.idxs), " add-other categories dropped."))
      }
    }


  } else {
    stop("Choose matrix or vector for out.format!")
  }

  return(dsheet.info.new)

}
