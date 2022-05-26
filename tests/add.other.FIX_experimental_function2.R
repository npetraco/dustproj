read.datasheet.FIX<-function(fpath, out.format="matrix", add.other.rm=TRUE, printQ=FALSE){

  raw.dat     <- read.xlsx(fpath, 1, header = FALSE)
  sect.starts <- which(is.na(raw.dat[,1]) == FALSE)
  sect.stops  <- sect.starts - 1
  sect.stops  <- c(sect.stops[-1], nrow(raw.dat))
  sect.idxs   <- cbind(raw.dat[sect.starts,1], sect.starts, sect.stops)
  num.sects   <- nrow(sect.idxs)
  if(printQ==TRUE){
    print(sect.idxs)
  }

  if(out.format=="matrix"){
    proc.dat.sht <- rep(list(NULL), num.sects)
  } else if(out.format=="vector"){
    proc.indic.vec <-NULL
    proc.note.vec  <-NULL
    proc.categ.mat <-NULL
  } else {
    stop("Choose matrix or vector for out.format!")
  }

  for(i in 1:num.sects){
    sect.num      <- i
    sect.info     <- raw.dat[sect.starts[sect.num]:sect.stops[sect.num],]
    #print(paste("Got here, Section",i))
    #print(sect.info)
    sect.out.info <- process.data.sheet.section(sect.info, out.format=out.format)
    #print(paste("Got here, Section",i))
    #print(sect.out.info)

    #print(paste("Got here, Section",i))
    if(out.format=="vector") {
      proc.indic.vec <-     c(proc.indic.vec, sect.out.info$indicator.vec)
      proc.note.vec  <-     c(proc.note.vec,  sect.out.info$note.vec)
      proc.categ.mat <- rbind(proc.categ.mat, sect.out.info$category.mat)
      #print(proc.indic.vec)
      #print(proc.note.vec)
      #print(proc.categ.mat)
    } else {
      proc.dat.sht[[i]] <- list(sect.out.info$indicator.mat, sect.out.info$note.mat)
    }
  }

  if(out.format=="vector") {
    colnames(proc.categ.mat) <- c("Class","Subclass","Attribute")
    all.proc.info <- list(
      proc.indic.vec,
      proc.note.vec,
      proc.categ.mat
    )
    names(all.proc.info) <- c("indicator.vec","note.vec","category.mat")
    #print(all.proc.info)
  } else { # for matrix partitionrd output:
    all.proc.info <- proc.dat.sht
    names(all.proc.info) <- paste0("Section",1:num.sects)
  }

  if(add.other.rm==TRUE) {
    all.proc.info <- remove.all.add.other.FIX(all.proc.info, out.format = out.format, printQ)
  }

  return(all.proc.info)
}
