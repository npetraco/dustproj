#' Replace the range of subclass names for a class
#'
#' Reads in the datasheet excel file and writes the modified excel file
#'
#' The function will XXXX
#'
#' @param XX The XX
#' @return The function will XX
#'
#'
#' @export
replace.a.subclass<-function(in.fpath.datasheet, subclass.col.range, replacement.names, out.fpath.datasheet){

  dat<-read.xlsx(in.fpath.datasheet,1,header=FALSE)

  # Subclass names are all in the first column
  dat[,1] <- as.matrix(dat[,1])

  # Make sure length of replacements is the same as the range to be replaced
  if(length(subclass.col.range) != length(replacement.names)) {
    stop("length(subclass.col.range) != length(replacement.names)")
  }

  # Do the replacement
  dat[subclass.col.range,1] <- as.matrix(replacement.names)

  # Write the result to an excel file
  write.xlsx(dat, file = out.fpath.datasheet, col.names = F, row.names = F, showNA = F)
  print(paste("Wrote:", out.fpath.datasheet))

}


#' Replace the range of subclass names for a class
#'
#' Reads in the datasheet excel file and writes the modified excel file
#'
#' The function will XXXX
#'
#' @param XX The XX
#' @return The function will XX
#'
#'
#' @export
replace.class.attributes<-function(in.fpath.datasheet, class.name, attributes.row.range, replacement.names, out.fpath.datasheet){

  dat <- read.xlsx(in.fpath.datasheet,1,header=FALSE)
  dat <- as.matrix(dat)

  # Class names are all in the first column
  classes.and.subclasses <- as.matrix(dat[,1])
  class.idx              <- which(classes.and.subclasses == class.name)

  # Make sure length of replacements is the same as the range to be replaced
  if(length(attributes.row.range) != length(replacement.names)) {
    stop("length(attributes.row.range) != length(replacement.names)")
  }

  # Do the replacement
  dat[class.idx, attributes.row.range] <- replacement.names
  #print(dat[class.idx, attributes.row.range])

  # Write the result to an excel file
  write.xlsx(dat, file = out.fpath.datasheet, col.names = F, row.names = F, showNA = F)
  print(paste("Wrote:", out.fpath.datasheet))

}


#' Test a datasheet conversion line by line and see if anything pops
#'
#' NOTE: Reloads conversion sheet every time run.
#'
#' Good for testing for missing subclasses and attributes in the conversion table
#'
#' @param XX The XX
#' @return The function will XX
#'
#'
#' @export
test.a.sheet.conversion<-function(datasheet.file.path, study.name, conversion.sheet.file.path, print.lvl=0){

  conv.info        <- parse.conversion.table(conversion.sheet.file.path, study.name = study.name)
  parsed.dsht.info <- parse.study.datasheet(datasheet.file.path, study.name = study.name)
  flat.dsht        <- parsed.dsht.info$study.flattened.datasheet

  for(i in 1:nrow(flat.dsht)) {

    if(print.lvl>0){
      print(paste("Row:",i))
    }

    row.new <- convert.study.row(flat.dsht[i, ], conv.info)

    if(print.lvl>1){
      compre <- cbind(
        as.matrix(flat.dsht[i, ]),
        t(as.matrix(row.new))
      )
      print(compre)
      print("++++++++++++++++++++++++++++++++++++++++++++++")
    }
  }

  print("Success if we got here!")

}


#' Cross compare two datasheets that shoud have the same common entries
#'
#' XXXX
#'
#' Good for testing a converted old study datasheet against its embeding into a reference format datasheet
#'
#' @param XX The XX
#' @return The function will XX
#'
#'
#' @export
compare.common<-function(dsheet.info1, dsheet.info2, stop.for.mismatchQ=FALSE, printQ=FALSE){

  cm1       <- dsheet.info1$category.mat
  iv1       <- dsheet.info1$indicator.vec
  chk.idxs1 <- which(iv1 == 1)       # Get the 1s categories in datasheet1

  cm2       <- dsheet.info2$category.mat
  iv2       <- dsheet.info2$indicator.vec
  chk.idxs2 <- which(iv2 == 1)       # Get the 1s categories in datasheet1

  if(length(chk.idxs1) != length(chk.idxs2)) {
    stop("Something went wrong!!!! The number of 1s in sheet 1 differs from the number of 1s in sheet 2!!!!!")
  }

  # The 1s categories in datasheet1 should be 1s categories in datasheet 2
  results.1in2 <- data.frame(array(NA, c(length(chk.idxs1),3)))
  for(i in 1:length(chk.idxs1)){

    cm1.row      <- cm1[chk.idxs1[i],]
    iv1.row.resp <- iv1[chk.idxs1[i]]
    cm2.row.idx  <- get.row.idx(cm1.row, cm2)

    resp.1.2.equalQ <- (iv1.row.resp == iv2[cm2.row.idx])
    if(resp.1.2.equalQ == FALSE){
      print(paste("Sheet1:", cm1.row[1],         cm1.row[2],         cm1.row[3],         "Resp1:", iv1.row.resp))
      print(paste("Sheet2:", cm2[cm2.row.idx,1], cm2[cm2.row.idx,2], cm2[cm2.row.idx,3], "Resp2:", iv2[cm2.row.idx]))
      warning("******* Found: Sheet1 response indicator is 1 but Sheet 2 response indicator NOT THE SAME!!!!!!! BAD!!!!!! *******")
    }


    if(printQ==TRUE) {
      print(paste("Sheet1:", cm1.row[1],         cm1.row[2],         cm1.row[3],         "Resp1:", iv1.row.resp))
      print(paste("Sheet2:", cm2[cm2.row.idx,1], cm2[cm2.row.idx,2], cm2[cm2.row.idx,3], "Resp2:", iv2[cm2.row.idx]))
      if(resp.1.2.equalQ == FALSE){
        print("******* Sheet1 response indicator is 1 but Sheet 2 response indicator NOT THE SAME!!!!!!! BAD!!!!!! *******")
      }
      print("================================================")
    }
    results.1in2[i,] <- data.frame(chk.idxs1[i], cm2.row.idx, resp.1.2.equalQ)
  }

  colnames(results.1in2) <- c("chk.sheet1.idx", "chk.sheet2.idx","sameQ")

  # Option to throw an error if a mismatch in sheet 1 1s categories found
  if(stop.for.mismatchQ == TRUE){

    foundQ = (FALSE %in% results.1in2[,3])

    if(foundQ==TRUE){
      mismatch.idxs <- which(results.1in2[,3] == FALSE)
      print(results.1in2[mismatch.idxs,])
      stop("Mismatch found!!!!!!!!!! Check input datasheets at the indices noted above!!!!!!!")
    }
  }

  return(results.1in2)

}
