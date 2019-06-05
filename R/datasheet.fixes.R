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

