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
