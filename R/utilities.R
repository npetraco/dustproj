#' Clean out spaces and set all characters to lowercase in a character vector
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
clean.chars<-function(char.vec, char.to.rm){

  cleaned.char.vec <- sapply(1:length(char.vec), function(xx){gsub(char.to.rm, "", tolower(char.vec[xx]), fixed = TRUE)})
  return(cleaned.char.vec)

}
