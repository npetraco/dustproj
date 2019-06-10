#' Load in reference format data sheets
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
load.datasheets<-function(dsheet.filepaths, order.infoQ=TRUE){

  # Initalize a master category matrix, indicator matrix and note matrix from the empty reference datasheet:
  empt.ref.dsheet.info <- read.datasheet(fpath = "inst/reference_datasheet.xlsx", out.format = "vector", add.other.rm = F)

  master.category.mat  <- tolower(empt.ref.dsheet.info$category.mat) # There are caps, but there should be no spaces.
  p.ini                <- nrow(master.category.mat)                  # initial number of categories (variables, so we call it p)
  n.ini                <- length(dsheet.filepaths)                   # number of datasheet files (observations, so we call it n)
  master.indicator.mat <- array(0,  c(p.ini, n.ini))                 # Usually we do nxp instead of pxn. Reversed because the data is pretty big, and I think it's easier to think about this way
  master.note.mat      <- array("", c(p.ini, n.ini))                 # We can always transpose later to nxp

  for(i in 1:length(dsheet.filepaths)) { # ****NOTE: i is the COLUMN index and j is the ROW index of the master matrices, OPPOSITE of the way we usually do it

    print(paste("File", i, dsheet.filepaths[i]))

    # Load a REFERENCE FORMAT datasheet
    dsheet.info <- read.datasheet(fpath = dsheet.filepaths[i], out.format = "vector")

    # Loop over the rows of the loaded datasheet:
    for(j in 1:nrow(dsheet.info$category.mat)) {

      dsheet.categ.vec     <- clean.chars(char.to.rm = " ", char.vec = dsheet.info$category.mat[j,]) # Grab a category row from the file. Set to lowercase and remove any spaces.
      master.categ.row.idx <- get.row.idx(dsheet.categ.vec, master.category.mat)                     # Grab the master index of row

      # Examine the category of the (file) datasheet and see if it is currently missing in the master
      if(is.na(master.categ.row.idx)) { # If the category is missing in the master, add it in:

        print("****************** New category found! ADDING: ******************")
        print(paste(dsheet.categ.vec[1], dsheet.categ.vec[2], dsheet.categ.vec[3], as.character(dsheet.info$indicator.vec[j])))

        # category not found, so add it to the bottom of master.category.matrix ...
        master.category.mat  <- rbind(master.category.mat, as.vector(dsheet.categ.vec))

        # ...along with the data
        # Indicator response:
        master.indicator.mat <- rbind(master.indicator.mat, rep(0, n.ini))                # need a new row of 0 across the bottom of the indicator matric
        master.indicator.mat[nrow(master.category.mat),i] <- dsheet.info$indicator.vec[j] # now put in the actual response from the file for the new category

        # Add the note too:
        master.note.mat <- rbind(master.note.mat, rep("", n.ini))               # need a new row of "" across the bottom of the indicator matric
        master.note.mat[nrow(master.category.mat),i] <- dsheet.info$note.vec[j] # now put in the actual response from the file for the new category

        # print(rbind(
        #   c(dsheet.categ.vec, dsheet.info$indicator.vec[j]),
        #   c(master.category.mat[nrow(master.category.mat),], as.character(master.indicator.mat[nrow(master.category.mat), i] ))
        # ))

      } else { # If the category is found in the master, just add in the data from the row in the file:
        # category  found, so just input data
        master.indicator.mat[master.categ.row.idx, i] <- dsheet.info$indicator.vec[j]
        master.note.mat[master.categ.row.idx, i]      <- dsheet.info$note.vec[j]

        # print(rbind(
        #   c(dsheet.categ.vec, dsheet.info$indicator.vec[j]),
        #   c(master.category.mat[master.categ.row.idx,], as.character(master.indicator.mat[master.categ.row.idx, i] ))
        #   ))

      }

    }

  }

  master.info <- list(
    master.category.mat,
    master.indicator.mat,
    master.note.mat
  )
  names(master.info) <- c(
    "master.category.mat",
    "master.indicator.mat",
    "master.note.mat"
  )

  if(order.infoQ == TRUE){
    master.info <- order.master.info(master.info)
  }

  return(master.info)

}
