library(dustproj)

options(max.print=1000000)
fdat  <- read.csv("tests/dust_data/old_formats_use_file_names.csv", header=T)

dat    <- read.csv("tests/dust_data/dust.data.as-of-6-10-19.csv", header=T)
dim(dat)

categs <- as.matrix(dat[,1:3])
X      <- t(dat[,4:ncol(dat)])
dim(X) # Should be n-observations by p-categories

lbl.loc  <- fdat[,3]
lbl.room <- fdat[,4]


fnmes <- as.matrix(fdat[,5])
snmes <- as.matrix(fdat[,2])
rootd <- "tests/dust_data/old_formats/"

#options(warn=1)
study.nme.prev <- ""
for(i in 1:length(fnmes)){

  study.nme <- snmes[i]

  # If study name changes get new conversion table
  if(study.nme != study.nme.prev){
    print(paste(i, study.nme, "*******************************************************************************"))
    study.conv.tbl  <- parse.conversion.table(
      fpath.convertion.table="inst/category_conversion_tables3.xlsx",
      study.name = study.nme)
  }
  fpth <- paste0(rootd,fnmes[i])
  print(paste(i, "Started working on:", fpth, "for:", study.nme))

  # Load an empty reference datasheet
  empty.ref.datasheet.info <- read.datasheet(fpath = "inst/reference_datasheet.xlsx",out.format = "vector")

  # Convert an old study datasheet to reference format and embed it in an empty reference style datasheet in vector form.
  # Note: any categories encountered in the old study data sheet NOT contained in the reference will be added. Warnings
  #       should be thrown however.
  converted.old.study.datasheet.info <- study2ref.datasheet(
    study.datasheet.file.path = fpth,
    study.name                = study.nme,
    study2ref.conversion.info = study.conv.tbl,
    reference.template.info   = empty.ref.datasheet.info,
    print.lvl                 = 0)


  # Info from the dust CSV file
  re.datasheet.info <- list(
    categs,
    X[i,],
    rep("",nrow(categs))
  )
  names(re.datasheet.info) <- c("category.mat","indicator.vec","note.vec")

  # Compare the info in the CSV file with the convertged data in the old study:
  cmpr.mat <- compare.common(re.datasheet.info, converted.old.study.datasheet.info, stop.for.mismatchQ = T, printQ = F)

  study.nme.prev <- study.nme
  print(paste(i, "Success:", fpth, "for:", study.nme.prev))
  print("------------------------")
  #assign("last.warning", NULL, envir = baseenv())

}

