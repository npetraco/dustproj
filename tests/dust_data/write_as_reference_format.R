library(dustproj)

#options(max.print=1000000)
fdat  <- read.csv("tests/dust_data/old_formats_use_file_names.csv", header=T)
fnmes <- as.matrix(fdat[,5])
snmes <- as.matrix(fdat[,2])
rootd <- "tests/dust_data/old_formats/"
outd <- "tests/dust_data/ref_formats/"

# Load an empty reference datasheet
empty.ref.datasheet.info <- read.datasheet(fpath = "inst/reference_datasheet.xlsx",out.format = "vector")


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

  # Convert an old study datasheet to reference format and embed it in an empty reference style datasheet in vector form.
  # Note: any categories encountered in the old study data sheet NOT contained in the reference will be added. Warnings
  #       should be thrown however.
  converted.old.study.datasheet.info <- study2ref.datasheet(
    study.datasheet.file.path = fpth,
    study.name                = study.nme,
    study2ref.conversion.info = study.conv.tbl,
    reference.template.info   = empty.ref.datasheet.info,
    print.lvl                 = 0)

  # Write converted, embeded datasheet
  write.datasheet(
    datasheet.info = converted.old.study.datasheet.info,
    out.fpath=paste0(outd,"ref_format_",fnmes[i]),
    section.class.order=NULL)

  study.nme.prev <- study.nme
  print(paste(i, "Done:", fpth, "for:", study.nme.prev))
  print("------------------------")
}
warnings()


