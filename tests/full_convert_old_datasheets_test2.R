library(dustproj)

options(max.print=1000000)

study.name <- "Original"
tfp <- "tests/dust_data/old_formats/dust_table_49.1_sfsc-mod.xlsx"
#rootd <- "tests/dust_data/old_formats/"
#setwd(rootd)

study.conv.tbl  <- parse.conversion.table(
  fpath.convertion.table="inst/category_conversion_tables3.xlsx",
  study.name = study.name)

# Load an empty reference datasheet
junk <- read.datasheet(fpath = "inst/reference_datasheet.xlsx",out.format = "vector")
junk$category.mat
junk$indicator.vec
junk$note.vec

# Convert an old study datasheet to reference format and embed it in an empty reference style datasheet in vector form.
# Note: any categories encountered in the old study data sheet NOT contained in the reference will be added. Warnings
#       should be thrown however.
junk2 <- study2ref.datasheet(
  study.datasheet.file.path = tfp,
  study.name                = study.name,
  study2ref.conversion.info = study.conv.tbl,
  reference.template.info   = junk,
  print.lvl                 = 0)

unique(junk2$category.mat[,1])
junk2$indicator.vec
junk2$note.vec
data.frame(junk2$category.mat,junk2$indicator.vec)

junk4 <- write.datasheet(
  datasheet.info = junk2,
  fpath="XXXX",
  section.class.order=NULL,
  printQ=FALSE)


junk3 <- convert.study.datasheet(
  study.datasheet.file.path = tfp,
  study.name                = study.name,
  study2ref.conversion.info = study.conv.tbl,
  print.lvl                 = 0)
junk3$full.converted.df[390,]
junk3$reduced.converted.df[387:396,]
