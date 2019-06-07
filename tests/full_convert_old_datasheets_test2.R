library(dustproj)

options(max.print=1000000)

study.name <- "FloridaPhase2"
tfp <- "tests/dust_data/old_formats/UCF10BR1_mod.xlsx"
#rootd <- "tests/dust_data/old_formats/"
#setwd(rootd)

study.conv.tbl  <- parse.conversion.table(
  fpath.convertion.table="inst/category_conversion_tables3.xlsx",
  study.name = study.name)

junk <- read.datasheet(fpath = "inst/reference_datasheet.xlsx",out.format = "vector")
junk$category.mat
junk$indicator.vec
junk$note.vec


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

write.datasheet(
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
