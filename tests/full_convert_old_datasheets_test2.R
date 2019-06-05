library(dustproj)

options(max.print=1000000)

study.name <- "FloridaPhase2"
tfp <- "tests/dust_data/old_formats/UCF021LR2_mod.xlsx"
#rootd <- "tests/dust_data/old_formats/"
#setwd(rootd)

study.conv.tbl  <- parse.conversion.table(
  fpath.convertion.table="inst/category_conversion_tables3.xlsx",
  study.name = study.name)

junk <- read.datasheet(fpath = "inst/reference_datasheet.xlsx",out.format = "vector")
junk$category.mat
junk$indicator.vec

