library(dustproj)

options(max.print=1000000)

study.name <- "Original"
tfp <- "tests/dust_data/old_formats/dust_table_15.2_sfsc-mod.xlsx"
#rootd <- "tests/dust_data/old_formats/"
#setwd(rootd)

study.conv.tbl  <- parse.conversion.table(
  fpath.convertion.table="inst/category_conversion_tables3.xlsx",
  study.name = study.name)

junk <- read.datasheet(fpath = "inst/reference_datasheet.xlsx",out.format = "vector")
junk$category.mat
junk$indicator.vec


study2ref.datasheet(
  study.datasheet.file.path = tfp,
  study.name                = study.name,
  study2ref.conversion.info = study.conv.tbl,
  reference.template.info   = junk,
  print.lvl                 = 0)

