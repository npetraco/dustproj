library(dustproj)

options(max.print=1000000)

study.name <- "FloridaPhase1"
#rootd <- "tests/dust_data/old_formats/"
#setwd(rootd)

study.conv.tbl  <- parse.conversion.table(
  fpath.convertion.table="inst/category_conversion_tables3.xlsx",
  study.name = study.name)
study.conv.tbl$class.conversions
study.conv.tbl$subclass.conversions
study.conv.tbl$attribute.conversions

tfp <- "tests/dust_data/old_formats/DB14_mod.xlsx"
pods <- parse.study.datasheet(
  fpath.datasheet =tfp,
  study.name = study.name)

pods.df <- data.frame(
  pods$study.flattened.datasheet,
  pods$study.flattened.data)

pods$studys.unique.classes
pods$studys.unique.subclasses
pods$studys.unique.attributes


junk <- convert.study.datasheet(
  study.datasheet.file.path = tfp,
  study.name                = study.name,
  study2ref.conversion.info = study.conv.tbl,
  print.lvl                 = 0)
junk$skip.idxs
junk$full.converted.df
junk$reduced.converted.df
junk$full.converted.df[junk$skip.idxs,]
pods.df[junk$skip.idxs,] # shoot a warning when tossing something with a 1


tv <- convert.study.row(pods$study.flattened.datasheet[491, ], study.conv.tbl)
tv
"skip" %in% tv

tv <- convert.study.row(pods$study.flattened.datasheet[347, ], study.conv.tbl)
tv
"skip" %in% tv

tv <- convert.study.row(pods$study.flattened.datasheet[9, ], study.conv.tbl)
tv
"skip" %in% tv

