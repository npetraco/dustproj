library(dustproj)

options(max.print=1000000)


study.name <- "Original"
study.conversion.table.info <- parse.conversion.table.expt3("inst/category_conversion_tables3.xlsx", study.name = study.name)

#fpth <- "inst/Original_datasheet_sfsc-mod.xlsx"
fpth <- "tests/dust_data/old_formats/dust_table_12.1_sfsc-mod.xlsx"
parsed.study.datasheet.info  <- parse.study.datasheet.expt(fpth, study.name = study.name)
dsheet.nodes                 <- parsed.study.datasheet.info$study.flattened.datasheet
# Need to add 1/0 and notes

dsheet.nodes

for(i in 1:nrow(dsheet.nodes)) {
  print(paste("Row:",i))
  row.new <- convert.study.row.expt2(dsheet.nodes[i, ], study.conversion.table.info)
  # compre <- cbind(
  #   as.matrix(dsheet.nodes[i, ]),
  #   t(as.matrix(row.new))
  # )
  # print(compre)
  # print("++++++++++++++++++++++++++++++++++++++++++++++")
}



