library(dustproj)

options(max.print=1000000)


study.name <- "Original"
study.conversion.table.info <- parse.conversion.table.expt3("inst/category_conversion_tables3.xlsx", study.name = study.name)

#fpth <- "inst/Original_datasheet_sfsc-mod.xlsx"
#fpth <- "tests/dust_data/old_formats/dust_table_12.1_sfsc-mod.xlsx"
#fpth <- "tests/dust_data/old_formats/dust_table_24.2_sfsc-mod.xlsx"
#fpth <- "tests/dust_data/old_formats/dust_table_1.5_sfsc-mod.xlsx"
fpth <- "tests/dust_data/old_formats/dust_table_14.2_sfsc-mod.xlsx"

test.a.sheet.conversion(datasheet.file.path = fpth,
                        study.name = study.name,
                        conversion.sheet.file.path = "inst/category_conversion_tables3.xlsx",
                        print.lvl=0)
