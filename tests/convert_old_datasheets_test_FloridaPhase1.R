library(dustproj)

options(max.print=1000000)

study.name <- "FloridaPhase1"


test.a.sheet.conversion(datasheet.file.path = "inst/FloridaPhase1_datasheet.xlsx",
                       study.name = study.name,
                       conversion.sheet.file.path = "inst/category_conversion_tables3.xlsx",
                       print.lvl=2)


rootd <- "tests/dust_data/old_formats/"


test.a.sheet.conversion(datasheet.file.path = paste0(rootd,"DB14_mod.xlsx"),
                        study.name = study.name,
                        conversion.sheet.file.path = "inst/category_conversion_tables3.xlsx",
                        print.lvl=2)

test.a.sheet.conversion(datasheet.file.path = paste0(rootd,"DB28_mod.xlsx"),
                        study.name = study.name,
                        conversion.sheet.file.path = "inst/category_conversion_tables3.xlsx",
                        print.lvl=2)

test.a.sheet.conversion(datasheet.file.path = paste0(rootd,"DB48.xlsx"),
                        study.name = study.name,
                        conversion.sheet.file.path = "inst/category_conversion_tables3.xlsx",
                        print.lvl=2)

test.a.sheet.conversion(datasheet.file.path = paste0(rootd,"DB49_mod.xlsx"),
                        study.name = study.name,
                        conversion.sheet.file.path = "inst/category_conversion_tables3.xlsx",
                        print.lvl=2)
