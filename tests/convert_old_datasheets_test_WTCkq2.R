library(dustproj)

options(max.print=1000000)

study.name <- "WTCq"


test.a.sheet.conversion(datasheet.file.path = "inst/WTCq_datasheet_sfsc-mod.xlsx",
                        study.name = study.name,
                        conversion.sheet.file.path = "inst/category_conversion_tables3.xlsx",
                        print.lvl=2)

test.a.sheet.conversion(datasheet.file.path = "tests/dust_data/old_formats/WTC-Q_Dust from Q flag, tape and rope_samp1_sfsc-mod.xlsx",
                        study.name = study.name,
                        conversion.sheet.file.path = "inst/category_conversion_tables3.xlsx",
                        print.lvl=0)

test.a.sheet.conversion(datasheet.file.path = "tests/dust_data/old_formats/WTC-Q_Dust from Q flag, tape and rope_samp2_sfsc-mod.xlsx",
                        study.name = study.name,
                        conversion.sheet.file.path = "inst/category_conversion_tables3.xlsx",
                        print.lvl=0)

test.a.sheet.conversion(datasheet.file.path = "tests/dust_data/old_formats/WTC-Q_Dust from Q flag, tape and rope_samp3_sfsc-mod.xlsx",
                        study.name = study.name,
                        conversion.sheet.file.path = "inst/category_conversion_tables3.xlsx",
                        print.lvl=0)

test.a.sheet.conversion(datasheet.file.path = "tests/dust_data/old_formats/WTC-Q_Dust from Q flag, tape and rope_samp4_sfsc-mod.xlsx",
                        study.name = study.name,
                        conversion.sheet.file.path = "inst/category_conversion_tables3.xlsx",
                        print.lvl=0)

test.a.sheet.conversion(datasheet.file.path = "tests/dust_data/old_formats/WTC-Q_Dust from Q flag, tape and rope_samp5_sfsc-mod.xlsx",
                        study.name = study.name,
                        conversion.sheet.file.path = "inst/category_conversion_tables3.xlsx",
                        print.lvl=0)


#----------------------------------------------------------------------------------------------
study.name <- "WTCk"


test.a.sheet.conversion(datasheet.file.path = "inst/WTCk_datasheet.xlsx",
                        study.name = study.name,
                        conversion.sheet.file.path = "inst/category_conversion_tables3.xlsx",
                        print.lvl=2)

test.a.sheet.conversion(datasheet.file.path = "tests/dust_data/old_formats/WTC-K_Site_Ground_Zero_911_mod_samp1.xlsx",
                        study.name = study.name,
                        conversion.sheet.file.path = "inst/category_conversion_tables3.xlsx",
                        print.lvl=0)

test.a.sheet.conversion(datasheet.file.path = "tests/dust_data/old_formats/WTC-K_Site_Ground_Zero_911_mod_samp2.xlsx",
                        study.name = study.name,
                        conversion.sheet.file.path = "inst/category_conversion_tables3.xlsx",
                        print.lvl=0)

test.a.sheet.conversion(datasheet.file.path = "tests/dust_data/old_formats/WTC-K_Site_Ground_Zero_911_mod_samp3.xlsx",
                        study.name = study.name,
                        conversion.sheet.file.path = "inst/category_conversion_tables3.xlsx",
                        print.lvl=0)

test.a.sheet.conversion(datasheet.file.path = "tests/dust_data/old_formats/WTC-K_Site_Ground_Zero_911_mod_samp4.xlsx",
                        study.name = study.name,
                        conversion.sheet.file.path = "inst/category_conversion_tables3.xlsx",
                        print.lvl=0)

test.a.sheet.conversion(datasheet.file.path = "tests/dust_data/old_formats/WTC-K_Site_Ground_Zero_911_mod_samp5.xlsx",
                        study.name = study.name,
                        conversion.sheet.file.path = "inst/category_conversion_tables3.xlsx",
                        print.lvl=0)
