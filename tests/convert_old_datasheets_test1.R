library(dustproj)

# convert.datasheet("inst/Original_datasheet.xlsx", study.name = "Original")
# convert.datasheet("inst/FloridaPhase1_datasheet.xlsx", study.name = "FloridaPhase1")
# convert.datasheet("inst/FloridaPhase2_datasheet.xlsx", study.name = "FloridaPhase2") # Throws NAs because of chars in the cells
# convert.datasheet("inst/FloridaPhase2_datasheet2_mod.xlsx", study.name = "FloridaPhase2")
# convert.datasheet("inst/WTCk_datasheet.xlsx", study.name = "WTCk")
# convert.datasheet("inst/WTCq_datasheet.xlsx", study.name = "WTCq")

convert.datasheet("inst/Original_datasheet.xlsx", study.name = "Original", "inst/category_conversion_tables2a.xlsx", print.level = 0)
convert.datasheet("inst/FloridaPhase1_datasheet.xlsx", study.name = "FloridaPhase1", "inst/category_conversion_tables2a.xlsx", print.level = 0)
convert.datasheet("inst/FloridaPhase2_datasheet2_mod.xlsx", study.name = "FloridaPhase2", "inst/category_conversion_tables2a.xlsx", print.level = 0)
convert.datasheet("inst/WTCk_datasheet.xlsx", study.name = "WTCk", "inst/category_conversion_tables2a.xlsx", print.level = 0)
convert.datasheet("inst/WTCq_datasheet.xlsx", study.name = "WTCq", "inst/category_conversion_tables2a.xlsx", print.level = 0)
#


junk <- read.dust.file_ballantyne_study("inst/FloridaPhase2_datasheet.xlsx")
read.dust.file_ballantyne_study("inst/FloridaPhase2_datasheet2_mod.xlsx")
junk[[1]]
junk[[2]]

junk <- read_excel(path = "inst/category_conversion_tables.xlsx", col_names=F)
head(junk)
junk[2,]
class(junk)
dim(junk)
