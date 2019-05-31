library(dustproj)

std.pth <- "inst/Original_datasheet.xlsx"
std.nme <- "Original"

std.pth <- "inst/FloridaPhase1_datasheet.xlsx"
std.nme <-"FloridaPhase1"

std.pth <- "inst/FloridaPhase2_datasheet2_mod.xlsx"
std.nme <-"FloridaPhase2"

std.pth <- "inst/WTCk_datasheet.xlsx"
std.nme <-"WTCk"

std.pth <- "inst/WTCq_datasheet.xlsx"
std.nme <-"WTCq"

options(max.print=1000000)
junk  <- parse.study.datasheet.expt(std.pth, study.name = std.nme)
head(junk$study.flattened.datasheet)
junk$study.flattened.datasheet

junk2 <- parse.conversion.table.expt2("inst/category_conversion_tables2b.xlsx", study.name = std.nme)
junk2$cl.scl.conversions
junk2$attribs.conversions
dim(junk2$cl.scl.conversions)
dim(junk2$attribs.conversions)
