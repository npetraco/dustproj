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


jkrow <- 287
junk$study.flattened.datasheet[jkrow,]

junk$study.flattened.datasheet[jkrow,1]
junk2$cl.scl.conversions[,2]
junk$study.flattened.datasheet[jkrow,1]
jcl.idx <- which(junk2$cl.scl.conversions[,2] == junk$study.flattened.datasheet[jkrow,1])
jcl.idx
junk2$cl.scl.conversions[jcl.idx, 2]
junk2$cl.scl.conversions[jcl.idx, 1]


junk$study.flattened.datasheet[jkrow,2]
jsc.idx <- which(junk2$cl.scl.conversions[,2] == junk$study.flattened.datasheet[jkrow,2])
junk2$cl.scl.conversions[jsc.idx, 2]
junk2$cl.scl.conversions[jsc.idx, 1]

junk$study.flattened.datasheet[jkrow,]
junk$study.flattened.datasheet[jkrow,3]
which(junk2$attribs.conversions[,3] == junk$study.flattened.datasheet[jkrow,1])

junk$study.flattened.datasheet[jkrow,1]
junk2$attribs.conversions[,3] == junk$study.flattened.datasheet[jkrow,1]
which(junk2$attribs.conversions[,3] == junk$study.flattened.datasheet[jkrow,1])
junk2$attribs.conversions[which(junk2$attribs.conversions[,3] == junk$study.flattened.datasheet[jkrow,1]), ]
junk2$attribs.conversions[which(junk2$attribs.conversions[,3] == junk$study.flattened.datasheet[jkrow,1]), 4]


jkrow <- 182
convert.study.row.expt(junk$study.flattened.datasheet[jkrow,], junk2)
