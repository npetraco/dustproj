library(dustproj)

options(max.print=1000000)

junk <- convert.datasheet2("inst/Original_datasheet.xlsx", study.name = "Original", "inst/category_conversion_tables2a.xlsx", print.level = 0)
junk <- convert.datasheet2("inst/FloridaPhase1_datasheet.xlsx", study.name = "FloridaPhase1", "inst/category_conversion_tables2a.xlsx", print.level = 0)
junk <- convert.datasheet2("inst/FloridaPhase2_datasheet2_mod.xlsx", study.name = "FloridaPhase2", "inst/category_conversion_tables2a.xlsx", print.level = 0)
junk <- convert.datasheet2("inst/WTCk_datasheet.xlsx", study.name = "WTCk", "inst/category_conversion_tables2a.xlsx", print.level = 1)
junk <- convert.datasheet2("inst/WTCq_datasheet.xlsx", study.name = "WTCq", "inst/category_conversion_tables2a.xlsx", print.level = 2)


a <- "a#g abcdefgtdkfef_jpg>pple"
sub("#g.*jpg>", "", a)
gsub("“", "", c("rugnylon“", "rugpolyester“", "olefinanyx-s", "nylon“", "polyester“"), fixed = TRUE)


junk <- parse.study.datasheet.expt("inst/Original_datasheet.xlsx", study.name = "Original")
junk <- parse.study.datasheet.expt("inst/FloridaPhase1_datasheet.xlsx", study.name = "FloridaPhase1")
junk <- parse.study.datasheet.expt("inst/FloridaPhase2_datasheet2_mod.xlsx", study.name = "FloridaPhase2")
junk <- parse.study.datasheet.expt("inst/WTCk_datasheet.xlsx", study.name = "WTCk")
junk <- parse.study.datasheet.expt("inst/WTCq_datasheet.xlsx", study.name = "WTCq")
head(junk$study.flattened.datasheet)
junk$study.flattened.datasheet


junk2 <- parse.conversion.table.expt("inst/category_conversion_tables2b.xlsx", study.name = "Original")
junk2$cl.scl.conversions
junk2$attribs.conversions
dim(junk2$cl.scl.conversions)
dim(junk2$attribs.conversions)
junk2$attribs.conversions

junk2$cl.scl.conversions

jkrow <- 287
junk$study.flattened.datasheet[jkrow,]

junk$study.flattened.datasheet[jkrow,1]
jcl.idx <- which(junk2$cl.scl.conversions[,2] == junk$study.flattened.datasheet[jkrow,1])

junk$study.flattened.datasheet[jkrow,2]
jsc.idx <- which(junk2$cl.scl.conversions[,2] == junk$study.flattened.datasheet[jkrow,2])

junk2$cl.scl.conversions[jcl.idx,1]
junk2$cl.scl.conversions[jsc.idx,1]

junk$study.flattened.datasheet[1,]
junk2$attribs.conversions
