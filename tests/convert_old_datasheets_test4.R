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
std.pth
std.nme
junk  <- parse.study.datasheet.expt(std.pth, study.name = std.nme)
junk

junk3 <- parse.conversion.table.expt3("inst/category_conversion_tables2b.xlsx", study.name = std.nme)
junk3$class.conversions
junk3$subclass.conversions
junk3$attribute.conversions

head(junk3$subclass.conversions)
junk
ji <- 534 # glass/mineralgrains
ji <- 426 # glass/mineralgrains to ref various
ji <- 413 # mineral fibers other  should fail
ji <- 539 # glass/mineralgraind other  should fail
fakerow <- c("mineralfibers", "yak","is.present?")
fakerow <- c("glass/mineralgrains", "other","is.present?")

junk$study.flattened.datasheet[ji, ]
convert.study.row.expt2(junk$study.flattened.datasheet[ji, ], junk3)

fakerow
convert.study.row.expt2(fakerow, junk3)

# test on another random original datasheet and see if the various glass/mineralgrains get labeled as is.present OR by the glass/mineralgrains attributes
# test also on other studies to see what the attribute labeling comes out as
