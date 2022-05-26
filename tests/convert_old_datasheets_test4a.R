options(max.print=1000000)

std.pth <- "tests/dust_data/old_formats/dust_table_2.5.xlsx"
std.nme <- "Original"
std.pth
std.nme
junk  <- parse.study.datasheet.expt(std.pth, study.name = std.nme)
junk


std.pth <- "inst/FloridaPhase1_datasheet.xlsx"
std.nme <-"FloridaPhase1"

std.pth <- "inst/FloridaPhase2_datasheet2_mod.xlsx"
std.nme <-"FloridaPhase2"

std.pth <- "inst/WTCk_datasheet.xlsx"
std.nme <-"WTCk"

std.pth
std.nme
junk  <- parse.study.datasheet.expt(std.pth, study.name = std.nme)
junk

# test on another random original datasheet and see if the various glass/mineralgrains get labeled as is.present OR by the glass/mineralgrains attributes
# test also on other studies to see what the attribute labeling comes out as
# ANSWER: all these look like they inherit the glass/mineralgrains attributes
