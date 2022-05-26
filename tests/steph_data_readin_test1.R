library(dustproj)

load.datasheets("/Users/karen2/latex/papers/dust/steph_diss/data/Data/D1.1.xlsx")
load.datasheets("/Users/karen2/latex/papers/dust/DUST_PROJECT/dustproj/tests/dust_data/old_formats/DB14_mod.xlsx")

tp1 <- "/Users/karen2/latex/papers/dust/steph_diss/data/Data/D20.4.xlsx"
in.fpths[1]
load.datasheets(c(in.fpths[1],tp1))

read.datasheet(fpath = in.fpths[1], out.format = "vector")
read.datasheet(fpath = tp1, out.format = "vector")

tp2 <- "/Users/karen2/latex/papers/dust/steph_diss/data/Data/D1.1.xlsx"
read.datasheet(fpath = tp2, out.format = "vector")
read.xlsx(tp2, 1, header = FALSE)

read.xlsx(in.fpths[1], 1, header = FALSE)


tp3 <- "/Users/karen2/latex/papers/dust/steph_diss/data/Data/D1.1_steph_test.xlsx"
read.xlsx(tp3, 1, header = FALSE)
read.datasheet(fpath = tp3, out.format = "vector")

read.xlsx("inst/reference_datasheet.xlsx", 1, header = FALSE)
read.datasheet(fpath = "inst/reference_datasheet.xlsx", out.format = "vector", printQ = T)

junk1 <- read.datasheet.FIX(fpath = "inst/reference_datasheet.xlsx", out.format = "vector",
                            printQ = T, add.other.rm = T)
junk1

junk2 <- read.datasheet.FIX(fpath = tp3, out.format = "vector", printQ = T, add.other.rm = F)
junk2

junk3 <- read.datasheet.FIX(fpath = in.fpths[1], out.format = "vector", printQ = T,
                            add.other.rm = T)
junk3



junk4 <- read.datasheet.FIX(fpath = "inst/reference_datasheet.xlsx", out.format = "matrix",
                            printQ = F, add.other.rm = T)
junk4

junk5 <- read.datasheet.FIX(fpath = tp3, out.format = "matrix", printQ = T, add.other.rm = T)
junk5

junk6 <- read.datasheet.FIX(fpath = in.fpths[1], out.format = "matrix", printQ = T,
                            add.other.rm = T)
junk6
