options(max.print=1000000)

std.pth <- "inst/Original_datasheet.xlsx"
std.nme <- "Original"
junk  <- parse.study.datasheet.expt(std.pth, study.name = std.nme)
junk

junk3 <- parse.conversion.table.expt3("inst/category_conversion_tables2c.xlsx", study.name = std.nme)
junk3$class.conversions
junk3$subclass.conversions
junk3$attribute.conversions

ji <- 534 # glass/mineralgrains to ref various
junk$study.flattened.datasheet[ji, ]
convert.study.row.expt2(junk$study.flattened.datasheet[ji, ], junk3)

ji <- 426 # glass/mineralgrains to ref glass/mineralgrains
junk$study.flattened.datasheet[ji, ]
convert.study.row.expt2(junk$study.flattened.datasheet[ji, ], junk3)

ji <- 413 # mineral fibers "other" subclass  should fail??
junk$study.flattened.datasheet[ji, ]
convert.study.row.expt2(junk$study.flattened.datasheet[ji, ], junk3)

ji <- 539 # glass/mineralgraind "other" attribute should fail??
junk$study.flattened.datasheet[ji, ]
convert.study.row.expt2(junk$study.flattened.datasheet[ji, ], junk3)

# Subclass that is not in the conversion sheet. Should fail:
fakerow <- c("mineralfibers", "yak","is.present?")
convert.study.row.expt2(fakerow, junk3)

# Another Subclass that is not in the conversion sheet. This Should fail too
fakerow <- c("glass/mineralgrains", "other","is.present?")
convert.study.row.expt2(fakerow, junk3)
#
# See if all rows in the study data sheet convert
for(i in 105:nrow(junk$study.flattened.datasheet)) {
  print(paste("Row:",i))
  jconv.row <- convert.study.row.expt2(junk$study.flattened.datasheet[i, ], junk3)
  print("Row was v")
  print(junk$study.flattened.datasheet[i, ])
  print("Row now v")
  print(jconv.row)
  print("++++++++++++++++++++++++++++++++++++++++++++++")
}
#
junk$study.flattened.datasheet[450,]
# Problems:
# row 50-60 and 83-93
# ref.class         ref.subclass            study.class       study.subclass
# [1,] "syntheticfibers" "rugnylontriangularx-s" "syntheticfibers" "rugnylon"
# [2,] "syntheticfibers" "nylonanyx-s"           "syntheticfibers" "rugnylon"
# row 94-104
# ref.class         ref.subclass                study.class       study.subclass
# [1,] "syntheticfibers" "rugpolyestertriangularx-s" "syntheticfibers" "rugpolyester"
# [2,] "syntheticfibers" "polyesteranyx-s"           "syntheticfibers" "rugpolyester"

# REPLACE IN ALL ORIGINAL DATASHEETS THE REFERENCE SUBCLASSES FOR SYNTHETIC FIBERS??? ABOUT 303
#
#
#

