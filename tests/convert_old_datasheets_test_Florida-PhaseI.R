options(max.print=1000000)

std.pth <- "inst/FloridaPhase1_datasheet.xlsx"
std.nme <-"FloridaPhase1"
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

ji <- 50 # synthetic fibers "nylon" subclass  cased problems in Original
junk$study.flattened.datasheet[ji, ]
convert.study.row.expt2(junk$study.flattened.datasheet[ji, ], junk3)

ji <- 84 # synthetic fibers alt "nylon" subclass  cased problems in Original
junk$study.flattened.datasheet[ji, ]
convert.study.row.expt2(junk$study.flattened.datasheet[ji, ], junk3)

ji <- 539 # glass/mineralgraind "other" attribute should fail??
junk$study.flattened.datasheet[ji, ]
convert.study.row.expt2(junk$study.flattened.datasheet[ji, ], junk3)

ji <- 587 # does special EXTRA entry for woodfibers-sawdust subclass work??
junk$study.flattened.datasheet[ji, ]
convert.study.row.expt2(junk$study.flattened.datasheet[ji, ], junk3)


# Subclass that is not in the conversion sheet. Should fail:
fakerow <- c("mineralfibers", "yak","is.present?")
convert.study.row.expt2(fakerow, junk3)

# Should  work
fakerow <- c("glass/mineralgrains", "other","is.present?")
convert.study.row.expt2(fakerow, junk3)

#
# See if all rows in the study data sheet convert
for(i in 1:nrow(junk$study.flattened.datasheet)) {
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
# Had to add subclasses to various:
#Paint Chips/smears		Paint Chips/smears					NOTE: Subclass name workaround for some Florida Phase I
#Wood Fibers Saw Dust		Wood fibers-Saw Dust					NOTE: Subclass name workaround for some Florida Phase I
