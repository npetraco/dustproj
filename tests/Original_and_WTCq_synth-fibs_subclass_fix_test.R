
replacement.syntheticfiber.subclasses <- c(
  "Rug Olefin Triangular X-S",
  "Rug Nylon Triangular X-S",
  "Rug Polyester Triangular X-S",
  "Olefin Any X-S",
  "Nylon Any X-S",
  "Polyester Any X-S",
  "Acrylic Dogbone X-S",
  "Acrylic Round X-S",
  "Acrylic Other X-S",
  "Modacrylic",
  "Rayon",
  "Acetate/Triacetate"
)

std.pth <- "inst/Original_datasheet.xlsx"
jdat<-read.xlsx(std.pth,1,header=FALSE)

jdat[,1] <- as.matrix(jdat[,1])
jdat[9:20,1]
length(jdat[9:20,1])
length(replacement.syntheticfiber.subclasses)
jdat[9:20,1] <- as.matrix(replacement.syntheticfiber.subclasses)
jdat[9:20,1]

write.xlsx(jdat, file = "tests/Original_synthfib-fix_test.xlsx",col.names = F, row.names = F, showNA = F)


# Now load re-written datasheet and see what happens:
options(max.print=1000000)

std.pth <- "tests/Original_synthfib-fix_test.xlsx"
std.nme <- "Original"
junk  <- parse.study.datasheet.expt(std.pth, study.name = std.nme)
junk

junk3 <- parse.conversion.table.expt3("inst/category_conversion_tables2d.xlsx", study.name = std.nme)
junk3$class.conversions
junk3$subclass.conversions
junk3$attribute.conversions

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



# Now lets try for WTCq
std.pth <- "inst/WTCq_datasheet.xlsx"
jdat<-read.xlsx(std.pth,1,header=FALSE)

jdat[,1] <- as.matrix(jdat[,1])
jdat[9:20,1]
length(jdat[9:20,1])
length(replacement.syntheticfiber.subclasses)
jdat[9:20,1] <- as.matrix(replacement.syntheticfiber.subclasses)
jdat[9:20,1]

write.xlsx(jdat, file = "tests/WTCq_synthfib-fix_test.xlsx",col.names = F, row.names = F, showNA = F)

std.pth <- "tests/WTCq_synthfib-fix_test.xlsx"
std.nme <- "WTCq"
junk  <- parse.study.datasheet.expt(std.pth, study.name = std.nme)
junk

junk3 <- parse.conversion.table.expt3("inst/category_conversion_tables2d.xlsx", study.name = std.nme)
junk3$class.conversions
junk3$subclass.conversions
junk3$attribute.conversions

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
