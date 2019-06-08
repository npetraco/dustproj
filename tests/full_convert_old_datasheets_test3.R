library(dustproj)

options(max.print=1000000)

study.name <- "Original"
tfp <- "tests/dust_data/old_formats/dust_table_49.1_sfsc-mod.xlsx"
#rootd <- "tests/dust_data/old_formats/"
#setwd(rootd)

# Get the conversion table for the PARTICULR study
study.conv.tbl  <- parse.conversion.table(
  fpath.convertion.table="inst/category_conversion_tables3.xlsx",
  study.name = study.name)

# Load an empty reference datasheet
empty.ref.datasheet.info <- read.datasheet(fpath = "inst/reference_datasheet.xlsx",out.format = "vector")
empty.ref.datasheet.info$category.mat
empty.ref.datasheet.info$indicator.vec
empty.ref.datasheet.info$note.vec

# Convert an old study datasheet to reference format and embed it in an empty reference style datasheet in vector form.
# Note: any categories encountered in the old study data sheet NOT contained in the reference will be added. Warnings
#       should be thrown however.
converted.old.study.datasheet.info <- study2ref.datasheet(
  study.datasheet.file.path = tfp,
  study.name                = study.name,
  study2ref.conversion.info = study.conv.tbl,
  reference.template.info   = empty.ref.datasheet.info,
  print.lvl                 = 0)

converted.old.study.datasheet.info$indicator.vec
converted.old.study.datasheet.info$note.vec
data.frame(converted.old.study.datasheet.info$category.mat, converted.old.study.datasheet.info$indicator.vec)

write.datasheet(
  datasheet.info = converted.old.study.datasheet.info,
  out.fpath="tests/dust_data/testf.xlsx",
  section.class.order=NULL)

# Load an empty reference datasheet
re.datasheet.info <- read.datasheet(fpath = "tests/dust_data/testf.xlsx",out.format = "vector")
re.datasheet.info$category.mat
re.datasheet.info$indicator.vec
re.datasheet.info$note.vec
warnings()

#read.datasheet(fpath = "tests/dust_data/testf.xlsx",out.format = "matrix")
# junk <- read.xlsx("tests/dust_data/testf.xlsx", 1, header = FALSE)
# junk
#
# junk2 <- read.xlsx(tfp, 1, header = FALSE)
# junk2
#
# junk3 <- read.xlsx("inst/reference_datasheet.xlsx", 1, header = FALSE)
# junk3
# dim(junk3)

re.datasheet.info$category.mat
re.datasheet.info$indicator.vec
re.datasheet.info$note.vec

converted.old.study.datasheet.info$category.mat
converted.old.study.datasheet.info$indicator.vec
converted.old.study.datasheet.info$note.vec

# Write check sheet read in to compare a re-written datasheet with a converted datasheet
# we want to make sure the category names and data responses are the same
compare.common(re.datasheet.info, converted.old.study.datasheet.info, stop.for.mismatchQ = T, printQ = T)

