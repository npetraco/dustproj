library(dustproj)

options(max.print=1000000)
fdat  <- read.csv("tests/dust_data/old_formats_use_file_names.csv", header=T)
fnmes <- as.matrix(fdat[,5])
snmes <- as.matrix(fdat[,2])
rootd <- "tests/dust_data/old_formats/"
# Load an empty reference datasheet
empty.ref.datasheet.info <- read.datasheet(fpath = "inst/reference_datasheet.xlsx",out.format = "vector")

ii <- 81   # Adds category and broke. Seems to write and re-load ok with new perilte categories.
ii <- 354  # Breaks again here. Datasheet was empty. Even the very original was. Probably forgot to save. Replaced it BR2. LR1 and LR2 were all the same
ii <- 358  # Check

fnmes[ii]
snmes[ii]

study.conv.tbl  <- parse.conversion.table(
  fpath.convertion.table="inst/category_conversion_tables3.xlsx",
  study.name = snmes[ii])

converted.old.study.datasheet.info <- study2ref.datasheet(
  study.datasheet.file.path = paste0(rootd,fnmes[ii]),
  study.name                = snmes[ii],
  study2ref.conversion.info = study.conv.tbl,
  reference.template.info   = empty.ref.datasheet.info,
  print.lvl                 = 0)
converted.old.study.datasheet.info

# Write converted, embeded datasheet
write.datasheet(
  datasheet.info = converted.old.study.datasheet.info,
  out.fpath="tests/dust_data/testf.xlsx",
  section.class.order=NULL)

# Load the converted, embeded, newly written datasheet for checking
re.datasheet.info <- read.datasheet(fpath = "tests/dust_data/testf.xlsx",out.format = "vector")
warnings()
assign("last.warning", NULL, envir = baseenv())

cmpr.mat <- compare.common(re.datasheet.info, converted.old.study.datasheet.info, stop.for.mismatchQ = T, printQ = F)
cmpr.mat
