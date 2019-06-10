# check master category record
# add to master category record
# organize master category record

library(dustproj)

options(max.print=1000000)
fdat  <- read.csv("tests/dust_data/old_formats_use_file_names.csv", header=T)
fnmes <- as.matrix(fdat[,5])
in.prefix <- "tests/dust_data/ref_formats/ref_format_"

in.fpths <- paste0(in.prefix,fnmes)
junk <- load.datasheets(in.fpths)

junk$master.category.mat
junk$master.indicator.mat
data.frame(junk$master.category.mat,
           junk$master.indicator.mat[,79:85]
)
fnmes[79:85]
126
470

rowSums(junk$master.indicator.mat)
plot(1:nrow(junk$master.indicator.mat), rowSums(junk$master.indicator.mat), typ="h")


junk2 <- load.datasheets(in.fpths, order.infoQ = T)
plot(1:nrow(junk2$master.indicator.mat), rowSums(junk2$master.indicator.mat), typ="h")

junk1a <- order.master.info(junk)
junk1a$master.category.mat
plot(1:nrow(junk1a$master.indicator.mat), rowSums(junk1a$master.indicator.mat), typ="h")
