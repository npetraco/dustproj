library(dustproj)

options(max.print=1000000)
fdat  <- read.csv("tests/dust_data/old_formats_use_file_names.csv", header=T)
fnmes <- as.matrix(fdat[,5])
in.prefix <- "tests/dust_data/ref_formats/ref_format_"

in.fpths <- paste0(in.prefix,fnmes)
master.dust.info <- load.datasheets(in.fpths)

rowSums(master.dust.info$master.indicator.mat)
plot(1:nrow(master.dust.info$master.indicator.mat), rowSums(master.dust.info$master.indicator.mat), typ="h")

