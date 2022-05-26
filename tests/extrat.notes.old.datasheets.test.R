library(dustproj)

options(max.print=1000000)

study.name <- "FloridaPhase2"

rootd <- "tests/dust_data/old_formats/"
setwd(rootd)

extract.old.datasheet.notes(fpath.datasheet="UCF5 BR2.xlsx", study.name=study.name, print.level=0)
