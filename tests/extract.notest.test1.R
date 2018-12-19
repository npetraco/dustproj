library(dustproj)

rawd <- read_excel(path = "inst/FloridaPhase1_datasheet.xlsx", col_names=F)
rawd <- read_excel(path = "inst/FloridaPhase2_datasheet.xlsx", col_names=F)
rawd <- read_excel(path = "inst/Original_datasheet.xlsx", col_names=F)
rawd <- read_excel(path = "inst/WTCk_datasheet.xlsx", col_names=F)
rawd <- read_excel(path = "inst/WTCq_datasheet.xlsx", col_names=F)


which(tolower(gsub(" ", "", rawd[,1], fixed = TRUE)) == "humanhairnatural")
which(tolower(gsub(" ", "", rawd[,1], fixed = TRUE)) == "humanhairtreated")
which(tolower(gsub(" ", "", rawd[,1], fixed = TRUE)) == "syntheticfibers")
which(tolower(gsub(" ", "", rawd[,1], fixed = TRUE)) == "animalhair")
which(tolower(gsub(" ", "", rawd[,1], fixed = TRUE)) == "naturalfibers")
which(tolower(gsub(" ", "", rawd[,1], fixed = TRUE)) == "mineralfibers")
which(tolower(gsub(" ", "", rawd[,1], fixed = TRUE)) == "glass/mineralgrains")
which(tolower(gsub(" ", "", rawd[,1], fixed = TRUE)) == "feathers") # Starts the Various category

which(tolower(gsub(" ", "", rawd[,1], fixed = TRUE)) == "feathers()") # Starts the Various category in Original_datasheet

extract.old.datasheet.notes("inst/Original_datasheet.xlsx",      study.name = "Original",      print.level=1)
extract.old.datasheet.notes("inst/FloridaPhase1_datasheet.xlsx", study.name = "FloridaPhase1", print.level=1)
extract.old.datasheet.notes("inst/FloridaPhase2_datasheet.xlsx", study.name = "FloridaPhase2", print.level=1)
extract.old.datasheet.notes("inst/WTCk_datasheet.xlsx",          study.name = "WTCk",          print.level=1)
extract.old.datasheet.notes("inst/WTCq_datasheet.xlsx",          study.name = "WTCq",          print.level=1)

rawd[1:4,]

max(which( !is.na(rawd[1,]) == TRUE))
rawd[2:4,]
