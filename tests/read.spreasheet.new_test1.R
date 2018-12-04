library(dustproj)

junk <- read.xlsx("inst/reference_datasheet.xlsx",1,header=FALSE)
dim(junk)
class(junk[,1])
which(is.na(junk[,1]) == F)
which(is.na(junk[,1]) == F)-1

junk <- read.xlsx("inst/reference_datasheet.xlsx",1,header=FALSE)
t(t(junk[,1]))
t(junk[,1])

process.data.sheet.section(junk[1:4,], out.format = "matrix")
warnings()

process.data.sheet.section(junk[5:8,], out.format = "matrix")
warnings()

process.data.sheet.section(junk[9:22,], out.format = "matrix")
warnings()

process.data.sheet.section(junk[23:29,], out.format = "matrix")
warnings()

process.data.sheet.section(junk[30:40,], out.format = "matrix")
warnings()

process.data.sheet.section(junk[41:53,], out.format = "matrix")
warnings()

process.data.sheet.section(junk[54:76,], out.format = "matrix")
warnings()
#

junk2 <- read.datasheet("inst/reference_datasheet.xlsx", out.format="vector",printQ = T)
warnings()

junk2$indicator.vec
length(junk2$indicator.vec)
junk2$note.vec
length(junk2$note.vec)
junk2$category.mat
dim(junk2$category.mat)

options(max.print=1000000)

junk3 <- read.datasheet("inst/reference_datasheet.xlsx", out.format="matrix",printQ = T)
junk3$Section1
junk3$Section2
junk3$Section3
junk3$Section4
junk3$Section5
junk3$Section6
junk3$Section7

tv <- c("RugOlefinTriangularX-S,Red","RugOlefinTriangularX-S,Blue")
strsplit(tv,",")

ta <- matrix(c(1,2,3,4,5,6), c(2,3))
ta[-c(1,2,length(0))]
ta[-c(1,2,NULL)]
tta <- t(ta)

as.numeric(ta)
as.numeric(tta)

cta <- matrix(as.character(c(1,2,3,4,5,6)), c(2,3))
tcta <- t(cta)
as.character(tcta)

tvr <- c("1","2","3")
tvc <- c("a","b","c")
tlm <- outer(tvr,tvc,FUN=paste,sep="*")
tlm
as.character(tlm)
