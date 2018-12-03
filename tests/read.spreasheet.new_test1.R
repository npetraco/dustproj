library(dustproj)

junk <- read.xlsx("inst/reference_datasheet.xlsx",1,header=FALSE)
dim(junk)
class(junk[,1])
which(is.na(junk[,1]) == F)
which(is.na(junk[,1]) == F)-1


read.datasheet("inst/reference_datasheet.xlsx", out.format="vector", printQ = F, snum.tmp=3)

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
