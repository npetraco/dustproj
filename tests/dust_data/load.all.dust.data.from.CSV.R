library(dustproj)

options(max.print=1000000)
#setwd("/home/npetraco/latex/papers/dust/steph_diss/dust_19/dustproj.lnk/")
fdat  <- read.csv("tests/dust_data/old_formats_use_file_names.csv", header=T)

dat    <- read.csv("tests/dust_data/dust.data.as-of-6-10-19.csv", header=T)
dim(dat)

categs <- as.matrix(dat[,1:3])
X      <- t(dat[,4:ncol(dat)])
dim(X) # Should be n-observations by p-categories

lbl.loc  <- fdat[,3]
lbl.room <- fdat[,4]

all.counts <- colSums(X)

class.lbls <- unique(categs[,1])
class.lbls

# Counts of all categories, colors indicate the class
plot(1:length(all.counts), all.counts, typ="h", xlab="Categ. #", ylab="count")
points(1:length(all.counts), rep(0,length(all.counts)), col=as.factor(categs[,1]))

# Drop the categories that never occured, colors indicate the class:
zero.idxs <- which(all.counts==0)
plot(1:length(all.counts[-zero.idxs]), all.counts[-zero.idxs], typ="h", xlab="Categ. #", ylab="count")
points(1:length(all.counts[-zero.idxs]), rep(0,length(all.counts[-zero.idxs])), col=as.factor(categs[-zero.idxs,1]))

legend(210, 300, legend=class.lbls,
       col=as.factor(class.lbls),  cex=0.8, pch=16)
