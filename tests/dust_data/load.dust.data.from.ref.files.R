library(dustproj)

options(max.print=1000000)
fdat  <- read.csv("tests/dust_data/old_formats_use_file_names.csv", header=T)
fnmes <- as.matrix(fdat[,5])
in.prefix <- "tests/dust_data/ref_formats/ref_format_"

in.fpths <- paste0(in.prefix,fnmes)
master.dust.info <- load.datasheets(in.fpths)

all.counts <- rowSums(master.dust.info$master.indicator.mat)
all.categ  <- master.dust.info$master.category.mat
data.frame(all.categ, all.counts)
plot(1:length(all.counts), all.counts, typ="h", xlab="Categ. #", ylab="count")
points(1:length(all.counts), rep(0,length(all.counts)), col=as.factor(all.categ[,1]))

# Drop the categories that never occured:
zero.idxs <- which(all.counts==0)
plot(1:length(all.counts[-zero.idxs]), all.counts[-zero.idxs], typ="h", xlab="Categ. #", ylab="count")
points(1:length(all.counts[-zero.idxs]), rep(0,length(all.counts[-zero.idxs])), col=as.factor(all.categ[-zero.idxs,1]))

data.frame(all.categ, master.dust.info$master.indicator.mat)

write.csv(data.frame(all.categ, master.dust.info$master.indicator.mat), "tests/dust_data/dust.data.as-of-6-10-19.csv", row.names = F)
head(fdat)
