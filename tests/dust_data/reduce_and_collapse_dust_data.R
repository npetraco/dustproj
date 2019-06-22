# Reduce and collapse

fdat     <- read.csv("/home/npetraco/latex/papers/dust/DUST PROJECT/dustproj/tests/dust_data/old_formats_use_file_names.csv", header=T)
dat      <- read.csv("/home/npetraco/latex/papers/dust/DUST PROJECT/dustproj/tests/dust_data/dust.data.as-of-6-10-19.csv", header=T)
lbl.loc  <- fdat[,3]
lbl.room <- fdat[,4]

categs <- as.matrix(dat[,1:3])
X      <- t(dat[,4:ncol(dat)]) # Should be n-observations by p-categories
all.counts <- colSums(X)

# First reduce to only those categories that were observed
zero.idxs  <- which(all.counts==0) # These catregories were not observed
X.red      <- X[,-zero.idxs]
categs.red <- categs[-zero.idxs,]
dim(X.red)
dim(categs.red)

# Check and see if any datasheets were empty
# dim(X.red)
# which(rowSums(X.red) == 0)

# Tack together into a reduced dataframe to be written to file
dat.red <- data.frame(categs.red, t(X.red))
dim(dat.red)
# WRITE CSV HERE
write.csv(dat.red, file = "/home/npetraco/latex/papers/dust/DUST PROJECT/dustproj/tests/dust_data/dust.data.as-of-6-10-19.reduced.csv", row.names = F)


# Now collapse attributes out of their subclasses, leaving only class and subclass. IE, if any attribute for a subclass occired, 1, otherwise 0
remove(dat)
remove(categs)
remove(X)

Xc.red     <- t(X.red) # X complement
classes    <- as.matrix(unique(dat.red$Class))
subclasses <- rep(list(NULL), length(classes))

categs.collapsed <- NULL
Xc.collapsed     <- NULL
for(i in 1:length(classes)) {

  blc.idxs <- which(dat.red$Class == classes[i])
  subclasses[[i]] <- as.matrix(unique(as.matrix(dat.red$Subclass[blc.idxs])))

  for(j in 1:length(subclasses[[i]])){
    csc.idxs <- which((dat.red$Class == classes[i]) & (dat.red$Subclass == subclasses[[i]][j]))

    if(class(Xc.red[csc.idxs,]) == "integer") {
      smp.ind.vec <- as.numeric(Xc.red[csc.idxs,]>0)
    } else {
      smp.ind.vec <- as.numeric(colSums(Xc.red[csc.idxs,])>0)
    }

    csc.vec <- matrix(c(classes[i], subclasses[[i]][j]), c(1,2))

    Xc.collapsed     <- rbind(Xc.collapsed, smp.ind.vec)
    categs.collapsed <- rbind(categs.collapsed, as.matrix(csc.vec))

    print(paste("Class:", classes[i], "Subclass:", subclasses[[i]][j]))

  }
}

categs.collapsed
dim(categs.collapsed)
dim(Xc.collapsed)

dat.collapsed <- data.frame(categs.collapsed, Xc.collapsed)
dim(dat.collapsed)
rownames(dat.collapsed) <- NULL
colnames(dat.collapsed) <- c("class","subclass", paste0("X",1:dim(Xc.collapsed)[2]))

#WRITE CSV HERE
write.csv(dat.collapsed, file = "/home/npetraco/latex/papers/dust/DUST PROJECT/dustproj/tests/dust_data/dust.data.as-of-6-10-19.reduced.collapsed.csv", row.names = F)
