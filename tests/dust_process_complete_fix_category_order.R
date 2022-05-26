# Reconfigure categories in original dust set to conform with categories and theor order in the
# florida 2 dust set.

# Get the indices:
orig2fla2.info <- read.csv(paste0(rootd,"category_names/orig2fla2.csv"))
#head(orig2fla2.info)
orig2fla2.trns.idxs <- as.numeric(na.omit(orig2fla2.info[,5]))

# Shuffle the categories in the original dust set:
X.orig.trns <- array(0,c(nrow(X),ncol(X.florida2)))
X.orig.trns[1:nrow(X),orig2fla2.trns.idxs] <- X[1:nrow(X),]
dim(X.orig.trns)
dim(X.florida2)

#Plot counts of orig trns vs orig to check
split.screen(c(2,1))
screen(1)
plot(colSums(X),typ="h")
screen(2)
plot(colSums(X.orig.trns),typ="h")
dev.off()


# Repeat process for Florida 1 dust set:

# Get the indices:
fla1.2.fla2.info <- read.csv(paste0(rootd,"category_names/fla12fla2.csv"))
head(fla1.2.fla2.info)
fla1.2.fla2.trns.idxs <- as.numeric(na.omit(fla1.2.fla2.info[,5]))
fla1.2.fla2.trns.idxs

# Shuffle the categories in the original dust set:
X.florida1.trns <- array(0,c(nrow(X.florida1),ncol(X.florida2)))
X.florida1.trns[1:nrow(X.florida1),fla1.2.fla2.trns.idxs] <- X.florida1[1:nrow(X.florida1),]
dim(X.florida1)
dim(X.florida2)
dim(X.florida1.trns)

#Plot counts of orig trns vs orig to check
split.screen(c(2,1))
screen(1)
plot(colSums(X.florida1),typ="h")
screen(2)
plot(colSums(X.florida1.trns),typ="h")


# Dims check:
dim(X.orig.trns)
dim(X.florida1.trns)
dim(X.florida2)
dim(X.wtc)

#Label names:
lbl.orig
lbl.florida1 + 79
lbl.florida2 + 83
lbl.wtc + 91

lbl <- c(
  lbl.orig,
  lbl.florida1 + 79,
  lbl.florida2 + 83,
  lbl.wtc + 91
)
length(lbl)

# All data:
X.all <- rbind(
  X.orig.trns,
  X.florida1.trns,
  X.florida2,
  X.wtc
)
dim(X.all)

# All labels:
lbl
count.group.replicates(lbl)

# Write data to csv:
write.csv(cbind(lbl,X.all),paste0(rootd,"dust_data.csv"))
write.csv(catg.names.florida2,paste0(rootd,"dust_data_categories.csv"))

# Alternative labels for the data:
lbl.full.names <- c(
  paste0("orig.",lbl.orig),
  paste0("florida1.",lbl.florida1 + 79),
  paste0("florida2.",lbl.florida2 + 83),
  paste0("wtc.",lbl.wtc + 91)
)
lbl.full.names

lbl.full.names.alt <- c(
  paste0("orig.",lbl.orig),
  paste0("florida1.",lbl.florida1),
  paste0("florida2.",lbl.florida2),
  paste0("wtc.",lbl.wtc)
)
lbl.full.names.alt

write.csv(cbind(lbl,lbl.full.names, lbl.full.names.alt) ,paste0(rootd,"dust_data_alt_labels.csv"))
