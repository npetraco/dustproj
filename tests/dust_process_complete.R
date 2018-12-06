source("/Users/npetraco/latex/papers/dust/DUST PROJECT/program_codes/sourceme.R")
source("/Users/npetraco/codes/R/chemometric_utilities/sourceme.R")
rootd<-"/Users/npetraco/math/Data/database/dust/"

# Load the whole database:

# Original dust study samples:
file.root<-"dust_table_"
path.to.files<-paste(rootd,file.root,sep="")
num.of.samples<-79
num.of.reps<-c(rep(5,36),rep(1,23),rep(5,20))

X<-NULL
for(i in 1:num.of.samples) {
  for(j in 1:num.of.reps[i]) {
    dfpath<-paste(path.to.files,i,".",j,".xlsx",sep="")
    dinfo<-read.dust.file(dfpath)
    data.vec<-dinfo[[1]]
    #data.vec<-t(info[,4])
    X<-rbind(X,data.vec)
    print(paste("Done sample",i,"replicate",j))
  }
}
catg.names.orig<-dinfo[[2]]
catg.counts.orig<-colSums(X)
#catg.percent.orig<-colSums(X)/nrow(X)*100

catg.names.orig
catg.counts.orig
catg.percent.orig

plot(catg.counts.orig,typ="h",ylab="counts",xlab="Debris Category", main="Dust Component Distribution Across Database")

lbl.orig<-generate.label.vec(num.of.reps)
lbl.orig<-as.numeric(lbl.orig)




#Now Load in the new dust samples from Florida, Phase I. They're are a few extra things in section 7 (cf. read.dust.file.EXPANDED)
new.samp1 <- read.dust.file.EXPANDED(paste0(rootd,"DB14_mod.xlsx"))
new.samp2 <- read.dust.file.EXPANDED(paste0(rootd,"DB28_mod.xlsx"))
new.samp3 <- read.dust.file.EXPANDED(paste0(rootd,"DB48.xlsx"))
new.samp4 <- read.dust.file.EXPANDED(paste0(rootd,"DB49_mod.xlsx"))

X.florida1 <- rbind(new.samp1[[1]],new.samp2[[1]], new.samp3[[1]], new.samp4[[1]])
dim(X.florida1)

num.of.reps.florida1<-c(rep(1,4))
lbl.florida1<-generate.label.vec(num.of.reps.florida1)
lbl.florida1<-as.numeric(lbl.florida1)

# Lets do our best to estimate the these category probs based on the data we see.
catg.names.florida1<-new.samp1[[2]]
catg.counts.florida1<-colSums(X.florida1)
#catg.percent.florida1<-colSums(X)/nrow(X)*100




#Now Load in the new dust samples from Florida, Phase II. They're are a few extra things in section 7 (cf. read.dust.file.ballantyne.study
#They are in a different order so we have to explixitly count the categories...
new.samp5 <- read.dust.file_ballantyne_study(paste0(rootd,"NY002 BR1_mod.xlsx"))
new.samp6 <- read.dust.file_ballantyne_study(paste0(rootd,"NY002 BR2_mod.xlsx"))
new.samp7 <- read.dust.file_ballantyne_study(paste0(rootd,"NY002 LR1_mod.xlsx"))
new.samp8 <- read.dust.file_ballantyne_study(paste0(rootd,"NY002 LR2_mod.xlsx"))

new.samp9 <- read.dust.file_ballantyne_study(paste0(rootd,"NY008 BR1_mod.xlsx"))
new.samp10 <- read.dust.file_ballantyne_study(paste0(rootd,"NY008 BR2_mod.xlsx"))
new.samp11 <- read.dust.file_ballantyne_study(paste0(rootd,"NY008 LR1_mod.xlsx"))
new.samp12 <- read.dust.file_ballantyne_study(paste0(rootd,"NY008 LR2_mod.xlsx"))

new.samp13 <- read.dust.file_ballantyne_study(paste0(rootd,"NY015 BR1_mod.xlsx"))
new.samp14 <- read.dust.file_ballantyne_study(paste0(rootd,"NY015 BR2_mod.xlsx"))
new.samp15 <- read.dust.file_ballantyne_study(paste0(rootd,"NY015 LR1_mod.xlsx"))
new.samp16 <- read.dust.file_ballantyne_study(paste0(rootd,"NY015 LR2_mod.xlsx"))

new.samp17 <- read.dust.file_ballantyne_study(paste0(rootd,"NY022 BR1_mod.xlsx"))
new.samp18 <- read.dust.file_ballantyne_study(paste0(rootd,"NY022 BR2_mod.xlsx"))
new.samp19 <- read.dust.file_ballantyne_study(paste0(rootd,"NY022 LR1_mod.xlsx"))
new.samp20 <- read.dust.file_ballantyne_study(paste0(rootd,"NY022 LR2_mod.xlsx"))

new.samp21 <- read.dust.file_ballantyne_study(paste0(rootd,"NY024 BR1_mod.xlsx"))
new.samp22 <- read.dust.file_ballantyne_study(paste0(rootd,"NY024 BR2_mod.xlsx"))
new.samp23 <- read.dust.file_ballantyne_study(paste0(rootd,"NY024 LR1_mod.xlsx"))
new.samp24 <- read.dust.file_ballantyne_study(paste0(rootd,"NY024 LR2_mod.xlsx"))

new.samp25 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF5 BR1_mod.xlsx"))
new.samp26 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF5 BR2_mod.xlsx"))
new.samp27 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF5 LR1_mod.xlsx"))
new.samp28 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF5 LR2_mod.xlsx"))

new.samp29 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF10BR1_mod.xlsx"))
new.samp30 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF10BR2_mod.xlsx"))
new.samp31 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF10LR1_mod.xlsx"))
new.samp32 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF10LR2_mod.xlsx"))

new.samp33 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF11BR1_mod.xlsx"))
new.samp34 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF11BR2_mod.xlsx"))
new.samp35 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF11LR1_mod.xlsx"))
new.samp36 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF11LR2_mod.xlsx"))

X.florida2 <- rbind(
  new.samp5[[1]],  new.samp6[[1]],  new.samp7[[1]],  new.samp8[[1]],
  new.samp9[[1]],  new.samp10[[1]], new.samp11[[1]], new.samp12[[1]],
  new.samp13[[1]], new.samp14[[1]], new.samp15[[1]], new.samp16[[1]],
  new.samp17[[1]], new.samp18[[1]], new.samp19[[1]], new.samp20[[1]],
  new.samp21[[1]], new.samp22[[1]], new.samp23[[1]], new.samp24[[1]],
  new.samp25[[1]], new.samp26[[1]], new.samp27[[1]], new.samp28[[1]],
  new.samp29[[1]], new.samp30[[1]], new.samp31[[1]], new.samp32[[1]],
  new.samp33[[1]], new.samp34[[1]], new.samp35[[1]], new.samp36[[1]]
  )
dim(X.florida2)

# Lets do our best to estimate the these category probs based on the data we see.
catg.names.florida2 <- new.samp5[[2]]
catg.counts.florida2 <- colSums(X.florida2)

num.of.reps.florida2 <- rep(4,8)
lbl.florida2<-generate.label.vec(num.of.reps.florida2)
lbl.florida2<-as.numeric(lbl.florida2)




# Load WTC dust samples
wtc.samp1 <- read.dust.file_ballantyne_study(paste0(rootd,"WTC-K_Site_Ground_Zero_911_mod_samp1.xlsx"))
wtc.samp2 <- read.dust.file_ballantyne_study(paste0(rootd,"WTC-K_Site_Ground_Zero_911_mod_samp2.xlsx"))
wtc.samp3 <- read.dust.file_ballantyne_study(paste0(rootd,"WTC-K_Site_Ground_Zero_911_mod_samp3.xlsx"))
wtc.samp4 <- read.dust.file_ballantyne_study(paste0(rootd,"WTC-K_Site_Ground_Zero_911_mod_samp4.xlsx"))
wtc.samp5 <- read.dust.file_ballantyne_study(paste0(rootd,"WTC-K_Site_Ground_Zero_911_mod_samp5.xlsx"))

X.wtc <- rbind(wtc.samp1[[1]],wtc.samp2[[1]], wtc.samp3[[1]], wtc.samp4[[1]], wtc.samp5[[1]])
dim(X.wtc)

# Labels for Florida 2
num.of.reps.wtc <- 5
lbl.wtc <- rep(1,5)

# Categories for WTC:
catg.names.wtc <- wtc.samp1[[2]]
catg.counts.wtc<-colSums(X.wtc)
catg.names.wtc
#

# Try to straighten out category names...
# See dustPfix_category_order
catg.names.orig          # Change to florida 2 categories and order
dim(catg.names.orig)
catg.names.florida1      # Change to florida 2 categories and order
dim(catg.names.florida1)
catg.names.florida2      # The "standard"
dim(catg.names.wtc)
catg.names.wtc           # Same as florida 2, so OK
#

write.csv(catg.names.orig,paste0(rootd,"category_names/catg.names.orig.csv"))
write.csv(catg.names.florida1,paste0(rootd,"category_names/catg.names.florida1.csv"))
write.csv(catg.names.florida2,paste0(rootd,"category_names/catg.names.florida2.csv"))
write.csv(catg.names.wtc,paste0(rootd,"category_names/catg.names.wtc.csv"))
