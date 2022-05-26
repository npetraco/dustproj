source("/Users/npetraco/latex/papers/dust/DUST PROJECT/program_codes/sourceme.R")
source("/Users/npetraco/codes/R/chemometric_utilities/sourceme.R")
rootd<-"/Users/npetraco/math/Data/database/dust/"

# Load the whole database:

#-----------------
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

#-----------------
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

#--------------------
#Now Load in the new dust samples from Florida, Phase II. They're are a few extra things in section 7 (cf. read.dust.file.ballantyne.study
#They are in a different order so we have to explixitly count the categories...
new.samp5 <- read.dust.file_ballantyne_study(paste0(rootd,"NY002 BR1_mod.xlsx"))
new.samp6 <- read.dust.file_ballantyne_study(paste0(rootd,"NY002 BR2_mod.xlsx"))
new.samp7 <- read.dust.file_ballantyne_study(paste0(rootd,"NY002 LR1_mod.xlsx"))
new.samp8 <- read.dust.file_ballantyne_study(paste0(rootd,"NY002 LR2_mod.xlsx"))

new.samp9 <- read.dust.file_ballantyne_study(paste0(rootd,"NY003BR1_mod.xlsx"))
new.samp10 <- read.dust.file_ballantyne_study(paste0(rootd,"NY003BR2_mod.xlsx"))
new.samp11 <- read.dust.file_ballantyne_study(paste0(rootd,"NY003LR1_mod.xlsx"))
new.samp12 <- read.dust.file_ballantyne_study(paste0(rootd,"NY003LR2_mod.xlsx"))

new.samp13 <- read.dust.file_ballantyne_study(paste0(rootd,"NY006BR1_mod.xlsx"))
new.samp14 <- read.dust.file_ballantyne_study(paste0(rootd,"NY006BR2_mod.xlsx"))
new.samp15 <- read.dust.file_ballantyne_study(paste0(rootd,"NY006LR1_mod.xlsx"))
new.samp16 <- read.dust.file_ballantyne_study(paste0(rootd,"NY006LR2_mod.xlsx"))

new.samp17 <- read.dust.file_ballantyne_study(paste0(rootd,"NY007BR1_mod.xlsx"))
new.samp18 <- read.dust.file_ballantyne_study(paste0(rootd,"NY007BR2_mod.xlsx"))
new.samp19 <- read.dust.file_ballantyne_study(paste0(rootd,"NY007LR1_mod.xlsx"))
new.samp20 <- read.dust.file_ballantyne_study(paste0(rootd,"NY007LR2_mod.xlsx"))

new.samp21 <- read.dust.file_ballantyne_study(paste0(rootd,"NY008 BR1_mod.xlsx"))
new.samp22 <- read.dust.file_ballantyne_study(paste0(rootd,"NY008 BR2_mod.xlsx"))
new.samp23 <- read.dust.file_ballantyne_study(paste0(rootd,"NY008 LR1_mod.xlsx"))
new.samp24 <- read.dust.file_ballantyne_study(paste0(rootd,"NY008 LR2_mod.xlsx"))

new.samp25 <- read.dust.file_ballantyne_study(paste0(rootd,"NY009BR1_mod.xlsx"))
new.samp26 <- read.dust.file_ballantyne_study(paste0(rootd,"NY009BR2_mod.xlsx"))
new.samp27 <- read.dust.file_ballantyne_study(paste0(rootd,"NY009LR1_mod.xlsx"))
new.samp28 <- read.dust.file_ballantyne_study(paste0(rootd,"NY009LR2_mod.xlsx"))

new.samp29 <- read.dust.file_ballantyne_study(paste0(rootd,"NY011BR1_mod.xlsx"))
new.samp30 <- read.dust.file_ballantyne_study(paste0(rootd,"NY011BR2_mod.xlsx"))
new.samp31 <- read.dust.file_ballantyne_study(paste0(rootd,"NY011LR1_mod.xlsx"))
new.samp32 <- read.dust.file_ballantyne_study(paste0(rootd,"NY011LR2_mod.xlsx"))

new.samp33 <- read.dust.file_ballantyne_study(paste0(rootd,"NY012BR1_mod.xlsx"))
new.samp34 <- read.dust.file_ballantyne_study(paste0(rootd,"NY012BR2_mod.xlsx"))
new.samp35 <- read.dust.file_ballantyne_study(paste0(rootd,"NY012LR1_mod.xlsx"))
new.samp36 <- read.dust.file_ballantyne_study(paste0(rootd,"NY012LR2_mod.xlsx"))

new.samp37 <- read.dust.file_ballantyne_study(paste0(rootd,"NY014BR1_mod.xlsx"))
new.samp38 <- read.dust.file_ballantyne_study(paste0(rootd,"NY014BR2_mod.xlsx"))
new.samp39 <- read.dust.file_ballantyne_study(paste0(rootd,"NY014LR1_mod.xlsx"))
new.samp40 <- read.dust.file_ballantyne_study(paste0(rootd,"NY014LR2_mod.xlsx"))

new.samp41 <- read.dust.file_ballantyne_study(paste0(rootd,"NY015 BR1_mod.xlsx"))
new.samp42 <- read.dust.file_ballantyne_study(paste0(rootd,"NY015 BR2_mod.xlsx"))
new.samp43 <- read.dust.file_ballantyne_study(paste0(rootd,"NY015 LR1_mod.xlsx"))
new.samp44 <- read.dust.file_ballantyne_study(paste0(rootd,"NY015 LR2_mod.xlsx"))

new.samp45 <- read.dust.file_ballantyne_study(paste0(rootd,"NY016BR1_mod.xlsx"))
new.samp46 <- read.dust.file_ballantyne_study(paste0(rootd,"NY016LR1_mod.xlsx"))

new.samp47 <- read.dust.file_ballantyne_study(paste0(rootd,"NY017BR1_mod.xlsx"))
new.samp48 <- read.dust.file_ballantyne_study(paste0(rootd,"NY017BR2_mod.xlsx"))
new.samp49 <- read.dust.file_ballantyne_study(paste0(rootd,"NY017LR1_mod.xlsx"))
new.samp50 <- read.dust.file_ballantyne_study(paste0(rootd,"NY017LR2_mod.xlsx"))

new.samp51 <- read.dust.file_ballantyne_study(paste0(rootd,"NY018BR1_mod.xlsx"))
new.samp52 <- read.dust.file_ballantyne_study(paste0(rootd,"NY018BR2_mod.xlsx"))
new.samp53 <- read.dust.file_ballantyne_study(paste0(rootd,"NY018LR1_mod.xlsx"))
new.samp54 <- read.dust.file_ballantyne_study(paste0(rootd,"NY018LR2_mod.xlsx"))

new.samp55 <- read.dust.file_ballantyne_study(paste0(rootd,"NY019BR1_mod.xlsx"))
new.samp56 <- read.dust.file_ballantyne_study(paste0(rootd,"NY019BR2_mod.xlsx"))
new.samp57 <- read.dust.file_ballantyne_study(paste0(rootd,"NY019LR1_mod.xlsx"))
new.samp58 <- read.dust.file_ballantyne_study(paste0(rootd,"NY019LR2_mod.xlsx"))

new.samp59 <- read.dust.file_ballantyne_study(paste0(rootd,"NY020BR1_mod.xlsx"))
new.samp60 <- read.dust.file_ballantyne_study(paste0(rootd,"NY020BR2_mod.xlsx"))
new.samp61 <- read.dust.file_ballantyne_study(paste0(rootd,"NY020LR1_mod.xlsx"))
new.samp62 <- read.dust.file_ballantyne_study(paste0(rootd,"NY020LR2_mod.xlsx"))

new.samp63 <- read.dust.file_ballantyne_study(paste0(rootd,"NY021BR1_mod.xlsx"))
new.samp64 <- read.dust.file_ballantyne_study(paste0(rootd,"NY021BR2_mod.xlsx"))
new.samp65 <- read.dust.file_ballantyne_study(paste0(rootd,"NY021LR1_mod.xlsx"))
new.samp66 <- read.dust.file_ballantyne_study(paste0(rootd,"NY021LR2_mod.xlsx"))

new.samp67 <- read.dust.file_ballantyne_study(paste0(rootd,"NY022 BR1_mod.xlsx"))
new.samp68 <- read.dust.file_ballantyne_study(paste0(rootd,"NY022 BR2_mod.xlsx"))
new.samp69 <- read.dust.file_ballantyne_study(paste0(rootd,"NY022 LR1_mod.xlsx"))
new.samp70 <- read.dust.file_ballantyne_study(paste0(rootd,"NY022 LR2_mod.xlsx"))

new.samp71 <- read.dust.file_ballantyne_study(paste0(rootd,"NY023BR1_mod.xlsx"))
new.samp72 <- read.dust.file_ballantyne_study(paste0(rootd,"NY023BR2_mod.xlsx"))
new.samp73 <- read.dust.file_ballantyne_study(paste0(rootd,"NY023LR1_mod.xlsx"))
new.samp74 <- read.dust.file_ballantyne_study(paste0(rootd,"NY023LR2_mod.xlsx"))

new.samp75 <- read.dust.file_ballantyne_study(paste0(rootd,"NY024 BR1_mod.xlsx"))
new.samp76 <- read.dust.file_ballantyne_study(paste0(rootd,"NY024 BR2_mod.xlsx"))
new.samp77 <- read.dust.file_ballantyne_study(paste0(rootd,"NY024 LR1_mod.xlsx"))
new.samp78 <- read.dust.file_ballantyne_study(paste0(rootd,"NY024 LR2_mod.xlsx"))

new.samp79 <- read.dust.file_ballantyne_study(paste0(rootd,"NY025BR1_mod.xlsx"))
new.samp80 <- read.dust.file_ballantyne_study(paste0(rootd,"NY025BR2_mod.xlsx"))
new.samp81 <- read.dust.file_ballantyne_study(paste0(rootd,"NY025LR1_mod.xlsx"))
new.samp82 <- read.dust.file_ballantyne_study(paste0(rootd,"NY025LR2_mod.xlsx"))

new.samp83 <- read.dust.file_ballantyne_study(paste0(rootd,"NY030BR1_mod.xlsx"))
new.samp84 <- read.dust.file_ballantyne_study(paste0(rootd,"NY030BR2_mod.xlsx"))
new.samp85 <- read.dust.file_ballantyne_study(paste0(rootd,"NY030LR1_mod.xlsx"))
new.samp86 <- read.dust.file_ballantyne_study(paste0(rootd,"NY030LR2_mod.xlsx"))

new.samp87 <- read.dust.file_ballantyne_study(paste0(rootd,"NY031BR1_mod.xlsx"))
new.samp88 <- read.dust.file_ballantyne_study(paste0(rootd,"NY031BR2_mod.xlsx"))
new.samp89 <- read.dust.file_ballantyne_study(paste0(rootd,"NY031LR1_mod.xlsx"))
new.samp90 <- read.dust.file_ballantyne_study(paste0(rootd,"NY031LR2_mod.xlsx"))

new.samp91 <- read.dust.file_ballantyne_study(paste0(rootd,"NY032BR1_mod.xlsx"))
new.samp92 <- read.dust.file_ballantyne_study(paste0(rootd,"NY032BR2_mod.xlsx"))
new.samp93 <- read.dust.file_ballantyne_study(paste0(rootd,"NY032LR1_mod.xlsx"))
new.samp94 <- read.dust.file_ballantyne_study(paste0(rootd,"NY032LR2_mod.xlsx"))

new.samp95 <- read.dust.file_ballantyne_study(paste0(rootd,"NY034BR1_mod.xlsx"))
new.samp96 <- read.dust.file_ballantyne_study(paste0(rootd,"NY034BR2_mod.xlsx"))
new.samp97 <- read.dust.file_ballantyne_study(paste0(rootd,"NY034LR1_mod.xlsx"))
new.samp98 <- read.dust.file_ballantyne_study(paste0(rootd,"NY034LR2_mod.xlsx"))

new.samp99 <- read.dust.file_ballantyne_study(paste0(rootd,"NY035BR1_mod.xlsx"))
new.samp100 <- read.dust.file_ballantyne_study(paste0(rootd,"NY035BR2_mod.xlsx"))
new.samp101 <- read.dust.file_ballantyne_study(paste0(rootd,"NY035LR1_mod.xlsx"))
new.samp102 <- read.dust.file_ballantyne_study(paste0(rootd,"NY035LR2_mod.xlsx"))

new.samp103 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF001BR1_mod.xlsx"))
new.samp104 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF001BR2_mod.xlsx"))
new.samp105 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF001LR1_mod.xlsx"))
new.samp106 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF001LR2_mod.xlsx"))

new.samp107 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF002BR1_mod.xlsx"))
new.samp108 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF002BR2_mod.xlsx"))
new.samp109 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF002LR1_mod.xlsx"))
new.samp110 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF002LR2_mod.xlsx"))

new.samp111 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF003BR1_mod.xlsx"))
new.samp112 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF003BR2_mod.xlsx"))
new.samp113 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF003LR1_mod.xlsx"))
new.samp114 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF003LR2_mod.xlsx"))

new.samp115 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF004BR1_mod.xlsx"))
new.samp116 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF004BR2_mod.xlsx"))
new.samp117 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF004LR1_mod.xlsx"))
new.samp118 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF004LR2_mod.xlsx"))

new.samp119 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF5 BR1_mod.xlsx"))
new.samp120 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF5 BR2_mod.xlsx"))
new.samp121 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF5 LR1_mod.xlsx"))
new.samp122 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF5 LR2_mod.xlsx"))

new.samp123 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF006BR1_mod.xlsx"))
new.samp124 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF006BR2_mod.xlsx"))
new.samp125 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF006LR1_mod.xlsx"))
new.samp126 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF006LR2_mod.xlsx"))

new.samp127 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF007BR1_mod.xlsx"))
new.samp128 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF007BR2_mod.xlsx"))
new.samp129 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF007LR1_mod.xlsx"))
new.samp130 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF007LR2_mod.xlsx"))

new.samp131 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF8BR1_mod.xlsx"))
new.samp132 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF8BR2_mod.xlsx"))
new.samp133 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF8LR1_mod.xlsx"))
new.samp134 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF8LR2_mod.xlsx"))

new.samp135 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF009BR1_mod.xlsx"))
new.samp136 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF009BR2_mod.xlsx"))
new.samp137 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF009LR1_mod.xlsx"))
new.samp138 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF009LR2_mod.xlsx"))

new.samp139 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF10BR1_mod.xlsx"))
new.samp140 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF10BR2_mod.xlsx"))
new.samp141 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF10LR1_mod.xlsx"))
new.samp142 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF10LR2_mod.xlsx"))

new.samp143 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF11BR1_mod.xlsx"))
new.samp144 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF11BR2_mod.xlsx"))
new.samp145 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF11LR1_mod.xlsx"))
new.samp146 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF11LR2_mod.xlsx"))

new.samp147 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF012BR1_mod.xlsx"))
new.samp148 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF012BR2_mod.xlsx"))
new.samp149 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF012LR1_mod.xlsx"))
new.samp150 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF012LR2_mod.xlsx"))

new.samp151 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF013BR1_mod.xlsx"))
new.samp152 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF013BR2_mod.xlsx"))
new.samp153 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF013LR1_mod.xlsx"))
new.samp154 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF013LR2_mod.xlsx"))

new.samp155 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF014BR1_mod.xlsx"))
new.samp156 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF014BR2_mod.xlsx"))
new.samp157 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF014LR1_mod.xlsx"))
new.samp158 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF014LR2_mod.xlsx"))

new.samp159 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF015BR1_mod.xlsx"))
new.samp160 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF015BR2_mod.xlsx"))
new.samp161 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF015LR1_mod.xlsx"))
new.samp162 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF015LR2_mod.xlsx"))

new.samp163 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF016BR1_mod.xlsx"))
new.samp164 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF016BR2_mod.xlsx"))
new.samp165 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF016LR1_mod.xlsx"))
new.samp166 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF016LR2_mod.xlsx"))

new.samp167 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF017BR1_mod.xlsx"))
new.samp168 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF017BR2_mod.xlsx"))
new.samp169 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF017LR1_mod.xlsx"))
new.samp170 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF017LR2_mod.xlsx"))

new.samp171 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF018BR1_mod.xlsx"))
new.samp172 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF018BR2_mod.xlsx"))
new.samp173 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF018LR1_mod.xlsx"))
new.samp174 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF018LR2_mod.xlsx"))

new.samp175 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF019BR1_mod.xlsx"))
new.samp176 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF019BR2_mod.xlsx"))
new.samp177 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF019LR1_mod.xlsx"))
new.samp178 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF019LR2_mod.xlsx"))

new.samp179 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF020BR1_mod.xlsx"))
new.samp180 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF020BR2_mod.xlsx"))
new.samp181 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF020LR1_mod.xlsx"))

new.samp182 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF021BR1_mod.xlsx"))
new.samp183 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF021BR2_mod.xlsx"))
new.samp184 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF021LR1_mod.xlsx"))
new.samp185 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF021LR2_mod.xlsx"))

new.samp186 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF022BR1_mod.xlsx"))
new.samp187 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF022BR2_mod.xlsx"))
new.samp188 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF022LR1_mod.xlsx"))
new.samp189 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF022LR2_mod.xlsx"))

new.samp190 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF023BR1_mod.xlsx"))
new.samp191 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF023BR2_mod.xlsx"))
new.samp192 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF023LR1_mod.xlsx"))
new.samp193 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF023LR2_mod.xlsx"))

new.samp194 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF024BR1_mod.xlsx"))
new.samp195 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF024LR1_mod.xlsx"))
new.samp196 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF024LR2_mod.xlsx"))

new.samp197 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF11BR1_mod.xlsx"))
new.samp198 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF11BR2_mod.xlsx"))
new.samp199 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF11LR1_mod.xlsx"))
new.samp200 <- read.dust.file_ballantyne_study(paste0(rootd,"UCF11LR2_mod.xlsx"))

X.florida2 <- rbind(
  new.samp5[[1]],  new.samp6[[1]],  new.samp7[[1]],  new.samp8[[1]],
  new.samp9[[1]],  new.samp10[[1]], new.samp11[[1]], new.samp12[[1]],
  new.samp13[[1]], new.samp14[[1]], new.samp15[[1]], new.samp16[[1]],
  new.samp17[[1]], new.samp18[[1]], new.samp19[[1]], new.samp20[[1]],
  new.samp21[[1]], new.samp22[[1]], new.samp23[[1]], new.samp24[[1]],
  new.samp25[[1]], new.samp26[[1]], new.samp27[[1]], new.samp28[[1]],
  new.samp29[[1]], new.samp30[[1]], new.samp31[[1]], new.samp32[[1]],
  new.samp33[[1]], new.samp34[[1]], new.samp35[[1]], new.samp36[[1]],
  new.samp37[[1]], new.samp38[[1]], new.samp39[[1]], new.samp40[[1]],
  new.samp41[[1]], new.samp42[[1]], new.samp43[[1]], new.samp44[[1]],
  new.samp45[[1]], new.samp46[[1]],
  new.samp47[[1]], new.samp48[[1]], new.samp49[[1]], new.samp50[[1]],
  new.samp51[[1]], new.samp52[[1]], new.samp53[[1]], new.samp54[[1]],
  new.samp55[[1]], new.samp56[[1]], new.samp57[[1]], new.samp58[[1]],
  new.samp59[[1]], new.samp60[[1]], new.samp61[[1]], new.samp62[[1]],
  new.samp63[[1]], new.samp64[[1]], new.samp65[[1]], new.samp66[[1]],
  new.samp67[[1]], new.samp68[[1]], new.samp69[[1]], new.samp70[[1]],
  new.samp71[[1]], new.samp72[[1]], new.samp73[[1]], new.samp74[[1]],
  new.samp75[[1]], new.samp76[[1]], new.samp77[[1]], new.samp78[[1]],
  new.samp79[[1]], new.samp80[[1]], new.samp81[[1]], new.samp82[[1]],
  new.samp83[[1]], new.samp84[[1]], new.samp85[[1]], new.samp86[[1]],
  new.samp87[[1]],  new.samp88[[1]],  new.samp89[[1]],  new.samp90[[1]],
  new.samp91[[1]],  new.samp92[[1]],  new.samp93[[1]],  new.samp94[[1]],
  new.samp95[[1]],  new.samp96[[1]],  new.samp97[[1]],  new.samp98[[1]],
  new.samp99[[1]],  new.samp100[[1]], new.samp101[[1]], new.samp102[[1]],
  new.samp103[[1]], new.samp104[[1]], new.samp105[[1]], new.samp106[[1]],
  new.samp107[[1]], new.samp108[[1]], new.samp109[[1]], new.samp110[[1]],
  new.samp111[[1]], new.samp112[[1]], new.samp113[[1]], new.samp114[[1]],
  new.samp115[[1]], new.samp116[[1]], new.samp117[[1]], new.samp118[[1]],
  new.samp119[[1]], new.samp120[[1]], new.samp121[[1]], new.samp122[[1]],
  new.samp123[[1]], new.samp124[[1]], new.samp125[[1]], new.samp126[[1]],
  new.samp127[[1]], new.samp128[[1]], new.samp129[[1]], new.samp130[[1]],
  new.samp131[[1]], new.samp132[[1]], new.samp133[[1]], new.samp134[[1]],
  new.samp135[[1]], new.samp136[[1]], new.samp137[[1]], new.samp138[[1]],
  new.samp139[[1]], new.samp140[[1]], new.samp141[[1]], new.samp142[[1]],
  new.samp143[[1]], new.samp144[[1]], new.samp145[[1]], new.samp146[[1]],
  new.samp147[[1]], new.samp148[[1]], new.samp149[[1]], new.samp150[[1]],
  new.samp151[[1]], new.samp152[[1]], new.samp153[[1]], new.samp154[[1]],
  new.samp155[[1]], new.samp156[[1]], new.samp157[[1]], new.samp158[[1]],
  new.samp159[[1]], new.samp160[[1]], new.samp161[[1]], new.samp162[[1]],
  new.samp163[[1]], new.samp164[[1]], new.samp165[[1]], new.samp166[[1]],
  new.samp167[[1]], new.samp168[[1]], new.samp169[[1]], new.samp170[[1]],
  new.samp171[[1]], new.samp172[[1]], new.samp173[[1]], new.samp174[[1]],
  new.samp175[[1]], new.samp176[[1]], new.samp177[[1]], new.samp178[[1]],
  new.samp179[[1]], new.samp180[[1]], new.samp181[[1]],
  new.samp182[[1]], new.samp183[[1]], new.samp184[[1]], new.samp185[[1]],
  new.samp186[[1]], new.samp187[[1]], new.samp188[[1]], new.samp189[[1]],
  new.samp190[[1]], new.samp191[[1]], new.samp192[[1]], new.samp193[[1]],
  new.samp194[[1]], new.samp195[[1]], new.samp196[[1]],
  new.samp197[[1]], new.samp198[[1]], new.samp199[[1]], new.samp200[[1]]
)
dim(X.florida2)

# Lets do our best to estimate the these category probs based on the data we see.
catg.names.florida2 <- new.samp5[[2]]
catg.counts.florida2 <- colSums(X.florida2)

num.of.reps.florida2 <- c(rep(4,10), rep(2,1), rep(4,33), rep(3,1), rep(4,3), rep(3,1), rep(4,1))
sum(num.of.reps.florida2)

lbl.florida2<-generate.label.vec(num.of.reps.florida2)
lbl.florida2<-as.numeric(lbl.florida2)
lbl.florida2

#----------------------
# Load WTC dust samples
wtc.samp1 <- read.dust.file_ballantyne_study(paste0(rootd,"WTC-K_Site_Ground_Zero_911_mod_samp1.xlsx"))
wtc.samp2 <- read.dust.file_ballantyne_study(paste0(rootd,"WTC-K_Site_Ground_Zero_911_mod_samp2.xlsx"))
wtc.samp3 <- read.dust.file_ballantyne_study(paste0(rootd,"WTC-K_Site_Ground_Zero_911_mod_samp3.xlsx"))
wtc.samp4 <- read.dust.file_ballantyne_study(paste0(rootd,"WTC-K_Site_Ground_Zero_911_mod_samp4.xlsx"))
wtc.samp5 <- read.dust.file_ballantyne_study(paste0(rootd,"WTC-K_Site_Ground_Zero_911_mod_samp5.xlsx"))

X.wtc <- rbind(wtc.samp1[[1]],wtc.samp2[[1]], wtc.samp3[[1]], wtc.samp4[[1]], wtc.samp5[[1]])
dim(X.wtc)

# Labels for WTC dust
num.of.reps.wtc <- 5
lbl.wtc <- rep(1,5)

# Categories for WTC:
catg.names.wtc <- wtc.samp1[[2]]
catg.counts.wtc<-colSums(X.wtc)
catg.names.wtc
#

#--------------------------------
# Straighten out category names between the different data sets...
catg.names.orig          # Change to florida 2 categories and order
dim(catg.names.orig)
catg.names.florida1      # Change to florida 2 categories and order
dim(catg.names.florida1)
catg.names.florida2      # The "standard"
dim(catg.names.wtc)
catg.names.wtc           # Same as florida 2, so OK


# For safe-keeping:
#write.csv(catg.names.orig,paste0(rootd,"category_names/catg.names.orig.csv"))
#write.csv(catg.names.florida1,paste0(rootd,"category_names/catg.names.florida1.csv"))
#write.csv(catg.names.florida2,paste0(rootd,"category_names/catg.names.florida2.csv"))
#write.csv(catg.names.wtc,paste0(rootd,"category_names/catg.names.wtc.csv"))

# Reconfigure categories in original dust set to conform with categories and the order in the
# florida 2 dust set.

# Get the indices to do the swap:
orig2fla2.info <- read.csv(paste0(rootd,"category_names/orig2fla2.csv"))
#head(orig2fla2.info)
orig2fla2.trns.idxs <- as.numeric(na.omit(orig2fla2.info[,5]))

# Shuffle the categories in the original dust set:
X.orig.trns <- array(0,c(nrow(X),ncol(X.florida2)))
X.orig.trns[1:nrow(X),orig2fla2.trns.idxs] <- X[1:nrow(X),]
dim(X.orig.trns)
dim(X.florida2)

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

#------------------------------
# Dims check:
dim(X.orig.trns)
dim(X.florida1.trns)
dim(X.florida2)
dim(X.wtc)

#Label names:
lbl.orig
lbl.florida1 + 79
lbl.florida2 + 83
lbl.wtc + 133

lbl <- c(
  lbl.orig,
  lbl.florida1 + 79,
  lbl.florida2 + 83,
  lbl.wtc + 133
)
lbl
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
  paste0("wtc.",lbl.wtc + 133)
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
