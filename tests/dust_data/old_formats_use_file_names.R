#Original
# Build file paths to loop over
rootd <- "tests/dust_data/old_formats/"
file.root<-"dust_table_"
#path.to.files<-paste(rootd,file.root,sep="")
path.to.files<-paste(file.root,sep="")
num.of.samples<-79
num.of.reps<-c(rep(5,36),rep(1,23),rep(5,20))
fpths.original <- NULL
count <- 1
for(i in 1:num.of.samples) {
  for(j in 1:num.of.reps[i]) {
    dfpath.in <- paste(path.to.files,i,".",j,"_sfsc-mod.xlsx",sep="")
    fpths.original     <- rbind(fpths.original, dfpath.in)
    print(paste(count, dfpath.in))
    count     <- count + 1
  }
}
fpths.original

#FloridaPhase1
fpths.floridaphase1 <- c(
  "DB14_mod.xlsx",
  "DB28_mod.xlsx",
  "DB48.xlsx",
  "DB49_mod.xlsx"
)
fpths.floridaphase1

#WTCk
fpths.wtck <- c(
  "WTC-K_Site_Ground_Zero_911_mod_samp1.xlsx",
  "WTC-K_Site_Ground_Zero_911_mod_samp2.xlsx",
  "WTC-K_Site_Ground_Zero_911_mod_samp3.xlsx",
  "WTC-K_Site_Ground_Zero_911_mod_samp4.xlsx",
  "WTC-K_Site_Ground_Zero_911_mod_samp5.xlsx"
)
fpths.wtck

#WTCq
fpths.wtcq <- c(
  "WTC-Q_Dust from Q flag, tape and rope_samp1_sfsc-mod.xlsx",
  "WTC-Q_Dust from Q flag, tape and rope_samp2_sfsc-mod.xlsx",
  "WTC-Q_Dust from Q flag, tape and rope_samp3_sfsc-mod.xlsx",
  "WTC-Q_Dust from Q flag, tape and rope_samp4_sfsc-mod.xlsx",
  "WTC-Q_Dust from Q flag, tape and rope_samp5_sfsc-mod.xlsx"
)
fpths.wtcq

# FloridaPhase2
fpths.floridaphase2 <- rbind(
  paste0(rootd,"NY002 BR1_mod.xlsx"),
  paste0(rootd,"NY002 BR2_mod.xlsx"),
  paste0(rootd,"NY002 LR1_mod.xlsx"),
  paste0(rootd,"NY002 LR2_mod.xlsx"),
  #*
  paste0(rootd,"NY003BR1_mod.xlsx"),
  paste0(rootd,"NY003BR2_mod.xlsx"),
  paste0(rootd,"NY003LR1_mod.xlsx"),
  paste0(rootd,"NY003LR2_mod.xlsx"),
  #*
  paste0(rootd,"NY006BR1_mod.xlsx"),
  paste0(rootd,"NY006BR2_mod.xlsx"),
  paste0(rootd,"NY006LR1_mod.xlsx"),
  paste0(rootd,"NY006LR2_mod.xlsx"),

  #*
  paste0(rootd,"NY007BR1_mod.xlsx"),
  paste0(rootd,"NY007BR2_mod.xlsx"),
  paste0(rootd,"NY007LR1_mod.xlsx"),
  paste0(rootd,"NY007LR2_mod.xlsx"),
  #*
  paste0(rootd,"NY008 BR1_mod.xlsx"),
  paste0(rootd,"NY008 BR2_mod.xlsx"),
  paste0(rootd,"NY008 LR1_mod.xlsx"),
  paste0(rootd,"NY008 LR2_mod.xlsx"),
  #*
  paste0(rootd,"NY009BR1_mod.xlsx"),
  paste0(rootd,"NY009BR2_mod.xlsx"),
  paste0(rootd,"NY009LR1_mod.xlsx"),
  paste0(rootd,"NY009LR2_mod.xlsx"),
  #*
  paste0(rootd,"NY011BR1_mod.xlsx"),
  paste0(rootd,"NY011BR2_mod.xlsx"),
  paste0(rootd,"NY011LR1_mod.xlsx"),
  paste0(rootd,"NY011LR2_mod.xlsx"),

  #* Spottes out of place 1 in these
  paste0(rootd,"NY012BR1_mod.xlsx"),
  paste0(rootd,"NY012BR2_mod.xlsx"),
  paste0(rootd,"NY012LR1_mod.xlsx"),
  paste0(rootd,"NY012LR2_mod.xlsx"),
  #*
  paste0(rootd,"NY014BR1_mod.xlsx"),
  paste0(rootd,"NY014BR2_mod.xlsx"),
  paste0(rootd,"NY014LR1_mod.xlsx"),
  paste0(rootd,"NY014LR2_mod.xlsx"),
  #*
  paste0(rootd,"NY015 BR1_mod.xlsx"),
  paste0(rootd,"NY015 BR2_mod.xlsx"),
  paste0(rootd,"NY015 LR1_mod.xlsx"),
  paste0(rootd,"NY015 LR2_mod.xlsx"),
  #*
  paste0(rootd,"NY016BR1_mod.xlsx"),
  paste0(rootd,"NY016LR1_mod.xlsx"),
  #*
  paste0(rootd,"NY017BR1_mod.xlsx"),
  paste0(rootd,"NY017BR2_mod.xlsx"),
  paste0(rootd,"NY017LR1_mod.xlsx"),
  paste0(rootd,"NY017LR2_mod.xlsx"),
  #*
  paste0(rootd,"NY018BR1_mod.xlsx"),
  paste0(rootd,"NY018BR2_mod.xlsx"),
  paste0(rootd,"NY018LR1_mod.xlsx"),
  paste0(rootd,"NY018LR2_mod.xlsx"),
  #*
  paste0(rootd,"NY019BR1_mod.xlsx"),
  paste0(rootd,"NY019BR2_mod.xlsx"),
  paste0(rootd,"NY019LR1_mod.xlsx"),
  paste0(rootd,"NY019LR2_mod.xlsx"),
  #*
  paste0(rootd,"NY020BR1_mod.xlsx"),
  paste0(rootd,"NY020BR2_mod.xlsx"),
  paste0(rootd,"NY020LR1_mod.xlsx"),
  paste0(rootd,"NY020LR2_mod.xlsx"),
  #*
  paste0(rootd,"NY021BR1_mod.xlsx"),
  paste0(rootd,"NY021BR2_mod.xlsx"),
  paste0(rootd,"NY021LR1_mod.xlsx"),
  paste0(rootd,"NY021LR2_mod.xlsx"),

  paste0(rootd,"NY022 BR1_mod.xlsx"),
  paste0(rootd,"NY022 BR2_mod.xlsx"),
  paste0(rootd,"NY022 LR1_mod.xlsx"),
  paste0(rootd,"NY022 LR2_mod.xlsx"),

  #*
  paste0(rootd,"NY023BR1_mod.xlsx"),
  paste0(rootd,"NY023BR2_mod.xlsx"),
  paste0(rootd,"NY023LR1_mod.xlsx"),
  paste0(rootd,"NY023LR2_mod.xlsx"),

  paste0(rootd,"NY024 BR1_mod.xlsx"),
  paste0(rootd,"NY024 BR2_mod.xlsx"),
  paste0(rootd,"NY024 LR1_mod.xlsx"),
  paste0(rootd,"NY024 LR2_mod.xlsx"),
  #*
  paste0(rootd,"NY025BR1_mod.xlsx"),
  paste0(rootd,"NY025BR2_mod.xlsx"),
  paste0(rootd,"NY025LR1_mod.xlsx"),
  paste0(rootd,"NY025LR2_mod.xlsx"),
  #*
  paste0(rootd,"NY030BR1_mod.xlsx"),
  paste0(rootd,"NY030BR2_mod.xlsx"),
  paste0(rootd,"NY030LR1_mod.xlsx"),
  paste0(rootd,"NY030LR2_mod.xlsx"),
  #*
  paste0(rootd,"NY031BR1_mod.xlsx"),
  paste0(rootd,"NY031BR2_mod.xlsx"),
  paste0(rootd,"NY031LR1_mod.xlsx"),
  paste0(rootd,"NY031LR2_mod.xlsx"),
  #*
  paste0(rootd,"NY032BR1_mod.xlsx"),
  paste0(rootd,"NY032BR2_mod.xlsx"),
  paste0(rootd,"NY032LR1_mod.xlsx"),
  paste0(rootd,"NY032LR2_mod.xlsx"),
  #*
  paste0(rootd,"NY034BR1_mod.xlsx"),
  paste0(rootd,"NY034BR2_mod.xlsx"),
  paste0(rootd,"NY034LR1_mod.xlsx"),
  paste0(rootd,"NY034LR2_mod.xlsx"),
  #*
  paste0(rootd,"NY035BR1_mod.xlsx"),
  paste0(rootd,"NY035BR2_mod.xlsx"),
  paste0(rootd,"NY035LR1_mod.xlsx"),
  paste0(rootd,"NY035LR2_mod.xlsx"),
  #*
  paste0(rootd,"UCF001BR1_mod.xlsx"),
  paste0(rootd,"UCF001BR2_mod.xlsx"),
  paste0(rootd,"UCF001LR1_mod.xlsx"),
  paste0(rootd,"UCF001LR2_mod.xlsx"),
  paste0(rootd,"UCF002BR1_mod.xlsx"),
  paste0(rootd,"UCF002BR2_mod.xlsx"),
  paste0(rootd,"UCF002LR1_mod.xlsx"),
  paste0(rootd,"UCF002LR2_mod.xlsx"),
  paste0(rootd,"UCF003BR1_mod.xlsx"),
  paste0(rootd,"UCF003BR2_mod.xlsx"),
  paste0(rootd,"UCF003LR1_mod.xlsx"),
  paste0(rootd,"UCF003LR2_mod.xlsx"),
  paste0(rootd,"UCF004BR1_mod.xlsx"),
  paste0(rootd,"UCF004BR2_mod.xlsx"),
  paste0(rootd,"UCF004LR1_mod.xlsx"),
  paste0(rootd,"UCF004LR2_mod.xlsx"),

  paste0(rootd,"UCF5 BR1_mod.xlsx"),
  paste0(rootd,"UCF5 BR2_mod.xlsx"),
  paste0(rootd,"UCF5 LR1_mod.xlsx"),
  paste0(rootd,"UCF5 LR2_mod.xlsx"),
  #*
  paste0(rootd,"UCF006BR1_mod.xlsx"),
  paste0(rootd,"UCF006BR2_mod.xlsx"),
  paste0(rootd,"UCF006LR1_mod.xlsx"),
  paste0(rootd,"UCF006LR2_mod.xlsx"),
  paste0(rootd,"UCF007BR1_mod.xlsx"),
  paste0(rootd,"UCF007BR2_mod.xlsx"),
  paste0(rootd,"UCF007LR1_mod.xlsx"),
  paste0(rootd,"UCF007LR2_mod.xlsx"),
  paste0(rootd,"UCF8BR1_mod.xlsx"),
  paste0(rootd,"UCF8BR2_mod.xlsx"),
  paste0(rootd,"UCF8LR1_mod.xlsx"),
  paste0(rootd,"UCF8LR2_mod.xlsx"),
  paste0(rootd,"UCF009BR1_mod.xlsx"),
  paste0(rootd,"UCF009BR2_mod.xlsx"),
  paste0(rootd,"UCF009LR1_mod.xlsx"),
  paste0(rootd,"UCF009LR2_mod.xlsx"),
  #*
  paste0(rootd,"UCF10BR1_mod.xlsx"),
  paste0(rootd,"UCF10BR2_mod.xlsx"),
  paste0(rootd,"UCF10LR1_mod.xlsx"),
  paste0(rootd,"UCF10LR2_mod.xlsx"),

  paste0(rootd,"UCF11BR1_mod.xlsx"),
  paste0(rootd,"UCF11BR2_mod.xlsx"),
  paste0(rootd,"UCF11LR1_mod.xlsx"),
  paste0(rootd,"UCF11LR2_mod.xlsx"),
  #*
  paste0(rootd,"UCF012BR1_mod.xlsx"),
  paste0(rootd,"UCF012BR2_mod.xlsx"),
  paste0(rootd,"UCF012LR1_mod.xlsx"),
  paste0(rootd,"UCF012LR2_mod.xlsx"),
  paste0(rootd,"UCF013BR1_mod.xlsx"),
  paste0(rootd,"UCF013BR2_mod.xlsx"),
  paste0(rootd,"UCF013LR1_mod.xlsx"),
  paste0(rootd,"UCF013LR2_mod.xlsx"),
  paste0(rootd,"UCF014BR1_mod.xlsx"),
  paste0(rootd,"UCF014BR2_mod.xlsx"),
  paste0(rootd,"UCF014LR1_mod.xlsx"),
  paste0(rootd,"UCF014LR2_mod.xlsx"),
  paste0(rootd,"UCF015BR1_mod.xlsx"),
  paste0(rootd,"UCF015BR2_mod.xlsx"),
  paste0(rootd,"UCF015LR1_mod.xlsx"),
  paste0(rootd,"UCF015LR2_mod.xlsx"),
  paste0(rootd,"UCF016BR1_mod.xlsx"),
  paste0(rootd,"UCF016BR2_mod.xlsx"),
  paste0(rootd,"UCF016LR1_mod.xlsx"),
  paste0(rootd,"UCF016LR2_mod.xlsx"),
  paste0(rootd,"UCF017BR1_mod.xlsx"),
  paste0(rootd,"UCF017BR2_mod.xlsx"),
  paste0(rootd,"UCF017LR1_mod.xlsx"),
  paste0(rootd,"UCF017LR2_mod.xlsx"),
  paste0(rootd,"UCF018BR1_mod.xlsx"),
  paste0(rootd,"UCF018BR2_mod.xlsx"),
  paste0(rootd,"UCF018LR1_mod.xlsx"),
  paste0(rootd,"UCF018LR2_mod.xlsx"),
  paste0(rootd,"UCF019BR1_mod.xlsx"),
  paste0(rootd,"UCF019BR2_mod.xlsx"),
  paste0(rootd,"UCF019LR1_mod.xlsx"),
  paste0(rootd,"UCF019LR2_mod.xlsx"),
  paste0(rootd,"UCF020BR1_mod.xlsx"),
  paste0(rootd,"UCF020BR2_mod.xlsx"),
  paste0(rootd,"UCF020LR1_mod.xlsx"),
  paste0(rootd,"UCF021BR1_mod.xlsx"),
  paste0(rootd,"UCF021BR2_mod.xlsx"),
  paste0(rootd,"UCF021LR1_mod.xlsx"),
  paste0(rootd,"UCF021LR2_mod.xlsx"),
  paste0(rootd,"UCF022BR1_mod.xlsx"),
  paste0(rootd,"UCF022BR2_mod.xlsx"),
  paste0(rootd,"UCF022LR1_mod.xlsx"),
  paste0(rootd,"UCF022LR2_mod.xlsx"),
  paste0(rootd,"UCF023BR1_mod.xlsx"),
  paste0(rootd,"UCF023BR2_mod.xlsx"),
  paste0(rootd,"UCF023LR1_mod.xlsx"),
  paste0(rootd,"UCF023LR2_mod.xlsx"),
  paste0(rootd,"UCF024BR1_mod.xlsx"),
  paste0(rootd,"UCF024LR1_mod.xlsx"),
  paste0(rootd,"UCF024LR2_mod.xlsx"),
  paste0(rootd,"UCF025BR1_mod.xlsx"),
  paste0(rootd,"UCF025BR2_mod.xlsx"),
  paste0(rootd,"UCF025LR1_mod.xlsx"),
  paste0(rootd,"UCF025LR2_mod.xlsx")
)
fpths.floridaphase2

fpths.all <- c(
  fpths.original,
  fpths.floridaphase1,
  fpths.floridaphase2,
  fpths.wtck,
  fpths.wtcq
)

length(fpths.all)

write.csv(fpths.all, "tests/dust_data/old_formats_use_file_names.csv")
