library(dustproj)

# Fix Synthetic Fiber subclasses of Original and WTCq data sheets. Write the modified data sheets to file
rootd <- "tests/dust_data/old_formats/"
file.root<-"dust_table_"
path.to.files<-paste(rootd,file.root,sep="")
num.of.samples<-79
num.of.reps<-c(rep(5,36),rep(1,23),rep(5,20))

replacement.syntheticfiber.subclasses <- c(
  "Rug Olefin Triangular X-S",
  "Rug Nylon Triangular X-S",
  "Rug Polyester Triangular X-S",
  "Olefin Any X-S",
  "Nylon Any X-S",
  "Polyester Any X-S",
  "Acrylic Dogbone X-S",
  "Acrylic Round X-S",
  "Acrylic Other X-S",
  "Modacrylic",
  "Rayon",
  "Acetate/Triacetate"
)


count <- 1
for(i in 1:num.of.samples) {
  for(j in 1:num.of.reps[i]) {
    dfpath.in  <- paste(path.to.files,i,".",j,".xlsx",sep="")
    dfpath.out <- paste(path.to.files,i,".",j,"_sfsc-mod.xlsx",sep="")
    print(paste(count, dfpath.in))
    #print(paste(count, dfpath.out))
    replace.a.subclass(in.fpath.datasheet  = dfpath.in,
                       subclass.col.range  = 9:20,
                       replacement.names   = replacement.syntheticfiber.subclasses,
                       out.fpath.datasheet = dfpath.out)
    #print(paste("Done sample",i,"replicate",j))
    count <- count + 1
  }
}


# WTCq study datasheets
#jdat <- read.xlsx("tests/dust_data/old_formats/WTC-Q_Dust from Q flag, tape and rope_samp1.xlsx", 1, header = FALSE)

replace.a.subclass(in.fpath.datasheet  = "tests/dust_data/old_formats/WTC-Q_Dust from Q flag, tape and rope_samp1.xlsx",
                   subclass.col.range  = 9:20,
                   replacement.names   = replacement.syntheticfiber.subclasses,
                   out.fpath.datasheet = "tests/dust_data/old_formats/WTC-Q_Dust from Q flag, tape and rope_samp1_sfsc-mod.xlsx")

replace.a.subclass(in.fpath.datasheet  = "tests/dust_data/old_formats/WTC-Q_Dust from Q flag, tape and rope_samp2.xlsx",
                   subclass.col.range  = 9:20,
                   replacement.names   = replacement.syntheticfiber.subclasses,
                   out.fpath.datasheet = "tests/dust_data/old_formats/WTC-Q_Dust from Q flag, tape and rope_samp2_sfsc-mod.xlsx")

replace.a.subclass(in.fpath.datasheet  = "tests/dust_data/old_formats/WTC-Q_Dust from Q flag, tape and rope_samp3.xlsx",
                   subclass.col.range  = 9:20,
                   replacement.names   = replacement.syntheticfiber.subclasses,
                   out.fpath.datasheet = "tests/dust_data/old_formats/WTC-Q_Dust from Q flag, tape and rope_samp3_sfsc-mod.xlsx")

replace.a.subclass(in.fpath.datasheet  = "tests/dust_data/old_formats/WTC-Q_Dust from Q flag, tape and rope_samp4.xlsx",
                   subclass.col.range  = 9:20,
                   replacement.names   = replacement.syntheticfiber.subclasses,
                   out.fpath.datasheet = "tests/dust_data/old_formats/WTC-Q_Dust from Q flag, tape and rope_samp4_sfsc-mod.xlsx")

replace.a.subclass(in.fpath.datasheet  = "tests/dust_data/old_formats/WTC-Q_Dust from Q flag, tape and rope_samp5.xlsx",
                   subclass.col.range  = 9:20,
                   replacement.names   = replacement.syntheticfiber.subclasses,
                   out.fpath.datasheet = "tests/dust_data/old_formats/WTC-Q_Dust from Q flag, tape and rope_samp5_sfsc-mod.xlsx")

