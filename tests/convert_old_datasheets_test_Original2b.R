library(dustproj)

options(max.print=1000000)

study.name <- "Original"

# Build file paths to loop over
rootd <- "tests/dust_data/old_formats/"
file.root<-"dust_table_"
path.to.files<-paste(rootd,file.root,sep="")
num.of.samples<-79
num.of.reps<-c(rep(5,36),rep(1,23),rep(5,20))
fpths <- NULL
count <- 1
for(i in 1:num.of.samples) {
  for(j in 1:num.of.reps[i]) {
    dfpath.in <- paste(path.to.files,i,".",j,"_sfsc-mod.xlsx",sep="")
    fpths     <- rbind(fpths, dfpath.in)
    print(paste(count, dfpath.in))
    count     <- count + 1
  }
}


#study.conversion.table.info <- parse.conversion.table.expt3("inst/category_conversion_tables3.xlsx", study.name = study.name)

# Now loop over the file paths and see what breaks:
fpths

for(i in 126:length(fpths)) {
  print(paste(i, fpths[i]))
  test.a.sheet.conversion(datasheet.file.path = fpths[i],
                          study.name = study.name,
                          conversion.sheet.file.path = "inst/category_conversion_tables3.xlsx",
                          print.lvl=0)
}

test.a.sheet.conversion(datasheet.file.path = fpths[111],
                        study.name = study.name,
                        conversion.sheet.file.path = "inst/category_conversion_tables3.xlsx",
                        print.lvl=2)
