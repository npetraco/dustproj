#' A few new categories added to Particle/grains class
#'
#' XXXX
#'
#' The function will XXXX
#'
#' @param XX The XX
#' @return The function will XX
#'
#'
#' @export
read.dust.file.EXPANDED<-function(fpath)
{

  dat<-read.xlsx(fpath,1,header=FALSE)

  #Natural hair class:
  cl1<-as.character(dat[1,1])
  #Fiber subclass (row labels for class)
  sc1.1<-as.character(dat[2,1])
  sc1.2<-as.character(dat[3,1])
  sc1.3<-as.character(dat[4,1])
  #Class attributes (column labels for class)
  cl1.attr<-apply(dat[1,2:11],1,as.character )
  #Fiber class/subclass data
  tmp<-dat[2:4,2:11]
  cl1.dat<-as.numeric(matrix(t(tmp),nrow=(dim(tmp)[1]*dim(tmp)[2]),ncol=1))
  cl1.dat[is.na(cl1.dat)]<-0 #Data column (0=absence, 1=presence)

  #Build the class block of the dataframe
  col.class<-rep(cl1, length(cl1.dat) )
  col.subclass<-rbind(
    matrix(rep(sc1.1,length(cl1.attr)),nrow=length(cl1.attr),ncol=1),
    matrix(rep(sc1.2,length(cl1.attr)),nrow=length(cl1.attr),ncol=1),
    matrix(rep(sc1.3,length(cl1.attr)),nrow=length(cl1.attr),ncol=1)
  )
  col.attributes<-rep(cl1.attr,3)
  cl1.block<-cbind(col.class,col.subclass,col.attributes)


  #Treated hair class:
  cl2<-as.character(dat[5,1])
  #Fiber subclass
  sc2.1<-as.character(dat[6,1])
  sc2.2<-as.character(dat[7,1])
  #Fiber attributes
  cl2.attr<-apply(dat[5,c(2,7,8,9)],1,as.character )
  #Fiber class/subclass data
  tmp<-dat[6:7,c(2,7,8,9)]
  cl2.dat<-as.numeric(matrix(t(tmp),nrow=(dim(tmp)[1]*dim(tmp)[2]),ncol=1))
  cl2.dat[is.na(cl2.dat)]<-0 #Data column (0=absence, 1=presence)

  #Build the class block of the dataframe
  col.class<-rep(cl2, length(cl2.dat) )
  col.subclass<-rbind(
    matrix(rep(sc2.1,length(cl2.attr)),nrow=length(cl2.attr),ncol=1),
    matrix(rep(sc2.2,length(cl2.attr)),nrow=length(cl2.attr),ncol=1)
  )
  col.attributes<-rep(cl2.attr,2)
  cl2.block<-cbind(col.class,col.subclass,col.attributes)


  #Synthetic fiber class
  cl3<-as.character(dat[8,1])
  #Fiber subclass
  sc3.1<-as.character(dat[9,1])
  sc3.2<-as.character(dat[10,1])
  sc3.3<-as.character(dat[11,1])
  sc3.4<-as.character(dat[12,1])
  sc3.5<-as.character(dat[13,1])
  sc3.6<-as.character(dat[14,1])
  sc3.7<-as.character(dat[15,1])
  sc3.8<-as.character(dat[16,1])
  sc3.9<-as.character(dat[17,1])
  sc3.10<-as.character(dat[18,1])
  sc3.11<-as.character(dat[19,1])
  sc3.12<-as.character(dat[20,1])
  #Fiber attributes
  cl3.attr<-apply(dat[8,2:12],1,as.character )
  #Fiber class/subclass data
  tmp<-dat[9:20,2:12]
  cl3.dat<-as.numeric(matrix(t(tmp),nrow=(dim(tmp)[1]*dim(tmp)[2]),ncol=1))
  cl3.dat[is.na(cl3.dat)]<-0 #Data column (0=absence, 1=presence)

  #Build the class block of the dataframe
  col.class<-rep(cl3, length(cl3.dat) )
  col.subclass<-rbind(
    matrix(rep(sc3.1,length(cl3.attr)),nrow=length(cl3.attr),ncol=1),
    matrix(rep(sc3.2,length(cl3.attr)),nrow=length(cl3.attr),ncol=1),
    matrix(rep(sc3.3,length(cl3.attr)),nrow=length(cl3.attr),ncol=1),
    matrix(rep(sc3.4,length(cl3.attr)),nrow=length(cl3.attr),ncol=1),
    matrix(rep(sc3.5,length(cl3.attr)),nrow=length(cl3.attr),ncol=1),
    matrix(rep(sc3.6,length(cl3.attr)),nrow=length(cl3.attr),ncol=1),
    matrix(rep(sc3.7,length(cl3.attr)),nrow=length(cl3.attr),ncol=1),
    matrix(rep(sc3.8,length(cl3.attr)),nrow=length(cl3.attr),ncol=1),
    matrix(rep(sc3.9,length(cl3.attr)),nrow=length(cl3.attr),ncol=1),
    matrix(rep(sc3.10,length(cl3.attr)),nrow=length(cl3.attr),ncol=1),
    matrix(rep(sc3.11,length(cl3.attr)),nrow=length(cl3.attr),ncol=1),
    matrix(rep(sc3.12,length(cl3.attr)),nrow=length(cl3.attr),ncol=1)
  )
  col.attributes<-rep(cl3.attr,12)
  cl3.block<-cbind(col.class,col.subclass,col.attributes)


  #Animal hair class:
  cl4<-as.character(dat[21,1])
  #Fiber subclass
  sc4.1<-as.character(dat[22,1])
  sc4.2<-as.character(dat[23,1])
  sc4.3<-as.character(dat[24,1])
  sc4.4<-as.character(dat[25,1])
  sc4.5<-as.character(dat[26,1])
  sc4.6<-as.character(dat[27,1])
  #Fiber attributes
  cl4.attr<-apply(dat[21,2:16],1,as.character )
  #Fiber class/subclass data
  tmp<-dat[22:27,2:16]
  cl4.dat<-as.numeric(matrix(t(tmp),nrow=(dim(tmp)[1]*dim(tmp)[2]),ncol=1))
  cl4.dat[is.na(cl4.dat)]<-0 #Data column (0=absence, 1=presence)

  #Build the class block of the dataframe
  col.class<-rep(cl4, length(cl4.dat) )
  col.subclass<-rbind(
    matrix(rep(sc4.1,length(cl4.attr)),nrow=length(cl4.attr),ncol=1),
    matrix(rep(sc4.2,length(cl4.attr)),nrow=length(cl4.attr),ncol=1),
    matrix(rep(sc4.3,length(cl4.attr)),nrow=length(cl4.attr),ncol=1),
    matrix(rep(sc4.4,length(cl4.attr)),nrow=length(cl4.attr),ncol=1),
    matrix(rep(sc4.5,length(cl4.attr)),nrow=length(cl4.attr),ncol=1),
    matrix(rep(sc4.6,length(cl4.attr)),nrow=length(cl4.attr),ncol=1)
  )
  col.attributes<-rep(cl4.attr,6)
  cl4.block<-cbind(col.class,col.subclass,col.attributes)


  #Natural Fiber class:
  cl5<-as.character(dat[28,1])
  #Fiber subclass
  sc5.1<-as.character(dat[29,1])
  sc5.2<-as.character(dat[30,1])
  sc5.3<-as.character(dat[31,1])
  sc5.4<-as.character(dat[32,1])
  sc5.5<-as.character(dat[33,1])
  sc5.6<-as.character(dat[34,1])
  sc5.7<-as.character(dat[35,1])
  sc5.8<-as.character(dat[36,1])
  sc5.9<-as.character(dat[37,1])
  sc5.10<-as.character(dat[38,1])
  #Fiber attributes
  cl5.attr<-apply(dat[28,2:16],1,as.character )
  #Fiber class/subclass data
  tmp<-dat[29:38,2:16]
  cl5.dat<-as.numeric(matrix(t(tmp),nrow=(dim(tmp)[1]*dim(tmp)[2]),ncol=1))
  cl5.dat[is.na(cl5.dat)]<-0 #Data column (0=absence, 1=presence)

  #Build the class block of the dataframe
  col.class<-rep(cl5, length(cl5.dat) )
  col.subclass<-rbind(
    matrix(rep(sc5.1,length(cl5.attr)),nrow=length(cl5.attr),ncol=1),
    matrix(rep(sc5.2,length(cl5.attr)),nrow=length(cl5.attr),ncol=1),
    matrix(rep(sc5.3,length(cl5.attr)),nrow=length(cl5.attr),ncol=1),
    matrix(rep(sc5.4,length(cl5.attr)),nrow=length(cl5.attr),ncol=1),
    matrix(rep(sc5.5,length(cl5.attr)),nrow=length(cl5.attr),ncol=1),
    matrix(rep(sc5.6,length(cl5.attr)),nrow=length(cl5.attr),ncol=1),
    matrix(rep(sc5.7,length(cl5.attr)),nrow=length(cl5.attr),ncol=1),
    matrix(rep(sc5.8,length(cl5.attr)),nrow=length(cl5.attr),ncol=1),
    matrix(rep(sc5.9,length(cl5.attr)),nrow=length(cl5.attr),ncol=1),
    matrix(rep(sc5.10,length(cl5.attr)),nrow=length(cl5.attr),ncol=1)
  )
  col.attributes<-rep(cl5.attr,10)
  cl5.block<-cbind(col.class,col.subclass,col.attributes)


  #Mineral Fiber class:
  cl6<-as.character(dat[39,1])
  #Fiber subclass
  sc6.1<-as.character(dat[40,1])
  sc6.2<-as.character(dat[41,1])
  sc6.3<-as.character(dat[42,1])
  #Fiber attributes
  #cl6.attr<-apply(dat[39,2:5],1,as.character )
  cl6.attr<-"is.present?"
  #Fiber class/subclass data
  tmp<-as.matrix(dat[40:42,2])
  cl6.dat<-as.numeric(matrix(t(tmp),nrow=(dim(tmp)[1]*dim(tmp)[2]),ncol=1))
  cl6.dat[is.na(cl6.dat)]<-0 #Data column (0=absence, 1=presence)

  #Build the class block of the dataframe
  col.class<-rep(cl6, length(cl6.dat) )
  col.subclass<-rbind(
    matrix(rep(sc6.1,length(cl6.attr)),nrow=length(cl6.attr),ncol=1),
    matrix(rep(sc6.2,length(cl6.attr)),nrow=length(cl6.attr),ncol=1),
    matrix(rep(sc6.3,length(cl6.attr)),nrow=length(cl6.attr),ncol=1)
  )
  col.attributes<-rep(cl6.attr,3)
  cl6.block<-cbind(col.class,col.subclass,col.attributes)


  #Particles/Grains class (Called Glass/Mineral Grains in data sheet):
  cl7<-as.character(dat[43,1])
  #Fiber subclass
  sc7.1<-as.character(dat[44,1])
  sc7.2<-as.character(dat[45,1])
  sc7.3<-as.character(dat[46,1])
  sc7.4<-as.character(dat[47,1])
  sc7.5<-as.character(dat[48,1])
  sc7.6<-as.character(dat[49,1])
  sc7.7<-as.character(dat[50,1])
  sc7.8<-as.character(dat[51,1])
  sc7.9<-as.character(dat[52,1])
  sc7.10<-as.character(dat[53,1])
  sc7.11<-as.character(dat[54,1])
  sc7.12<-as.character(dat[55,1])
  sc7.13<-as.character(dat[56,1])
  sc7.14<-as.character(dat[57,1])
  sc7.15<-as.character(dat[58,1])
  sc7.16<-as.character(dat[59,1])
  sc7.17<-as.character(dat[60,1])
  sc7.18<-as.character(dat[61,1])
  sc7.19<-as.character(dat[62,1])
  sc7.20<-as.character(dat[63,1])
  sc7.21<-as.character(dat[64,1])
  sc7.22<-as.character(dat[65,1]) #New fiber category
  sc7.23<-as.character(dat[66,1]) #New fiber category
  sc7.24<-as.character(dat[67,1]) #New fiber category
  sc7.25<-as.character(dat[68,1]) #New fiber category
  sc7.26<-as.character(dat[69,1]) #New fiber category
  sc7.27<-as.character(dat[70,1]) #New fiber category
  sc7.28<-as.character(dat[71,1]) #New fiber category
  sc7.29<-as.character(dat[72,1]) #New fiber category
  #Fiber attributes
  cl7.attr<-apply(dat[43,2:7],1,as.character )
  #Fiber class/subclass data
  tmp<-dat[44:72,2:7]
  cl7.dat<-as.numeric(matrix(t(tmp),nrow=(dim(tmp)[1]*dim(tmp)[2]),ncol=1))
  cl7.dat[is.na(cl7.dat)]<-0 #Data column (0=absence, 1=presence)

  #Build the class block of the dataframe
  col.class<-rep(cl7, length(cl7.dat) )
  col.subclass<-rbind(
    matrix(rep(sc7.1,length(cl7.attr)),nrow=length(cl7.attr),ncol=1),
    matrix(rep(sc7.2,length(cl7.attr)),nrow=length(cl7.attr),ncol=1),
    matrix(rep(sc7.3,length(cl7.attr)),nrow=length(cl7.attr),ncol=1),
    matrix(rep(sc7.4,length(cl7.attr)),nrow=length(cl7.attr),ncol=1),
    matrix(rep(sc7.5,length(cl7.attr)),nrow=length(cl7.attr),ncol=1),
    matrix(rep(sc7.6,length(cl7.attr)),nrow=length(cl7.attr),ncol=1),
    matrix(rep(sc7.7,length(cl7.attr)),nrow=length(cl7.attr),ncol=1),
    matrix(rep(sc7.8,length(cl7.attr)),nrow=length(cl7.attr),ncol=1),
    matrix(rep(sc7.9,length(cl7.attr)),nrow=length(cl7.attr),ncol=1),
    matrix(rep(sc7.10,length(cl7.attr)),nrow=length(cl7.attr),ncol=1),
    matrix(rep(sc7.11,length(cl7.attr)),nrow=length(cl7.attr),ncol=1),
    matrix(rep(sc7.12,length(cl7.attr)),nrow=length(cl7.attr),ncol=1),
    matrix(rep(sc7.13,length(cl7.attr)),nrow=length(cl7.attr),ncol=1),
    matrix(rep(sc7.14,length(cl7.attr)),nrow=length(cl7.attr),ncol=1),
    matrix(rep(sc7.15,length(cl7.attr)),nrow=length(cl7.attr),ncol=1),
    matrix(rep(sc7.16,length(cl7.attr)),nrow=length(cl7.attr),ncol=1),
    matrix(rep(sc7.17,length(cl7.attr)),nrow=length(cl7.attr),ncol=1),
    matrix(rep(sc7.18,length(cl7.attr)),nrow=length(cl7.attr),ncol=1),
    matrix(rep(sc7.19,length(cl7.attr)),nrow=length(cl7.attr),ncol=1),
    matrix(rep(sc7.20,length(cl7.attr)),nrow=length(cl7.attr),ncol=1),
    matrix(rep(sc7.21,length(cl7.attr)),nrow=length(cl7.attr),ncol=1),
    matrix(rep(sc7.22,length(cl7.attr)),nrow=length(cl7.attr),ncol=1), #New fiber category
    matrix(rep(sc7.23,length(cl7.attr)),nrow=length(cl7.attr),ncol=1), #New fiber category
    matrix(rep(sc7.24,length(cl7.attr)),nrow=length(cl7.attr),ncol=1), #New fiber category
    matrix(rep(sc7.25,length(cl7.attr)),nrow=length(cl7.attr),ncol=1), #New fiber category
    matrix(rep(sc7.26,length(cl7.attr)),nrow=length(cl7.attr),ncol=1), #New fiber category
    matrix(rep(sc7.27,length(cl7.attr)),nrow=length(cl7.attr),ncol=1), #New fiber category
    matrix(rep(sc7.28,length(cl7.attr)),nrow=length(cl7.attr),ncol=1), #New fiber category
    matrix(rep(sc7.29,length(cl7.attr)),nrow=length(cl7.attr),ncol=1)  #New fiber category
  )
  col.attributes<-rep(cl7.attr,29)
  cl7.block<-cbind(col.class,col.subclass,col.attributes)

  datvec<-c(cl1.dat,cl2.dat,cl3.dat,cl4.dat,cl5.dat,cl6.dat,cl7.dat)
  namesmat<-rbind(cl1.block,cl2.block,cl3.block,cl4.block,cl5.block,cl6.block,cl7.block)

  return(list(datvec,namesmat))

}
