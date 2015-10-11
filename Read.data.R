require("ggplot2")
source("multplot.R")
read.data <- function(dates="20150907",spe="m1601",location="dc"){
  file_name <-"/Users/moving/Documents/期货交易/201509/"
  file_name <-paste0(file_name,location,"/")
  file_name <- paste0(file_name,dates,"/",spe,"_",dates,".csv")
  read.csv(file=file_name,fileEncoding = "GBK")
  
}

summary.dir <- function(a){
  x<-summary(a$方向)
  x<-c(x,x[1]/sum(x)*100)
  names(x)<-c("B","S","buy ratio")
  x
}
plottrends <- function(a){ 
  #par(mfrow=c(1,2))
 #plot(a$最新,family='STXihei')
 #plot(a$成交类型,family='STXihei')
  titles<- a$合约代码[1]
  date <- a$时间
  mm<-summary.dir(a)[3]
  mm<-paste(mm)
  p1<-ggplot(data=a,aes_string(x="成交类型",fill="成交类型"))+
    geom_bar()+theme_gray(base_family = "STXihei")+labs(title =paste(titles,date))
  p2<-ggplot(data=a,aes_string(x="(1:length(最新))",y="最新"))+
    geom_point()+theme_gray(base_family = "STXihei")+labs(title=paste("buy ratio",mm))
  multiplot(p1,p2)
  }

allinone <- function(dates="20150901",spe="m1601",location="dc")
{
  an <- read.data(dates,spe,location)
  plottrends(an)
  summary.dir(an)
}
#allinone(dates = "20150901",spe = "TA601",location = "zc")
