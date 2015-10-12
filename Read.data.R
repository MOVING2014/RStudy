require("ggplot2")
source("multplot.R")
read.data <- function(dates="20150907",spe="m1601",location="dc"){
  file_name <-"/Users/moving/Documents/期货交易/201509/"
  file_name <-paste0(file_name,location,"/")
  file_name <- paste0(file_name,dates,"/",spe,"_",dates,".csv")
  a<-read.csv(file=file_name,fileEncoding = "GBK")
  a[a$成交额!=0,]
  
}

summary.dir <- function(a){
  x<-summary(a$方向)
  x<-c(x,x[1]/sum(x)*100)
  names(x)<-c("B","S","buy ratio")
  x
}
plottrends <- function(a,bb=1000,cc=1000){ 
  titles<- a$合约代码[1]
  date <- a$时间
  mm<-summary.dir(a)[3]
  mm<-paste(mm)
  a$index<-c(1:nrow(a))
  a_kc<-a[a$开仓>bb,]
  a_pc<-a[a$平仓>cc,]
  p1<-ggplot(data=a,aes_string(x="成交类型",fill="成交类型"))+
    geom_bar()+theme_gray(base_family = "STXihei")+labs(title =paste(titles,date))
  
  p2<-ggplot(data=a,aes_string(x="index",y="最新"))+
    geom_point()+theme_gray(base_family = "STXihei")+labs(title=paste("buy ratio",mm))+
    geom_point(data=a_kc,aes_string(x="index",y="最新",colour="方向"),size=5,alpha=0.5)+
    geom_point(data=a_pc,aes_string(x="index",y="最新",colour="方向",fill="方向"),shape=2,size=5)
  
  multiplot(p1,p2)
  }

allinone <- function(dates="20150901",spe="m1601",location="dc")
{
  an <- read.data(dates,spe,location)
  plottrends(an)
  plotbar(an)
  plotbox(an)
  an$hour <- substr(an$时间,9,13)
  an<-an[substr(an$hour,4,5) %in% c("09","10","11","13","14","21","22","23"),]
  print(summary(an))
}

plotbox <- function(a1){
  
  a1$hour <- substr(a1$时间,9,13)
  a1<-a1[substr(a1$hour,4,5) %in% c("09","10","11","13","14","21","22","23"),]
  cc<-diff(range(a1$最新/30))
  p <- ggplot(data=a1,aes_string(x="最新"))+geom_bar(aes_string(fill="factor(成交类型)"),position = "dodge")+
    coord_flip()+facet_wrap(~hour,nrow =1)+
    geom_point(data=a1[1,],aes_string(y="0",x="最新"),colour="red",size=5)+
    geom_point(data=a1[nrow(a1),],aes_string(y="0",x="最新"),colour="blue",size=5)+
    theme_gray(base_family = "STXihei")
  multiplot(p)
}
plotbar<-function(a1){
  a1$hour <- substr(a1$时间,9,13)
  a1<-a1[substr(a1$hour,4,5) %in% c("09","10","11","13","14","21","22","23"),]
  p <- ggplot(data=a1,aes_string(x="hour",fill="hour"))+
  geom_bar(position = "dodge")+
    facet_wrap(~成交类型,nrow =1)+
    theme_gray(base_family = "STXihei")
  multiplot(p)
}

#allinone(dates = "20150902",spe = "TA601",location = "zc")
getdate<-function(x="201509"){
goodsdates <- c("20150901"
                ,"20150902"
                ,"20150907"
                ,"20150908"
                ,"20150909"
                ,"20150910"
                ,"20150911"
                ,"20150914"
                ,"20150915"
                ,"20150916"
                ,"20150917"
                ,"20150918"
                ,"20150921"
                ,"20150922"
                ,"20150923"
                ,"20150924"
                ,"20150925"
                ,"20150928"
                ,"20150929"
                ,"20150930"
                )
m <- list("201509"=goodsdates)
m[[x]]
}

allinonefor <- function(spe="m1601",location="dc",dates="201509"){
y <- getdate(dates)
for (i in y) {
  allinone(dates = i,spe = spe,location = location )
}
}