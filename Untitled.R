p<-function(x,lamda){
  lamda^x*exp(-lamda)/gamma(x+1)
}


f1 <- function(n){
  x<-as.integer(runif(1e6)*1e6)
  y<-sample(x,100*n)
  y<-sort(y)
  y<-y[1:100*n]
  y1<-c(0,y[1:(length(y)-1)])
  m <- abs(y1-y)/1e6*100
  m
}
f2 <- function(n){max(f1(n))}


f3 <- function(n){
  i<-0
  while(i<n){
    if(i==0) x <- f2(20)
    else x <- c(x,f2(20))
    i<-i+1
  }
  x
}



dpois(20*3,20)*1e3




teacher <- function(x,...) UseMethod("teacher")
teacher.lecture <- function(){print ("mathmatics")}



a <- read.csv(file="/Users/moving/Documents/期货交易/201509/dc/20150901/m1601_20150901_gbk.csv")
