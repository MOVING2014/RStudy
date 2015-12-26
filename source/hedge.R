require("ggplot2")
read.data.index <- function(x="m9888",dates="2000"){
  m9888 <- read.csv(file = paste0("/Users/moving/Documents/期货交易/",x,".csv"),header = FALSE,stringsAsFactors = FALSE)
  names(m9888) <- c("Date","OPEN","HIGH","LOW","CLOSE","VOLUME","Holds")
  m9888[m9888$Date>dates,c("Date","CLOSE")]
}

a9888 <- read.data.index("a9888")
y9888 <- read.data.index(x="y9888")
m9888 <- read.data.index("m9888")


product_list <-c("a9888","y9888","m9888")


merge_data <- function(product_list=c("a9888","y9888","m9888")){
  m <- merge(x=get(product_list[1]),y=get(product_list[2]),by = c("Date"))
  for (i in 3:length(product_list)) {
    m <- merge(m,get(product_list[i]),by = c("Date"))
  }
  names(m)<-c("dates",product_list)
  m
}

index.data <- m[,c("Date","CLOSE.x","CLOSE.y","CLOSE")]
names(index.data)<- c("dates","a9888","m9888","y9888")
index.data$dates <- as.POSIXct(index.data$dates)

index.data0 <- index.data[index.data$dates>
                            as.POSIXct("2014-01-01") & 
                            index.data$dates<as.POSIXct("2016-01-01"),]

model<-lm(log(index.data0$a9888)~log(index.data0$y9888)+log(index.data0$m9888))

summary(model)
c <- model$coefficients
names(c)<- c("intercept","y9888","m9888")
c


library("psych")
corr.test(index.data0[,-1])




combinedIndexlog <- c["y9888"] *log(index.data0$y9888) +
  c["m9888"]*log(index.data0$m9888)-
  log(index.data0$a9888)

combinedIndex <- c["y9888"] *(index.data0$y9888) +
  c["m9888"]*(index.data0$m9888)-
  (index.data0$a9888)


p1<- qplot(combinedIndex)
p2 <- qplot(x=index.data0$dates,y= combinedIndex) + scale_x_datetime(date_breaks  = "1 month")+
  theme(axis.text.x  = element_text(angle=90))

multiplot(p1,p2)

qplot(x=index.data0$dates,y= combinedIndex)



qplot(x=index.data0$dates,y= combinedIndex) + scale_x_datetime(date_breaks = "2 weeks")





