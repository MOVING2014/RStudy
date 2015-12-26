require("ggplot2")
read.data.index <- function(x="m9888",dates="2000"){
  m9888 <- read.csv(file = paste0("/Users/moving/Documents/期货交易/",x,".csv"),header = FALSE,stringsAsFactors = FALSE)
  names(m9888) <- c("Date","OPEN","HIGH","LOW","CLOSE","VOLUME","Holds")
  m9888[m9888$Date>dates,]
}

a9888 <- read.data.index("a9888")
y9888 <- read.data.index(x="y9888")
m9888 <- read.data.index()

m <- merge(a9888,m9888,by = c("Date"))
m <- merge(m,y9888,by = c("Date"))

index.data <- m[,c("Date","CLOSE.x","CLOSE.y","CLOSE")]
names(index.data)<- c("dates","a9888","m9888","y9888")
index.data$dates <- as.POSIXct(index.data$dates)

index.data0 <- index.data[index.data$dates>
                            as.POSIXct("2015-01-01") & 
                            index.data$dates<as.POSIXct("2015-12-01"),]

model<-lm(log(index.data0$a9888)~log(index.data0$y9888)+log(index.data0$m9888))

summary(model)
c <- model$coefficients
names(c)<- c("intercept","y9888","m9888")
c



combinedIndexlog <- c["y9888"] *log(index.data0$y9888) +
  c["m9888"]*log(index.data0$m9888)-
  log(index.data0$a9888)

combinedIndex <- c["y9888"] *(index.data0$y9888) +
  c["m9888"]*(index.data0$m9888)-
  (index.data0$a9888)


p1<- qplot(combinedIndex)
p2 <- qplot(x=index.data0$dates,y= combinedIndex)
multiplot(p1,p2)







