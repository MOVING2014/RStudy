read.data.index.sina.delta <- function(Y0="Y0",type="CO"){
Y0 <- readSinaData(spe = Y0)
if(type=="CO"){
Y0$delta <- Y0$CLOSE - Y0$OPEN
}else if(type=="HO"){
  Y0$delta <- Y0$HIGH - Y0$OPEN
}else if(type=="HL"){
  Y0$delta <- Y0$HIGH - Y0$LOW
}else if(type=="CC"){
  Y0$delta <- c(0,Y0$CLOSE[2:nrow(Y0)] - Y0$CLOSE[1:(nrow(Y0)-1)])
}else if(type=="OC"){
  Y0$delta <- c(0,Y0$OPEN[2:nrow(Y0)] - Y0$CLOSE[1:(nrow(Y0)-1)])
}else if(type=="LO"){
  Y0$delta <- Y0$LOW - Y0$OPEN
}

Y0$year <- substr(Y0$Date,1,4)
Y0
}

densityPlot <- function(Y0,year="2015",...){
#for test purpose
#Y0 <- read.data.index.sina.delta("v0",type = "HO")
#year="2009"
parameters <- list(...)
dataPlot <- Y0[which(Y0$year==year),]
boxplotStats <- boxplot.stats(dataPlot$delta)
sdPlot <- sd(dataPlot$delta)
if(length(boxplotStats$out) > 0 ){
   sdPlot <- sd(dataPlot$delta[-which(dataPlot$delta %in% boxplotStats$out)])
}
#sdPlot
meanPlot <- mean(dataPlot$delta)
#meanPlot
m <- ggplot(dataPlot, aes(x = delta)) 

p <- qplot(x=1,y=1,geom = "blank")
if(nrow(dataPlot) > 0){
p <- m + 
  geom_histogram(aes_string(y="..density..",fill="year"),fill="black") + 
  stat_function(fun = dnorm,arg = list(mean = meanPlot, sd=sdPlot), colour = "red") +
  #facet_wrap(~ year) +
  geom_vline(xintercept = c(meanPlot-sdPlot,meanPlot+sdPlot,meanPlot),color="blue") +
  geom_vline(xintercept = boxplotStats$stats,color="gray") +
  labs(title = paste0("m=",format(meanPlot,digits = 2),
                      "|s=",format(sdPlot,digits = 2),
                      "|c=",nrow(dataPlot),
                      "|ol=",length(boxplotStats$out)))+
  labs(x = paste0(year,"|",parameters$spe,"|",parameters$type))
}
list(p=p,sd=sdPlot,mean=meanPlot)
}

 #p2008 <- densityPlot(Y0,year = "2008")
# p2009 <- densityPlot(Y0,year = "2009")
# p2010 <- densityPlot(Y0,year = "2010")
# p2011 <- densityPlot(Y0,year = "2011")
# p2012 <- densityPlot(Y0,year = "2012")
# p2013 <- densityPlot(Y0,year = "2013")
# p2014 <- densityPlot(Y0,year = "2014")
# p2015 <- densityPlot(Y0,year = "2015")
# p2016 <- densityPlot(Y0,year = "2016")
# multiplot(p2008$p,p2011$p,p2014$p,
#           p2009$p,p2012$p,p2015$p,
#           p2010$p,p2013$p,p2016$p,
#           cols = 3)


qqPlot1 <- function(Y0){

ggplot(Y0, aes_string(sample="delta",color="year")) + 
  stat_qq() + 
  facet_wrap(~year,scales = "free_y") +
  #geom_vline(xintercept = c(-1,1)) + 
  labs(title = "QQ Plot By Year")
}
  
  
testDot <- function(...){
  a <- list(...)
  pnorm(...)
  
}
#m <- ggplot(dataPlot, aes(x = Date, y = delta)) 
#p <- m + 
#  geom_line(aes_string(y="delta",fill="year"),fill="black") 
#想看一下 CO和HO的关系，以及CO有没有时间依赖

