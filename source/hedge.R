require("ggplot2")
read.data.index <- function(x="m9888",dates="2000"){
  m9888 <- read.csv(file = paste0("/Users/moving/Documents/期货交易/",x,".csv"),header = FALSE,stringsAsFactors = FALSE)
  names(m9888) <- c("Date","OPEN","HIGH","LOW","CLOSE","VOLUME","Holds")
  m9888[m9888$Date>dates,c("Date","CLOSE")]
}


merge_data <- function(product_list=c("a9888","y9888","m9888")){
  m <- merge(x=get(product_list[1]),y=get(product_list[2]),by = c("Date"))
  for (i in 3:length(product_list)) {
    m <- merge(m,get(product_list[i]),by = c("Date"))
  }
  names(m)<-c("dates",product_list)
  m$dates <- as.POSIXct(m$dates)
  m
}

model_function <- function(x =datax,ind=index.data){
  y <- ncol(x)
  l <- list()

  for(i in 1:y){
    f <- paste0("ind$",x[1,i],"~ind$",x[2,i])  #生产方程表达式
    if(nrow(x)>2){
      for(j in 3:nrow(x)){
        f <- paste0(f,"+ind$",x[j,i])
      }
      }
    model<-lm(as.formula(f))
    modelsummary<- summary(model)
    l[[i]] <- c(x[,i],modelsummary$r.squared,model$coefficients[-1])
  }
  l
}

model_data3 <- function(index.data=index.data0,datax,i=1){
  x <-model_function(ind = index.data,x = datax)
  combinedIndex <- -1*index.data0[x[[i]][1]]  + 
    index.data0[x[[i]][2]]*as.double(x[[i]][5]) + 
    index.data0[x[[i]][3]] *as.double(x[[i]][6])
  unlist(combinedIndex)
}

model_data2 <- function(index.data=index.data0,datax,i=1){
  x <-model_function(ind = index.data,x = datax)
  combinedIndex <- -1*index.data0[x[[i]][1]]  + 
    index.data0[x[[i]][2]]*as.double(x[[i]][4]) 
  unlist(combinedIndex)
}


get_index.data0 <- function(index.data,begin="2014-01-01",end="2016-01-01"){
index.data0 <- index.data[index.data$dates>
                            as.POSIXct(begin) & 
                            index.data$dates<as.POSIXct(end),]
index.data0

}


a9888 <- read.data.index("a9888")
y9888 <- read.data.index(x="y9888")
m9888 <- read.data.index("m9888")
c9888 <- read.data.index("c9888")
product_list <-c("a9888","y9888","m9888","c9888")



index.data <- merge_data(product_list = product_list)

datax <- combn(product_list[1:length(product_list)], 3) 

model_function(x = datax,ind = index.data0)

index.data0 <- get_index.data0(index.data,begin = "2014-06-01", end = "2015-12-01")

l<-model_function(x = datax,ind = index.data0)
combinedIndex <- model_data2(index.data0,datax = datax,i=3)


p1<- qplot(combinedIndex)
p2 <- qplot(x=index.data0$dates,y= combinedIndex) + 
  scale_x_datetime(date_breaks  = "1 month")+
  theme(axis.text.x  = element_text(angle=90))
multiplot(p1,p2)



