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
  l <-list()

  for(i in 1:y){
    f <- paste0("ind$",x[1,i],"~ind$",x[2,i])  #生产方程表达式
    if(nrow(x)>2){
      for(j in 3:nrow(x)){
        f <- paste0(f,"+ind$",x[j,i])
      }
      }
    model<-lm(as.formula(f))
    modelsummary<- summary(model)
    #l <- rbind(l,c(x[,i],1))
   l[[i]] <- c(x[,i],modelsummary$r.squared,model$coefficients[-1])
  }
  l <- t(as.data.frame(l))
  row.names(l) <-c(1:nrow(l))
  
  l <- as.data.frame(l)
  names(l)[nrow(x)+1] <- "Rsqure"
  
  if(ncol(l)==6){  # calculation with 3 variables
    
    l$formula <- paste0( "index.data$",l[,1],"* (-1)"
                         ,"+","index.data$", l[,2],"*(", l[,2+3] ,")"
                         ,"+","index.data$",l[,3],"*(", l[,3+3],")")
  }
  
  if(ncol(l)==4){  # calculation with 2 variables
    
    l$formula <- paste0( "index.data$",l[,1],"* (-1)"
                         ,"+","index.data$", l[,2],"*(", l[,2+2] ,")")
  }
  if(ncol(l)==8){  # calculation with 2 variables
    
    l$formula <- paste0( "index.data$",l[,1],"* (-1)"
                         ,"+","index.data$", l[,2],"*(", l[,2+4] ,")"
                         ,"+","index.data$",l[,3],"*(", l[,3+4],")"
                         ,"+","index.data$",l[,4],"*(", l[,3+4],")")
  }
  
  l
}


# model_function()
# m<-model_function()
# > model_function(x = datax,ind = index.data0)
# V1    V2    V3            Rsqure                 V5                 V6
# 1 a9888 y9888 m9888 0.342984325291156 0.0610850256939007  0.337430592244809
# 2 a9888 y9888 c9888 0.478647769933671  0.212267155858971  0.437265050962009
# 3 a9888 m9888 c9888 0.505066622955399  0.302911549963297  0.409160614534154
# 4 y9888 m9888 c9888 0.750933182313661   1.11569716312219 0.0121091537036765
# formula
# 1 index.data$a9888* (-1)+index.data$y9888*(0.0610850256939007)+index.data$m9888*(0.337430592244809)
# 2  index.data$a9888* (-1)+index.data$y9888*(0.212267155858971)+index.data$c9888*(0.437265050962009)
# 3  index.data$a9888* (-1)+index.data$m9888*(0.302911549963297)+index.data$c9888*(0.409160614534154)
# 4  index.data$y9888* (-1)+index.data$m9888*(1.11569716312219)+index.data$c9888*(0.0121091537036765)


get_index.data0 <- function(index.data,begin="2014-01-01",end="2016-01-01"){
index.data0 <- index.data[index.data$dates>
                            as.POSIXct(begin) & 
                            index.data$dates<as.POSIXct(end),]
index.data0

}

######初始化 赋值#####
a9888 <- read.data.index("a9888")
y9888 <- read.data.index(x="y9888")
m9888 <- read.data.index("m9888")
c9888 <- read.data.index("c9888")
SR888 <- read.data.index("SR888")
TA888 <- read.data.index("TA888")
l9888 <- read.data.index("l9888")
p9888 <- read.data.index("p9888")
v9888 <- read.data.index("v9888")

product_list <-c("a9888","y9888"
                 ,"m9888","c9888"
                 ,"SR888","TA888"
                 ,"l9888","p9888"
                 ,"v9888")



index.data <- merge_data(product_list = product_list)
# > head(index.data)
# dates a9888 y9888 m9888 c9888
# 1 2006-01-09  2746  5096  2392  1389
# 2 2006-01-10  2735  5086  2389  1387
# 3 2006-01-11  2711  5080  2360  1384
# 4 2006-01-12  2686  5086  2339  1368
# 5 2006-01-13  2663  5036  2310  1373
# 6 2006-01-16  2686  5058  2329  1387

datax <- combn(product_list[1:length(product_list)], 4) 
# > datax
# [,1]    [,2]    [,3]    [,4]   
# [1,] "a9888" "a9888" "a9888" "y9888"
# [2,] "y9888" "y9888" "m9888" "m9888"
# [3,] "m9888" "c9888" "c9888" "c9888"



index.data0 <- get_index.data0(index.data,begin = "2015-10-01", end = "2015-12-01")
l<-model_function(x = datax,ind = index.data0)


# > head(index.data)
# dates a9888 y9888 m9888 c9888 combinedIndex
# 1 2006-01-09  2746  5096  2392  1389     -1627.577
# 2 2006-01-10  2735  5086  2389  1387     -1618.200
# 3 2006-01-11  2711  5080  2360  1384     -1604.352
# 4 2006-01-12  2686  5086  2339  1368     -1586.071
# 5 2006-01-13  2663  5036  2310  1373     -1575.911
# 6 2006-01-16  2686  5058  2329  1387     -1591.156



######绘图 ####
index.data$combinedIndex <- eval(parse(text=l$formula[1]))

y<-index.data$combinedIndex[index.data$dates %in% index.data0$dates]
p1<- qplot(y)+
  labs(title=paste0(substr(l$Rsqure[1],1,4),"|",gsub("index.data\\$","",l$formula[1])))
p2 <- qplot(x=index.data0$dates,y= y) + 
  scale_x_datetime(date_breaks  = "10 day")+
  theme(axis.text.x  = element_text(angle=90))
multiplot(p1,p2)



l <- l[order(l$Rsqure),]
for(i in 1:nrow(l)){ 

index.data$combinedIndex <- eval(parse(text=l$formula[i]))
y<-index.data$combinedIndex[index.data$dates %in% index.data0$dates]
p1<- qplot(y)+
  labs(title=paste0(substr(l$Rsqure[i],1,4),"|",gsub("index.data\\$","",l$formula[i])))
p2 <- qplot(x=index.data0$dates,y= y) + 
  scale_x_datetime(date_breaks  = "10 day")+
  theme(axis.text.x  = element_text(angle=90))
multiplot(p1,p2)

}


# > l$formula[1]
# [1] "index.data$a9888* (-1)+index.data$y9888*(0.247648494462398)+index.data$m9888*(0.607367728460447)"
# > gsub("index.data\\$","",l$formula[1])
# [1] "a9888* (-1)+y9888*(0.247648494462398)+m9888*(0.607367728460447)"
# 
