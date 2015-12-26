

for(i in attributes(spes)$names)
{ 
  x<-(spes[[i]])
  p1<-ggplot(data=x,aes(x=x$CLOSE-x$OPEN))+
    geom_histogram(fill="blue")+
    labs(title=i)+xlab("CLOSE-OPEN")+
    geom_vline(x=mean(x$CLOSE-x$OPEN),color="red")+
    geom_vline(x=median(x$CLOSE-x$OPEN),color="blue")

  p2<-ggplot(data=x,aes(x=x$HIGH-x$LOW))+
    geom_histogram(fill="blue")+
    labs(title=i)+xlab("HIGH-LOW")+
    geom_vline(x=mean(x$HIGH-x$LOW),color="red")+
    geom_vline(x=median(x$HIGH-x$LOW),color="blue")
  cc <- x$CLOSE[2:nrow(x)]-x$CLOSE[1:(nrow(x)-1)]
  cc <- as.data.frame(cc)
  p3<-ggplot(data=cc, aes(x=cc))+
    geom_histogram(fill="blue") +
    labs(title=i)+xlab("CLOSE-CLOSE")+
    geom_vline(x=mean(cc$cc),color="red")+
    geom_vline(x=median(cc$cc),color="blue")
  multiplot(p1,p2,p3,cols=2)
  
  x$C_O<-x$CLOSE-x$OPEN
  x$flag <- 'na'
  x$flag[1:(nrow(x)-1)] <- x$C_O[2:nrow(x)] > 0
  x$flag2 <- 'na'
  x$flag2[1:(nrow(x)-1)] <- x$CLOSE[2:nrow(x)]-x$CLOSE[1:(nrow(x)-1)] > 0
  x$month <- substr(row.names(x),1,7)
  
  x<-x[1:(nrow(x)-1),]
  p1<-ggplot(data=x,aes(x=x$flag,y=x$C_O))+
    labs(title=i)+xlab("CLOSE-OPEN")+
    geom_boxplot()+
    facet_wrap(facets = ~month)+
    geom_hline(y=0,color="red")
  
  p2<-ggplot(data=x,aes(x=x$flag2,y=x$C_O))+
    geom_boxplot()+
    facet_wrap(facets = ~month)+
    labs(title=i)+xlab("CLOSE-CLOSE")+
    geom_hline(y=0,color="red")
  print(p1)
  print(p2)
  mmmm<-spes[[i]]
  barChart(mmmm)
  
}




