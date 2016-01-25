m1601 <- generateBarData()
SR601 <- generateBarData(spe="SR601",location="zc",dates="201509")
ag1512 <- generateBarData(spe="ag1512",location="sc",dates="201509")
TA601 <- generateBarData(spe="TA601",location="zc",dates="201509")
Y1601 <- generateBarData(spe="Y1601",location="dc",dates="201509")
RM601 <- generateBarData(spe="RM601",location="zc",dates="201509")
rb1601<- generateBarData(spe="rb1601",location = "sc",dates="201509")
l1601<- generateBarData(spe="l1601",location = "dc",dates="201509")
a1601 <- generateBarData(spe="a1601",location="dc",dates="201510")
y1605 <- generateBarData(spe="y1605",location="dc",dates="201510")

write.data<-function(spe="m1601"){
  file=paste0("/Users/moving/Documents/期货交易/201509/",spe,".csv")
 write.table(get(spe),file=file,append = T,sep = ",",col.names = F)
 #write.csv(get(spe),file=file,append = FALSE)

}

m1601 <- generateBarData(spe="m1601",location="dc",dates="201510")
SR601 <- generateBarData(spe="SR601",location="zc",dates="201510")
ag1512 <- generateBarData(spe="ag1512",location="sc",dates="201510")
TA601 <- generateBarData(spe="TA601",location="zc",dates="201510")
Y1601 <- generateBarData(spe="Y1601",location="dc",dates="201510")
RM601 <- generateBarData(spe="RM601",location="zc",dates="201510")
rb1601<- generateBarData(spe="rb1601",location = "sc",dates="201510")
l1601<- generateBarData(spe="l1601",location = "dc",dates="201510")

# write.data(spe="m1601")
# write.data(spe="Y1601")
# write.data(spe="SR601")
# write.data(spe="TA601")
# write.data(spe="ag1512")
# write.data(spe = "RM601")
# write.data(spe = "rb1601")
# write.data(spe="l1601")
# write.data(spe="a1601")
#write.data(spe="y1605")


# spes<<- list(m1601=m1601,SR601=SR601)



