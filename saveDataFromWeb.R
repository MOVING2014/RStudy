source("datafromWeb.R")
filename <- Sys.Date()
variales <- c("TA0","RB0","L0","Y0","m0","a0","c0","P0","V0","SR0","AG0","CF0")

for(i in variales){
  filename_variable <- paste0("/Users/moving/StaticsStudy/data/",i,"_",filename,".csv")
  write.csv(x=readSinaData(i),file=filename_variable,row.names = FALSE)

}



# a9888 <- read.data.index.sina("a0")
# y9888 <- read.data.index.sina(x="y0")
# m9888 <- read.data.index.sina("m0")
# c9888 <- read.data.index.sina("c0")
# SR888 <- read.data.index.sina("SR0")
# TA888 <- read.data.index.sina("TA0")
# l9888 <- read.data.index.sina("L0")
# p9888 <- read.data.index.sina("P0")
# v9888 <- read.data.index.sina("V0")
# AG0 <- read.data.index.sina("AG0")
# RB0 <- read.data.index.sina("RB0")
# CF0 <- read.data.index.sina("CF0")

