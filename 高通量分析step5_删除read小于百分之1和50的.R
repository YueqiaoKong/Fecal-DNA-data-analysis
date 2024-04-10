#每个pcr反应中小于总reads数1%以及小于50条的MOTU归零

sdata <- read.csv("data/MY01_合并-荒漠猫.csv",header = T, stringsAsFactors = F)
head(sdata)
sdata <- as.data.frame(sdata)

x <- nrow(sdata)
y <- ncol(sdata)

for(i in 1:(x-1)){
  for(j in 2:(y-1)){
    if((as.numeric(sdata[i,j])<50)|
       (as.numeric(sdata[i,j])<(as.numeric(sdata[i,y])*10))){
      sdata[i,j] <- 0
    }
  }
}
sdata <- sdata[,-y]

write.csv(sdata,"data/MY01_荒漠猫.csv")

