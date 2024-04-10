#三次重复的合并，求平均值

sdata <- read.csv("data/MY06_赤狐.csv",header = T, stringsAsFactors = F)
head(sdata)
sdata <- as.data.frame(sdata)

x <- nrow(sdata)
y <- ncol(sdata)
my_list <- seq(from=1, to=x, by=3)

newdata <- sdata[1,]
# sdata[1,2]
# 第一个值：第一行，第二列

for(i in my_list){
  print(i)
  a <- sdata[c(i,(i+1),(i+2)),]
  a1 <- as.data.frame(a[,-1])
  a2 <- rbind(a1,Total=colSums(a1))
  temp <- a2[4,]*(1/3)
  temp <- cbind(sdata[i,1],temp)
  colnames(temp)[1] <- "ID"
  newdata <- rbind(newdata,temp)
}
newdata <- newdata[-1,]

write.csv(newdata,"data/处理后MY06_赤狐.csv")
