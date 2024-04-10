# 计算WPOO&RRA #
# kyq202307 #


# ----------------------------------------------------------
# 计算wPOO

sdata <- read.csv(file = "data/荒漠猫_WPOO_R.csv", 
                  header = T, stringsAsFactors = F)

blank <- read.csv(file = "data/荒漠猫_NA.csv", 
                  header = T, stringsAsFactors = F)

x <- nrow(sdata) #有多少行
y <- ncol(sdata) #有多少列

#blank$WPOO <- NA

for(i in 1:(x-1)){
  print(sdata[i,1])
  for(j in 2:y){
    if(sdata[x,j]==0){
      blank[i,j] <- 0
    }else{
      blank[i,j] <- sdata[i,j]/sdata[x,j]
    }
  }
}

write.csv(blank,"data/荒漠猫_WPOO_R处理后.csv")

# ----------------------------------------------------------
# 计算RRA


sdata <- read.csv(file = "data/赤狐_RRA_R.csv", 
                  header = T, stringsAsFactors = F)

blank <- read.csv(file = "data/赤狐_NA.csv", 
                  header = T, stringsAsFactors = F)

x <- nrow(sdata) #有多少行
y <- ncol(sdata) #有多少列

for(i in 1:(x-1)){
  print(sdata[i,1])
  for(j in 2:y){
    if(sdata[x,j]==0){
      blank[i,j] <- 0
    }else{
      blank[i,j] <- sdata[i,j]/sdata[x,j]
    }
  }
}

write.csv(blank,"data/赤狐_RRA_R处理后.csv")

