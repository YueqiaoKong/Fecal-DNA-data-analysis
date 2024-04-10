#把多行对应为同一物种的seq的count数加和#
#kyq 202306#

require(data.table)

sdata <- read.csv("data/MY06_1.csv",header = T, stringsAsFactors = F)
head(sdata)

sdata <- as.data.table(sdata)

species_ <- sdata$species[!duplicated(sdata$species)]
counts <- sdata[1,]

# 为了方便懒得写新的了
# 这里数字要改成表格列数
# 也就是物种名这一列
for (i in 1:length(species_)) {
  a <- sdata[sdata$species==species_[i],]
  a1 <- as.data.frame(a[,-144])
  a2 <- rbind(a1,Total=colSums(a1))
  x <- nrow(a2)
  temp <- a2[x,]
  temp[,144] <- a[1,144]
  counts <- rbind(counts,temp)
}

counts <- counts[-1,]
counts <- as.data.frame(counts)

write.csv(counts, "data/MY06_合并.csv")
