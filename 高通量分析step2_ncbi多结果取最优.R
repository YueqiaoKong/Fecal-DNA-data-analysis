#在excel对每个seq对应的物种分类信息进行筛选，保留≥95%的信息
#导入R，使用这个代码：
# 1. 首先筛选identity最大的，如果identity最大的不止一个，
# 2. 那就看MaxScore列，保留最大的，如果MaxScore最大的不止一个，
# 3. 那就都保留下来，用逗号分割

require(data.table)

sdata <- read.csv("data/MY06.csv",header = T, stringsAsFactors = F)
head(sdata)

sdata <- as.data.table(sdata)
#分类筛选最大值行
maxIndt_data <- sdata[sdata[, .I[indt == max(indt)], by=ID]$V1]
maxScore_data <- maxIndt_data[maxIndt_data[, .I[i == max(i)], by=ID]$V1]

# write.csv(maxScore_data,"data/trycombineMY06.csv")
temp <- as.data.frame(maxScore_data)

ID_ <- temp$ID[!duplicated(temp$ID)]
Species <- matrix(NA,nrow=length(ID_),ncol=3)

for (i in 1:length(ID_)) {
  a <- temp[temp$ID==ID_[i],]
  Species[i,1] <- a[1,2]
  Species[i,2] <- a[1,4]
  Species[i,3] <- paste(a[,17],collapse = ",")
}
Species <- as.data.frame(Species)

write.csv(Species,"data/combineMY06.csv")

