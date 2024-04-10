# 计算bray curtis距离矩阵 #

#安装R包
install.packages("vegan")
install.packages("ape")
install.packages("phangorn")

#加载所需R包
library(vegan)
library(ape)
library(phangorn)

df <- read.csv("data/braycurtis.csv")
df <- df[,-1]

df_dist <- vegdist(t(df),method = 'bray')#使用bray curtis方法计算距离矩阵

df_hc1 <- hclust(df_dist,method="average")#使用类平均法进行聚类
plot(as.dendrogram(df_hc1),type="rectangle",horiz=T)
