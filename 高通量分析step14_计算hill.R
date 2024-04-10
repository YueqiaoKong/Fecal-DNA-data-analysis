# Hill number #
# kyq 202308 #

#MOTU文件
otu.table <- read.csv("data/adonis_otu.csv", row.names = 1)


# 1.计算Hill diversity----
library("hillR")

dummy = FD::dummy
#richness
a <- hill_taxa(comm = otu.table, q = 0)
#shannon
b <- hill_taxa(comm = otu.table, q = 1)
#simpson
c <- hill_taxa(comm = otu.table, q = 2)


# 2.计算生态位宽度
library(spaa)

otu.table2 <- t(otu.table)
d <- niche.width(otu.table2, method = c("shannon", "levins"))


# 3.计算pielou均匀度指数
library(vegan)

shannon = diversity(otu.table,"shannon")
temp0 <- rbind(a,b,c,d,shannon)
temp1 <- t(temp0)
write.csv(temp1,"data/diversity.csv")
