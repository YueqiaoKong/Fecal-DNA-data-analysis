# 物种列表核对与对齐 #
# kyq 202307 #
library(dplyr)

sdata <- read.csv(file = "data/赤狐.csv", 
                   header = T, stringsAsFactors = F)

check <- read.csv(file = "data/核对列表.csv", 
                    header = T, stringsAsFactors = F)


specie_check <- left_join(check, sdata)

specie_check[is.na(specie_check)]=0

write.csv(specie_check,"data/赤狐_check.csv")

# ———————————————————————————————————————————————————————————— ####
# 第二次核对 删掉全为0的猎物MOTUs #

library(dplyr)
sdata <- read.csv(file = "data/荒漠猫_check.csv", 
                  header = T, stringsAsFactors = F)

check <- read.csv(file = "data/核对列表2.csv", 
                  header = T, stringsAsFactors = F)


specie_check <- left_join(check, sdata)

specie_check[is.na(specie_check)]=0

write.csv(specie_check,"data/荒漠猫_check2.csv")
