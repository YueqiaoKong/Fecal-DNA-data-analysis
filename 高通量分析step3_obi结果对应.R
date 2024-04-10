#将obi导出的结果与物种中文名对应#
# kyq 2022306 #

library(dplyr)

# 序列号拉丁名中文名对应 ----
obi <- read.csv(file = "data/MY06_annotate_sort_sequences.csv", 
                   header = T, stringsAsFactors = F)
combine <- read.csv(file = "data/combineMY06.csv", 
                  header = T, stringsAsFactors = F)


specie_new <- left_join(obi, combine)

write.csv(specie_new,"data/MY06.csv")
