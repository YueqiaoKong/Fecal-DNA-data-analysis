# 寻找拉丁名对应的物种分类信息 #
# kyq 2022305 #

library(dplyr)

# 序列号拉丁名中文名对应 ----
specie <- read.csv(file = "data/MY06_.csv", 
                   header = T, stringsAsFactors = F)
check <- read.csv(file = "data/specie_ch.csv", 
                   header = T, stringsAsFactors = F)

specie_new <- left_join(specie, check)

write.csv(specie_new,"data/MY06_check.csv")


# sequenceID与上述信息对应 ----

blast <- read.csv(file = "data/MY06blast.csv", 
                   header = T, stringsAsFactors = F)
check <- read.csv(file = "data/MY06_check.csv", 
                  header = T, stringsAsFactors = F)
head(check)
blast_new <- left_join(blast, check, by = "TSeq_accver")

blast_new1 <- blast_new %>% 
  group_by(ID) %>% 
  filter(!duplicated(TSeq_orgname))

write.csv(blast_new1,"data/MY06.csv")


# obi结果对应中文名等信息 ----

blast <- read.csv(file = "data/MY01_annotate_sort_sequences.csv", 
                  header = T, stringsAsFactors = F)
check <- read.csv(file = "data/MY01_check.csv", 
                  header = T, stringsAsFactors = F)
head(check)
blast_new <- left_join(blast, check, by = "ID")

write.csv(blast_new1,"data/MY01.csv")
