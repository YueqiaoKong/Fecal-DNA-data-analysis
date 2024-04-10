# ANOSIM - perMANOVA #
# KYQ 202308 #

# 要输入两个表格
# otu表:列名是变量（食物），行名是样品 eg. adonis_otu.csv
# 分组表:第一列是样品，第二列是各自的组别 eg. adonis_species_group.csv

# 1.对比物种间差异----
library("vegan")

otu <- read.csv("data/adonis_otu.csv", row.names = 1)
species <- read.csv("data/adonis_species_group.csv")

#聚类，但对我来说没啥用暂时
# distance.bray <- vegdist(otu,method = 'bray')
# hclust.diet<-hclust(distance.bray,method = "average")
# plot(hclust.diet)

#ANOSIM
# distance.bray<-vegdist(otu,method = 'bray')
# anosim.result<-anosim(distance.bray,species$group,permutations = 999)
# summary(anosim.result)

#PERMANOVA
adonis2(otu~ group,data = species,permutations = 999,method="bray")->ad1
ad1

#pairwise comparison for PERMANOVA
pairwise.adonis <-function(x,factors, sim.method, p.adjust.m)
{
  co = as.matrix(combn(unique(factors),2))
  pairs = c()
  F.Model =c()
  R2 = c()
  p.value = c()
  
  for(elem in 1:ncol(co)){
    ad = adonis(x[factors %in%c(as.character(co[1,elem]),as.character(co[2,elem])),] ~
                  factors[factors %in%c(as.character(co[1,elem]),as.character(co[2,elem]))] , method =sim.method);
    pairs =c(pairs,paste(co[1,elem],'vs',co[2,elem]));
    F.Model =c(F.Model,ad$aov.tab[1,4]);
    R2 = c(R2,ad$aov.tab[1,5]);
    p.value = c(p.value,ad$aov.tab[1,6])
  }
  
  p.adjusted =p.adjust(p.value,method=p.adjust.m)
  pairw.res = data.frame(pairs,F.Model,R2,p.value,p.adjusted)
  return(pairw.res)
}

pairwise.adonis(otu, species$group, sim.method="bray", p.adjust.m= "bonferroni")




# 2.对比荒漠猫季节间差异----
library("vegan")

otu <- read.csv("data/adonis_otu_cmcat.csv", row.names = 1)
species <- read.csv("data/adonis_season_group.csv")

#聚类，但对我来说没啥用暂时
# distance.bray <- vegdist(otu,method = 'bray')
# hclust.diet<-hclust(distance.bray,method = "average")
# plot(hclust.diet)

#ANOSIM
# distance.bray<-vegdist(otu,method = 'bray')
# anosim.result<-anosim(distance.bray,species$group,permutations = 999)
# summary(anosim.result)

#PERMANOVA
adonis2(otu~ group,data = species,permutations = 999,method="bray")->ad1
ad1

#pairwise comparison for PERMANOVA
pairwise.adonis <-function(x,factors, sim.method, p.adjust.m)
{
  co = as.matrix(combn(unique(factors),2))
  pairs = c()
  F.Model =c()
  R2 = c()
  p.value = c()
  
  for(elem in 1:ncol(co)){
    ad = adonis(x[factors %in%c(as.character(co[1,elem]),as.character(co[2,elem])),] ~
                  factors[factors %in%c(as.character(co[1,elem]),as.character(co[2,elem]))] , method =sim.method);
    pairs =c(pairs,paste(co[1,elem],'vs',co[2,elem]));
    F.Model =c(F.Model,ad$aov.tab[1,4]);
    R2 = c(R2,ad$aov.tab[1,5]);
    p.value = c(p.value,ad$aov.tab[1,6])
  }
  
  p.adjusted =p.adjust(p.value,method=p.adjust.m)
  pairw.res = data.frame(pairs,F.Model,R2,p.value,p.adjusted)
  return(pairw.res)
}

pairwise.adonis(otu, species$group, sim.method="bray", p.adjust.m= "bonferroni")
