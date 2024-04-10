library("ggplot2")
library("dplyr")
#library("PairedData")

#荒漠猫季节对比#----
df <- read.csv("data/diversity_result_CMCAT.csv")
head(df)

group_by(df, group) %>%
  summarise(
    mean_richness = mean(richness, na.rm = TRUE),
    mean_shannon = mean(shannon, na.rm = TRUE),
    mean_simpson = mean(simpson, na.rm = TRUE),
    mean_breadth = mean(breadth, na.rm = TRUE),
    mean_pielous = mean(pielous, na.rm = TRUE)
  )

#两组中每组的数据是否服从正态分布？<0.05意味着与正态分布有显著差异
#使用with()和shapiro.test()的函数来为每组样本计算Shapiro-Wilk测试
#如果两组数据中只有一组不是正态分布就得要使用非参数两样本Wilcoxon秩检验。
with(df, shapiro.test(pielous[group == "G"])) 
with(df, shapiro.test(pielous[group == "NG"])) 

#这两个总体是否符合方差齐性？使用F检验来检验方差齐性
#p值>0.05意味着两组数据方差没有显著差异，也就是，方差齐性
res.ftest <- var.test(pielous ~ group, data = df)
res.ftest

#计算两独立样本Wilcoxon检验
res <- wilcox.test(pielous ~ group, data = df, var.equal = TRUE, alternative = "less")
res





#三个物种的对比对比#----
df <- read.csv("data/diversity_result_ALL.csv")
head(df)

group_by(df, group) %>%
  summarise(
    mean_richness = mean(richness, na.rm = TRUE),
    mean_shannon = mean(shannon, na.rm = TRUE),
    mean_simpson = mean(simpson, na.rm = TRUE),
    mean_breadth = mean(breadth, na.rm = TRUE),
    mean_pielous = mean(pielous, na.rm = TRUE)
  )


df <- read.csv("data/diversity_result_ALL.csv")
head(df)

#两组中每组的数据是否服从正态分布？<0.05意味着与正态分布有显著差异
#使用with()和shapiro.test()的函数来为每组样本计算Shapiro-Wilk测试
#如果两组数据中只有一组不是正态分布就得要使用非参数两样本Wilcoxon秩检验。
with(df, shapiro.test(pielous[group == "G"])) 
with(df, shapiro.test(pielous[group == "NG"])) 

#这两个总体是否符合方差齐性？使用F检验来检验方差齐性
#p值>0.05意味着两组数据方差没有显著差异，也就是，方差齐性
res.ftest <- var.test(pielous ~ group, data = df)
res.ftest

#计算两独立样本Wilcoxon检验
res <- wilcox.test(pielous ~ group, data = df, var.equal = TRUE, alternative = "less")
res
