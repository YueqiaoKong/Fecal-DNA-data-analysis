# 绘制带置信区间的物种积累曲线 #
# kyq2307 #

#install.packages("iNEXT")

library(iNEXT)
library(ggplot2)

# 举个栗子 -----------------------------------------------------------
data(ant)
str(ant)

t <- seq(1, 700, by=10)
out.inc <- iNEXT(ant, q=0, datatype="incidence_freq", size=t)

# Sample‐size‐based R/E curves
ggiNEXT(out.inc, type=1, color.var="Assemblage") +
  theme_bw(base_size = 18)

# Sample completeness curves
ggiNEXT(out.inc, type=2, color.var="Assemblage") +
  ylim(c(0.9,1)) +
  theme_bw(base_size = 18)

# Coverage‐based R/E curves
ggiNEXT(out.inc, type=3, color.var ="Assemblage") +
  xlim(c(0.9,1)) +
  theme_bw(base_size = 18) +
  theme(legend.position="bottom",
        legend.title=element_blank(),
        text=element_text(size=18),
        legend.box = "vertical")


# 食性数据-不同物种---------------------------------------------------------
prey <- read.csv("data/prey_freq.csv")

#提取行生成向量
lcat0 <- as.matrix(prey[1,])
lcat <- as.vector(lcat0[-1])
lcat <- as.double(lcat)
rfox0 <- as.matrix(prey[2,])
rfox <- as.vector(rfox0[-1])
rfox <- as.double(rfox)
cmcat0 <- as.matrix(prey[3,])
cmcat <- as.vector(cmcat0[-1])
cmcat <- as.double(cmcat)

#创建列表
prey_list <- list(lcat, rfox, cmcat)

#为列表元素命名
names(prey_list) <- c("Leopard cat", "Red fox", "Chinese mountain cat")
print(prey_list)

t <- seq(1, 300, by=5)
out.inc <- iNEXT(prey_list, q=0, datatype="incidence_freq", size=t)

# Sample‐size‐based R/E curves
ggiNEXT(out.inc, type=1, color.var="Assemblage") +
  theme_bw(base_size = 18)


# 食性数据-不同季节---------------------------------------------------------
prey <- read.csv("data/prey_freq_season.csv")

#提取行生成向量
spring0 <- as.matrix(prey[1,])
spring <- as.vector(spring0[-1])
spring <- as.double(spring)

summer0 <- as.matrix(prey[2,])
summer <- as.vector(summer0[-1])
summer <- as.double(summer)

autumn0 <- as.matrix(prey[3,])
autumn <- as.vector(autumn0[-1])
autumn <- as.double(autumn)

winter0 <- as.matrix(prey[4,])
winter <- as.vector(winter0[-1])
winter <- as.double(winter)

#创建列表
prey_list <- list(spring, summer, autumn, winter)

#为列表元素命名
names(prey_list) <- c("spring", "summer", "autumn", "winter")
print(prey_list)

t <- seq(1, 300, by=5)
out.inc <- iNEXT(prey_list, q=0, datatype="incidence_freq", size=t)

# Sample‐size‐based R/E curves
ggiNEXT(out.inc, type=1, color.var="Assemblage") +
  theme_bw(base_size = 18)


# 食性数据-2季节---------------------------------------------------------
prey <- read.csv("data/prey_freq_2season.csv")

#提取行生成向量
G0 <- as.matrix(prey[1,])
G <- as.vector(G0[-1])
G <- as.double(G)

NG0 <- as.matrix(prey[2,])
NG <- as.vector(NG0[-1])
NG <- as.double(NG)

#创建列表
prey_list <- list(G, NG)

#为列表元素命名
names(prey_list) <- c("Growing season", "Non-growing season")
print(prey_list)

t <- seq(1, 300, by=5)
out.inc <- iNEXT(prey_list, q=0, datatype="incidence_freq", size=t)

# Sample‐size‐based R/E curves
ggiNEXT(out.inc, type=1, color.var="Assemblage") +
  theme_bw(base_size = 18)
