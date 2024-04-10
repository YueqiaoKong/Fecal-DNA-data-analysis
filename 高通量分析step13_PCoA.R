# PCoA #
# KYQ 202308 #

library(vegan)
library(ape)
library(ggplot2)
library(grid)
library(dplyr)
library(multcomp)
library(patchwork)

# 1.对比物种间差异----
otu <- read.csv("data/adonis_otu.csv", row.names = 1)
species <- read.csv("data/adonis_species_group.csv")

data <- vegdist(otu)
pcoa<- pcoa(data, correction = "none", rn = NULL)
PC1 = pcoa$vectors[,1]
PC2 = pcoa$vectors[,2]
plotdata <- data.frame(rownames(pcoa$vectors),PC1,PC2,species$group)
colnames(plotdata) <-c("sample","PC1","PC2","Group")
pc1 <-floor(pcoa$values$Relative_eig[1]*100)
pc2 <-floor(pcoa$values$Relative_eig[2]*100)

plotdata$Group <- factor(plotdata$Group,
         levels = c("Chinese_mountain_cat","Red_fox","Leopard_cat"))


yf <- plotdata
yd1 <- yf %>% group_by(Group) %>% summarise(Max = max(PC1))
yd2 <- yf %>% group_by(Group) %>% summarise(Max = max(PC2))
yd1$Max <- yd1$Max + max(yd1$Max)*0.1
yd2$Max <- yd2$Max + max(yd2$Max)*0.1

fit1 <- aov(PC1~Group,data = plotdata)
tuk1<-glht(fit1,linfct=mcp(Group="Tukey"))
res1 <- cld(tuk1,alpah=0.05)

fit2 <- aov(PC2~Group,data = plotdata)
tuk2<-glht(fit2,linfct=mcp(Group="Tukey"))
res2 <- cld(tuk2,alpah=0.05)

test <- data.frame(PC1 = res1$mcletters$Letters,
                   PC2 = res2$mcletters$Letters,
                   yd1 = yd1$Max,
                   yd2 = yd2$Max,
                   Group = yd1$Group)
test$Group <- factor(test$Group,
                     levels = c("Chinese_mountain_cat","Red_fox","Leopard_cat"))


cbbPalette <- c("#B2182B","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7","#CC6666","#9999CC","#66CC99","#99999","#ADD1E5")
p1 <- ggplot(plotdata,aes(Group,PC1)) +
  geom_boxplot(aes(fill = Group)) +
  geom_text(data = test,aes(x = Group,y = yd1,label = PC1),
            size = 7,color = "black",fontface = "bold") +
  coord_flip() +
  scale_fill_manual(values=cbbPalette) +
  theme_bw()+
  theme(axis.ticks.length = unit(0.4,"lines"), 
        axis.ticks = element_line(color='black'),
        axis.line = element_line(colour = "black"), 
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_text(colour='black',size=20,face = "bold"),
        axis.text.x=element_blank(),
        legend.position = "none")
p1

p3 <- ggplot(plotdata,aes(Group,PC2)) +
  geom_boxplot(aes(fill = Group)) +
  geom_text(data = test,aes(x = Group,y = yd2,label = PC2),
            size = 7,color = "black",fontface = "bold") +
  scale_fill_manual(values=cbbPalette) +
  theme_bw()+
  theme(axis.ticks.length = unit(0.4,"lines"), 
        axis.ticks = element_line(color='black'),
        axis.line = element_line(colour = "black"), 
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_text(colour='black',size=20,angle = 45,
                                 vjust = 1,hjust = 1,face = "bold"),
        axis.text.y=element_blank(),
        legend.position = "none")
p3

p2<-ggplot(plotdata, aes(PC1, PC2)) +
  geom_point(aes(fill=Group),size=8,pch = 21)+
  scale_fill_manual(values=cbbPalette,name = "Group")+
  xlab(paste("PC1 ( ",pc1,"%"," )",sep="")) + 
  ylab(paste("PC2 ( ",pc2,"%"," )",sep=""))+
  xlim(ggplot_build(p1)$layout$panel_scales_y[[1]]$range$range) +
  ylim(ggplot_build(p3)$layout$panel_scales_y[[1]]$range$range) +
  theme(text=element_text(size=30))+
  geom_vline(aes(xintercept = 0),linetype="dotted")+
  geom_hline(aes(yintercept = 0),linetype="dotted")+
  theme(panel.background = element_rect(fill='white', colour='black'),
        panel.grid=element_blank(), 
        axis.title = element_text(color='black',size=34),
        axis.ticks.length = unit(0.4,"lines"), axis.ticks = element_line(color='black'),
        axis.line = element_line(colour = "black"), 
        axis.title.x=element_text(colour='black', size=34,vjust = 7),
        axis.title.y=element_text(colour='black', size=34,vjust = -2),
        axis.text=element_text(colour='black',size=28),
        legend.title=element_text(size = 24,face = "bold"),
        legend.text=element_text(size=20),
        legend.key=element_blank(),legend.position = c(0.88,0.13),
        legend.background = element_rect(colour = "black"),
        legend.key.height=unit(1,"cm")) +
  guides(fill = guide_legend(ncol = 1))
p2

p2<-ggplot(plotdata, aes(PC1, PC2)) +
  geom_point(aes(fill=Group),size=8,pch = 21)+
  scale_fill_manual(values=cbbPalette,name = "Group")+
  xlim(ggplot_build(p1)$layout$panel_scales_y[[1]]$range$range) +
  ylim(ggplot_build(p3)$layout$panel_scales_y[[1]]$range$range) +
  theme(text=element_text(size=30))+
  geom_vline(aes(xintercept = 0),linetype="dotted")+
  geom_hline(aes(yintercept = 0),linetype="dotted")+
  theme(panel.background = element_rect(fill='white', colour='black'),
        panel.grid=element_blank(), 
        axis.title = element_text(color='black',size=34),
        axis.ticks.length = unit(0.4,"lines"), axis.ticks = element_line(color='black'),
        axis.line = element_line(colour = "black"), 
        axis.text=element_text(colour='black',size=28),
        legend.title=element_text(size = 24,face = "bold"),
        legend.text=element_text(size=19),
        legend.key=element_blank(),legend.position = c(0.21,0.89),
        legend.background = element_rect(colour = "black"),
        legend.key.height=unit(1,"cm")) +
  guides(fill = guide_legend(ncol = 1))

p2

#(1000*1000 image!)

otu.adonis=adonis2(data~group,data = species,distance = "bray")
otu.adonis
