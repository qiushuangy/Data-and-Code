###
library(sp)
library(raster)
library(sf)
library(tcltk)
library(rgdal)
library(rgeos) 
library(dplyr)
library(utils)
library(lattice)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(RColorBrewer)
library(ncdf4)
library(gridExtra)
library(plyr)
library(stats)
library(scales)
library(base)
library(sysfonts)
library(showtextdb)
library(showtext)
library(ggsignif)
library(latex2exp)
library(cowplot)
library(ggpubr)
library(ggforce)
library(ggbeeswarm)
library(ggthemes)
library(paletteer)
library(TTR)

setwd("E:\\PhD_qiushuang\\GEE_CF_CP\\AllFigures\\writing\\Review_two")
getwd()


# Figure 4----------

#1: fire data-----------
### read SR data
SR_P50.FY<-raster(".\\Figure4.data\\fire\\SR_50.tif")
SR_P2550.FY<-raster(".\\Figure4.data\\fire\\SR_2550.tif")
SR_P1025.FY<-raster(".\\Figure4.data\\fire\\SR_1025.tif")
SR_P510.FY<-raster(".\\Figure4.data\\fire\\SR_510.tif")

### read LE data
LE_P50.FY<-raster(".\\Figure4.data\\fire\\LE50.tif")
LE_P2550.FY<-raster(".\\Figure4.data\\fire\\LE2550.tif")
LE_P1025.FY<-raster(".\\Figure4.data\\fire\\LE1025.tif")
LE_P510.FY<-raster(".\\Figure4.data\\fire\\LE510.tif")

### match together
#p50
overlay.p50<-stack(LE_P50.FY,SR_P50.FY)
overlay.p50.df <- data.frame(na.omit(values(overlay.p50)))
names(overlay.p50.df) <- c("LE","SR")
#p2550
overlay.p2550<-stack(LE_P2550.FY,SR_P2550.FY)
overlay.p2550.df <- data.frame(na.omit(values(overlay.p2550)))
names(overlay.p2550.df) <- c( "LE","SR")
#1025
overlay.p1025<-stack(LE_P1025.FY,SR_P1025.FY)
overlay.p1025.df <- data.frame(na.omit(values(overlay.p1025)))
names(overlay.p1025.df) <- c( "LE","SR")
#510
overlay.p510<-stack(LE_P510.FY,SR_P510.FY)
overlay.p510.df <- data.frame(na.omit(values(overlay.p510)))
names(overlay.p510.df) <- c( "LE","SR")

fire.all<-rbind(overlay.p50.df,overlay.p2550.df,overlay.p1025.df,overlay.p510.df)


#2: harvest data-----------
### read SR data
SR_P50.CY<-raster(".\\Figure4.data\\cut\\SR_50_cy.tif")
SR_P2550.CY<-raster(".\\Figure4.data\\cut\\SR_2550_cy.tif")
SR_P1025.CY<-raster(".\\Figure4.data\\cut\\SR_1025_cy.tif")
SR_P510.CY<-raster(".\\Figure4.data\\cut\\SR_510_cy.tif")

### read LE data
LE_P50.CY<-raster(".\\Figure4.data\\cut\\LE50.tif")
LE_P2550.CY<-raster(".\\Figure4.data\\cut\\LE2550.tif")
LE_P1025.CY<-raster(".\\Figure4.data\\cut\\LE1025.tif")
LE_P510.CY<-raster(".\\Figure4.data\\cut\\LE510.tif")


#match
#p50
cy.overlay.p50<-stack(LE_P50.CY,SR_P50.CY)
cy.overlay.p50.df <- data.frame(na.omit(values(cy.overlay.p50)))
names(cy.overlay.p50.df) <- c( "LE","SR")
#p2550
cy.overlay.p2550<-stack(LE_P2550.CY,SR_P2550.CY)
cy.overlay.p2550.df <- data.frame(na.omit(values(cy.overlay.p2550)))
names(cy.overlay.p2550.df) <- c( "LE","SR")
#1025
cy.overlay.p1025<-stack(LE_P1025.CY,SR_P1025.CY)
cy.overlay.p1025.df <- data.frame(na.omit(values(cy.overlay.p1025)))
names(cy.overlay.p1025.df) <- c( "LE","SR")
#510
cy.overlay.p510<-stack(LE_P510.CY,SR_P510.CY)
cy.overlay.p510.df <- data.frame(na.omit(values(cy.overlay.p510)))
names(cy.overlay.p510.df) <- c( "LE","SR")

cut.all<-rbind(cy.overlay.p50.df,cy.overlay.p2550.df,cy.overlay.p1025.df,cy.overlay.p510.df)


#3: plot -----------------------------
fire.all1<-fire.all
fire.all1$type<-c("fire")

cut.all1<-cut.all
cut.all1$type<-c("harvest")

all.df<-rbind(fire.all1,cut.all1)

#LE boxplot
LE.DF<-all.df[,c(1,3)]
LE.DF$idx<-c("LE")
names(LE.DF) <- c("mean", "type","idx")

SR.DF<-all.df[,c(2,3)]
SR.DF$idx<-c("SR")
names(SR.DF) <- c("mean", "type","idx")

LE_SR.df<-rbind(LE.DF,SR.DF)

LE.SR.boxplot<-ggplot(LE_SR.df,aes(x=type,y=mean,fill=idx))+
  stat_boxplot(geom="errorbar",position = position_dodge(0.8),width=0.2,cex=0.2)+
  geom_boxplot(linewidth = 0.1,width=0.8 ,outlier.shape=NA)+
  annotate("rect", xmin = 0.4, xmax = 1.5, ymin = -Inf, ymax = Inf, alpha = 0.2,fill="#c1f1fc") +
  annotate("rect", xmin = 1.5, xmax = Inf, ymin = -Inf, ymax = Inf,alpha = 0.2,fill="#ebffac") +
  
  # 添加分组辅助�?
  geom_vline(xintercept = 1.5, lty="dashed", color = "grey50", linewidth = 0.5)+
  geom_hline(aes(yintercept=0),color="red")+
  scale_y_continuous(breaks = seq(-10, 5, 5),labels = c('-10','-5','0','5')
                     ,limits = c(-10,5)) +  
  annotate('text',x=0.8,y=-8,label=" -1.43 ",size=2,family="serif")+
  annotate('text',x=1.2,y=-4,label=" 0.72 ",size=2,family="serif")+
  annotate('text',x=1.8,y=-5,label=" -1.15 ",size=2,family="serif")+
  annotate('text',x=2.2,y= -3,label=" -0.48 ",size=2,family="serif")+
  
  #主题 
  theme_bw()+
  theme(axis.text.y = element_text(family=Fon,size=10, color = "#204056"),
        axis.text.x = element_text(family=Fon,size=10,  color = "#204056"),
        axis.text = element_text(family=Fon,size=8,color="black"),
        axis.title = element_text(family=Fon,size=10),
        aspect.ratio = 0.8,
        panel.grid = element_blank())+
  theme(plot.title = element_text(family=Fon,face='bold',size=8,hjust = 0.5))+ 
  theme(
    legend.title = element_blank(),
    legend.text = element_text(family=Fon,size=6,color="black"), 
    legend.key.size =unit(8,"pt"), 
    legend.key.height =unit(8,"pt"),
    legend.key.width =unit(7,"pt"),
    legend.background = element_blank(),
    legend.position = c(0.8,0.2))+
  #自定义颜�?  
  scale_fill_manual(values = c("#47cf73", "#fcd000"))+
  labs(x=NULL,y='�? W/m2') 
LE.SR.boxplot


