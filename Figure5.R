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


#Figure 5-------------------------------------------------------------------

#1: read data --------------------------------------------------------------
# fire 
fy.lst.et.df<-read.csv(paste0(".\\Figure5.data\\fireyear\\FY_LST_ET.csv"),header = TRUE)
fy.lst.alb.df<-read.csv(paste0(".\\Figure5.data\\fireyear\\FY_LST_Alb.csv"),header = TRUE)
fy.lst.ndvi.df<-read.csv(paste0(".\\Figure5.data\\fireyear\\FY_LST_NDVI.csv"),header = TRUE)
# cut 
cy.lst.et.df<-read.csv(paste0(".\\Figure5.data\\cutyear\\CY_LST_ET.csv"),header = TRUE)
cy.lst.alb.df<-read.csv(paste0(".\\Figure5.data\\cutyear\\CY_LST_Alb.csv"),header = TRUE)
cy.lst.ndvi.df<-read.csv(paste0(".\\Figure5.data\\cutyear\\CY_LST_NDVI.csv"),header = TRUE)

#2: calc relationships and record it----------------------------------------
linear <- lm(fy.lst.et.df[,2] ~ fy.lst.et.df[,3])
summary(linear) 


#3: plot relationship of LST and ET ----------------------------------------
lst.et.fy.df1<-fy.lst.et.df
lst.et.fy.df1$type<-c("fire")

lst.et.cy.df1<-cy.lst.et.df
lst.et.cy.df1$type<-c("harvest")

lst.et.all.df<-rbind(lst.et.fy.df1,lst.et.cy.df1)

Fon<-'serif'#设置字体为新罗马
LST.ET.map<-ggplot(lst.et.all.df, aes(x = et, y = lst1, group=type,colour=type)) + 
  scale_y_continuous(limits = c(-1,3),breaks = c(-1,0,1,2,3),expand = c(0,0))+
  scale_x_continuous(limits = c(-0.6,0.3),breaks = c(-0.6,-0.3,0,0.3),expand = c(0,0))+
  scale_colour_manual( name=NULL,   values =c("#E64B35FF","royalblue3"),guide=guide_legend(reverse=T))+
  stat_smooth(formula=y~x,fill="lightgrey",alpha=0.7,size=1,method="lm",se=TRUE,level=0.95)+
  theme_bw()+
  annotate('text',x=-0.084,y=2.6,label=" y= -2.96x + 0.09  (R  = 0.19  p< 0.001) ",
           color="red",size=4,family="serif")+
  annotate('text',x=-0.065,y=2.7,label=" 2 ",color="red",size=3,family="serif")+
  annotate('text',x=-0.084,y=2.1,label=" y= -2.40x + 0.02  (R  = 0.26  p< 0.001) ",
           color="royalblue3",size=4,family="serif")+
  annotate('text',x=-0.065,y=2.2,label=" 2 ",color="royalblue3",size=3,family="serif")+
  geom_hline(aes(yintercept=0),color="grey50")+
  theme(axis.line.x=element_line(linetype=1,color="grey30",size=0), 
        axis.ticks.x=element_line(linetype=1,color="grey30",size=0), 
        axis.ticks.length.x=unit(0.15,'cm'), 
        axis.line.y=element_line(linetype=1,color="grey30",size=0),
        axis.ticks.y=element_line(color="black",size=0.5,lineend = 1),
        axis.ticks.length.y=unit(0.15,'cm'),
        axis.title = element_text(family=Fon, face='bold',size=10,color="black"),
        axis.text = element_text(family=Fon, size=12,color="black"))+ 
  theme(legend.position = "none")+
  theme(plot.title = element_text(family=Fon, face='bold',size=14,hjust = 0.5),
        panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"),
        aspect.ratio = 0.75)+
  labs(y="△LST(�?)",x="△ET")
LST.ET.map


#4: plot relationship of LST and Albedo -----------------------------------
lst.albedo.fy.df1<-fy.lst.alb.df
lst.albedo.fy.df1$type<-c("fire")

lst.albedo.cy.df1<-cy.lst.alb.df
lst.albedo.cy.df1$type<-c("harvest")

lst.albedo.all.df<-rbind(lst.albedo.fy.df1,lst.albedo.cy.df1)

LST.Albedo.map<-ggplot(lst.albedo.all.df, aes(x = albedo, y =lst1 , group=type,colour=type)) + 
  scale_y_continuous(limits = c(-1,3),breaks = c(-1,0,1,2,3),expand = c(0,0))+
  scale_x_continuous(limits = c(-0.062,0.042),breaks = c(-0.06,-0.04,-0.02,0,0.02,0.04),expand = c(0,0))+
  scale_colour_manual( name=NULL,   values =c("#E64B35FF","royalblue3"),guide=guide_legend(reverse=T))+
  stat_smooth(formula=y~x,fill="lightgrey",alpha=0.7,size=1,method="lm",se=TRUE,level=0.95)+
  theme_bw()+
  annotate('text',x=-0.008,y=2.6,label=" y= -35.24x + 0.13  (R  = 0.16  p< 0.001) ",
           color="red",size=4,family="serif")+
  annotate('text',x=-0.004,y=2.7,label=" 2 ",color="red",size=3,family="serif")+
  annotate('text',x=-0.008,y=2.1,label=" y=  27.78x + 0.07  (R  = 0.07  p= 0.009) ",
           color="royalblue3",size=4,family="serif")+
  annotate('text',x=-0.004,y=2.2,label=" 2 ",color="royalblue3",size=3,family="serif")+
  geom_hline(aes(yintercept=0),color="grey50")+
  theme(axis.line.x=element_line(linetype=1,color="grey30",size=0), 
        axis.ticks.x=element_line(linetype=1,color="grey30",size=0),
        axis.ticks.length.x=unit(0.15,'cm'), 
        axis.line.y=element_line(linetype=1,color="grey30",size=0),
        axis.ticks.y=element_line(color="black",size=0.5,lineend = 1),
        axis.ticks.length.y=unit(0.15,'cm'),
        axis.title = element_text(family=Fon, face='bold',size=10,color="black"),
        axis.text = element_text(family=Fon, size=12,color="black"))+ 
  theme(plot.title = element_text(family=Fon, face='bold',size=14,hjust = 0.5),
        panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"),
        aspect.ratio = 0.75)+
  theme(legend.position = "none")+
  labs(y="△LST(�?)",x="△Albedo")
LST.Albedo.map


#5: plot relationship of LST and NDVI --------------------------------------
lst.ndvi.fy.df1<-fy.lst.ndvi.df
lst.ndvi.fy.df1$type<-c("fire")

lst.ndvi.cy.df1<-cy.lst.ndvi.df
lst.ndvi.cy.df1$type<-c("harvest")

lst.ndvi.all.df<-rbind(lst.ndvi.fy.df1,lst.ndvi.cy.df1)

LST.NDVI.map<-ggplot(lst.ndvi.all.df, aes(x = ndvi, y =lst1 , group=type,colour=type)) + 
  scale_y_continuous(limits = c(-1,3),breaks = c(-1,0,1,2,3),expand = c(0,0))+
  scale_x_continuous(limits = c(-0.3,0.1),breaks = c(-0.3,-0.2,-0.1,0,0.1),expand = c(0,0))+
  scale_colour_manual( name=NULL,   values =c("#E64B35FF","royalblue3"),guide=guide_legend(reverse=T))+
  stat_smooth(formula=y~x,fill="lightgrey",alpha=0.7,size=1,method="lm",se=TRUE,level=0.95)+
  theme_bw()+
  annotate('text',x=-0.084,y=2.6,label=" y= -8.33x - 0.15  (R  = 0.47  p< 0.001) ",
           color="red",size=4,family="serif")+
  annotate('text',x=-0.08,y=2.7,label=" 2 ",color="red",size=3,family="serif")+
  annotate('text',x=-0.084,y=2.1,label=" y= -3.94x - 0.02  (R  = 0.15  p< 0.001) ",
           color="royalblue3",size=4,family="serif")+
  annotate('text',x=-0.08,y=2.2,label=" 2 ",color="royalblue3",size=3,family="serif")+
  geom_hline(aes(yintercept=0),color="grey50")+
  theme(axis.line.x=element_line(linetype=1,color="grey30",size=0),
        axis.ticks.x=element_line(linetype=1,color="grey30",size=0), 
        axis.ticks.length.x=unit(0.15,'cm'), 
        axis.line.y=element_line(linetype=1,color="grey30",size=0),
        axis.ticks.y=element_line(color="black",size=0.5,lineend = 1),
        axis.ticks.length.y=unit(0.15,'cm'),
        axis.title = element_text(family=Fon, face='bold',size=10,color="black"),
        axis.text = element_text(family=Fon, size=12,color="black"))+ 
  theme(legend.position = "none")+
  theme(plot.title = element_text(family=Fon, face='bold',size=14,hjust = 0.5),
        panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"),
        aspect.ratio = 0.75)+
  labs(y="△LST(�?)",x="△NDVI")
LST.NDVI.map




