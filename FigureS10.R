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

#Figure S10-----------------------------

#1: zone1-----------
fy.zone1.df<-read.csv(paste0(".\\FigureS10.data\\FY_zone1.csv"),header = TRUE)
cy.zone1.df<-read.csv(paste0(".\\FigureS10.data\\CY_zone1.csv"),header = TRUE)

zone1.df<-rbind(fy.zone1.df,cy.zone1.df)

Fon<-'serif'
LST.zone1.p0<-ggplot(zone1.df, aes(x = per, y = lst,group=type,colour=type)) + 
  geom_point(alpha=0.8,size=2.0)+
  scale_x_continuous(limits = c(0,105),breaks = c(seq(0,100,20)),expand = c(0,0))+
  scale_y_continuous(limits = c(-0.5,2.5),breaks = c(0,1,2),expand = c(0,0))+
  scale_colour_manual( name=NULL,   values =c("#E64B35FF","royalblue3"),guide=guide_legend(reverse=T))+
  stat_smooth(formula=y~x,fill="lightgrey",alpha=0.7,size=1.5,method="lm",se=TRUE,level=0.95)+
  theme_bw()+
  annotate('text',x=35,y=1.75,label=" R  = 0.77    p<0.001 ",color="red",size=5,family="serif")+
  annotate('text',x=35,y=1.25,label="R  = 0.14    p<0.001",color="royalblue3",size=5,family="serif")+
  annotate('text',x=14,y=1.35,label="2",color="royalblue3",size=3,family="serif")+
  annotate('text',x=14,y=1.85,label="2",color="red",size=3,family="serif")+
  annotate('text',x=90,y=1.5,label=" Zone1 ",color="black",size=5,family="serif")+
  geom_hline(aes(yintercept=0),color="grey50")+
  theme(axis.line.x=element_line(linetype=1,color="grey30",size=0), 
        axis.ticks.x=element_line(linetype=1,color="grey30",size=0), 
        axis.ticks.length.x=unit(0.15,'cm'), 
        axis.line.y=element_line(linetype=1,color="grey30",size=0),
        axis.ticks.y=element_line(color="black",size=0.5,lineend = 1),
        axis.ticks.length.y=unit(0.15,'cm'),
        axis.title = element_text(family=Fon, face='bold',size=10,color="black"),
        axis.text = element_text(family=Fon, size=13,color="black"))+ 
  theme(legend.position = "none")+
  theme(plot.title = element_text(family=Fon, face='bold',size=14,hjust = 0.5),
        aspect.ratio = 1,
        panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"))+ 
  labs(x=NULL,y=NULL)
LST.zone1.p0


#2: zone2-----------
fy.zone2.df<-read.csv(paste0(".\\FigureS10.data\\FY_zone2.csv"),header = TRUE)
cy.zone2.df<-read.csv(paste0(".\\FigureS10.data\\CY_zone2.csv"),header = TRUE)

zone2.df<-rbind(fy.zone2.df,cy.zone2.df)

Fon<-'serif'
LST.zone2.p0<-ggplot(zone2.df, aes(x = per, y = lst,group=type,colour=type)) + 
  geom_point(alpha=0.8,size=2.0)+
  scale_x_continuous(limits = c(0,105),breaks = c(seq(0,100,20)),expand = c(0,0))+
  scale_y_continuous(limits = c(-0.5,2.5),breaks = c(0,1,2),expand = c(0,0))+
  scale_colour_manual( name=NULL,   values =c("#E64B35FF","royalblue3"),guide=guide_legend(reverse=T))+
  stat_smooth(formula=y~x,fill="lightgrey",alpha=0.7,size=1.5,method="lm",se=TRUE,level=0.95)+
  theme_bw()+
  annotate('text',x=35,y=1.75,label="   R  = 0.51      p<0.001 ",color="red",size=5,family="serif")+
  annotate('text',x=35,y=1.25,label="R  = 0.002    p=0.31",color="royalblue3",size=5,family="serif")+
  annotate('text',x=14,y=1.35,label="2",color="royalblue3",size=3,family="serif")+
  annotate('text',x=14,y=1.85,label="2",color="red",size=3,family="serif")+
  annotate('text',x=90,y=1.5,label=" Zone2 ",color="black",size=5,family="serif")+
  geom_hline(aes(yintercept=0),color="grey50")+
  theme(axis.line.x=element_line(linetype=1,color="grey30",size=0), 
        axis.ticks.x=element_line(linetype=1,color="grey30",size=0), 
        axis.ticks.length.x=unit(0.15,'cm'), 
        axis.line.y=element_line(linetype=1,color="grey30",size=0),
        axis.ticks.y=element_line(color="black",size=0.5,lineend = 1),
        axis.ticks.length.y=unit(0.15,'cm'),
        axis.title = element_text(family=Fon, face='bold',size=10,color="black"),
        axis.text = element_text(family=Fon, size=13,color="black"))+ 
  theme(legend.position = "none")+
  theme(plot.title = element_text(family=Fon, face='bold',size=14,hjust = 0.5),
        aspect.ratio = 1,
        panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"))+
  labs(x=NULL,y=NULL)
LST.zone2.p0


#3: zone3-----------
fy.zone3.df<-read.csv(paste0(".\\FigureS10.data\\FY_zone3.csv"),header = TRUE)
cy.zone3.df<-read.csv(paste0(".\\FigureS10.data\\CY_zone3.csv"),header = TRUE)

zone3.df<-rbind(fy.zone3.df,cy.zone3.df)

Fon<-'serif'#设置字体为新罗马
LST.zone3.p0<-ggplot(zone3.df, aes(x = per, y = lst,group=type,colour=type)) + 
  geom_point(alpha=0.8,size=2.0)+
  scale_x_continuous(limits = c(0,105),breaks = c(seq(0,100,20)),expand = c(0,0))+
  scale_y_continuous(limits = c(-0.5,2.5),breaks = c(0,1,2),expand = c(0,0))+
  scale_colour_manual( name=NULL,   values =c("#E64B35FF","royalblue3"),guide=guide_legend(reverse=T))+
  stat_smooth(formula=y~x,fill="lightgrey",alpha=0.7,size=1.5,method="lm",se=TRUE,level=0.95)+
  theme_bw()+
  annotate('text',x=35,y=1.75,label="  R  = 0.79      p<0.001 ",color="red",size=5,family="serif")+
  annotate('text',x=35,y=1.25,label="R  = 0.014    p=0.23",color="royalblue3",size=5,family="serif")+
  annotate('text',x=14,y=1.35,label="2",color="royalblue3",size=3,family="serif")+
  annotate('text',x=14,y=1.85,label="2",color="red",size=3,family="serif")+
  annotate('text',x=90,y=1.5,label=" Zone3 ",color="black",size=5,family="serif")+
  geom_hline(aes(yintercept=0),color="grey50")+
  theme(axis.line.x=element_line(linetype=1,color="grey30",size=0), 
        axis.ticks.x=element_line(linetype=1,color="grey30",size=0),
        axis.ticks.length.x=unit(0.15,'cm'), 
        axis.line.y=element_line(linetype=1,color="grey30",size=0),
        axis.ticks.y=element_line(color="black",size=0.5,lineend = 1),
        axis.ticks.length.y=unit(0.15,'cm'),
        axis.title = element_text(family=Fon, face='bold',size=10,color="black"),
        axis.text = element_text(family=Fon, size=13,color="black"))+ 
  theme(legend.position = "none")+
  theme(plot.title = element_text(family=Fon, face='bold',size=14,hjust = 0.5),
        aspect.ratio = 1,
        panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"))+
  labs(x=NULL,y=NULL)
LST.zone3.p0


#4: zone4-----------
fy.zone4.df<-read.csv(paste0(".\\FigureS10.data\\FY_zone4.csv"),header = TRUE)
cy.zone4.df<-read.csv(paste0(".\\FigureS10.data\\CY_zone4.csv"),header = TRUE)

zone4.df<-rbind(fy.zone4.df,cy.zone4.df)

Fon<-'serif'#设置字体为新罗马
LST.zone4.p0<-ggplot(zone4.df, aes(x = per, y = lst,group=type,colour=type)) + 
  geom_point(alpha=0.8,size=2.0)+
  scale_x_continuous(limits = c(0,105),breaks = c(seq(0,100,20)),expand = c(0,0))+
  scale_y_continuous(limits = c(-0.5,2.5),breaks = c(0,1,2),expand = c(0,0))+
  scale_colour_manual(name=NULL,   values =c("#E64B35FF","royalblue3"),
                      guide=guide_legend(reverse=T))+
  stat_smooth(formula=y~x,fill="lightgrey",alpha=0.7,size=1.5,method="lm",se=TRUE,level=0.95)+
  theme_bw()+
  annotate('text',x=35,y=1.75,label=" R  = 0.76    p<0.001 ",color="red",size=5,family="serif")+
  annotate('text',x=35,y=1.25,label="R  = 0.78    p<0.001",color="royalblue3",size=5,family="serif")+
  annotate('text',x=14,y=1.35,label="2",color="royalblue3",size=3,family="serif")+
  annotate('text',x=14,y=1.85,label="2",color="red",size=3,family="serif")+
  annotate('text',x=90,y=1.5,label=" Zone4 ",color="black",size=5,family="serif")+
  geom_hline(aes(yintercept=0),color="grey50")+
  theme(axis.line.x=element_line(linetype=1,color="grey30",size=0), 
        axis.ticks.x=element_line(linetype=1,color="grey30",size=0), 
        axis.ticks.length.x=unit(0.15,'cm'), 
        axis.line.y=element_line(linetype=1,color="grey30",size=0),
        axis.ticks.y=element_line(color="black",size=0.5,lineend = 1),
        axis.ticks.length.y=unit(0.15,'cm'),
        axis.title = element_text(family=Fon, face='bold',size=10,color="black"),
        axis.text = element_text(family=Fon, size=13,color="black") )+ 
  theme(legend.position = "none")+
  theme(plot.title = element_text(family=Fon, face='bold',size=14,hjust = 0.5),
        aspect.ratio = 1,
        panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"))+
  labs(x=NULL,y=NULL)
LST.zone4.p0


#5: zone5-----------
fy.zone5.df<-read.csv(paste0(".\\FigureS10.data\\FY_zone5.csv"),header = TRUE)
cy.zone5.df<-read.csv(paste0(".\\FigureS10.data\\CY_zone5.csv"),header = TRUE)

zone5.df<-rbind(fy.zone5.df,cy.zone5.df)

Fon<-'serif'
LST.zone5.p0<-ggplot(zone5.df, aes(x = per, y = lst,group=type,colour=type)) + 
  geom_point(alpha=0.8,size=2.0)+
  scale_x_continuous(limits = c(0,105),breaks = c(seq(0,100,20)),expand = c(0,0))+
  scale_y_continuous(limits = c(-0.5,2.5),breaks = c(0,1,2),expand = c(0,0))+
  scale_colour_manual(name=NULL,   values =c("#E64B35FF","royalblue3"),
                      guide=guide_legend(reverse=T))+
  stat_smooth(formula=y~x,fill="lightgrey",alpha=0.7,size=1.5,method="lm",se=TRUE,level=0.95)+
  theme_bw()+
  annotate('text',x=35,y=1.75,label=" R  = 0.47    p<0.001 ",color="red",size=5,family="serif")+
  annotate('text',x=35,y=1.25,label="R  = 0.66    p<0.001",color="royalblue3",size=5,family="serif")+
  annotate('text',x=14,y=1.35,label="2",color="royalblue3",size=3,family="serif")+
  annotate('text',x=14,y=1.85,label="2",color="red",size=3,family="serif")+
  annotate('text',x=90,y=1.5,label=" Zone5 ",color="black",size=5,family="serif")+
  geom_hline(aes(yintercept=0),color="grey50")+
  theme(axis.line.x=element_line(linetype=1,color="grey30",size=0),
        axis.ticks.x=element_line(linetype=1,color="grey30",size=0),
        axis.ticks.length.x=unit(0.15,'cm'), 
        axis.line.y=element_line(linetype=1,color="grey30",size=0),
        axis.ticks.y=element_line(color="black",size=0.5,lineend = 1),
        axis.ticks.length.y=unit(0.15,'cm'),
        axis.title = element_text(family=Fon, face='bold',size=10,color="black"),
        axis.text = element_text(family=Fon, size=13,color="black") )+ 
  theme(legend.position = "none")+
  theme(plot.title = element_text(family=Fon, face='bold',size=14,hjust = 0.5),
        aspect.ratio = 1,
        panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"))+ 
  labs(x=NULL,y=NULL)
LST.zone5.p0




