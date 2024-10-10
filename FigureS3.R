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

#Figure S3-----------------------------

#Note: spatial plot of ET/NDVI/Albedo were plotted in Figure 2.
#this part contains its latitude distribution

#1: fire year ----------
#(1):ET----
fy.et.lat.df<-read.csv(paste0("E:\\PhD_qiushuang\\GEE_CF_CP\\AllFigures\\writing\\Review_two\\FigureS3.data\\FY_ET.lat.csv"),header = TRUE)

Fon<-'serif'
et.FY_lat.map<-ggplot(data=fy.et.lat.df,aes(x=Total_mn,y=lat1))+
  scale_y_continuous(breaks = c(20,30,40,50, 60),labels = c('20¡ãN','30¡ãN','40¡ãN','50¡ãN','60¡ãN'),
                     limit=c(15,55)) +
  scale_x_continuous(breaks= c(-0.3,0,0.2),labels = c('-0.3','0','0.2')) +
  geom_vline(xintercept =0,linetype="dashed")+
  geom_path(colour="red",alpha=1,size=0.7)+
  geom_ribbon(aes(xmin=Total_mn-Total_sd,xmax=Total_mn+Total_sd),fill="red",alpha=0.2)+ 
  theme_minimal()+
  theme_bw()+
  theme(axis.line.x=element_line(linetype=1,color="grey30",size=0), 
        axis.ticks.x=element_line(color="black",size=0.5,lineend = 1), 
        axis.ticks.length.x=unit(0.15,'cm'), 
        axis.line.y=element_line(linetype=1,color="grey30",size=0),
        axis.ticks.y=element_line(color="black",size=0.5,lineend = 1),
        axis.ticks.length.y=unit(0.15,'cm'))+
  theme(axis.title = element_text(family=Fon, face='bold',size=11,color="black"),
        axis.text = element_text(family=Fon,  size=11,color="black"))+
  labs(x=NULL,y=NULL)+
  coord_cartesian(xlim=c(-0.35,0.24),ylim = c(15, NA))
et.FY_lat.map

#(2):albedo----
fy.alb.lat.df<-read.csv(paste0("E:\\PhD_qiushuang\\GEE_CF_CP\\AllFigures\\writing\\Review_two\\FigureS3.data\\FY_Alb.lat.csv"),header = TRUE)

Albedo.FY_lat.map<-ggplot(data=fy.alb.lat.df,aes(x=Total_mn,y=lat1))+
  scale_y_continuous(breaks = c(20,30,40,50, 60),labels = c('20¡ãN','30¡ãN','40¡ãN','50¡ãN','60¡ãN'),
                     limit=c(15,55)) +
  scale_x_continuous(breaks= c(-0.02,0,0.02),labels = c('-0.02','0','0.02')) +
  geom_vline(xintercept =0,linetype="dashed")+
  geom_path(colour="red",alpha=1,size=0.7)+
  geom_ribbon(aes(xmin=Total_mn-Total_sd,xmax=Total_mn+Total_sd),fill="red",alpha=0.2)+ 
  theme_minimal()+
  theme_bw()+
  theme(axis.line.x=element_line(linetype=1,color="grey30",size=0),
        axis.ticks.x=element_line(color="black",size=0.5,lineend = 1), 
        axis.ticks.length.x=unit(0.15,'cm'), 
        axis.line.y=element_line(linetype=1,color="grey30",size=0),
        axis.ticks.y=element_line(color="black",size=0.5,lineend = 1),
        axis.ticks.length.y=unit(0.15,'cm'))+
  theme(axis.title = element_text(family=Fon, face='bold',size=11,color="black"),
        axis.text = element_text(family=Fon, size=11,color="black"))+
  labs(x=NULL,y=NULL)+
  coord_cartesian(xlim=c(-0.02,0.024),ylim = c(15, NA))
Albedo.FY_lat.map

#(3):NDVI----
fy.ndvi.lat.df<-read.csv(paste0("E:\\PhD_qiushuang\\GEE_CF_CP\\AllFigures\\writing\\Review_two\\FigureS3.data\\FY_NDVI.lat.csv"),header = TRUE)

Fon<-'serif'
NDVI.FY_lat.map<-ggplot(data=fy.ndvi.lat.df,aes(x=Total_mn,y=lat1))+
  scale_y_continuous(breaks = c(20,30,40,50, 60),labels = c('20¡ãN','30¡ãN','40¡ãN','50¡ãN','60¡ãN'),
                     limit=c(15,55)) +
  scale_x_continuous(breaks= c(-0.1,0,0.05),labels = c('-0.1','0','0.05')) +
  geom_vline(xintercept =0,linetype="dashed")+
  geom_path(colour="red",alpha=1,size=0.7)+
  geom_ribbon(aes(xmin=Total_mn-Total_sd,xmax=Total_mn+Total_sd),fill="red",alpha=0.2)+
  theme_minimal()+
  theme_bw()+
  theme(axis.line.x=element_line(linetype=1,color="grey30",size=0), 
        axis.ticks.x=element_line(color="black",size=0.5,lineend = 1),
        axis.ticks.length.x=unit(0.15,'cm'), 
        axis.line.y=element_line(linetype=1,color="grey30",size=0),
        axis.ticks.y=element_line(color="black",size=0.5,lineend = 1),
        axis.ticks.length.y=unit(0.15,'cm'))+
  theme(axis.title = element_text(family=Fon, face='bold',size=11,color="black"),
        axis.text = element_text(family=Fon, size=11,color="black"))+
  labs(x=NULL,y=NULL)+
  coord_cartesian(xlim=c(-0.1,0.07),ylim = c(15, NA))
NDVI.FY_lat.map


#2: harvest year ----------
#(1):ET----
cy.et.lat.df<-read.csv(paste0("E:\\PhD_qiushuang\\GEE_CF_CP\\AllFigures\\writing\\Review_two\\FigureS3.data\\CY_ET.lat.csv"),header = TRUE)

Fon<-'serif'
et.CY_lat.map<-ggplot(data=cy.et.lat.df,aes(x=Total_mn,y=lat1))+
  scale_y_continuous(breaks = c(20,30,40,50, 60),labels = c('20¡ãN','30¡ãN','40¡ãN','50¡ãN','60¡ãN'),
                     limit=c(15,55)) +
  scale_x_continuous(breaks= c(-0.3,0,0.2),labels = c('-0.3','0','0.2')) +
  geom_vline(xintercept =0,linetype="dashed")+
  geom_path(colour="red",alpha=1,size=0.7)+
  geom_ribbon(aes(xmin=Total_mn-Total_sd,xmax=Total_mn+Total_sd),fill="red",alpha=0.2)+ 
  theme_minimal()+
  theme_bw()+
  theme(axis.line.x=element_line(linetype=1,color="grey30",size=0), 
        axis.ticks.x=element_line(color="black",size=0.5,lineend = 1),
        axis.ticks.length.x=unit(0.15,'cm'), 
        axis.line.y=element_line(linetype=1,color="grey30",size=0),
        axis.ticks.y=element_line(color="black",size=0.5,lineend = 1),
        axis.ticks.length.y=unit(0.15,'cm'))+
  theme(axis.title = element_text(family=Fon, face='bold',size=11,color="black"),
        axis.text = element_text(family=Fon,size=11,color="black"))+
  labs(x=NULL,y=NULL)+
  coord_cartesian(xlim=c(-0.35,0.24),ylim = c(15, NA))
et.CY_lat.map

#(2):albedo----
cy.alb.lat.df<-read.csv(paste0("E:\\PhD_qiushuang\\GEE_CF_CP\\AllFigures\\writing\\Review_two\\FigureS3.data\\CY_Alb.lat.csv"),header = TRUE)

Fon<-'serif'
Albedo.CY_lat.map<-ggplot(data=cy.alb.lat.df,aes(x=Total_mn,y=lat1))+
  scale_y_continuous(breaks = c(20,30,40,50, 60),labels = c('20¡ãN','30¡ãN','40¡ãN','50¡ãN','60¡ãN'),
                     limit=c(15,55)) +
  scale_x_continuous(breaks= c(-0.02,0,0.02),labels = c('-0.02','0','0.02')) +
  geom_vline(xintercept =0,linetype="dashed")+
  geom_path(colour="red",alpha=1,size=0.7)+
  geom_ribbon(aes(xmin=Total_mn-Total_sd,xmax=Total_mn+Total_sd),fill="red",alpha=0.2)+ 
  theme_minimal()+
  theme_bw()+
  theme(axis.line.x=element_line(linetype=1,color="grey30",size=0), 
        axis.ticks.x=element_line(color="black",size=0.5,lineend = 1), 
        axis.ticks.length.x=unit(0.15,'cm'), 
        axis.line.y=element_line(linetype=1,color="grey30",size=0),
        axis.ticks.y=element_line(color="black",size=0.5,lineend = 1),
        axis.ticks.length.y=unit(0.15,'cm'))+
  theme(axis.title = element_text(family=Fon,face='bold',size=11,color="black"),
        axis.text = element_text(family=Fon, size=11,color="black"))+
  labs(x=NULL,y=NULL)+
  coord_cartesian(xlim=c(-0.02,0.024),ylim = c(15, NA))
Albedo.CY_lat.map

#(3):NDVI----
cy.ndvi.lat.df<-read.csv(paste0("E:\\PhD_qiushuang\\GEE_CF_CP\\AllFigures\\writing\\Review_two\\FigureS3.data\\CY_NDVI.lat.csv"),header = TRUE)

Fon<-'serif'
NDVI.CY_lat.map<-ggplot(data=NDVI.lat.df1,aes(x=Total_mn,y=lat1))+
  scale_y_continuous(breaks = c(20,30,40,50, 60),labels = c('20¡ãN','30¡ãN','40¡ãN','50¡ãN','60¡ãN'),
                     limit=c(15,55)) +
  scale_x_continuous(breaks= c(-0.1,0,0.05),labels = c('-0.1','0','0.05')) +
  geom_vline(xintercept =0,linetype="dashed")+
  geom_path(colour="red",alpha=1,size=0.7)+
  geom_ribbon(aes(xmin=Total_mn-Total_sd,xmax=Total_mn+Total_sd),fill="red",alpha=0.2)+ 
  theme_minimal()+
  theme_bw()+
  theme(axis.line.x=element_line(linetype=1,color="grey30",size=0),
        axis.ticks.x=element_line(color="black",size=0.5,lineend = 1), 
        axis.ticks.length.x=unit(0.15,'cm'),
        axis.line.y=element_line(linetype=1,color="grey30",size=0),
        axis.ticks.y=element_line(color="black",size=0.5,lineend = 1),
        axis.ticks.length.y=unit(0.15,'cm'))+
  theme(axis.title = element_text(family=Fon, face='bold',size=11,color="black"),
        axis.text = element_text(family=Fon,  size=11,color="black"))+
  labs(x=NULL,y=NULL)+
  coord_cartesian(xlim=c(-0.1,0.07),ylim = c(15, NA))
NDVI.CY_lat.map



