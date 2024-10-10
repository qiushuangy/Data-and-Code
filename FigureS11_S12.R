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

#Figure S11 and S12
# 1: Figure S11 (fire year)------------------------------------------
###(1): summary data----------------
fy50.ts.df<-read.csv(paste0(".\\FigureS11_S12.data\\FY50_ts.csv"),header = TRUE)
fy2550.ts.df<-read.csv(paste0(".\\FigureS11_S12.data\\FY2550_ts.csv"),header = TRUE)
fy1025.ts.df<-read.csv(paste0(".\\FigureS11_S12.data\\FY1025_ts.csv"),header = TRUE)
fy510.ts.df<-read.csv(paste0(".\\FigureS11_S12.data\\FY510_ts.csv"),header = TRUE)

fy.all50.ts.df1<-fy50.ts.df
fy.all2550.ts.df1<-fy2550.ts.df
fy.all1025.ts.df1<-fy1025.ts.df
fy.all510.ts.df1<-fy510.ts.df

fy.all50.ts.df1$per<-c("I")
fy.all2550.ts.df1$per<-c("II")
fy.all1025.ts.df1$per<-c("III")
fy.all510.ts.df1$per<-c("IV")

fy.data.50.df<-rbind(fy.all50.ts.df1,fy.all2550.ts.df1,fy.all1025.ts.df1,fy.all510.ts.df1)
fy.data.50.df$type<-c("fire")


###(2): plot-------------
### LST
Fon<-'serif'
fy.LST.ts.Map<-ggplot(fy.data.50.df,aes(x=year,y=LSTmean,group=per,color=per))+
  geom_line(size=1,alpha=0.8)+ geom_point(size=1.8,alpha=0.7)+
  scale_x_continuous(breaks=seq(-2,16,2),limits = c(-2.3,16))+
  geom_errorbar(aes(ymin=LSTmean-LSTsd,ymax=LSTmean+LSTsd), width=0.5,cex=0.55)+
  geom_hline(aes(yintercept=0),alpha=0.65)+
  scale_y_continuous(breaks = c(seq(-0.8,1.2,0.4)),limits = c(-0.8,1.2))+
  scale_colour_manual( name=NULL,values =c("#d73027","#fdb863", "#a6d96a", "#1a9850"))+
  theme_minimal()+
  theme(axis.line.x=element_line(linetype=1,color="grey30",size=0), 
        axis.ticks.x=element_line(color="black",size=0.5,lineend = 1),
        axis.ticks.length.x=unit(0.15,'cm'),
        axis.line.y=element_line(linetype=1,color="grey30",size=0),
        axis.ticks.y=element_line(color="black",size=0.5,lineend = 1),
        axis.ticks.length.y=unit(0.15,'cm'))+
  theme(axis.title = element_text(family=Fon, face='bold',size=13,color="black"),
        axis.text = element_text(family=Fon, size=13,color="black"),
        plot.title = element_text(family=Fon, face='bold',size=13,hjust = 0.5),
        aspect.ratio = 0.7,
        panel.border = element_rect(fill=NA,color="black", size=0.7,linetype="solid"))+
  theme(legend.direction = "horizontal",
        legend.title = element_text(face = "bold",family = Fon,colour = "black",size = 13), 
        legend.text =element_text(family=Fon,size=12,color="black"), 
        legend.key.size =unit(15,"pt"),
        legend.key.height =unit(10,"pt"),
        legend.key.width =unit(10,"pt"),
        legend.position = c(0.60,0.88))+
  theme(panel.grid.major = element_line(),
        panel.grid.minor = element_blank())+
  labs(x=NULL,y="△LST (℃)")
fy.LST.ts.Map

### ET
fy.ET.ts.Map<-ggplot(fy.data.50.df,aes(x=year,y=ETmean,group=per,color=per))+
  geom_line(size=1,alpha=0.8)+ geom_point(size=1.8,alpha=0.7)+
  scale_x_continuous(breaks=seq(-2,16,2),limits = c(-2.3,16))+
  geom_errorbar(aes(ymin=ETmean-ETsd,ymax=ETmean+ETsd), width=0.5,cex=0.55)+
  geom_hline(aes(yintercept=0),alpha=0.65)+
  scale_y_continuous(breaks = c(seq(-0.2,0.2,0.1)),limits = c(-0.2,0.2))+
  scale_colour_manual( name=NULL,values =c("#d73027","#fdb863", "#a6d96a", "#1a9850"))+
  theme_minimal()+
  theme(axis.line.x=element_line(linetype=1,color="grey30",size=0), 
        axis.ticks.x=element_line(color="black",size=0.5,lineend = 1), 
        axis.ticks.length.x=unit(0.15,'cm'), 
        axis.line.y=element_line(linetype=1,color="grey30",size=0),
        axis.ticks.y=element_line(color="black",size=0.5,lineend = 1),
        axis.ticks.length.y=unit(0.15,'cm'))+
  theme(axis.title = element_text(family=Fon, face='bold',size=13,color="black"),
        axis.text = element_text(family=Fon, size=13,color="black"),
        plot.title = element_text(family=Fon, face='bold',size=13,hjust = 0.5),
        aspect.ratio = 0.7,
        panel.border = element_rect(fill=NA,color="black", size=0.7,
                                    linetype="solid"))+
  theme(legend.direction = "horizontal",
        legend.title = element_text(face = "bold",family = Fon,colour = "black",size = 13), 
        legend.text = element_text(family=Fon,size=12,color="black"), 
        legend.key.size =unit(15,"pt"),
        legend.key.height =unit(10,"pt"),
        legend.key.width =unit(10,"pt"),
        legend.position = c(0.6,0.88))+
  theme(panel.grid.major = element_line(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_line())+
  labs(x=NULL,y="△ET",title = NULL)
fy.ET.ts.Map

### Albedo
fy.Albedo.ts.Map<-ggplot(fy.data.50.df,aes(x=year,y=Albedomean,group=per,color=per))+
  geom_line(size=1,alpha=0.8)+ geom_point(size=1.8,alpha=0.7)+
  scale_x_continuous(breaks=seq(-2,16,2),limits = c(-2.3,16.1))+
  geom_errorbar(aes(ymin=Albedomean-Albedosd,ymax=Albedomean+Albedosd), width=0.5,cex=0.55)+
  geom_hline(aes(yintercept=0),alpha=0.65)+
  scale_y_continuous(breaks = c(seq(-0.02,0.08,0.02)),limits = c(-0.02,0.08))+
  scale_colour_manual( name=NULL,values =c("#d73027","#fdb863", "#a6d96a", "#1a9850"))+
  theme_minimal()+
  theme(axis.line.x=element_line(linetype=1,color="grey30",size=0), 
        axis.ticks.x=element_line(color="black",size=0.5,lineend = 1),
        axis.ticks.length.x=unit(0.15,'cm'), 
        axis.line.y=element_line(linetype=1,color="grey30",size=0),
        axis.ticks.y=element_line(color="black",size=0.5,lineend = 1),
        axis.ticks.length.y=unit(0.15,'cm'))+
  theme(axis.title = element_text(family=Fon, face='bold',size=13,color="black"),
        axis.text = element_text(family=Fon, size=13,color="black"),
        plot.title = element_text(family=Fon, face='bold',size=13,hjust = 0.5),
        aspect.ratio = 0.7,
        panel.border = element_rect(fill=NA,color="black", size=0.7,
                                    linetype="solid"))+
  theme(legend.direction = "horizontal",
        legend.title = element_text(face = "bold",family = Fon,colour = "black",size = 13), 
        legend.text = element_text(family=Fon,size=12,color="black"),
        legend.key.size =unit(15,"pt"), 
        legend.key.height =unit(10,"pt"),
        legend.key.width =unit(10,"pt"),
        legend.position = c(0.6,0.88))+
  theme(panel.grid.major = element_line(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x=NULL,y="△Albedo",title = NULL)
fy.Albedo.ts.Map

### NDVI
fy.NDVI.ts.Map<-ggplot(fy.data.50.df,aes(x=year,y=NDVImean,group=per,color=per))+
  geom_line(size=1,alpha=0.8)+ geom_point(size=1.8,alpha=0.7)+
  scale_x_continuous(breaks=seq(-2,16,2),limits = c(-2.3,16.1))+
  geom_errorbar(aes(ymin=NDVImean-NDVIsd,ymax=NDVImean+NDVIsd), width=0.5,cex=0.55)+
  geom_hline(aes(yintercept=0),alpha=0.65)+
  scale_y_continuous(breaks = c(seq(-0.2,0.1,0.1)),limits = c(-0.2,0.1))+
  scale_colour_manual( name=NULL,values =c("#d73027","#fdb863", "#a6d96a", "#1a9850"))+
  theme_minimal()+
  theme(axis.line.x=element_line(linetype=1,color="grey30",size=0), 
        axis.ticks.x=element_line(color="black",size=0.5,lineend = 1), 
        axis.ticks.length.x=unit(0.15,'cm'), 
        axis.line.y=element_line(linetype=1,color="grey30",size=0),
        axis.ticks.y=element_line(color="black",size=0.5,lineend = 1),
        axis.ticks.length.y=unit(0.15,'cm'))+
  theme(axis.title = element_text(family=Fon, face='bold',size=13,color="black"),
        axis.text = element_text(family=Fon, size=13,color="black"),
        plot.title = element_text(family=Fon, face='bold',size=13,hjust = 0.5),
        aspect.ratio = 0.7,
        panel.border = element_rect(fill=NA,color="black", size=0.7,linetype="solid"))+
  theme(legend.direction = "horizontal",
        legend.title = element_text(face = "bold",family = Fon,colour = "black",size = 13), 
        legend.text = element_text(family=Fon,size=12,color="black"),
        legend.key.size =unit(15,"pt"),
        legend.key.height =unit(10,"pt"),
        legend.key.width =unit(10,"pt"),
        legend.position = c(0.6,0.88))+
  theme(panel.grid.major = element_line(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_line())+
  labs(x=NULL,y="△NDVI",title = NULL)
fy.NDVI.ts.Map


# 2: Figure S12 (harvest year)------------------------------------------
###(1): summary data------------------
cy50.ts.df<-read.csv(paste0(".\\FigureS11_S12.data\\CY50_ts.csv"),header = TRUE)
cy2550.ts.df<-read.csv(paste0(".\\FigureS11_S12.data\\CY2550_ts.csv"),header = TRUE)
cy1025.ts.df<-read.csv(paste0(".\\FigureS11_S12.data\\CY1025_ts.csv"),header = TRUE)
cy510.ts.df<-read.csv(paste0(".\\FigureS11_S12.data\\CY510_ts.csv"),header = TRUE)

cy.all50.ts.df1<-cy50.ts.df
cy.all2550.ts.df1<-cy2550.ts.df
cy.all1025.ts.df1<-cy1025.ts.df
cy.all510.ts.df1<-cy510.ts.df

cy.all50.ts.df1$per<-c("I")
cy.all2550.ts.df1$per<-c("II")
cy.all1025.ts.df1$per<-c("III")
cy.all510.ts.df1$per<-c("IV")

cy.data.50.df<-rbind(cy.all50.ts.df1,cy.all2550.ts.df1,cy.all1025.ts.df1,cy.all510.ts.df1)
cy.data.50.df$type<-c("harvest")


###(2): plot-------------
### LST
cy.LST.ts.Map<-ggplot(cy.data.50.df,aes(x=year,y=LSTmean,group=per,color=per))+
  geom_line(size=1,alpha=0.8)+ geom_point(size=1.8,alpha=0.7)+
  scale_x_continuous(breaks=seq(-2,16,2),limits = c(-2.3,16.1))+
  geom_errorbar(aes(ymin=LSTmean-LSTsd,ymax=LSTmean+LSTsd), width=0.5,cex=0.55)+
  geom_hline(aes(yintercept=0),alpha=0.65)+
  scale_y_continuous(breaks = c(seq(-0.8,1.2,0.4)),limits = c(-0.8,1.2))+
  scale_colour_manual( name=NULL,values =c("#d73027","#fdb863", "#a6d96a", "#1a9850"))+
  theme_minimal()+
  theme(axis.line.x=element_line(linetype=1,color="grey30",size=0), 
        axis.ticks.x=element_line(color="black",size=0.5,lineend = 1), 
        axis.ticks.length.x=unit(0.15,'cm'), 
        axis.line.y=element_line(linetype=1,color="grey30",size=0),
        axis.ticks.y=element_line(color="black",size=0.5,lineend = 1),
        axis.ticks.length.y=unit(0.15,'cm'))+
  theme(axis.title = element_text(family=Fon, face='bold',size=13,color="black"),
        axis.text = element_text(family=Fon, size=13,color="black"),
        plot.title = element_text(family=Fon, face='bold',size=13,hjust = 0.5),
        aspect.ratio = 0.7,
        panel.border = element_rect(fill=NA,color="black", size=0.7,linetype="solid"))+
  theme(legend.direction = "horizontal",
        legend.title = element_text(face = "bold",family = Fon,colour = "black",size = 13), 
        legend.text = element_text(family=Fon,size=12,color="black"), 
        legend.key.size =unit(15,"pt"), 
        legend.key.height =unit(10,"pt"),
        legend.key.width =unit(10,"pt"),
        legend.position = c(0.6,0.88))+
  theme(panel.grid.major = element_line(),
        panel.grid.minor = element_blank())+
  labs(x=NULL,y="△LST (℃)",title = NULL)
cy.LST.ts.Map

### ET
cy.ET.ts.Map<-ggplot(cy.data.50.df,aes(x=year,y=ETmean,group=per,color=per))+
  geom_line(size=1,alpha=0.8)+ geom_point(size=1.8,alpha=0.7)+
  scale_x_continuous(breaks=seq(-2,16,2),limits = c(-2.3,16.1))+
  geom_errorbar(aes(ymin=ETmean-ETsd,ymax=ETmean+ETsd), width=0.5,cex=0.55)+
  geom_hline(aes(yintercept=0),alpha=0.65)+
  scale_y_continuous(breaks = c(seq(-0.2,0.2,0.1)),limits = c(-0.24,0.2))+
  scale_colour_manual( name=NULL,values =c("#d73027","#fdb863", "#a6d96a", "#1a9850"))+
  theme_minimal()+
  theme(axis.line.x=element_line(linetype=1,color="grey30",size=0), 
        axis.ticks.x=element_line(color="black",size=0.5,lineend = 1), 
        axis.ticks.length.x=unit(0.15,'cm'), 
        axis.line.y=element_line(linetype=1,color="grey30",size=0),
        axis.ticks.y=element_line(color="black",size=0.5,lineend = 1),
        axis.ticks.length.y=unit(0.15,'cm'))+
  theme(axis.title = element_text(family=Fon, face='bold',size=13,color="black"),
        axis.text = element_text(family=Fon, size=13,color="black"),
        plot.title = element_text(family=Fon, face='bold',size=13,hjust = 0.5),
        aspect.ratio = 0.7,
        panel.border = element_rect(fill=NA,color="black", size=0.7,linetype="solid"))+
  theme(legend.direction = "horizontal",
        legend.title = element_text(face = "bold",family = Fon,colour = "black",size = 13), 
        legend.text = element_text(family=Fon,size=12,color="black"), 
        legend.key.size =unit(15,"pt"), 
        legend.key.height =unit(10,"pt"),
        legend.key.width =unit(10,"pt"),
        legend.position = c(0.6,0.88))+
  theme(panel.grid.major = element_line(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_line())+
  labs(x=NULL,y="△ET",title = NULL)
cy.ET.ts.Map

### Albedo
cy.Albedo.ts.Map<-ggplot(cy.data.50.df,aes(x=year,y=Albedomean,group=per,color=per))+
  geom_line(size=1,alpha=0.8)+ geom_point(size=1.8,alpha=0.7)+
  scale_x_continuous(breaks=seq(-2,16,2),limits = c(-2.3,16.1))+
  geom_errorbar(aes(ymin=Albedomean-Albedosd,ymax=Albedomean+Albedosd), width=0.5,cex=0.55)+
  geom_hline(aes(yintercept=0),alpha=0.65)+
  scale_y_continuous(breaks = c(seq(-0.02,0.08,0.02)),limits = c(-0.02,0.08))+
  scale_colour_manual( name=NULL,values =c("#d73027","#fdb863", "#a6d96a", "#1a9850"))+
  theme_minimal()+
  theme(axis.line.x=element_line(linetype=1,color="grey30",size=0), 
        axis.ticks.x=element_line(color="black",size=0.5,lineend = 1),
        axis.ticks.length.x=unit(0.15,'cm'), 
        axis.line.y=element_line(linetype=1,color="grey30",size=0),
        axis.ticks.y=element_line(color="black",size=0.5,lineend = 1),
        axis.ticks.length.y=unit(0.15,'cm'))+
  theme(axis.title = element_text(family=Fon, face='bold',size=13,color="black"),
        axis.text = element_text(family=Fon, size=13,color="black"),
        plot.title = element_text(family=Fon, face='bold',size=13,hjust = 0.5),
        aspect.ratio = 0.7,
        panel.border = element_rect(fill=NA,color="black", size=0.7,linetype="solid"))+
  theme(legend.direction = "horizontal",
        legend.title = element_text(face = "bold",family = Fon,colour = "black",size = 13), 
        legend.text = element_text(family=Fon,size=12,color="black"),
        legend.key.size =unit(15,"pt"), 
        legend.key.height =unit(10,"pt"),
        legend.key.width =unit(10,"pt"),
        legend.position = c(0.6,0.88))+
  theme(panel.grid.major = element_line(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x=NULL,y="△Albedo",title = NULL)
cy.Albedo.ts.Map

### NDVI
cy.NDVI.ts.Map<-ggplot(cy.data.50.df,aes(x=year,y=NDVImean,group=per,color=per))+
  geom_line(size=1,alpha=0.8)+ geom_point(size=1.8,alpha=0.7)+
  scale_x_continuous(breaks=seq(-2,16,2),limits = c(-2.3,16.1))+
  geom_errorbar(aes(ymin=NDVImean-NDVIsd,ymax=NDVImean+NDVIsd), width=0.5,cex=0.55)+
  geom_hline(aes(yintercept=0),alpha=0.65)+
  scale_y_continuous(breaks = c(seq(-0.2,0.1,0.1)),limits = c(-0.2,0.1))+
  scale_colour_manual( name=NULL,values =c("#d73027","#fdb863", "#a6d96a", "#1a9850"))+
  theme_minimal()+
  theme(axis.line.x=element_line(linetype=1,color="grey30",size=0), 
        axis.ticks.x=element_line(color="black",size=0.5,lineend = 1), 
        axis.ticks.length.x=unit(0.15,'cm'), 
        axis.line.y=element_line(linetype=1,color="grey30",size=0),
        axis.ticks.y=element_line(color="black",size=0.5,lineend = 1),
        axis.ticks.length.y=unit(0.15,'cm'))+
  theme(axis.title = element_text(family=Fon, face='bold',size=13,color="black"),
        axis.text = element_text(family=Fon, size=13,color="black"),
        plot.title = element_text(family=Fon, face='bold',size=13,hjust = 0.5),
        aspect.ratio = 0.7,
        panel.border = element_rect(fill=NA,color="black", size=0.7,
                                    linetype="solid"))+
  theme(legend.direction = "horizontal",
        legend.title = element_text(face = "bold",family = Fon,colour = "black",size = 13), 
        legend.text = element_text(family=Fon,size=12,color="black"),
        legend.key.size =unit(15,"pt"), 
        legend.key.height =unit(10,"pt"),
        legend.key.width =unit(10,"pt"),
        legend.position = c(0.6,0.88))+
  theme(panel.grid.major = element_line(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_line())+
  labs(x=NULL,y="△NDVI",title = NULL)
cy.NDVI.ts.Map
