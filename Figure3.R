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

# Figure 4----------------------------
# 1: read data-----------------------
fy.df<-read.csv(paste0(".\\Figure3.data\\FY_ts.csv"),header = TRUE)
cy.df<-read.csv(paste0(".\\Figure3.data\\CY_ts.csv"),header = TRUE)

fy.df$type<-c("fire")
cy.df$type<-c("harvest")

data.df<-rbind(fy.df,cy.df)

# 2: plot LST-----------------------
Fon<-'serif'#设置字体为新罗马
LST.ts.Map<-ggplot(data.df,aes(x=year,y=LSTmean,group=type,color=type))+
  geom_line(linewidth=1)+ geom_point(size=1.8)+
  scale_x_continuous(breaks=seq(-2,16,2),limits = c(-2.3,16.1))+
  geom_errorbar(aes(ymin=LSTmean-LSTsd,ymax=LSTmean+LSTsd), width=0.5,cex=0.55)+
  geom_hline(aes(yintercept=0),alpha=0.65)+
  scale_y_continuous(breaks = c(-0.2,0,0.2,0.4,0.6),limits = c(-0.2,0.6))+
  scale_colour_manual( name=NULL,values =c("#E64B35FF","royalblue3"))+
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
        aspect.ratio = 0.75,
        panel.border = element_rect(fill=NA,color="black", size=0.7,
                                    linetype="solid"))+
  theme(legend.position="none")+
  theme(panel.grid.major = element_line(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_line())+
  labs(x=NULL,y="△LST(�?)",title = NULL)
LST.ts.Map

# 3: plot ET-----------------------
ET.ts.Map<-ggplot(data.df,aes(x=year,y=ETmean,group=type,color=type))+
  geom_line(size=1)+ geom_point(size=1.8)+
  scale_x_continuous(breaks=seq(-2,16,2),limits = c(-2.3,16.1))+
  geom_errorbar(aes(ymin=ETmean-ETsd,ymax=ETmean+ETsd), width=0.5,cex=0.55)+
  geom_hline(aes(yintercept=0),alpha=0.65)+
  scale_y_continuous(breaks = c(-0.1,0,0.1),limits = c(-0.1,0.1))+
  scale_colour_manual( name=NULL,values =c("#E64B35FF","royalblue3"))+
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
        aspect.ratio = 0.75,
        panel.border = element_rect(fill=NA,color="black", size=0.7,
                                    linetype="solid"))+
  theme(legend.position="none")+
  theme(panel.grid.major = element_line(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_line())+
  labs(x=NULL,y="△ET",title = NULL)
ET.ts.Map

# 4: plot Albedo-----------------------
Albedo.ts.Map<-ggplot(data.df,aes(x=year,y=Albedomean,group=type,color=type))+
  geom_line(size=1)+ geom_point(size=1.8)+
  scale_x_continuous(breaks=seq(-2,16,2),limits = c(-2.3,16.1))+
  geom_errorbar(aes(ymin=Albedomean-Albedosd,ymax=Albedomean+Albedosd), width=0.5,cex=0.55)+
  geom_hline(aes(yintercept=0),alpha=0.65)+
  scale_y_continuous(breaks = c(seq(-0.01,0.03,0.01)),limits = c(-0.01,0.03))+
  scale_colour_manual( name=NULL,values =c("#E64B35FF","royalblue3"))+
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
        aspect.ratio = 0.75,
        panel.border = element_rect(fill=NA,color="black", size=0.7,
                                    linetype="solid"))+
  theme(legend.position="none")+
  theme(panel.grid.major = element_line(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_line())+
  labs(x=NULL,y="△Albedo",title = NULL)
Albedo.ts.Map

# 5: plot NDVI-----------------------
NDVI.ts.Map<-ggplot(data.df,aes(x=year,y=NDVImean,group=type,color=type))+
  geom_line(size=1)+ geom_point(size=1.8)+
  scale_x_continuous(breaks=seq(-2,16,2),limits = c(-2.3,16.1))+
  geom_errorbar(aes(ymin=NDVImean-NDVIsd,ymax=NDVImean+NDVIsd), width=0.5,cex=0.55)+
  geom_hline(aes(yintercept=0),alpha=0.65)+
  scale_y_continuous(breaks = c(seq(-0.08,0.04,0.04)),limits = c(-0.06,0.04))+
  scale_colour_manual( name=NULL,values =c("#E64B35FF","royalblue3"))+
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
        aspect.ratio = 0.75,
        panel.border = element_rect(fill=NA,color="black", size=0.7,
                                    linetype="solid"))+
  theme(legend.position="none")+
  theme(panel.grid.major = element_line(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_line())+
  labs(x=NULL,y="△NDVI",title = NULL)
NDVI.ts.Map



