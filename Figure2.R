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

# Figure 2-----
# 一、The LST change due to fire-induced (a) and harvest-induced (b) forest cover loss. 

#1、spatial plot of LST in fire year  -----------------------------------------------
### mean of all percentage 
raster2spdf <- function(r){
  df <- as.data.frame(as(r, "SpatialPixelsDataFrame"))
  colnames(df) <- c("value", "x", "y")
  df
}

proj.geo = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0 "
climate2.analysis<-readOGR("E:\\PhD_qiushuang\\R_data\\GEE_Export_China3\\climate_zone\\8zone_84.shp",
                           use_iconv = TRUE,encoding = "UTF-8")
zone1<-fortify(climate2.analysis)
China.boudary<-readOGR("E:\\PhD_qiushuang\\Boundary\\gridChina2022\\焦\\中国\\领土.shp",use_iconv = TRUE,encoding = "UTF-8")
zone2<-fortify(China.boudary)

### fire year data
LSTper5100.FY<-raster(".\\Figure2.data\\fireyear\\Tm_FYp5100.tif")
ETper5100.FY<-raster(".\\Figure2.data\\fireyear\\ET_FYp5100.tif")
Albedoper5100.FY<-raster(".\\Figure2.data\\fireyear\\Alb_FYp5100.tif")
NDVIper5100.FY<-raster(".\\Figure2.data\\fireyear\\NDVI_FYp5100.tif")

### mask each other
LSTper5100.FY1<-mask(LSTper5100.FY,ETper5100.FY)
LSTper5100.FY2<-mask(LSTper5100.FY1,Albedoper5100.FY)
LSTper5100.FY3<-mask(LSTper5100.FY2,NDVIper5100.FY)

ETper5100.FY3<-mask(ETper5100.FY,LSTper5100.FY3)
Albedoper5100.FY3<-mask(Albedoper5100.FY,LSTper5100.FY3)
NDVIper5100.FY3<-mask(NDVIper5100.FY,LSTper5100.FY3)

LSTper5100.FY.df<-raster2spdf(LSTper5100.FY3)
colnames(LSTper5100.FY.df)<-c("deltaLST","x", "y")

ETper5100.FY.df<-raster2spdf(ETper5100.FY3)
colnames(ETper5100.FY.df)<-c("deltaET","x", "y")

Albedoper5100.FY.df<-raster2spdf(Albedoper5100.FY3)
colnames(Albedoper5100.FY.df)<-c("deltaAlbedo","x", "y")

NDVIper5100.FY.df<-raster2spdf(NDVIper5100.FY3)
colnames(NDVIper5100.FY.df)<-c("deltaNDVI","x", "y")

### spatial plot of LST 
base.maps<-ggplot()+geom_path(data=zone2, aes(x = long, y = lat, group = group), color = "black", size = 0.1)
base.maps

LSTper5100.FY.df1<-LSTper5100.FY.df

# extract climate zones and retain five zones needed
LST.sp = LSTper5100.FY.df1[,c(2:3)]
coordinates(LST.sp) <- ~x + y
projection(LST.sp) <- proj.geo

LST.zone.df = over(LST.sp,climate2.analysis)
LST.zone.df2 <- data.frame(LSTper5100.FY.df1,LST.zone.df[,4]) 
colnames(LST.zone.df2) <- c('deltaLST','x','y','zone')

LST.zone.df2$zone[LST.zone.df2$zone>=6]=NA 
LST.zone.df3<-na.omit(LST.zone.df2)

LSTper5100.FY.df.final=LST.zone.df3

q25<-quantile(LSTper5100.FY.df.final[,1],probs=c(0.05,0.25,0.5,0.75,0.95))

LSTper5100.FY.df.final1<-LSTper5100.FY.df.final
LSTper5100.FY.df.final1$deltaLST[LSTper5100.FY.df.final1$deltaLST>0.6]=0.6
LSTper5100.FY.df.final1$deltaLST[LSTper5100.FY.df.final1$deltaLST< -0.6]=-0.6

### spatial plot
Fon<-'serif'
LSTp5100.FY.000<-base.maps+
  geom_tile(data=LSTper5100.FY.df.final1, aes(x=x, y=y, fill=deltaLST), alpha=1)+
  geom_path(data=zone2, aes(x = long, y = lat, group = group), color = "black", size = 0.6) +
  geom_polygon(data=zone1, aes(x = long, y = lat, group = group), color = "black", size = 0.6, fill = NA) +
  scale_y_continuous(breaks = seq(0, 60, 10),labels = c('0°N','10°N','20°N','30°N','40°N','50°N','60°N')) +
  scale_x_continuous(breaks= seq(70,140,10),labels = c('70°E','80°E','90°E','100°E','110°E','120°E','130°E','140°E')) +
  scale_fill_gradientn(colours=c("#313695","#74add1","white","#f46d43","#a50026"),#红蓝配色
                       breaks= c(-0.6,0,0.6),labels=c("-0.6"," 0"," 0.6"), 
                       limits = c(-0.63,0.63))+
  theme_minimal()+
  coord_cartesian(ylim=c(15,55))+ 
  theme(panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"))+ 
  theme(legend.position = "none")+
  theme(axis.line.x=element_line(linetype=1,color="grey30",size=0), 
        axis.ticks.x=element_line(color="black",size=0.5,lineend = 1),
        axis.ticks.length.x=unit(0.15,'cm'), 
        axis.line.y=element_line(linetype=1,color="grey30",size=0),
        axis.ticks.y=element_line(color="black",size=0.5,lineend = 1),
        axis.ticks.length.y=unit(0.15,'cm'))+
  theme(axis.title = element_text(family=Fon,face='bold',size=13,color="black"),
        axis.text = element_text(family=Fon,size=12,color="black"))+ 
  theme(plot.title = element_text(family=Fon,face='bold',size=13,hjust = 0.5))+  
  theme(aspect.ratio = 1, #调节长宽比
        panel.grid = element_blank(),
        panel.border = element_rect(fill=NA,color="black",size=0.8,linetype="solid"))+
  theme(legend.title = element_blank(), 
        legend.text = element_text(family=Fon,size=10,color="black"),
        legend.key.size =unit(15,"pt"), 
        legend.key.height =unit(10,"pt"),
        legend.key.width =unit(10,"pt"),
        legend.position = c(0.15,0.18))+
  labs(x=NULL,y=NULL) + ggtitle(NULL)
LSTp5100.FY.000

LSTp5100.FY.001<-base.maps+
  geom_tile(data=LSTper5100.FY.df.final1, aes(x=x, y=y, fill=deltaLST), alpha=1)+
  geom_path(data=zone2, aes(x = long, y = lat, group = group), color = "black", size = 0.6)+
  geom_polygon(data=zone1, aes(x = long, y = lat, group = group), color = "black", size = 0.6, fill = NA) +
  scale_fill_gradientn(colours=c("#313695","#74add1","white","#f46d43","#a50026"),#红蓝配色
                       breaks= c(-0.6,0,0.6),limits = c(-0.63,0.63))+
  coord_cartesian(xlim=c(105,125),ylim=c(3,23))+  
  theme_minimal()+ 
  theme(legend.position = "none")+  
  theme(aspect.ratio = 1,  
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA,color="gray60",linetype=1,size=0.8),
        plot.margin=unit(c(0,0,0,0),"mm"))
LSTp5100.FY.001

LST5100.mean.map = ggdraw() +
  draw_plot(LSTp5100.FY.000) +
  draw_plot(LSTp5100.FY.001, x = 0.80, y = 0.095, width = 0.15, height = 0.15)
LST5100.mean.map


#2、spatial plot of LST in harvest year  -----------------------------------------------
### harvest year data
LSTper5100.CY<-raster(".\\Figure2.data\\cutyear\\Tm_CYp5100.tif")
ETper5100.CY<-raster(".\\Figure2.data\\cutyear\\ET_CYp5100.tif")
Albedoper5100.CY<-raster(".\\Figure2.data\\cutyear\\Alb_CYp5100.tif")
NDVIper5100.CY<-raster(".\\Figure2.data\\cutyear\\NDVI_CYp5100.tif")

### mask each other
LSTper5100.CY1<-mask(LSTper5100.CY,ETper5100.CY)
LSTper5100.CY2<-mask(LSTper5100.CY1,Albedoper5100.CY)
LSTper5100.CY3<-mask(LSTper5100.CY2,NDVIper5100.CY)

ETper5100.CY3<-mask(ETper5100.CY,LSTper5100.CY3)
Albedoper5100.CY3<-mask(Albedoper5100.CY,LSTper5100.CY3)
NDVIper5100.CY3<-mask(NDVIper5100.CY,LSTper5100.CY3)

LSTper5100.CY.df<-raster2spdf(LSTper5100.CY3)
colnames(LSTper5100.CY.df)<-c("deltaLST","x", "y")

ETper5100.CY.df<-raster2spdf(ETper5100.CY3)
colnames(ETper5100.CY.df)<-c("deltaET","x", "y")

Albedoper5100.CY.df<-raster2spdf(Albedoper5100.CY3)
colnames(Albedoper5100.CY.df)<-c("deltaAlbedo","x", "y")

NDVIper5100.CY.df<-raster2spdf(NDVIper5100.CY3)
colnames(NDVIper5100.CY.df)<-c("deltaNDVI","x", "y")

### spatial plot 
base.maps<-ggplot()+geom_path(data=zone2, aes(x = long, y = lat, group = group), color = "black", size = 0.1)
base.maps

LSTper5100.CY.df1<-LSTper5100.CY.df

#only select climate zone I-V
LST.sp = LSTper5100.CY.df1[,c(2:3)]
coordinates(LST.sp) <- ~x + y
projection(LST.sp) <- proj.geo

LST.zone.df = over(LST.sp,climate2.analysis)
LST.zone.df2 <- data.frame(LSTper5100.CY.df1,LST.zone.df[,4]) 
colnames(LST.zone.df2) <- c('deltaLST','x','y','zone')

LST.zone.df2$zone[LST.zone.df2$zone>=6]=NA 
LST.zone.df3<-na.omit(LST.zone.df2)

LSTper5100.CY.df.final=LST.zone.df3
q25.cy<-quantile(LSTper5100.CY.df.final[,1],probs=c(0.05,0.25,0.5,0.75,0.95))
q25.cy

LSTper5100.CY.df.final1<-LSTper5100.CY.df.final
LSTper5100.CY.df.final1$deltaLST[LSTper5100.CY.df.final1$deltaLST>0.6]=0.6
LSTper5100.CY.df.final1$deltaLST[LSTper5100.CY.df.final1$deltaLST< -0.6]=-0.6

### spatial plot
Fon<-'serif'#设置字体为新罗马
LSTp5100.CY.000<-base.maps+
  geom_tile(data=LSTper5100.CY.df.final1, aes(x=x, y=y, fill=deltaLST), alpha=1)+
  geom_path(data=zone2, aes(x = long, y = lat, group = group), color = "black", size = 0.6) +
  geom_polygon(data=zone1, aes(x = long, y = lat, group = group), color = "black", size = 0.6, fill = NA) +
  scale_y_continuous(breaks = seq(0, 60, 10),labels = c('0°N','10°N','20°N','30°N','40°N','50°N','60°N')) +
  scale_x_continuous(breaks= seq(70,140,10),labels = c('70°E','80°E','90°E','100°E','110°E','120°E','130°E','140°E')) +
  scale_fill_gradientn(colours=c("#313695","#74add1","white","#f46d43","#a50026"),#红蓝配色
                       breaks= c(-0.6,0,0.6),labels=c("-0.6"," 0"," 0.6"), 
                       limits = c(-0.63,0.63))+
  theme_minimal()+
  coord_cartesian(ylim=c(15,55))+ 
  theme(panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"))+ 
  theme(legend.position = "none")+
  theme(axis.line.x=element_line(linetype=1,color="grey30",size=0), 
        axis.ticks.x=element_line(color="black",size=0.5,lineend = 1), 
        axis.ticks.length.x=unit(0.15,'cm'), 
        axis.line.y=element_line(linetype=1,color="grey30",size=0),
        axis.ticks.y=element_line(color="black",size=0.5,lineend = 1),
        axis.ticks.length.y=unit(0.15,'cm'))+
  theme(axis.title = element_text(family=Fon,face='bold',size=13,color="black"),
        axis.text = element_text(family=Fon,size=12,color="black"))+ 
  theme(plot.title = element_text(family=Fon,face='bold',size=13,hjust = 0.5))+  
  theme(aspect.ratio = 1, #调节长宽比panel.grid = element_blank(),
        panel.border = element_rect(fill=NA,color="black",size=0.8,linetype="solid"))+
  theme(legend.title = element_blank(), 
        legend.text = element_text(family=Fon,size=10,color="black"), 
        legend.key.size =unit(15,"pt"),
        legend.key.height =unit(10,"pt"),
        legend.key.width =unit(10,"pt"),
        legend.position = c(0.15,0.18))+
  labs(x=NULL,y=NULL) +ggtitle(NULL)
LSTp5100.CY.000

LSTp5100.CY.001<-base.maps+
  geom_tile(data=LSTper5100.CY.df.final1, aes(x=x, y=y, fill=deltaLST), alpha=1)+
  geom_path(data=zone2, aes(x = long, y = lat, group = group), color = "black", size = 0.6)+
  geom_polygon(data=zone1, aes(x = long, y = lat, group = group), color = "black", size = 0.6, fill = NA) +
  scale_fill_gradientn(colours=c("#313695","#74add1","white","#f46d43","#a50026"),#红蓝配色
                       breaks= c(-0.6,0,0.6),limits = c(-0.63,0.63))+
  coord_cartesian(xlim=c(105,125),ylim=c(3,23))+  
  theme_minimal()+  
  theme(legend.position = "none")+  
  theme(aspect.ratio = 1,  
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA,color="gray60",linetype=1,size=0.8),
        plot.margin=unit(c(0,0,0,0),"mm"))
LSTp5100.CY.001

LST5100.CY.mean.map = ggdraw() +
  draw_plot(LSTp5100.CY.000) +
  draw_plot(LSTp5100.CY.001, x = 0.80, y = 0.095, width = 0.15, height = 0.15)
LST5100.CY.mean.map


#3、latitude distribution of both 2°*2°(figure2.c)-----------------------------------------------
fy.lat.lat.df<-read.csv(paste0(".\\Figure2.data\\fireyear\\FY_LST.lat.csv"),header = TRUE)
cy.lat.lat.df<-read.csv(paste0(".\\Figure2.data\\cutyear\\CY_LST.lat.csv"),header = TRUE)

all.lst.lat.df<-rbind(fy.lat.lat.df,cy.lat.lat.df)

Fon<-'serif'#设置字体为新罗马
lst.all_lat.map<-ggplot(data=all.lst.lat.df,aes(x=Total_mn,y=lat,group=type,fill=type))+
  scale_y_continuous(breaks = c(20,30,40,50, 60),
                     labels = c('20°N','30°N','40°N','50°N','60°N'),limit=c(15,55)) +
  scale_x_continuous(breaks= c(-0.5,0,0.5,1),labels = c('-0.5','0','0.5','1')) +
  geom_path(aes(color=type),alpha=1,size=0.8)+
  scale_color_manual( name=NULL,   values =c("royalblue3","#E64B35FF"))+
  geom_ribbon(aes(xmin=Total_mn-Total_sd,xmax=Total_mn+Total_sd,fill=type),
              alpha=0.3,show.legend = FALSE)+ 
  scale_fill_manual( name=NULL,   values =c("royalblue3","#E64B35FF"))+
  geom_vline(xintercept =0,linetype="dashed")+
  theme_minimal()+#背景灰色去除
  theme_bw()+
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
        panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"))+
  labs(x=NULL,y=NULL)+
  coord_cartesian(xlim=c(-0.5,1),ylim = c(15, NA))
lst.all_lat.map


#4、relationship of LST and loss percent-----------------------------------------------
LST.fy.df<-read.csv(paste0(".\\Figure2.data\\figure2.d\\FY_LST.csv"),header = TRUE)
LST.cy.df<-read.csv(paste0(".\\Figure2.data\\figure2.d\\CY_LST.csv"),header = TRUE)

#计算相关性
#fire
cor.test(LST.fy.df[,3], LST.fy.df[,2])
linear <- lm(LST.fy.df[,3] ~ LST.fy.df[,2])
summary(linear) 

#harvest
cor.test(LST.cy.df[,3], LST.cy.df[,2]) #relationships
linear <- lm(LST.cy.df[,3] ~ LST.cy.df[,2])
summary(linear) 

#all
LST.all.df<-rbind(LST.fy.df,LST.cy.df)

Fon<-'serif'#设置字体为新罗马
LST.all.p0<-ggplot(LST.all.df, aes(x = per, y = lst,group=type,colour=type)) + 
  geom_point(alpha=0.8,size=2.0)+
  scale_x_continuous(limits = c(0,105),breaks = c(seq(0,100,20)),expand = c(0,0))+
  scale_y_continuous(limits = c(-0.5,2.5),breaks = c(0,1,2),expand = c(0,0))+
  scale_colour_manual( name=NULL,   values =c("#E64B35FF","royalblue3"),guide=guide_legend(reverse=T))+
  stat_smooth(formula=y~x,fill="lightgrey",alpha=0.7,size=1.5,method="lm",se=TRUE,level=0.95)+
  theme_bw()+
  annotate('text',x=35,y=1.75,label=" R  = 0.89    p<0.001 ",color="red",size=5,family="serif")+
  annotate('text',x=35,y=1.25,label="R  = 0.78    p<0.001",color="royalblue3",size=5,family="serif")+
  annotate('text',x=14,y=1.35,label="2",color="royalblue3",size=3,family="serif")+
  annotate('text',x=14,y=1.85,label="2",color="red",size=3,family="serif")+
  annotate('text',x=90,y=1.5,label=" All ",color="black",size=5,family="serif")+
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
        aspect.ratio = 1,
        panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"))+
  labs(x=NULL,y=NULL)
LST.all.p0

#5、spatial plot of ET/Albedo/NDVI in fire year (FigureS3) -----------------------------------------------
#(1)、ET spatial distribution----------------
ETper5100.FY.df1<-ETper5100.FY.df

#only select climate zone I-V
ET.sp = ETper5100.FY.df1[,c(2:3)]
coordinates(ET.sp) <- ~x + y
projection(ET.sp) <- proj.geo

ET.zone.df = over(ET.sp,climate2.analysis)
ET.zone.df2 <- data.frame(ETper5100.FY.df1,ET.zone.df[,4]) 
colnames(ET.zone.df2) <- c('deltaET','x','y','zone')

ET.zone.df2$zone[ET.zone.df2$zone>=6]=NA 
ET.zone.df3<-na.omit(ET.zone.df2)

ETper5100.FY.df.final=ET.zone.df3

ETp5100.FY.000<-base.maps+
  geom_tile(data=ETper5100.FY.df.final, aes(x=x, y=y, fill=deltaET), alpha=1)+
  geom_path(data=zone2, aes(x = long, y = lat, group = group), color = "black", size = 0.6) +
  geom_polygon(data=zone1, aes(x = long, y = lat, group = group), color = "black", size = 0.6, fill = NA) +
  scale_y_continuous(breaks = seq(0, 60, 10),labels = c('0°N','10°N','20°N','30°N','40°N','50°N','60°N')) +
  scale_x_continuous(breaks= seq(70,140,10),labels = c('70°E','80°E','90°E','100°E','110°E','120°E','130°E','140°E')) +
  scale_fill_gradientn( colours=c("#1a1a1a","#543005","#7f3b08","#b35806","#e08214",#黄蓝色
              '#f5f5f5',
              "#4292c6","#2171b5","#08519c","#08306b","#2d004b"),
    breaks= c(-0.5,0,0.5),labels=c("-0.5","  0"," 0.5"), 
    limits = c(-0.53,0.53))+
  theme_minimal()+
  coord_cartesian(ylim=c(15,55))+
  theme(panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"))+ 
  theme(legend.position = "none")+
  theme(axis.line.x=element_line(linetype=1,color="grey30",size=0), 
        axis.ticks.x=element_line(color="black",size=0.5,lineend = 1), 
        axis.ticks.length.x=unit(0.15,'cm'),
        axis.line.y=element_line(linetype=1,color="grey30",size=0),
        axis.ticks.y=element_line(color="black",size=0.5,lineend = 1),
        axis.ticks.length.y=unit(0.15,'cm'))+
  theme(axis.title = element_text(family=Fon, 
                                  face='bold',size=13,color="black"),
        axis.text = element_text(family=Fon, 
                                 size=12,color="black"))+
  theme(plot.title = element_text(family=Fon,face='bold',size=13,hjust = 0.5))+ 
  theme(aspect.ratio = 1, #调节长宽比
        panel.grid = element_blank(),
        panel.border = element_rect(fill=NA,color="black",size=0.8,linetype="solid"))+
  theme(legend.title = element_blank(), 
        legend.text = element_text(family=Fon,size=10,color="black"),
        legend.key.size =unit(15,"pt"), 
        legend.key.height =unit(10,"pt"),
        legend.key.width =unit(10,"pt"),
        legend.position = c(0.15,0.18))+
  labs(x=NULL,y=NULL) + ggtitle(NULL)
ETp5100.FY.000

ETp5100.FY.001<-base.maps+
  geom_tile(data=ETper5100.FY.df.final, aes(x=x, y=y, fill=deltaET), alpha=1)+
  geom_path(data=zone2, aes(x = long, y = lat, group = group), color = "black", size = 0.6)+
  geom_polygon(data=zone1, aes(x = long, y = lat, group = group), color = "black", size = 0.6, fill = NA) +
  scale_fill_gradientn( colours=c("#1a1a1a","#543005","#7f3b08","#b35806","#e08214",#黄蓝色
              '#f5f5f5',
              "#4292c6","#2171b5","#08519c","#08306b","#2d004b"),
    breaks= c(-0.5,0,0.5),limits = c(-0.53,0.53))+
  coord_cartesian(xlim=c(105,125),ylim=c(3,23))+ 
  theme_minimal()+  #
  theme(legend.position = "none")+  #
  theme(aspect.ratio = 1,  #
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA,color="gray60",linetype=1,size=0.8),
        plot.margin=unit(c(0,0,0,0),"mm"))
ETp5100.FY.001

fy.ET_5100 = ggdraw() +
  draw_plot(ETp5100.FY.000) +
  draw_plot(ETp5100.FY.001, x = 0.80, y = 0.095, width = 0.15, height = 0.15)
fy.ET_5100


#(2)、Albedo spatial distribution----------------------------------------
Albedoper5100.FY.df1<-Albedoper5100.FY.df

#only select climate zone I-V
Albedo.sp = Albedoper5100.FY.df1[,c(2:3)]
coordinates(Albedo.sp) <- ~x + y
projection(Albedo.sp) <- proj.geo

Albedo.zone.df = over(Albedo.sp,climate2.analysis)
Albedo.zone.df2 <- data.frame(Albedoper5100.FY.df1,Albedo.zone.df[,4]) 
colnames(Albedo.zone.df2) <- c('deltaAlbedo','x','y','zone')

Albedo.zone.df2$zone[Albedo.zone.df2$zone>=6]=NA 
Albedo.zone.df3<-na.omit(Albedo.zone.df2)

Albedoper5100.FY.df.final=Albedo.zone.df3

Albedoper5100.FY.df.final1<-Albedoper5100.FY.df.final
Albedoper5100.FY.df.final1$deltaAlbedo[Albedoper5100.FY.df.final1$deltaAlbedo> 0.04]=0.04
Albedoper5100.FY.df.final1$deltaAlbedo[Albedoper5100.FY.df.final1$deltaAlbedo< -0.04]=-0.04

Albedop5100.FY.000<-base.maps+
  geom_tile(data=Albedoper5100.FY.df.final1, aes(x=x, y=y, fill=deltaAlbedo), alpha=1)+
  geom_path(data=zone2, aes(x = long, y = lat, group = group), color = "black", size = 0.6) +
  geom_polygon(data=zone1, aes(x = long, y = lat, group = group), color = "black", size = 0.6, fill = NA) +
  scale_y_continuous(breaks = seq(0, 60, 10),labels = c('0°N','10°N','20°N','30°N','40°N','50°N','60°N')) +
  scale_x_continuous(breaks= seq(70,140,10),labels = c('70°E','80°E','90°E','100°E','110°E','120°E','130°E','140°E')) +
  scale_fill_gradientn(
    colours=c("#1a1a1a","#543005","#7f3b08","#b35806","#e08214",#黄蓝色
              '#f5f5f5',
              "#4292c6","#2171b5","#08519c","#08306b","#2d004b"),
    breaks= c(-0.04,0,0.04),labels=c("-0.04","   0"," 0.04"), 
    limits = c(-0.043,0.043))+
  theme_minimal()+
  coord_cartesian(ylim=c(15,55))+ #
  theme(panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"))+ #
  theme(legend.position = "none")+
  theme(axis.line.x=element_line(linetype=1,color="grey30",size=0), #
        axis.ticks.x=element_line(color="black",size=0.5,lineend = 1), #
        axis.ticks.length.x=unit(0.15,'cm'), #
        axis.line.y=element_line(linetype=1,color="grey30",size=0),
        axis.ticks.y=element_line(color="black",size=0.5,lineend = 1),
        axis.ticks.length.y=unit(0.15,'cm'))+
  theme(axis.title = element_text(family=Fon, face='bold',size=13,color="black"),
        axis.text = element_text(family=Fon, size=11,color="black"))+
  theme(plot.title = element_text(family=Fon,face='bold',size=13,hjust = 0.5))+  
  theme(aspect.ratio = 1, 
        panel.grid = element_blank(),
        panel.border = element_rect(fill=NA,color="black",size=0.8,linetype="solid"))+
  theme(legend.title = element_blank(), 
        legend.text = element_text(family=Fon,size=10,color="black"), 
        legend.key.size =unit(15,"pt"), 
        legend.key.height =unit(10,"pt"),
        legend.key.width =unit(10,"pt"),
        legend.position = c(0.15,0.18))+
  labs(x=NULL,y=NULL) + ggtitle(NULL)
Albedop5100.FY.000

Albedop5100.FY.001<-base.maps+
  geom_tile(data=Albedoper5100.FY.df.final1, aes(x=x, y=y, fill=deltaAlbedo), alpha=1)+
  geom_path(data=zone2, aes(x = long, y = lat, group = group), color = "black", size = 0.6)+
  geom_polygon(data=zone1, aes(x = long, y = lat, group = group), color = "black", size = 0.6, fill = NA) +
  scale_fill_gradientn( colours=c("#1a1a1a","#543005","#7f3b08","#b35806","#e08214",#黄蓝色
              '#f5f5f5',
              "#4292c6","#2171b5","#08519c","#08306b","#2d004b"),
    breaks= c(-0.04,0,0.04),limits = c(-0.043,0.043))+
  coord_cartesian(xlim=c(105,125),ylim=c(3,23))+
  theme_minimal()+  
  theme(legend.position = "none")+  
  theme(aspect.ratio = 1, 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA,color="gray60",linetype=1,size=0.8),
        plot.margin=unit(c(0,0,0,0),"mm"))
Albedop5100.FY.001

fy.Albedo_5100 = ggdraw() +
  draw_plot(Albedop5100.FY.000) +
  draw_plot(Albedop5100.FY.001, x = 0.80, y = 0.095, width = 0.15, height = 0.15)
fy.Albedo_5100


#(3)、NDVI spatial distribution----------------------------------------
NDVIper5100.FY.df1<-NDVIper5100.FY.df

#only select climate zone I-V
NDVI.sp = NDVIper5100.FY.df1[,c(2:3)]
coordinates(NDVI.sp) <- ~x + y
projection(NDVI.sp) <- proj.geo

NDVI.zone.df = over(NDVI.sp,climate2.analysis)
NDVI.zone.df2 <- data.frame(NDVIper5100.FY.df1,NDVI.zone.df[,4]) 
colnames(NDVI.zone.df2) <- c('deltaNDVI','x','y','zone')

NDVI.zone.df2$zone[NDVI.zone.df2$zone>=6]=NA 
NDVI.zone.df3<-na.omit(NDVI.zone.df2)

NDVIper5100.FY.df.final=NDVI.zone.df3

NDVIp5100.FY.000<-base.maps+
  geom_tile(data=NDVIper5100.FY.df.final, aes(x=x, y=y, fill=deltaNDVI), alpha=1)+
  geom_path(data=zone2, aes(x = long, y = lat, group = group), color = "black", size = 0.6) +
  geom_polygon(data=zone1, aes(x = long, y = lat, group = group), color = "black", size = 0.6, fill = NA) +
  scale_y_continuous(breaks = seq(0, 60, 10),labels = c('0°N','10°N','20°N','30°N','40°N','50°N','60°N')) +
  scale_x_continuous(breaks= seq(70,140,10),labels = c('70°E','80°E','90°E','100°E','110°E','120°E','130°E','140°E')) +
  scale_fill_gradientn( colours=c("#1a1a1a","#543005","#7f3b08","#b35806","#e08214",#黄蓝色
              '#f5f5f5',
              "#4292c6","#2171b5","#08519c","#08306b","#2d004b"),
    breaks= c(-0.30,0,0.30),labels=c("-0.3","  0"," 0.3"), 
    limits = c(-0.33,0.33))+
  theme_minimal()+
  coord_cartesian(ylim=c(15,55))+ 
  theme(panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"))+ 
  theme(legend.position = "none")+
  theme(axis.line.x=element_line(linetype=1,color="grey30",size=0), 
        axis.ticks.x=element_line(color="black",size=0.5,lineend = 1), 
        axis.ticks.length.x=unit(0.15,'cm'), 
        axis.line.y=element_line(linetype=1,color="grey30",size=0),
        axis.ticks.y=element_line(color="black",size=0.5,lineend = 1),
        axis.ticks.length.y=unit(0.15,'cm'))+
  theme(axis.title = element_text(family=Fon,face='bold',size=13,color="black"),
        axis.text = element_text(family=Fon, size=12,color="black"))+
  theme(plot.title = element_text(family=Fon,face='bold',size=13,hjust = 0.5))+ 
  theme(aspect.ratio = 1, #
        panel.grid = element_blank(),
        panel.border = element_rect(fill=NA,color="black",size=0.8,linetype="solid"))+
  theme(legend.title = element_blank(), 
        legend.text = element_text(family=Fon,size=10,color="black"), 
        legend.key.size =unit(15,"pt"),
        legend.key.height =unit(10,"pt"),
        legend.key.width =unit(10,"pt"),
        legend.position = c(0.15,0.18))+
  labs(x=NULL,y=NULL) + ggtitle(NULL)
NDVIp5100.FY.000

NDVIp5100.FY.001<-base.maps+
  geom_tile(data=NDVIper5100.FY.df.final, aes(x=x, y=y, fill=deltaNDVI), alpha=1)+
  geom_path(data=zone2, aes(x = long, y = lat, group = group), color = "black", size = 0.6)+
  geom_polygon(data=zone1, aes(x = long, y = lat, group = group), color = "black", size = 0.6, fill = NA) +
  scale_fill_gradientn( colours=c("#1a1a1a","#543005","#7f3b08","#b35806","#e08214",#黄蓝色
              '#f5f5f5',
              "#4292c6","#2171b5","#08519c","#08306b","#2d004b"),
    breaks= c(-0.3,0,0.3),limits = c(-0.33,0.33))+
  coord_cartesian(xlim=c(105,125),ylim=c(3,23))+  
  theme_minimal()+ 
  theme(legend.position = "none")+  
  theme(aspect.ratio = 1,  
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA,color="gray60",linetype=1,size=0.8),
        plot.margin=unit(c(0,0,0,0),"mm"))
NDVIp5100.FY.001

fy.NDVI_5100 = ggdraw() +
  draw_plot(NDVIp5100.FY.000) +
  draw_plot(NDVIp5100.FY.001, x = 0.80, y = 0.095, width = 0.15, height = 0.15)
fy.NDVI_5100


#6、spatial plot of ET/Albedo/NDVI in harvest year  ------------------------------------------
#(1)、ET spatial distribution----------------------------------------
ETper5100.CY.df1<-ETper5100.CY.df

#only select climate zone I-V
ET.sp = ETper5100.CY.df1[,c(2:3)]
coordinates(ET.sp) <- ~x + y
projection(ET.sp) <- proj.geo

ET.zone.df = over(ET.sp,climate2.analysis)
ET.zone.df2 <- data.frame(ETper5100.CY.df1,ET.zone.df[,4]) 
colnames(ET.zone.df2) <- c('deltaET','x','y','zone')

ET.zone.df2$zone[ET.zone.df2$zone>=6]=NA 
ET.zone.df3<-na.omit(ET.zone.df2)

ETper5100.CY.df.final=ET.zone.df3

ETp5100.CY.000<-base.maps+
  geom_tile(data=ETper5100.CY.df.final, aes(x=x, y=y, fill=deltaET), alpha=1)+
  geom_path(data=zone2, aes(x = long, y = lat, group = group), color = "black", size = 0.6) +
  geom_polygon(data=zone1, aes(x = long, y = lat, group = group), color = "black", size = 0.6, fill = NA) +
  scale_y_continuous(breaks = seq(0, 60, 10),labels = c('0°N','10°N','20°N','30°N','40°N','50°N','60°N')) +
  scale_x_continuous(breaks= seq(70,140,10),labels = c('70°E','80°E','90°E','100°E','110°E','120°E','130°E','140°E')) +
  scale_fill_gradientn(
    colours=c("#1a1a1a","#543005","#7f3b08","#b35806","#e08214",#黄蓝色
              '#f5f5f5',
              "#4292c6","#2171b5","#08519c","#08306b","#2d004b"),
    breaks= c(-0.5,0,0.5),labels=c("-0.5","  0"," 0.5"), 
    limits = c(-0.53,0.53))+
  theme_minimal()+
  coord_cartesian(ylim=c(15,55))+  
  theme(panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"))+ 
  theme(legend.position = "none")+
  theme(axis.line.x=element_line(linetype=1,color="grey30",size=0), 
        axis.ticks.x=element_line(color="black",size=0.5,lineend = 1), 
        axis.ticks.length.x=unit(0.15,'cm'), 
        axis.line.y=element_line(linetype=1,color="grey30",size=0),
        axis.ticks.y=element_line(color="black",size=0.5,lineend = 1),
        axis.ticks.length.y=unit(0.15,'cm'))+
  theme(axis.title = element_text(family=Fon, face='bold',size=13,color="black"),
        axis.text = element_text(family=Fon, size=12,color="black"))+
  theme(plot.title = element_text(family=Fon,face='bold',size=13,hjust = 0.5))+ 
  theme(aspect.ratio = 1, 
        panel.grid = element_blank(),
        panel.border = element_rect(fill=NA,color="black",size=0.8,linetype="solid"))+
  theme(legend.title = element_blank(), 
        legend.text = element_text(family=Fon,size=10,color="black"),
        legend.key.size =unit(15,"pt"), 
        legend.key.height =unit(10,"pt"),
        legend.key.width =unit(10,"pt"),
        legend.position = c(0.15,0.18))+
  labs(x=NULL,y=NULL) +ggtitle(NULL)
ETp5100.CY.000

ETp5100.CY.001<-base.maps+
  geom_tile(data=ETper5100.CY.df.final, aes(x=x, y=y, fill=deltaET), alpha=1)+
  geom_path(data=zone2, aes(x = long, y = lat, group = group), color = "black", size = 0.6)+
  geom_polygon(data=zone1, aes(x = long, y = lat, group = group), color = "black", size = 0.6, fill = NA) +
  scale_fill_gradientn( colours=c("#1a1a1a","#543005","#7f3b08","#b35806","#e08214",#黄蓝色
              '#f5f5f5',
              "#4292c6","#2171b5","#08519c","#08306b","#2d004b"),
    breaks= c(-0.5,0,0.5),limits = c(-0.53,0.53))+
  coord_cartesian(xlim=c(105,125),ylim=c(3,23))+  
  theme_minimal()+ 
  theme(legend.position = "none")+  
  theme(aspect.ratio = 1,  
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA,color="gray60",linetype=1,size=0.8),
        plot.margin=unit(c(0,0,0,0),"mm"))
ETp5100.CY.001

ET5100.CY.mean.map = ggdraw() +
  draw_plot(ETp5100.CY.000) +
  draw_plot(ETp5100.CY.001, x = 0.80, y = 0.095, width = 0.15, height = 0.15)
ET5100.CY.mean.map


#(2)、Albedo spatial distribution----------------------------------------
Albedoper5100.CY.df1<-Albedoper5100.CY.df

#only select climate zone I-V
Albedo.sp = Albedoper5100.CY.df1[,c(2:3)]
coordinates(Albedo.sp) <- ~x + y
projection(Albedo.sp) <- proj.geo

Albedo.zone.df = over(Albedo.sp,climate2.analysis)
Albedo.zone.df2 <- data.frame(Albedoper5100.CY.df1,Albedo.zone.df[,4]) 
colnames(Albedo.zone.df2) <- c('deltaAlbedo','x','y','zone')

Albedo.zone.df2$zone[Albedo.zone.df2$zone>=6]=NA 
Albedo.zone.df3<-na.omit(Albedo.zone.df2)

Albedoper5100.CY.df.final=Albedo.zone.df3

Albedoper5100.CY.df.final1<-Albedoper5100.CY.df.final
Albedoper5100.CY.df.final1$deltaAlbedo[Albedoper5100.CY.df.final1$deltaAlbedo> 0.04]=0.04
Albedoper5100.CY.df.final1$deltaAlbedo[Albedoper5100.CY.df.final1$deltaAlbedo< -0.04]=-0.04

Albedop5100.CY.000<-base.maps+
  geom_tile(data=Albedoper5100.CY.df.final1, aes(x=x, y=y, fill=deltaAlbedo), alpha=1)+
  geom_path(data=zone2, aes(x = long, y = lat, group = group), color = "black", size = 0.6) +
  geom_polygon(data=zone1, aes(x = long, y = lat, group = group), color = "black", size = 0.6, fill = NA) +
  scale_y_continuous(breaks = seq(0, 60, 10),labels = c('0°N','10°N','20°N','30°N','40°N','50°N','60°N')) +
  scale_x_continuous(breaks= seq(70,140,10),labels = c('70°E','80°E','90°E','100°E','110°E','120°E','130°E','140°E')) +
  scale_fill_gradientn( colours=c("#1a1a1a","#543005","#7f3b08","#b35806","#e08214",#黄蓝色
              '#f5f5f5',
              "#4292c6","#2171b5","#08519c","#08306b","#2d004b"),
    breaks= c(-0.04,0,0.04),labels=c("-0.05","   0"," 0.05"), 
    limits = c(-0.043,0.043))+
  theme_minimal()+
  coord_cartesian(ylim=c(15,55))+  
  theme(panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"))+
  theme(legend.position = "none")+
  theme(axis.line.x=element_line(linetype=1,color="grey30",size=0),
        axis.ticks.x=element_line(color="black",size=0.5,lineend = 1), 
        axis.ticks.length.x=unit(0.15,'cm'), 
        axis.line.y=element_line(linetype=1,color="grey30",size=0),
        axis.ticks.y=element_line(color="black",size=0.5,lineend = 1),
        axis.ticks.length.y=unit(0.15,'cm'))+
  theme(axis.title = element_text(family=Fon, face='bold',size=13,color="black"),
        axis.text = element_text(family=Fon, size=12,color="black"))+
  theme(plot.title = element_text(family=Fon,face='bold',size=13,hjust = 0.5))+  
  theme(aspect.ratio = 1, 
        panel.grid = element_blank(),
        panel.border = element_rect(fill=NA,color="black",size=0.8,linetype="solid"))+
  theme(legend.title = element_blank(), 
        legend.text = element_text(family=Fon,size=10,color="black"),
        legend.key.size =unit(15,"pt"),
        legend.key.height =unit(10,"pt"),
        legend.key.width =unit(10,"pt"),
        legend.position = c(0.15,0.18))+
  labs(x=NULL,y=NULL) +ggtitle(NULL)
Albedop5100.CY.000

Albedop5100.CY.001<-base.maps+
  geom_tile(data=Albedoper5100.CY.df.final1, aes(x=x, y=y, fill=deltaAlbedo), alpha=1)+
  geom_path(data=zone2, aes(x = long, y = lat, group = group), color = "black", size = 0.6)+
  geom_polygon(data=zone1, aes(x = long, y = lat, group = group), color = "black", size = 0.6, fill = NA) +
  scale_fill_gradientn(  colours=c("#1a1a1a","#543005","#7f3b08","#b35806","#e08214",#黄蓝色
              '#f5f5f5',
              "#4292c6","#2171b5","#08519c","#08306b","#2d004b"),
    breaks= c(-0.04,0,0.04),limits = c(-0.043,0.043))+
  coord_cartesian(xlim=c(105,125),ylim=c(3,23))+  
  theme_minimal()+ 
  theme(legend.position = "none")+  
  theme(aspect.ratio = 1,  
        axis.text = element_blank(),axis.ticks = element_blank(),
        axis.title = element_blank(),panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA,color="gray60",linetype=1,size=0.8),
        plot.margin=unit(c(0,0,0,0),"mm"))
Albedop5100.CY.001

cy.Albedo_5100 = ggdraw() +
  draw_plot(Albedop5100.CY.000) +
  draw_plot(Albedop5100.CY.001, x = 0.80, y = 0.095, width = 0.15, height = 0.15)
cy.Albedo_5100

#(3)、NDVI spatial distribution ----------------------------------------
NDVIper5100.CY.df1<-NDVIper5100.CY.df

#only select climate zone I-V
NDVI.sp = NDVIper5100.CY.df1[,c(2:3)]
coordinates(NDVI.sp) <- ~x + y
projection(NDVI.sp) <- proj.geo

NDVI.zone.df = over(NDVI.sp,climate2.analysis)
NDVI.zone.df2 <- data.frame(NDVIper5100.CY.df1,NDVI.zone.df[,4]) 
colnames(NDVI.zone.df2) <- c('deltaNDVI','x','y','zone')

NDVI.zone.df2$zone[NDVI.zone.df2$zone>=6]=NA 
NDVI.zone.df3<-na.omit(NDVI.zone.df2)

NDVIper5100.CY.df.final=NDVI.zone.df3

NDVIp5100.CY.000<-base.maps+
  geom_tile(data=NDVIper5100.CY.df.final, aes(x=x, y=y, fill=deltaNDVI), alpha=1)+
  geom_path(data=zone2, aes(x = long, y = lat, group = group), color = "black", size = 0.6) +
  geom_polygon(data=zone1, aes(x = long, y = lat, group = group), color = "black", size = 0.6, fill = NA) +
  scale_y_continuous(breaks = seq(0, 60, 10),labels = c('0°N','10°N','20°N','30°N','40°N','50°N','60°N')) +
  scale_x_continuous(breaks= seq(70,140,10),labels = c('70°E','80°E','90°E','100°E','110°E','120°E','130°E','140°E')) +
  scale_fill_gradientn(
    colours=c("#1a1a1a","#543005","#7f3b08","#b35806","#e08214",#黄蓝色
              '#f5f5f5',
              "#4292c6","#2171b5","#08519c","#08306b","#2d004b"),
    breaks= c(-0.30,0,0.30),labels=c("-0.3","  0"," 0.3"), 
    limits = c(-0.33,0.33))+
  theme_minimal()+
  coord_cartesian(ylim=c(15,55))+  
  theme(panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"))+ 
  theme(legend.position = "none")+
  theme(axis.line.x=element_line(linetype=1,color="grey30",size=0),
        axis.ticks.x=element_line(color="black",size=0.5,lineend = 1), 
        axis.ticks.length.x=unit(0.15,'cm'),
        axis.line.y=element_line(linetype=1,color="grey30",size=0),
        axis.ticks.y=element_line(color="black",size=0.5,lineend = 1),
        axis.ticks.length.y=unit(0.15,'cm'))+
  theme(axis.title = element_text(family=Fon, face='bold',size=13,color="black"),
        axis.text = element_text(family=Fon,  size=12,color="black"))+
  theme(plot.title = element_text(family=Fon,face='bold',size=13,hjust = 0.5))+  
  theme(aspect.ratio = 1, 
        panel.grid = element_blank(),
        panel.border = element_rect(fill=NA,color="black",size=0.8,linetype="solid"))+
  theme(legend.title = element_blank(), 
        legend.text = element_text(family=Fon,size=10,color="black"), 
        legend.key.size =unit(15,"pt"), 
        legend.key.height =unit(10,"pt"),
        legend.key.width =unit(10,"pt"),
        legend.position = c(0.15,0.18))+
  labs(x=NULL,y=NULL) +ggtitle(NULL)
NDVIp5100.CY.000

NDVIp5100.CY.001<-base.maps+
  geom_tile(data=NDVIper5100.CY.df.final, aes(x=x, y=y, fill=deltaNDVI), alpha=1)+
  geom_path(data=zone2, aes(x = long, y = lat, group = group), color = "black", size = 0.6)+
  geom_polygon(data=zone1, aes(x = long, y = lat, group = group), color = "black", size = 0.6, fill = NA) +
  scale_fill_gradientn( colours=c("#1a1a1a","#543005","#7f3b08","#b35806","#e08214",#黄蓝色
              '#f5f5f5',
              "#4292c6","#2171b5","#08519c","#08306b","#2d004b"),
    breaks= c(-0.3,0,0.3),limits = c(-0.33,0.33))+
  coord_cartesian(xlim=c(105,125),ylim=c(3,23))+  
  theme_minimal()+  
  theme(legend.position = "none")+  
  theme(aspect.ratio = 1, 
        axis.text = element_blank(),axis.ticks = element_blank(),
        axis.title = element_blank(),panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA,color="gray60",linetype=1,size=0.8),
        plot.margin=unit(c(0,0,0,0),"mm"))
NDVIp5100.CY.001

cy.NDVI_5100 = ggdraw() +
  draw_plot(NDVIp5100.CY.000) +
  draw_plot(NDVIp5100.CY.001, x = 0.80, y = 0.095, width = 0.15, height = 0.15)
cy.NDVI_5100

