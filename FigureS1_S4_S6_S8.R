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

#Figure S1/S4/S6/S8-----------------------------

#Spatial distribution of LST/ET/Albedo/NDVI under different percent in fire year

# Note:The code we provided is based on examples where the proportion is greater than 50%. 
# The method for plotting different proportions can refer to this and will not be elaborated on here."


#一、take per50 as an example-----------------------------------------------------------------
#1、read data and mask each other-----------------------------------------------------
raster2spdf <- function(r){
  df <- as.data.frame(as(r, "SpatialPixelsDataFrame"))
  colnames(df) <- c("value", "x", "y")
  df
}

LSTper50.FY<-raster(".\\FigureS1_S4_S6_S8.data\\Tm_FYp50.tif")
ETper50.FY<-raster(".\\FigureS1_S4_S6_S8.data\\ET_FYp50.tif")
Albedoper50.FY<-raster(".\\FigureS1_S4_S6_S8.data\\Alb_FYp50.tif")
NDVIper50.FY<-raster(".\\FigureS1_S4_S6_S8.data\\NDVI_FYp50.tif")

LSTper50.FY1<-mask(LSTper50.FY,ETper50.FY)
LSTper50.FY2<-mask(LSTper50.FY1,Albedoper50.FY)
LSTper50.FY3<-mask(LSTper50.FY2,NDVIper50.FY)

ETper50.FY3<-mask(ETper50.FY,LSTper50.FY3)
Albedoper50.FY3<-mask(Albedoper50.FY,LSTper50.FY3)
NDVIper50.FY3<-mask(NDVIper50.FY,LSTper50.FY3)

LSTper50.FY.df<-raster2spdf(LSTper50.FY3)
colnames(LSTper50.FY.df)<-c("deltaLST","x", "y")

ETper50.FY.df<-raster2spdf(ETper50.FY3)
colnames(ETper50.FY.df)<-c("deltaET","x", "y")

Albedoper50.FY.df<-raster2spdf(Albedoper50.FY3)
colnames(Albedoper50.FY.df)<-c("deltaAlbedo","x", "y")

NDVIper50.FY.df<-raster2spdf(NDVIper50.FY3)
colnames(NDVIper50.FY.df)<-c("deltaNDVI","x", "y")

#prepare boundaries
proj.geo = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0 "
climate2.analysis<-readOGR("E:\\PhD_qiushuang\\R_data\\GEE_Export_China3\\climate_zone\\8zone_84.shp",
                           use_iconv = TRUE,encoding = "UTF-8")
zone1<-fortify(climate2)
China.boudary<-readOGR("E:\\PhD_qiushuang\\Boundary\\gridChina2022\\焦\\中国\\领土.shp",use_iconv = TRUE,encoding = "UTF-8")
zone2<-fortify(China.boudary)

### plot
base.maps<-ggplot()+geom_path(data=zone2, aes(x = long, y = lat, group = group), color = "black", size = 0.1)
base.maps

#2、plot LST per50------------------------------------------
LSTper50.FY.df1<-LSTper50.FY.df

#only select climate zone I-V
LST.sp = LSTper50.FY.df1[,c(2:3)]
coordinates(LST.sp) <- ~x + y
projection(LST.sp) <- proj.geo

LST.zone.df = over(LST.sp,climate2.analysis)
LST.zone.df2 <- data.frame(LSTper50.FY.df1,LST.zone.df[,4]) 
colnames(LST.zone.df2) <- c('deltaLST','x','y','zone')

LST.zone.df2$zone[LST.zone.df2$zone>=6]=NA 
LST.zone.df3<-na.omit(LST.zone.df2)

LSTper50.FY.df1.final=LST.zone.df3

LSTper50.FY.df1.final1<-LSTper50.FY.df1.final
LSTper50.FY.df1.final1$deltaLST[LSTper50.FY.df1.final1$deltaLST>0.6]=0.6
LSTper50.FY.df1.final1$deltaLST[LSTper50.FY.df1.final1$deltaLST< -0.6]=-0.6

Fon<-'serif'#设置字体为新罗马
LSTp50.FY.000<-base.maps+
  geom_tile(data=LSTper50.FY.df1.final1, aes(x=x, y=y, fill=deltaLST), alpha=1)+
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
LSTp50.FY.000

LSTp50.FY.001<-base.maps+
  geom_tile(data=LSTper50.FY.df1.final1, aes(x=x, y=y, fill=deltaLST), alpha=1)+
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
LSTp50.FY.001

LST.mean.map = ggdraw() +
  draw_plot(LSTp50.FY.000) +
  draw_plot(LSTp50.FY.001, x = 0.80, y = 0.095, width = 0.15, height = 0.15)
LST.mean.map


#3、plot ET per50------------------------------------------
ETper50.FY.df1<-ETper50.FY.df

#only select climate zone I-V
ET.sp = ETper50.FY.df1[,c(2:3)]
coordinates(ET.sp) <- ~x + y
projection(ET.sp) <- proj.geo

ET.zone.df = over(ET.sp,climate2.analysis)
ET.zone.df2 <- data.frame(ETper50.FY.df1,ET.zone.df[,4]) 
colnames(ET.zone.df2) <- c('deltaET','x','y','zone')

ET.zone.df2$zone[ET.zone.df2$zone>=6]=NA 
ET.zone.df3<-na.omit(ET.zone.df2)

ETper50.FY.df.final=ET.zone.df3

ETp50.FY.000<-base.maps+
  geom_tile(data=ETper50.FY.df.final, aes(x=x, y=y, fill=deltaET), alpha=1)+
  geom_path(data=zone2, aes(x = long, y = lat, group = group), color = "black", size = 0.6) +
  geom_polygon(data=zone1, aes(x = long, y = lat, group = group), color = "black", size = 0.6, fill = NA) +
  scale_y_continuous(breaks = seq(0, 60, 10),labels = c('0°N','10°N','20°N','30°N','40°N','50°N','60°N')) +
  scale_x_continuous(breaks= seq(70,140,10),labels = c('70°E','80°E','90°E','100°E','110°E','120°E','130°E','140°E')) +
  scale_fill_gradientn(colours=c("#1a1a1a","#543005","#7f3b08","#b35806","#e08214",#黄蓝色
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
ETp50.FY.000

ETp50.FY.001<-base.maps+
  geom_tile(data=ETper50.FY.df.final, aes(x=x, y=y, fill=deltaET), alpha=1)+
  geom_path(data=zone2, aes(x = long, y = lat, group = group), color = "black", size = 0.6)+
  geom_polygon(data=zone1, aes(x = long, y = lat, group = group), color = "black", size = 0.6, fill = NA) +
  scale_fill_gradientn(colours=c("#1a1a1a","#543005","#7f3b08","#b35806","#e08214",#黄蓝色
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
ETp50.FY.001

fy.ET_50 = ggdraw() +
  draw_plot(ETp50.FY.000) +
  draw_plot(ETp50.FY.001, x = 0.80, y = 0.095, width = 0.15, height = 0.15)
fy.ET_50


#4、plot Albedo per50------------------------------------------
Albedoper50.FY.df1<-Albedoper50.FY.df

#only select climate zone I-V
Albedo.sp = Albedoper50.FY.df1[,c(2:3)]
coordinates(Albedo.sp) <- ~x + y
projection(Albedo.sp) <- proj.geo

Albedo.zone.df = over(Albedo.sp,climate2.analysis)
Albedo.zone.df2 <- data.frame(Albedoper50.FY.df1,Albedo.zone.df[,4]) 
colnames(Albedo.zone.df2) <- c('deltaAlbedo','x','y','zone')

Albedo.zone.df2$zone[Albedo.zone.df2$zone>=6]=NA 
Albedo.zone.df3<-na.omit(Albedo.zone.df2)

Albedoper50.FY.df.final=Albedo.zone.df3

Albedoper50.FY.df.final1<-Albedoper50.FY.df.final
Albedoper50.FY.df.final1$deltaAlbedo[Albedoper50.FY.df.final1$deltaAlbedo>0.05]=0.05
Albedoper50.FY.df.final1$deltaAlbedo[Albedoper50.FY.df.final1$deltaAlbedo< -0.05]=-0.05

Albedop50.FY.000<-base.maps+
  geom_tile(data=Albedoper50.FY.df.final1, aes(x=x, y=y, fill=deltaAlbedo), alpha=1)+
  geom_path(data=zone2, aes(x = long, y = lat, group = group), color = "black", size = 0.6) +
  geom_polygon(data=zone1, aes(x = long, y = lat, group = group), color = "black", size = 0.6, fill = NA) +
  scale_y_continuous(breaks = seq(0, 60, 10),labels = c('0°N','10°N','20°N','30°N','40°N','50°N','60°N')) +
  scale_x_continuous(breaks= seq(70,140,10),labels = c('70°E','80°E','90°E','100°E','110°E','120°E','130°E','140°E')) +
  scale_fill_gradientn(colours=c("#1a1a1a","#543005","#7f3b08","#b35806","#e08214",#黄蓝色
              '#f5f5f5',
              "#4292c6","#2171b5","#08519c","#08306b","#2d004b"),
    breaks= c(-0.04,0,0.04),labels=c("-0.04","   0"," 0.04"), 
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
        axis.text = element_text(family=Fon, size=11,color="black"))+
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
Albedop50.FY.000

Albedop50.FY.001<-base.maps+
  geom_tile(data=Albedoper50.FY.df.final1, aes(x=x, y=y, fill=deltaAlbedo), alpha=1)+
  geom_path(data=zone2, aes(x = long, y = lat, group = group), color = "black", size = 0.6)+
  geom_polygon(data=zone1, aes(x = long, y = lat, group = group), color = "black", size = 0.6, fill = NA) +
  scale_fill_gradientn(colours=c("#1a1a1a","#543005","#7f3b08","#b35806","#e08214",#黄蓝色
              '#f5f5f5',
              "#4292c6","#2171b5","#08519c","#08306b","#2d004b"),
    breaks= c(-0.04,0,0.04),limits = c(-0.043,0.043))+
  coord_cartesian(xlim=c(105,125),ylim=c(3,23))+  
  theme_minimal()+  #背景灰色去除
  theme(legend.position = "none")+  #不显示图例
  theme(aspect.ratio = 1,  #调节长宽比
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA,color="gray60",linetype=1,size=0.8),
        plot.margin=unit(c(0,0,0,0),"mm"))
Albedop50.FY.001

fy.Albedo_50 = ggdraw() +
  draw_plot(Albedop50.FY.000) +
  draw_plot(Albedop50.FY.001, x = 0.80, y = 0.095, width = 0.15, height = 0.15)
fy.Albedo_50


#5、plot NDVI per50------------------------------------------
NDVIper50.FY.df1<-NDVIper50.FY.df

#only select climate zone I-V
NDVI.sp = NDVIper50.FY.df1[,c(2:3)]
coordinates(NDVI.sp) <- ~x + y
projection(NDVI.sp) <- proj.geo

NDVI.zone.df = over(NDVI.sp,climate2.analysis)
NDVI.zone.df2 <- data.frame(NDVIper50.FY.df1,NDVI.zone.df[,4]) 
colnames(NDVI.zone.df2) <- c('deltaNDVI','x','y','zone')

NDVI.zone.df2$zone[NDVI.zone.df2$zone>=6]=NA 
NDVI.zone.df3<-na.omit(NDVI.zone.df2)

NDVIper50.FY.df.final=NDVI.zone.df3

NDVIp50.FY.000<-base.maps+
  geom_tile(data=NDVIper50.FY.df.final, aes(x=x, y=y, fill=deltaNDVI), alpha=1)+
  geom_path(data=zone2, aes(x = long, y = lat, group = group), color = "black", size = 0.6) +
  geom_polygon(data=zone1, aes(x = long, y = lat, group = group), color = "black", size = 0.6, fill = NA) +
  scale_y_continuous(breaks = seq(0, 60, 10),labels = c('0°N','10°N','20°N','30°N','40°N','50°N','60°N')) +
  scale_x_continuous(breaks= seq(70,140,10),labels = c('70°E','80°E','90°E','100°E','110°E','120°E','130°E','140°E')) +
  scale_fill_gradientn(colours=c("#1a1a1a","#543005","#7f3b08","#b35806","#e08214",#黄蓝色
              '#f5f5f5',
              "#4292c6","#2171b5","#08519c","#08306b","#2d004b"),
    breaks= c(-0.30,0,0.30),labels=c("-0.3","  0"," 0.3"), 
    limits = c(-0.33,0.33))+
  theme_minimal()+
  coord_cartesian(ylim=c(15,55))+ #
  theme(panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"))+ 
  theme(legend.position = "none")+
  theme(axis.line.x=element_line(linetype=1,color="grey30",size=0), 
        axis.ticks.x=element_line(color="black",size=0.5,lineend = 1),
        axis.ticks.length.x=unit(0.15,'cm'), 
        axis.line.y=element_line(linetype=1,color="grey30",size=0),
        axis.ticks.y=element_line(color="black",size=0.5,lineend = 1),
        axis.ticks.length.y=unit(0.15,'cm'))+
  theme(axis.title = element_text(family=Fon, face='bold',size=13,color="black"),
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
NDVIp50.FY.000

NDVIp50.FY.001<-base.maps+
  geom_tile(data=NDVIper50.FY.df.final, aes(x=x, y=y, fill=deltaNDVI), alpha=1)+
  geom_path(data=zone2, aes(x = long, y = lat, group = group), color = "black", size = 0.6)+
  geom_polygon(data=zone1, aes(x = long, y = lat, group = group), color = "black", size = 0.6, fill = NA) +
  scale_fill_gradientn(colours=c("#1a1a1a","#543005","#7f3b08","#b35806","#e08214",#黄蓝色
              '#f5f5f5',
              "#4292c6","#2171b5","#08519c","#08306b","#2d004b"),
    breaks= c(-0.3,0,0.3),limits = c(-0.33,0.33))+
  coord_cartesian(xlim=c(105,125),ylim=c(3,23))+   
  theme_minimal()+  #背景灰色去除
  theme(legend.position = "none")+  #不显示图例
  theme(aspect.ratio = 1,  #调节长宽比
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA,color="gray60",linetype=1,size=0.8),
        plot.margin=unit(c(0,0,0,0),"mm"))
NDVIp50.FY.001

fy.NDVI_50 = ggdraw() +
  draw_plot(NDVIp50.FY.000) +
  draw_plot(NDVIp50.FY.001, x = 0.80, y = 0.095, width = 0.15, height = 0.15)
fy.NDVI_50





