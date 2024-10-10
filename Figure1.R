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
library(terra)
library(trend)
library(spatialEco)


#Figure 1. Spatial patterns of percentage of total forest cover loss, mean annual fire- 
#          and harvest- induced forest cover loss between 2001 and 2020


setwd("E:\\PhD_qiushuang\\GEE_CF_CP\\AllFigures\\writing\\Review_two")
getwd()

# 一、calc  variables --------------------------------------------------------------------------------

# 1、variables1:percentage of total forest loss=total tree loss/forest area --------------------------
variables1<-list.files(path=".\\Figure1.data\\v1\\",pattern=".tif")
variables1.all<-stack(paste0(".\\Figure1.data\\v1\\",variables1))

#(1)、outlier removal
fun <- function(x) { x[x==0] <-NA; return(x) }
variables1.all2<-calc(variables1.all,fun)
fun1 <- function(x) { x[x>=1] <-NA; return(x) }
variables1.all3<-calc(variables1.all2,fun1)

#(2)、mean
var1.yearly<-calc(variables1.all3,mean,na.rm=TRUE)
writeRaster(var1.yearly,filename=paste0(".\\Figure1.data\\", "v1_mean",".tif"),format="GTiff", overwrite=TRUE)

#(3)、trend calculation
### calc MK
NoNa = function(x){#calculateing the number of non-NA value in the layer
  len = length(which(!is.na(x)))
  return(len)}

### keep values greater than 10
var1.len.mean = calc(variables1.all3, NoNa)
Sys.time()

var1.mask1 = var1.len.mean >= 10
var1.mask1[var1.mask1 == 0] = NA
variables1.all4 = variables1.all3*var1.mask1
variables1.all4<-rast(variables1.all4)

### calculating trends
var1.trend.mean = raster.kendall(variables1.all4, tau = FALSE, intercept = FALSE,  p.value = TRUE,
                                 z.value = FALSE, confidence = FALSE)
var1.slop.mean = var1.trend.mean[[1]]
var1.pvalue.mean = var1.trend.mean[[2]]
# plot(var1.slop.mean)
# plot(var1.pvalue.mean)

var1.slop.mean<-stack(var1.slop.mean)
var1.pvalue.mean<-stack(var1.pvalue.mean)

### output
writeRaster(var1.slop.mean,filename=paste0(".\\Figure1.data\\", "var1_slop.tif"), format="GTiff", overwrite=TRUE)
writeRaster(var1.pvalue.mean,filename=paste0(".\\Figure1.data\\", "var1_pvalue.tif"),format="GTiff", overwrite=TRUE)


# 2、variables2:percentage of fire-induced forest loss=fire-induced loss/total tree loss--------------
variables2<-list.files(path=".\\Figure1.data\\v2\\",pattern=".tif")
variables2.all<-stack(paste0(".\\Figure1.data\\v2\\",variables2))

#(1)、outlier removal
variables2.all2<-calc(variables2.all,fun)
variables2.all3<-calc(variables2.all2,fun1)

#(2)、mean
var2.yearly<-calc(variables2.all3,mean,na.rm=TRUE)
writeRaster(var2.yearly,filename=paste0(".\\Figure1.data\\", "var2_mean",".tif"),format="GTiff", overwrite=TRUE)

#(3)、trend calculation
### calc MK
var2.len.mean = calc(variables2.all3, NoNa)

var2.mask1 = var2.len.mean >= 10
var2.mask1[var2.mask1 == 0] = NA
variables2.all4 = variables2.all3*var2.mask1
variables2.all4<-rast(variables2.all4)

var2.trend.mean = raster.kendall(variables2.all4, tau = FALSE, intercept = FALSE,  p.value = TRUE,
                                 z.value = FALSE, confidence = FALSE)
var2.slop.mean = var2.trend.mean[[1]]
var2.pvalue.mean = var2.trend.mean[[2]]
# plot(var2.slop.mean)
# plot(var2.pvalue.mean)

var2.slop.mean<-stack(var2.slop.mean)
var2.pvalue.mean<-stack(var2.pvalue.mean)

### output
writeRaster(var2.slop.mean,filename=paste0(".\\Figure1.data\\", "var2_slop.tif"),format="GTiff", overwrite=TRUE)
writeRaster(var2.pvalue.mean,filename=paste0(".\\Figure1.data\\", "var2_pvalue.tif"),format="GTiff", overwrite=TRUE)


# 3、variables3:percentage of harvest-induced forest loss=harvest-induced loss/total tree loss--------------------------
variables2_cut<-list.files(path=".\\Figure1.data\\v3\\",pattern=".tif")
variables2_cut.all<-stack(paste0(".\\Figure1.data\\v3\\",variables2_cut))

#(1)、outlier removal
variables2_cut.all2<-calc(variables2_cut.all,fun)
variables2_cut.all3<-calc(variables2_cut.all2,fun1)

#(2)、mean
var2_cut.yearly<-calc(variables2_cut.all3,mean,na.rm=TRUE)
writeRaster(var2_cut.yearly,filename=paste0(".\\Figure1.data\\", "var3_mean",".tif"),format="GTiff", overwrite=TRUE)

#(3)、trend calculation
### calc MK
var2_cut.len.mean = calc(variables2_cut.all3, NoNa)

var2_cut.mask1 = var2_cut.len.mean >= 10
var2_cut.mask1[var2_cut.mask1 == 0] = NA
variables2_cut.all4 = variables2_cut.all3*var2_cut.mask1
variables2_cut.all4<-rast(variables2_cut.all4)

var2_cut.trend.mean = raster.kendall(variables2_cut.all4, tau = FALSE, intercept = FALSE,  p.value = TRUE,
                                     z.value = FALSE, confidence = FALSE)
var2_cut.slop.mean = var2_cut.trend.mean[[1]]
var2_cut.pvalue.mean = var2_cut.trend.mean[[2]]

var2_cut.slop.mean<-stack(var2_cut.slop.mean)
var2_cut.pvalue.mean<-stack(var2_cut.pvalue.mean)

### output
writeRaster(var2_cut.slop.mean,filename=paste0(".\\Figure1.data\\", "var3_slop.tif"),
            format="GTiff", overwrite=TRUE)
writeRaster(var2_cut.pvalue.mean,filename=paste0(".\\Figure1.data\\", "var3_pvalue.tif"),
            format="GTiff", overwrite=TRUE)


# 二、plot  variables-----------------------------------------------------

# 1、variable 1 and its trend---------------------------------------------

raster2spdf <- function(r){
  df <- as.data.frame(as(r, "SpatialPixelsDataFrame"))
  colnames(df) <- c("value", "x", "y")
  df
}

#set coordinates and read climate zones and boundaries
proj.geo = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0 "
climate2.analysis<-readOGR("E:\\PhD_qiushuang\\R_data\\GEE_Export_China3\\climate_zone\\8zone_84.shp",
                           use_iconv = TRUE,encoding = "UTF-8")
zone1<-fortify(climate2.analysis)
China.boudary<-readOGR("E:\\PhD_qiushuang\\Boundary\\gridChina2022\\焦\\中国\\领土.shp",use_iconv = TRUE,encoding = "UTF-8")
zone2<-fortify(China.boudary)


# (1)、mean: --------------------------
### spatial distribution
var1.tif<-raster(".\\Figure1.data\\v1_mean.tif")

var1.df<-raster2spdf(var1.tif)
colnames(var1.df)<-c("var1mean","x", "y")

var1.sp = var1.df[,c(2:3)]
coordinates(var1.sp) <- ~x + y
projection(var1.sp) <- proj.geo

### extract climate zones and retain five zones needed
var1.zone.df = over(var1.sp,climate2.analysis)
var1.zone.df2 <- data.frame(var1.df,var1.zone.df[,4]) 

colnames(var1.zone.df2) <- c('var1mean','x','y','zone')

var1.zone.df2$zone[var1.zone.df2$zone>=6]=NA 
var1.zone.df3<-na.omit(var1.zone.df2)

var1.zone.df3$var1mean2<-(var1.zone.df3$var1mean)*100

### plot
base.maps<-ggplot()+geom_path(data=zone2, aes(x = long, y = lat, group = group), color = "black", size = 0.1)
base.maps

Fon<-'serif'
var1.mean.0<-base.maps+
  geom_tile(data=var1.zone.df3, aes(x=x, y=y, fill=var1mean2), alpha=1)+
  geom_path(data=zone2, aes(x = long, y = lat, group = group), color = "black", size = 0.6) +
  geom_polygon(data=zone1, aes(x = long, y = lat, group = group), 
               color = "black", size = 0.6, fill = NA) +
  scale_y_continuous(breaks = seq(0, 60, 10),
                     labels = c('0°N','10°N','20°N','30°N','40°N','50°N','60°N')) +
  scale_x_continuous(breaks= seq(70,140,10),
                     labels = c('70°E','80°E','90°E','100°E','110°E','120°E','130°E','140°E')) +
  scale_fill_gradientn(colours=c("#fff3ed",'firebrick2',"red4"),
                       breaks= c(0,3,6),limit=c(0,6.1))+
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
        axis.text = element_text(family=Fon, size=12,color="black"),
        plot.title = element_text(family=Fon, face='bold',size=13,hjust = 0.5),
        aspect.ratio = 1, 
        panel.grid = element_blank(),
        panel.border = element_rect(fill=NA,color="black", size=0.8,
                                    linetype="solid"))+
  theme(legend.title = element_blank(), 
        legend.text = element_text(family=Fon,size=10,color="black"), 
        legend.key.size =unit(15,"pt"), 
        legend.key.height =unit(10,"pt"),
        legend.key.width =unit(10,"pt"),
        legend.position = c(0.15,0.18))+
  labs(x=NULL,y=NULL) +
  ggtitle(NULL)
var1.mean.0

var1.mean.1<-base.maps+
  geom_tile(data=var1.zone.df3, aes(x=x, y=y, fill=var1mean2), alpha=1)+
  geom_path(data=zone2, aes(x = long, y = lat, group = group), color = "black", size = 0.6)+
  geom_polygon(data=zone1, aes(x = long, y = lat, group = group), color = "black", size = 0.6, fill = NA) +
  scale_fill_gradientn(colours=c("#fff3ed",'firebrick2',"red4"),
                       breaks= c(0,3,6))+
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
var1.mean.1

var1.mean.map = ggdraw() +
  draw_plot(var1.mean.0) +
  draw_plot(var1.mean.1, x = 0.80, y = 0.095, width = 0.15, height = 0.15)
var1.mean.map


### latitude distribution 2°*2°
variables1<-list.files(path=".\\Figure1.data\\v1\\",pattern=".tif")

lat1 = seq(0, 60 , by =2)
year<-seq(2001,2020,1)
empty.df<-data.frame(seq(0, 60 , by =1)) 
colnames(empty.df)<-c("lat1")

for( i in 1:length(variables1) ){
  tif<-stack(paste0(".\\Figure1.data\\v1\\",variables1[i]))
  
  tif1<-calc(tif,fun)
  v1.df<-raster2spdf(tif1)
  
  var1.sp = v1.df[,c(2:3)]
  coordinates(var1.sp) <- ~x + y
  projection(var1.sp) <- proj.geo
  
  var1.zone.df = over(var1.sp,climate2.analysis)
  var1.zone.df2 <- data.frame(v1.df,var1.zone.df[,4]) 
  
  colnames(var1.zone.df2) <- c('var1mean','x','y','zone')
  
  var1.zone.df2$zone[var1.zone.df2$zone>=6]=NA 
  var1.zone.df3<-na.omit(var1.zone.df2)
 
  var1.zone.df3$lat1=-100
  
  for (j in 1:(length(lat1)-1)){
    idx = which(var1.zone.df3$y >= lat1[j] & var1.zone.df3$y< lat1[j+1])
    var1.zone.df3$lat1[idx] = lat1[j]+1
  } 
  
  v1_lat.mn = aggregate(var1mean~lat1 , FUN=mean, na.rm=TRUE, data = var1.zone.df3)
  colnames(v1_lat.mn)[2] <-paste0("v1_",year[i])
  
  data.df<-merge(empty.df,v1_lat.mn,by="lat1")
  empty.df<-data.df
}


var1.lat.df<-data.df
var1.lat.df[var1.lat.df==0] = NA

var1.lat.df$Total_mn<-rowMeans(var1.lat.df[,c(seq(2,21,1))],na.rm = TRUE)
var1.lat.df$Total_mn[sapply(var1.lat.df$Total_mn,is.na)] = NA 

var1.lat.df$Total_sd<-0

for (i in 1:nrow(var1.lat.df)){
  var1.lat.df[i,23]<-sd(var1.lat.df[i,c(seq(2,21,1))],na.rm = TRUE)
}

var1.lat.df$Total_mn2<-var1.lat.df$Total_mn*100
var1.lat.df$Total_sd2<-var1.lat.df$Total_sd*100

### plot lat distribution
Fon<-'serif'
v1_lat.map<-ggplot(data=var1.lat.df,aes(x=Total_mn2,y=lat1))+
  scale_y_continuous(breaks = c(20,30,40,50, 60),labels = c('20°N','30°N','40°N','50°N','60°N'),
                     limit=c(15,55)) +
  scale_x_continuous(breaks= c(0,1.5,3),labels = c('0','1.5','3')) +
  geom_vline(xintercept =0,linetype="dashed")+
  geom_path(colour="red",alpha=1,size=0.7)+
  geom_ribbon(aes(xmin=Total_mn2-Total_sd2,xmax=Total_mn2+Total_sd2),fill="red",alpha=0.2)+ 
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
  coord_cartesian(xlim=c(0,3.2),ylim = c(15, NA))
v1_lat.map


# (2)、significant trend: --------------------------
var1.trend.tif<-raster(".\\Figure1.data\\var1_slop.tif")
var1.p.tif<-raster(".\\Figure1.data\\var1_pvalue.tif")

var1.p.tif1<-var1.p.tif
var1.p.tif1[var1.p.tif1>0.1]=NA

var1.trend.sign<-mask(var1.trend.tif,var1.p.tif1)

var1.trend.sign.df<-raster2spdf(var1.trend.sign)
colnames(var1.trend.sign.df)<-c("var1trend","x", "y")


var1.trend.sign.sp = var1.trend.sign.df[,c(2:3)]
coordinates(var1.trend.sign.sp) <- ~x + y
projection(var1.trend.sign.sp) <- proj.geo

var1.trend.sign.zone.df = over(var1.trend.sign.sp,climate2.analysis)
var1.trend.sign.zone.df2 <- data.frame(var1.trend.sign.df,var1.trend.sign.zone.df[,4]) 

colnames(var1.trend.sign.zone.df2) <- c('var1trend','x','y','zone')

var1.trend.sign.zone.df2$zone[var1.trend.sign.zone.df2$zone>=6]=NA 
var1.trend.sign.zone.df3<-na.omit(var1.trend.sign.zone.df2)

var1.trend.sign.zone.df3$var1trend2<-(var1.trend.sign.zone.df3$var1trend)*100

var1.trend.sign.zone.df4<-var1.trend.sign.zone.df3
var1.trend.sign.zone.df4$var1trend2[var1.trend.sign.zone.df4$var1trend2>=0.2]=0.2
var1.trend.sign.zone.df4$var1trend2[var1.trend.sign.zone.df4$var1trend2<= -0.2]= -0.2

### plot
base.maps<-ggplot()+geom_path(data=zone2, aes(x = long, y = lat, group = group), color = "black", size = 0.1)
base.maps

Fon<-'serif'
var1.trend.sign.0<-base.maps+
  geom_tile(data=var1.trend.sign.zone.df4, aes(x=x, y=y, fill=var1trend2), alpha=1)+
  geom_path(data=zone2, aes(x = long, y = lat, group = group), color = "black", size = 0.6) +
  geom_polygon(data=zone1, aes(x = long, y = lat, group = group), color = "black", size = 0.6, fill = NA) +
  scale_y_continuous(breaks = seq(0, 60, 10),
                     labels = c('0°N','10°N','20°N','30°N','40°N','50°N','60°N')) +
  scale_x_continuous(breaks= seq(70,140,10),
                     labels = c('70°E','80°E','90°E','100°E','110°E','120°E','130°E','140°E')) +
  scale_fill_gradientn(colours=c("blue1","white","firebrick3"),
                       breaks= c(-0.2,0,0.2),
                       labels=c("-0.2","0","0.2"),
                       limit=c(-0.22,0.22))+
  theme_minimal()+
  coord_cartesian(ylim=c(15,55))+ 
  theme(axis.line.x=element_line(linetype=1,color="grey30",size=0), 
        axis.ticks.x=element_line(color="black",size=0.5,lineend = 1), 
        axis.ticks.length.x=unit(0.15,'cm'), 
        axis.line.y=element_line(linetype=1,color="grey30",size=0),
        axis.ticks.y=element_line(color="black",size=0.5,lineend = 1),
        axis.ticks.length.y=unit(0.15,'cm'))+
  theme(axis.title = element_text(family=Fon, face='bold',size=13,color="black"),
        axis.text = element_text(family=Fon, size=12,color="black"),
        plot.title = element_text(family=Fon, face='bold',size=13,hjust = 0.5),
        aspect.ratio = 1, #调节长宽比
        panel.grid = element_blank(),
        panel.border = element_rect(fill=NA,color="black", size=0.8,linetype="solid"))+
  theme(legend.title = element_blank(), 
        legend.text = element_text(family=Fon,size=10,color="black"), 
        legend.key.size =unit(15,"pt"), 
        legend.key.height =unit(10,"pt"),
        legend.key.width =unit(10,"pt"),
        legend.position = c(0.15,0.18))+
  labs(x=NULL,y=NULL) + ggtitle(NULL)
var1.trend.sign.0

var1.trend.sign.1<-base.maps+
  geom_tile(data=var1.trend.sign.zone.df4, aes(x=x, y=y, fill=var1trend2), alpha=1)+
  geom_path(data=zone2, aes(x = long, y = lat, group = group), color = "black", size = 0.6)+
  geom_polygon(data=zone1, aes(x = long, y = lat, group = group), color = "black", size = 0.6, fill = NA) +
  scale_fill_gradientn(colours=c("blue1","white","firebrick3"),
                       breaks= c(-0.2,0,0.2))+
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
var1.trend.sign.1

var1.trend.sign.map = ggdraw() +
  draw_plot(var1.trend.sign.0) +
  draw_plot(var1.trend.sign.1, x = 0.80, y = 0.095, width = 0.15, height = 0.15)
var1.trend.sign.map


### latitude distribution 2°*2°
var1.trend.sign.zone.df5<-var1.trend.sign.zone.df3

var1.trend.sign.zone.df5$lat1=-100
lat1 = seq(0, 60 , by =2)

for (j in 1:(length(lat1)-1)){
  idx = which(var1.trend.sign.zone.df5$y >= lat1[j] & var1.trend.sign.zone.df5$y< lat1[j+1])
  var1.trend.sign.zone.df5$lat1[idx] = lat1[j]+1
} 

v1trend.sign_lat.mn = aggregate(var1trend2~lat1 , FUN=mean, na.rm=TRUE, data = var1.trend.sign.zone.df5)

### plot
v1trend.sign_lat.map<-ggplot(data=v1trend.sign_lat.mn,aes(x=var1trend2,y=lat1))+
  scale_y_continuous(breaks = c(20,30,40,50, 60),labels = c('20°N','30°N','40°N','50°N','60°N')) +
  scale_x_continuous(breaks= c(-0.1,0,0.1),labels = c('-0.1','0','0.1')) +
  geom_vline(xintercept =0,linetype="dashed")+
  geom_path(colour="red",alpha=1,size=0.7)+
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
  labs(x=NULL,y=element_blank())+
  coord_cartesian(xlim=c(-0.1,0.22),ylim = c(15, NA))
v1trend.sign_lat.map


####
# the plot and calculation methods for variable 2 and 3 are 
# the same as for variable 1, and will not be repeated here.

# 2、variable 2 and its trend---------------------------------------------

#same as variable1

# 3、variable 3 and its trend---------------------------------------------

#same as variable1


