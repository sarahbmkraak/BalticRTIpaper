#### first the baseline maps for gear that reduces cod catches (FLEX)
library(lubridate)
library(maps)
library(mapdata)
library(mapplots)
library(dplyr)
library(vmstools)
rm(list=ls())

legend.vms=function(x,breaks,round=0,suffix='',type=1,pch=15,pt.cex=2.5,bg='lightblue',col=NULL,extra.col=NULL, extra.text=NULL,...){
  # this function adds a legend to the VMS plot
  #
  #   x       position of the legend (e.g 'topright','center' etc)
  #   breaks  break points used in plot.vms()
  #   round   number of decimals to use in the legend
  #   suffix  a suffix like 'kg' or 'h'
  #   ...     other parameters that can be passed on to legend()
  if(max(breaks,na.rm=T)>0){
    ncol=length(breaks)-1
    if(missing(col)) col=c('white',heat.colors(ncol-1)[(ncol-1):1])
    if (type==0) legend=paste(suffix, sep = '')
    if (type==1) legend=paste(round(breaks[(ncol+1):2],round),suffix,sep='')
    if (type==2) legend=paste(round(breaks[(ncol):1],round),' - ',round(breaks[(ncol+1):2],round),suffix,sep='')
    if (!is.null(extra.col) & !is.null(extra.text))
      legend=paste(round(breaks[(ncol+1):2],round),suffix,sep='')
    legend(x,legend=legend,col=col[ncol:1],pch=pch,pt.cex=pt.cex,bg=bg,...)
  }
}

### read data
maps_data<-read.table("BalticRTIpaper_data.csv",header=TRUE,sep=",", fill=T)
maps_data<-data.frame(maps_data)

### fine resolution of lat and lon at 0.05 degrees
maps_data$lat_fine<-data.frame(CSquare2LonLat(as.character(maps_data$CSquare),0.05))$SI_LATI
maps_data$lon_fine<-data.frame(CSquare2LonLat(as.character(maps_data$CSquare),0.05))$SI_LONG

### we need lat and lon columns for the RTI resolution of 0.5 degrees longitude x 0.2 degrees latitude
maps_data$lat_RTI<-(round((maps_data$lat_fine+0.1)*5,0)/5)-0.1
maps_data$lon_RTI<-round(maps_data$lon_fine*2,0)/2

# limits of maps
xlim <- c(9.5,18)
ylim <- c(53.5,56)

## First make the subset for the years 2014-2017, which are used for the starting map
maps_data_wo2018<-subset(maps_data, year<2018)

### Calculate CPUE at the resolution of 0.5 degrees longitude x 0.2 degrees latitude and week
aggCODr_catches_wo2018<- aggregate(list(z=maps_data_wo2018$raised_catch_cod),list(x=maps_data_wo2018$lon_RTI,y=maps_data_wo2018$lat_RTI, p=maps_data_wo2018$year, q=maps_data_wo2018$week),sum, na.rm=TRUE)
aggCODeffort_wo2018 <- aggregate(list(z=maps_data_wo2018$Effort_hrs),list(x=maps_data_wo2018$lon_RTI,y=maps_data_wo2018$lat_RTI, p=maps_data_wo2018$year, q=maps_data_wo2018$week),sum, na.rm=TRUE)
aggCODeffort_wo2018$NEW_CPUE_COD<-aggCODr_catches_wo2018$z/aggCODeffort_wo2018$z

aggPLEr_catches_wo2018 <- aggregate(list(z=maps_data_wo2018$raised_catch_ple),list(x=maps_data_wo2018$lon_RTI,y=maps_data_wo2018$lat_RTI, p=maps_data_wo2018$year, q=maps_data_wo2018$week),sum, na.rm=TRUE)
aggPLEeffort_wo2018 <- aggregate(list(z=maps_data_wo2018$Effort_hrs),list(x=maps_data_wo2018$lon_RTI,y=maps_data_wo2018$lat_RTI, p=maps_data_wo2018$year, q=maps_data_wo2018$week),sum, na.rm=TRUE)
aggPLEeffort_wo2018$NEW_CPUE_PLE<-aggPLEr_catches_wo2018$z/aggPLEeffort_wo2018$z

### change name (leftover, because in previous versions I used another step here)
aggCOD_under_4.4<-aggCODeffort_wo2018
aggPLE_under_4.4<-aggPLEeffort_wo2018

### make grid with CPUE relative to the mean (of all data without 2018)
CPUE_grid_COD_4.4<-make.grid(aggCOD_under_4.4$x, aggCOD_under_4.4$y, aggCOD_under_4.4$NEW_CPUE_COD, byx=0.5 , byy=0.2 , xlim=xlim, ylim=ylim, fun = mean)
mean_CEL_COD_4.4=mean(CPUE_grid_COD_4.4, na.rm = T)
max_CELL_COD_4.4=ifelse(mean(CPUE_grid_COD_4.4, na.rm = T)>0,max(CPUE_grid_COD_4.4, na.rm = T)/mean(CPUE_grid_COD_4.4, na.rm = T),0)
#here the cod catches are reduced by 75%
RTI_grid_COD_4.4<-0.25*CPUE_grid_COD_4.4/mean_CEL_COD_4.4
RTI_grid_COD<-RTI_grid_COD_4.4

CPUE_grid_PLE<-make.grid(aggPLE_under_4.4$x, aggPLE_under_4.4$y, aggPLE_under_4.4$NEW_CPUE_PLE, byx=0.5 , byy=0.2 , xlim=xlim, ylim=ylim, fun = mean)
mean_CEL_PLE=mean(CPUE_grid_PLE, na.rm = T)
max_CELL_PLE=ifelse(mean(CPUE_grid_PLE, na.rm = T)>0,max(CPUE_grid_PLE, na.rm = T)/mean(CPUE_grid_PLE, na.rm = T),0)
## here the plaice catches are reduced by 10%
RTI_grid_PLE<-0.9*CPUE_grid_PLE/mean_CEL_PLE

### make the levels of the tariff bins
# first I make three objects of the same shape; their values will be changed inside the loop
RTI_grid_COD_levels<- RTI_grid_COD
RTI_grid_PLE_levels<- RTI_grid_PLE
RTI_grid_COMBINED<- RTI_grid_COD
for (i in 1:18)
{for (j in 1:13)
{ 
  RTI_grid_COD_levels[i,j] <- ifelse(RTI_grid_COD_4.4[i,j]>5,6,ifelse(RTI_grid_COD_4.4[i,j]>2,5,ifelse(RTI_grid_COD_4.4[i,j]>1,4,ifelse(RTI_grid_COD_4.4[i,j]>0.5,3,ifelse(RTI_grid_COD_4.4[i,j]>0.1,2,1)))))
  RTI_grid_PLE_levels[i,j] <- ifelse(RTI_grid_PLE[i,j]>5,6,ifelse(RTI_grid_PLE[i,j]>2,5,ifelse(RTI_grid_PLE[i,j]>1,4,ifelse(RTI_grid_PLE[i,j]>0.5,3,ifelse(RTI_grid_PLE[i,j]>0.1,2,1)))))
  RTI_grid_COMBINED[i,j]<-max(RTI_grid_COD_levels[i,j],RTI_grid_PLE_levels[i,j])
}}

# make the map based on data of 2014-2017 for week 1 (baseline RTIs)
# limits of maps
xlim <- c(9.5,18)
ylim <- c(54,55.5)


png(filename="baseline_RTI_COMBINED.png", units = "cm", width = 19, height = 12, res = 600)
par(mfrow=c(1,1), mar=c(4,4,3,1), oma=c(1,1,1,1))
breaks <- c(0,1.1,2.1,3.1,4.1,5.1,6.1)
basemap(xlim=c(9.5,18.5),ylim=ylim,xaxt='n',yaxt='n',xlab='Longitude (°E)',ylab='Latitude (°N)',main=paste('RTI tariffs for week 1 of 2018, based on data from 2014-2017'), bg='white')
axis(1,0:90,labels=paste0(0:90))
axis(2,0:90,labels=paste0(0:90))
draw.grid(RTI_grid_COMBINED,breaks, col=c('white', heat.colors(9)[7],heat.colors(9)[6],heat.colors(8)[4],heat.colors(8)[2],'black'))
map('worldHires',col='darkgreen',fill=T,add=T)
text(11, 53.75, 'Germany', font=2, col='white')
text(13.6, 55.75, 'Denmark', font=2, col='white')
text(16, 53.75, 'Poland', font=2, col='white')
legend.vms("bottomright", breaks,round=0,type=0, paste(c('Closure', '5 RTIs', '2 RTIs', '1 RTI', '0.5 RTI', '0.1 RTI')),col=c('white', heat.colors(9)[7],heat.colors(9)[6],heat.colors(8)[4],heat.colors(8)[2],'black'), inset=0.01, title="Tariffs", bg='grey')
dev.off()
