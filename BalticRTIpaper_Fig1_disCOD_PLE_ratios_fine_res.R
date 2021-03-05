###### Maps of COD CPUE, PLE CPUE and ratios of COD CPUE to PLE CPUE

rm(list=ls())
library(maps)
library(mapdata)
library(mapplots)
library(dplyr)
library(vmstools)

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


# limits of maps
xlim <- c(9.5,18.5)
ylim <- c(53.5,56.5)


### calculate CPUE at the spatial and temporal resolution for the maps (fine and weekly)
aggCODr_catches<- aggregate(list(z=maps_data$raised_catch_cod),list(x=maps_data$lon_fine,y=maps_data$lat_fine, p=maps_data$year, q=maps_data$week),sum, na.rm=TRUE)
aggCODeffort <- aggregate(list(z=maps_data$Effort_hrs),list(x=maps_data$lon_fine,y=maps_data$lat_fine, p=maps_data$year, q=maps_data$week),sum, na.rm=TRUE)
aggCODeffort$NEW_CPUE_COD<-aggCODr_catches$z/aggCODeffort$z

aggPLEr_catches <- aggregate(list(z=maps_data$raised_catch_ple),list(x=maps_data$lon_fine,y=maps_data$lat_fine, p=maps_data$year, q=maps_data$week),sum, na.rm=TRUE)
aggPLEeffort <- aggregate(list(z=maps_data$Effort_hrs),list(x=maps_data$lon_fine,y=maps_data$lat_fine, p=maps_data$year, q=maps_data$week),sum, na.rm=TRUE)
aggPLEeffort$NEW_CPUE_PLE<-aggPLEr_catches$z/aggPLEeffort$z


### make grid with CPUE (of all data)
CPUE_grid_COD<-make.grid(aggCODeffort$x, aggCODeffort$y, aggCODeffort$NEW_CPUE_COD, byx=0.05 , byy=0.05 , xlim=xlim, ylim=ylim, fun = mean)
max(CPUE_grid_COD,na.rm = T)
CPUE_grid_PLE<-make.grid(aggPLEeffort$x, aggPLEeffort$y, aggPLEeffort$NEW_CPUE_PLE, byx=0.05 , byy=0.05 , xlim=xlim, ylim=ylim, fun = mean)
max(CPUE_grid_PLE,na.rm = T)

### MAPS displaying average weekly CPUE 
# limits of maps
xlim <- c(9.5,18)
ylim <- c(54,55.5)

# COD
png(filename="COD_CPUE.png", units = "cm", width = 19, height = 12, res = 600)
par(mfrow=c(1,1), mar=c(4,4,3,1), oma=c(1,1,1,1))
breaks <- c(0,50,100,200,300,max(CPUE_grid_COD,na.rm = T))
basemap(xlim=xlim,ylim=ylim,xaxt='n',yaxt='n',xlab='Longitude (°E)',ylab='Latitude (°N)', bg='light blue')
axis(1,0:90,labels=paste0(0:90))
axis(2,0:90,labels=paste0(0:90))
draw.grid(CPUE_grid_COD,breaks, col=c(heat.colors(12)[10], heat.colors(12)[8],heat.colors(12)[7],heat.colors(12)[5],heat.colors(12)[1]))
map('worldHires',col='darkgreen',fill=T,add=T)
text(11, 53.75, 'Germany', font=2, col='white')
text(13.6, 55.75, 'Denmark', font=2, col='white')
text(16, 53.75, 'Poland', font=2, col='white')
legend.vms("bottomright", breaks, type=2, col=c(heat.colors(12)[10], heat.colors(12)[8],heat.colors(12)[7],heat.colors(12)[5],heat.colors(12)[1]), inset=0.01, title="kg/h", bg='grey')
dev.off()

# PLE
png(filename="PLE_CPUE.png", units = "cm", width = 19, height = 12, res = 600)
par(mfrow=c(1,1), mar=c(4,4,3,1), oma=c(1,1,1,1))
breaks <- c(0,10,20,50,100,max(CPUE_grid_PLE,na.rm = T))
basemap(xlim=xlim,ylim=ylim,xaxt='n',yaxt='n',xlab='Longitude (°E)',ylab='Latitude (°N)', bg='light blue')
axis(1,0:90,labels=paste0(0:90))
axis(2,0:90,labels=paste0(0:90))
draw.grid(CPUE_grid_PLE,breaks, col=c(heat.colors(12)[10], heat.colors(12)[8],heat.colors(12)[7],heat.colors(12)[5],heat.colors(12)[1]))
map('worldHires',col='darkgreen',fill=T,add=T)
text(11, 53.75, 'Germany', font=2, col='white')
text(13.6, 55.75, 'Denmark', font=2, col='white')
text(16, 53.75, 'Poland', font=2, col='white')
legend.vms("bottomright", breaks, type=2, col=c(heat.colors(12)[10], heat.colors(12)[8],heat.colors(12)[7],heat.colors(12)[5],heat.colors(12)[1]), inset=0.01, title="kg/h", bg='grey')
dev.off()


# ratio of cod to plaice
png(filename="ratios.png", units = "cm", width = 19, height = 12, res = 600)
par(mfrow=c(1,1), mar=c(4,4,3,1), oma=c(1,1,1,1))
breaks <- c(0,0.1,0.3,1,3,10,100000)
basemap(xlim=xlim,ylim=ylim,xaxt='n',yaxt='n',xlab='Longitude (°E)',ylab='Latitude (°N)', bg='white')
axis(1,0:90,labels=paste0(0:90))
axis(2,0:90,labels=paste0(0:90))
draw.grid(CPUE_grid_COD/CPUE_grid_PLE,breaks, col=c("#B2182B","#EF8A62", "#FDDBC7", "#D1E5F0", "#67A9CF","#2166AC"))
map('worldHires',col='darkgreen',fill=T,add=T)
text(11, 53.75, 'Germany', font=2, col='white')
text(13.6, 55.75, 'Denmark', font=2, col='white')
text(16, 53.75, 'Poland', font=2, col='white')
legend.vms("bottomright", breaks,round=0,type=0, paste(c('>10', '3-10', '1-3', '0.3-1', '0.1-0.3', '<0.1')), col = c("#B2182B","#EF8A62", "#FDDBC7", "#D1E5F0", "#67A9CF","#2166AC"), inset=0.01, title="ratio", bg='grey') #title has to be different for different speies
dev.off()

