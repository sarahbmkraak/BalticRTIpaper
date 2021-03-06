### simulation
library(lubridate)
library(maps)
library(mapdata)
library(mapplots)
library(dplyr)
library(vmstools)
rm(list=ls())

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

### Calculate CPUE at the resolution of 0.5 degrees longitude x 0.2 degrees latitude and month
aggCODr_catches_wo2018<- aggregate(list(z=maps_data_wo2018$raised_catch_cod),list(x=maps_data_wo2018$lon_RTI,y=maps_data_wo2018$lat_RTI, p=maps_data_wo2018$year, q=maps_data_wo2018$month),sum, na.rm=TRUE)
aggCODeffort_wo2018 <- aggregate(list(z=maps_data_wo2018$Effort_hrs),list(x=maps_data_wo2018$lon_RTI,y=maps_data_wo2018$lat_RTI, p=maps_data_wo2018$year, q=maps_data_wo2018$month),sum, na.rm=TRUE)
aggCODeffort_wo2018$NEW_CPUE_COD<-aggCODr_catches_wo2018$z/aggCODeffort_wo2018$z

aggPLEr_catches_wo2018 <- aggregate(list(z=maps_data_wo2018$raised_catch_ple),list(x=maps_data_wo2018$lon_RTI,y=maps_data_wo2018$lat_RTI, p=maps_data_wo2018$year, q=maps_data_wo2018$month),sum, na.rm=TRUE)
aggPLEeffort_wo2018 <- aggregate(list(z=maps_data_wo2018$Effort_hrs),list(x=maps_data_wo2018$lon_RTI,y=maps_data_wo2018$lat_RTI, p=maps_data_wo2018$year, q=maps_data_wo2018$month),sum, na.rm=TRUE)
aggPLEeffort_wo2018$NEW_CPUE_PLE<-aggPLEr_catches_wo2018$z/aggPLEeffort_wo2018$z

### change name (leftover, because in previous versions I used another step here)
aggCOD_under_4.4<-aggCODeffort_wo2018
aggPLE_under_4.4<-aggPLEeffort_wo2018

### make grid with CPUE relative to the mean (of all data without 2018)
CPUE_grid_COD_4.4<-make.grid(aggCOD_under_4.4$x, aggCOD_under_4.4$y, aggCOD_under_4.4$NEW_CPUE_COD, byx=0.5 , byy=0.2 , xlim=xlim, ylim=ylim, fun = mean)
mean_CEL_COD_4.4=mean(CPUE_grid_COD_4.4, na.rm = T)
max_CELL_COD_4.4=ifelse(mean(CPUE_grid_COD_4.4, na.rm = T)>0,max(CPUE_grid_COD_4.4, na.rm = T)/mean(CPUE_grid_COD_4.4, na.rm = T),0)
RTI_grid_COD_4.4<-CPUE_grid_COD_4.4/mean_CEL_COD_4.4
RTI_grid_COD<-RTI_grid_COD_4.4

CPUE_grid_PLE<-make.grid(aggPLE_under_4.4$x, aggPLE_under_4.4$y, aggPLE_under_4.4$NEW_CPUE_PLE, byx=0.5 , byy=0.2 , xlim=xlim, ylim=ylim, fun = mean)
mean_CEL_PLE=mean(CPUE_grid_PLE, na.rm = T)
max_CELL_PLE=ifelse(mean(CPUE_grid_PLE, na.rm = T)>0,max(CPUE_grid_PLE, na.rm = T)/mean(CPUE_grid_PLE, na.rm = T),0)
RTI_grid_PLE<-CPUE_grid_PLE/mean_CEL_PLE

# here you can give weighting; with no weighting these values are =1; 
# if you want to reduce fishing on a species you can reduce the weighting, e.g. to 0.75; 
# if you want to increase fishing on a species you can increase the weighting, e.g. to 1.25;
# this way the thresholds for the levels change, meaning that with lower/higher relative CPUE the RTI levels change
# (i.e. pay more or less for the same relative CPUE)
wghtCOD<-1.07
wghtPLE<-0.94

### make the levels of the tariff bins
# first I make three objects of the same shape; their values will be changed inside the loop
RTI_grid_COD_levels<- RTI_grid_COD
RTI_grid_PLE_levels<- RTI_grid_PLE
RTI_grid_COMBINED<- RTI_grid_PLE
for (i in 1:18)
{for (j in 1:13)
{ 
  # NOTE THAT HERE THE THRESHOLDS OF THE LEVELS ARE MULTIPLIED BY WEIGHTS! These weights can be 1 or some other value
  RTI_grid_COD_levels[i,j] <- ifelse(RTI_grid_COD_4.4[i,j]>5*wghtCOD,6,ifelse(RTI_grid_COD_4.4[i,j]>2*wghtCOD,5,ifelse(RTI_grid_COD_4.4[i,j]>1*wghtCOD,4,ifelse(RTI_grid_COD_4.4[i,j]>0.5*wghtCOD,3,ifelse(RTI_grid_COD_4.4[i,j]>0.1*wghtCOD,2,1)))))
  RTI_grid_PLE_levels[i,j] <- ifelse(RTI_grid_PLE[i,j]>5*wghtPLE,6,ifelse(RTI_grid_PLE[i,j]>2*wghtPLE,5,ifelse(RTI_grid_PLE[i,j]>1*wghtPLE,4,ifelse(RTI_grid_PLE[i,j]>0.5*wghtPLE,3,ifelse(RTI_grid_PLE[i,j]>0.1*wghtPLE,2,1)))))
  RTI_grid_COMBINED[i,j]<-max(RTI_grid_COD_levels[i,j],RTI_grid_PLE_levels[i,j])
}}

### Now we bring in the 2018 data; these will be used to make monthly RTI updates
maps_data_2018<-subset(maps_data, year==2018)

### as above we need to calculate the CPUE per cel and month
### COD ###
aggCODr_catches_2018<- aggregate(list(z=maps_data_2018$raised_catch_cod),list(x=maps_data_2018$lon_RTI,y=maps_data_2018$lat_RTI, p=maps_data_2018$year, q=maps_data_2018$month),sum, na.rm=TRUE)
aggCODeffort_2018 <- aggregate(list(z=maps_data_2018$Effort_hrs),list(x=maps_data_2018$lon_RTI,y=maps_data_2018$lat_RTI, p=maps_data_2018$year, q=maps_data_2018$month),sum, na.rm=TRUE)
aggCODeffort_2018$NEW_CPUE_COD<-aggCODr_catches_2018$z/aggCODeffort_2018$z
### PLE ###
aggPLEr_catches_2018 <- aggregate(list(z=maps_data_2018$raised_catch_ple),list(x=maps_data_2018$lon_RTI,y=maps_data_2018$lat_RTI, p=maps_data_2018$year, q=maps_data_2018$month),sum, na.rm=TRUE)
aggPLEeffort_2018 <- aggregate(list(z=maps_data_2018$Effort_hrs),list(x=maps_data_2018$lon_RTI,y=maps_data_2018$lat_RTI, p=maps_data_2018$year, q=maps_data_2018$month),sum, na.rm=TRUE)
aggPLEeffort_2018$NEW_CPUE_PLE<-aggPLEr_catches_2018$z/aggPLEeffort_2018$z

# rename (leftover of previous version of this script)
aggPLE_under_4.4_2018<-aggPLEeffort_2018
aggCOD_under_4.4_2018<-aggCODeffort_2018


# now for 12 months, in month 1 baseline RTIs are valid; update[,,2] is RTIs for month 2 updated by month 1 info, etc.
updateCOD<- array(dim=c(18,13,12))
updatePLE<- array(dim=c(18,13,12))
updateCOMBINED<- array(dim=c(18,13,12))
updateCOD[,,1] <- RTI_grid_COD_levels
updatePLE[,,1] <- RTI_grid_PLE_levels
updateCOMBINED[,,1]<-RTI_grid_COMBINED

# 1 is for COD, 2 is for PLE
newinfo <- array(dim=c(18,13,12,2))

for (k in 1:11)
{
  # now take month by month
### here the relative CPUEs are calculated 
   #COD
  aggCOD_under_4.4_2018_1=subset(aggCOD_under_4.4_2018, q==k)
  CPUE_grid_COD_1<-make.grid(aggCOD_under_4.4_2018_1$x, aggCOD_under_4.4_2018_1$y, aggCOD_under_4.4_2018_1$NEW_CPUE_COD, byx=0.5 , byy=0.2 , xlim=xlim, ylim=ylim, fun = mean)
  mean_CEL_COD_1=mean(CPUE_grid_COD_1, na.rm = T)
  max_CELL_COD_1=ifelse(mean(CPUE_grid_COD_1, na.rm = T)>0,max(CPUE_grid_COD_1, na.rm = T)/mean(CPUE_grid_COD_1, na.rm = T),0)
  # relative to mean
  RTI_grid_COD_1<-CPUE_grid_COD_1/mean_CEL_COD_1
  newinfo[,,k,1]<-RTI_grid_COD_1
  
  #PLE
  aggPLE_under_4.4_2018_1=subset(aggPLE_under_4.4_2018, q==k)
  CPUE_grid_PLE_1<-make.grid(aggPLE_under_4.4_2018_1$x, aggPLE_under_4.4_2018_1$y, aggPLE_under_4.4_2018_1$NEW_CPUE_PLE, byx=0.5 , byy=0.2 , xlim=xlim, ylim=ylim, fun = mean)
  mean_CEL_PLE_1=mean(CPUE_grid_PLE_1, na.rm = T)
  max_CELL_PLE_1=ifelse(mean(CPUE_grid_PLE_1, na.rm = T)>0,max(CPUE_grid_PLE_1, na.rm = T)/mean(CPUE_grid_PLE_1, na.rm = T),0)
  # relative to mean
  RTI_grid_PLE_1<-CPUE_grid_PLE_1/mean_CEL_PLE_1
  newinfo[,,k,2]<-RTI_grid_PLE_1
  

  ###  here the levels are calculated, first we make objects of the right shape whose values will be changed in the loop
  RTI_grid_COD_1_levels<- RTI_grid_COD_1
  RTI_grid_PLE_1_levels<- RTI_grid_PLE_1
  for (i in 1:18)
  {for (j in 1:13)
  {
    RTI_grid_COD_1_levels[i,j] <- ifelse(RTI_grid_COD_1[i,j]>5*wghtCOD,6,ifelse(RTI_grid_COD_1[i,j]>2*wghtCOD,5,ifelse(RTI_grid_COD_1[i,j]>1*wghtCOD,4,ifelse(RTI_grid_COD_1[i,j]>0.5*wghtCOD,3,ifelse(RTI_grid_COD_1[i,j]>0.1*wghtCOD,2,1)))))
    RTI_grid_PLE_1_levels[i,j] <- ifelse(RTI_grid_PLE_1[i,j]>5*wghtPLE,6,ifelse(RTI_grid_PLE_1[i,j]>2*wghtPLE,5,ifelse(RTI_grid_PLE_1[i,j]>1*wghtPLE,4,ifelse(RTI_grid_PLE_1[i,j]>0.5*wghtPLE,3,ifelse(RTI_grid_PLE_1[i,j]>0.1*wghtPLE,2,1)))))
    ## here I don't have to make the levels for COMBINED because I do it in the next loop, the one for update
  }}
  
  # now update: if the monthly value is higher or lower make the updated level +1 or -1 respectively
  ## note that in this script we assume that when
  ## a cell has no info to start with, but in a particular month info becomes available, the cell immediately gets the level 
  ## of that info instead of increasing maxinally 1 level from the lowest level (which should be the level of no info)
  for (i in 1:18)
  {for (j in 1:13)
  {
    updateCOD[i,j,k+1] <- ifelse(is.na(updateCOD[i,j,k]),RTI_grid_COD_1_levels[i,j],ifelse(is.na(RTI_grid_COD_1_levels[i,j]),updateCOD[i,j,k],ifelse(RTI_grid_COD_1_levels[i,j]>updateCOD[i,j,k],updateCOD[i,j,k]+1,ifelse(RTI_grid_COD_1_levels[i,j]<updateCOD[i,j,k],updateCOD[i,j,k]-1,updateCOD[i,j,k]))))
    updatePLE[i,j,k+1] <- ifelse(is.na(updatePLE[i,j,k]),RTI_grid_PLE_1_levels[i,j],ifelse(is.na(RTI_grid_PLE_1_levels[i,j]),updatePLE[i,j,k],ifelse(RTI_grid_PLE_1_levels[i,j]>updatePLE[i,j,k],updatePLE[i,j,k]+1,ifelse(RTI_grid_PLE_1_levels[i,j]<updatePLE[i,j,k],updatePLE[i,j,k]-1,updatePLE[i,j,k]))))
    #take the highest value of the two
    updateCOMBINED[i,j,k+1]<-max(updateCOD[i,j,k+1],updatePLE[i,j,k+1])
  }}
}  


## in the simulation I want to see whether fishing according to RTIs for month k with actual relative CPUE as in month k leads to overshoot
## fishing with let's say 1000 RTIs (1 RTI is 5 fishing day equivalents, spent in one month in one cell) e.g. 25 boats 40 months
## so pick a position in the i,j,k array, look up it's level, translate that into #RTIs and cumulate them until the total is 1000; use 'while'
## check with the i,j,k pointer in the newinfo what the relative CPUE was and cumulate that - if over 1000 it is an overshoot

## the array and z (below) are 100 for trying-out (because the simulations are quite slow); for the final results they should be 1000
catchdist<-array(dim=c(1000,2))
for (z in 1:1000)
{
  total_catchCOD<-0
  total_catchPLE<-0
  cumulateRTI<-0
  while(cumulateRTI<2170) # the following loop goes on as long as the unequality is fulfilled
  { # choose each time a random cell in a random month
    # I made the "field" a bit smaller here, because at the edges were so many cells with NA and they slow down the simulations very much
    a <-   round(runif(1,min=1.5,max=18.49)) # a and b are coordinates of a random cell
    b <-   round(runif(1,min=2.5,max=12.49))
    d <-   round(runif(1,min=0.5,max=12.49)) # d is a random month
    cell_in_updateCOMBINED<- updateCOMBINED[a,b,d]
    #### remember that newinfo has the real actual catches of both species in each cell and each month
    # if there was no catch in month d in that cell, we have to choose a new one (no cumulation, i.e cum=0); if the cell has NA, this mean it was never fished in, we choose a new one (no cumulation), else we translate the levels back to RTI tariffs and cumulate; note that level 6 = closed and no fishing takes place
    cum<-ifelse(is.na(newinfo[a,b,d,1])&&is.na(newinfo[a,b,d,2]),0,ifelse(is.na(cell_in_updateCOMBINED),0,ifelse(cell_in_updateCOMBINED==1,0.1,ifelse(cell_in_updateCOMBINED==2,0.5,ifelse(cell_in_updateCOMBINED==3,1,ifelse(cell_in_updateCOMBINED==4,2,ifelse(cell_in_updateCOMBINED==5,5,0)))))))
    cumulateRTI<-cumulateRTI+cum
    catchCOD<-ifelse(is.na(newinfo[a,b,d,1]),0,ifelse(cum==0,0,newinfo[a,b,d,1]))
    catchPLE<-ifelse(is.na(newinfo[a,b,d,2]),0,ifelse(cum==0,0,newinfo[a,b,d,2]))
    total_catchCOD<-total_catchCOD+catchCOD
    total_catchPLE<-total_catchPLE+catchPLE
  }
  catchdist[z,1]<-total_catchCOD/1000
  catchdist[z,2]<-total_catchPLE/1000
}

#COD
# % below 0.8
sum(catchdist[,1]<0.8)
# % between 0.8 and 0.9
sum(catchdist[,1]<0.9)-sum(catchdist[,1]<0.8)
# % between 0.9 and 1
sum(catchdist[,1]<1)-sum(catchdist[,1]<0.9)
# % between 1 and 1.1
sum(catchdist[,1]<1.1)-sum(catchdist[,1]<1)
# % 1.1 and above
sum(catchdist[,1]>=1.1)



#PLE
# % below 0.8
sum(catchdist[,2]<0.8)
# % between 0.8 and 0.9
sum(catchdist[,2]<0.9)-sum(catchdist[,2]<0.8)
# % between 0.9 and 1
sum(catchdist[,2]<1)-sum(catchdist[,2]<0.9)
# % between 1 and 1.1
sum(catchdist[,2]<1.1)-sum(catchdist[,2]<1)
# % 1.1 and above
sum(catchdist[,2]>=1.1)

hist(catchdist[,1],50,xlim= c(0.7,1.2), panel.nul = rect(0.4, 0, 0.6, 1e6, col='#98FB98', border=NA), panel.first = rect(c(0,0.6), 0, c(0.4,1), 1e6, col='#FFED83', border=NA), panel.second = rect(-1, 0, 0.2, 1e6, col='#F98B88', border=NA), panel.third = rect(0.8, 0, 1, 1e6, col='#F98B88', border=NA), main='distribution of relative cod catch',xlab='ratio actual catch : intended catch')
abline(v=1.0,col=2,lwd=2,lty=5)

hist(catchdist[,2],50,xlim= c(0.7,1.2), panel.nul = rect(0.4, 0, 0.6, 1e6, col='#98FB98', border=NA), panel.first = rect(c(0,0.6), 0, c(0.4,1), 1e6, col='#FFED83', border=NA), panel.second = rect(-1, 0, 0.2, 1e6, col='#F98B88', border=NA), panel.third = rect(0.8, 0, 1, 1e6, col='#F98B88', border=NA), main='distribution of relative plaice catch',xlab='ratio actual catch : intended catch')
abline(v=1.0,col=2,lwd=2,lty=5)

#hist(catchdist[,1],50,xlim= c(0.7,1.2), panel.first = rect(c(0,0.6), 0, c(0.4,1), 1e6, col='light grey', border=NA), panel.second = rect(-1, 0, 0.2, 1e6, col='dark grey', border=NA), panel.third = rect(0.8, 0, 1, 1e6, col='dark grey', border=NA), main='distribution of relative cod catch',xlab='ratio actual catch : intended catch')
#abline(v=1.0,col=1,lwd=2,lty=5)

#hist(catchdist[,2],50,xlim= c(0.7,1.2), panel.first = rect(c(0,0.6), 0, c(0.4,1), 1e6, col='light grey', border=NA), panel.second = rect(-1, 0, 0.2, 1e6, col='dark grey', border=NA), panel.third = rect(0.8, 0, 1, 1e6, col='dark grey', border=NA), main='distribution of relative plaice catch',xlab='ratio actual catch : intended catch')
#abline(v=1.0,col=1,lwd=2,lty=5)
