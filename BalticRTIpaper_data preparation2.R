rm(list=ls())
library(vmstools)
# read in the data that were prepared by CvD with standardized procedures used to process and merge the VMS and logbook data (Hintzen et al., 2012).
BalticRTIpaper_data<-read.table("CSquares_mbcg_aggr_2013-2018.csv",header=TRUE,sep=",", fill=T)
BalticRTIpaper_data<-data.frame(BalticRTIpaper_data)
# 61411 observations

# only OTB
BalticRTIpaper_data<-subset(BalticRTIpaper_data, Gear_code=="OTB")
# 57042 observations

# only OTB_DEF_>=105_1_120
BalticRTIpaper_data<-subset(BalticRTIpaper_data, Metier=="OTB_DEF_>=105_1_120")
# 40064 observations

# only observation with non-zero catch of either cod or plaice
BalticRTIpaper_data<-subset(BalticRTIpaper_data, LE_KG_COD + LE_KG_PLE>0)
# 38443 observations

# add the FAO area to the data (SD 22, 24 and 25)
data(ICESareas)
res   <- data.frame(CSquare2LonLat(as.character(BalticRTIpaper_data$CSquare),0.05))
areas <- ICESarea(res,ICESareas)
BalticRTIpaper_data$SD<-ICESareas@data[ICESarea(res,ICESareas ,proj4string=NULL,fast=TRUE),]$SubDivisio

# add statistical rectangle to the data
BalticRTIpaper_data$rectangle<-ICESrectangle(CSquare2LonLat(as.character(BalticRTIpaper_data$CSquare),0.05))

tbl <- table(BalticRTIpaper_data$SD)
tbl

# only observations in SD 22, 24 and 25 (that is, exclude SD 26)
BalticRTIpaper_data<-subset(BalticRTIpaper_data, SD!=26)
# 37786 observations
# 23 observations with SD = NA are removed with this operation; probably they had coordinates on land

library(lubridate)
# add year, quarter, month and week to the data
BalticRTIpaper_data$year<-year(as.POSIXlt(BalticRTIpaper_data$Date, format="%d/%m/%Y"))
BalticRTIpaper_data$quarter<-quarter(as.POSIXlt(BalticRTIpaper_data$Date, format="%d/%m/%Y"))
BalticRTIpaper_data$month<-month(as.POSIXlt(BalticRTIpaper_data$Date, format="%d/%m/%Y"))
BalticRTIpaper_data$week<-week(as.POSIXlt(BalticRTIpaper_data$Date, format="%d/%m/%Y"))

# only observations in 2014-2018 (that is, exclude 2013)
BalticRTIpaper_data<-subset(BalticRTIpaper_data, year!=2013)

# read in the raising factors by year, SD and quarter; based on Sven Stötera's observer data and filling in
extrapolation_data<-read.table("extrapolated_values_Z.csv",header=TRUE,sep=";", fill=T)
extrapolation_data<-data.frame(extrapolation_data)

# merge, so that the raising factors become incorporated in the main file
BalticRTIpaper_data<-merge(BalticRTIpaper_data,extrapolation_data)

# raise the catches with their raising factors (that is raise from landings to total catches including estimated discards)
BalticRTIpaper_data$raised_catch_cod<-BalticRTIpaper_data$extrapolated_COD*BalticRTIpaper_data$LE_KG_COD
BalticRTIpaper_data$raised_catch_ple<-BalticRTIpaper_data$extrapolated_PLE*BalticRTIpaper_data$LE_KG_PLE

# calculate CPUE
BalticRTIpaper_data$CPUEcod<-BalticRTIpaper_data$raised_catch_cod/BalticRTIpaper_data$Effort_hrs
BalticRTIpaper_data$CPUEple<-BalticRTIpaper_data$raised_catch_ple/BalticRTIpaper_data$Effort_hrs

# find outliers
plot(BalticRTIpaper_data$Effort_hrs,BalticRTIpaper_data$CPUEcod,xlab='Effort (h)', ylab='CPUE (kg/h)',xlim=c(0,50))
abline(v=0.5,col=2)

plot(BalticRTIpaper_data$Effort_hrs,BalticRTIpaper_data$CPUEple,xlab='Effort (h)', ylab='CPUE (kg/h)',xlim=c(0,50))
abline(v=0.5,col=2)

# exclude outliers
BalticRTIpaper_data<-subset(BalticRTIpaper_data, Effort_hrs>0.5)



write.csv(BalticRTIpaper_data,"BalticRTIpaper_data.csv", row.names = TRUE)
