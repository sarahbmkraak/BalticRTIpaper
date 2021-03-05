#### correlations between cell's CPUEs from week to week or month to month
rm(list=ls())
library(maps)
library(mapdata)
library(mapplots)
library(dplyr)
library(vmstools)
library(tidyr)
library(lubridate)
maps_data<-read.table("BalticRTIpaper_data.csv",header=TRUE,sep=",", fill=T)
maps_data<-data.frame(maps_data)

maps_data$lat_fine<-data.frame(CSquare2LonLat(as.character(maps_data$CSquare),0.05))$SI_LATI
maps_data$lon_fine<-data.frame(CSquare2LonLat(as.character(maps_data$CSquare),0.05))$SI_LONG

xlim <- c(9.5,21.5)
ylim <- c(53.5,56.5)
maps_data<-subset(maps_data, lat_fine>53.5&&lat_fine<56.5&&lon_fine>9.5&&lon_fine<21.5)

### CSquares-mid are at a resolution of 0.1 by 0.1 degrees
# by deleting the last two characters from the CSquare string of 0.05 x 0.05 degrees we arrive at a string for 0.1 x 0.1 degree
maps_data$CSquare_mid <- substr(maps_data$CSquare,1,nchar(as.character(maps_data$CSquare))-2)


### CPUE aggregated by CSquares_mid and weeks ###
aggCOD1catch<- aggregate(list(catch=maps_data$raised_catch_cod),list(CSquare_mid=maps_data$CSquare_mid,week=maps_data$week, year=maps_data$year),sum, na.rm=TRUE)
aggCOD1effort<- aggregate(list(effort=maps_data$Effort_hrs),list(CSquare_mid=maps_data$CSquare_mid,week=maps_data$week, year=maps_data$year),sum, na.rm=TRUE)
aggCOD1<-aggCOD1effort
aggCOD1$NEW_CPUE_COD<-aggCOD1catch$catch/aggCOD1effort$effort
aggCOD1$effort<-NULL

### COD 2014 ###
library(dplyr) #filter needs this package
aggCOD1_2014<-data.frame(filter(aggCOD1, aggCOD1$year=="2014"))
correlation_COD_2014<-spread(aggCOD1_2014, week, NEW_CPUE_COD)

library(data.table)

COD_2014<-data.table(
cor(correlation_COD_2014$"1", correlation_COD_2014$"2",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2014$"2", correlation_COD_2014$"3",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2014$"3", correlation_COD_2014$"4",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2014$"4", correlation_COD_2014$"5",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2014$"5", correlation_COD_2014$"6",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2014$"6", correlation_COD_2014$"7",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2014$"7", correlation_COD_2014$"8",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2014$"8", correlation_COD_2014$"9",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2014$"9", correlation_COD_2014$"10",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2014$"10", correlation_COD_2014$"11",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2014$"11", correlation_COD_2014$"12",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2014$"12", correlation_COD_2014$"13",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2014$"13", correlation_COD_2014$"14",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2014$"14", correlation_COD_2014$"15",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2014$"15", correlation_COD_2014$"16",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2014$"16", correlation_COD_2014$"17",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2014$"17", correlation_COD_2014$"18",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2014$"18", correlation_COD_2014$"19",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2014$"19", correlation_COD_2014$"20",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2014$"20", correlation_COD_2014$"21",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2014$"21", correlation_COD_2014$"22",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2014$"22", correlation_COD_2014$"23",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2014$"23", correlation_COD_2014$"24",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2014$"24", correlation_COD_2014$"25",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2014$"25", correlation_COD_2014$"26",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2014$"26", correlation_COD_2014$"27",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2014$"27", correlation_COD_2014$"28",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2014$"28", correlation_COD_2014$"29",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2014$"29", correlation_COD_2014$"30",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2014$"30", correlation_COD_2014$"31",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2014$"31", correlation_COD_2014$"32",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2014$"32", correlation_COD_2014$"33",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2014$"33", correlation_COD_2014$"34",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2014$"34", correlation_COD_2014$"35",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2014$"35", correlation_COD_2014$"36",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2014$"36", correlation_COD_2014$"37",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2014$"37", correlation_COD_2014$"38",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2014$"38", correlation_COD_2014$"39",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2014$"39", correlation_COD_2014$"40",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2014$"40", correlation_COD_2014$"41",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2014$"41", correlation_COD_2014$"42",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2014$"42", correlation_COD_2014$"43",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2014$"43", correlation_COD_2014$"44",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2014$"44", correlation_COD_2014$"45",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2014$"45", correlation_COD_2014$"46",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2014$"46", correlation_COD_2014$"47",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2014$"47", correlation_COD_2014$"48",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2014$"48", correlation_COD_2014$"49",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2014$"49", correlation_COD_2014$"50",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2014$"50", correlation_COD_2014$"51",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2014$"51", correlation_COD_2014$"52",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2014$"52", correlation_COD_2014$"53",use="pairwise.complete.obs", method = "pearson"))
write.table(COD_2014, file = "COD_weeks.csv", sep = ",", append = TRUE, col.names = T, row.names = F)

### COD 2015 ###

aggCOD1_2015<-data.frame(filter(aggCOD1, aggCOD1$year=="2015"))
correlation_COD_2015<-spread(aggCOD1_2015, week, NEW_CPUE_COD)

COD_2015<-data.table(
cor(correlation_COD_2015$"1", correlation_COD_2015$"2",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2015$"2", correlation_COD_2015$"3",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2015$"3", correlation_COD_2015$"4",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2015$"4", correlation_COD_2015$"5",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2015$"5", correlation_COD_2015$"6",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2015$"6", correlation_COD_2015$"7",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2015$"7", correlation_COD_2015$"8",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2015$"8", correlation_COD_2015$"9",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2015$"9", correlation_COD_2015$"10",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2015$"10", correlation_COD_2015$"11",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2015$"11", correlation_COD_2015$"12",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2015$"12", correlation_COD_2015$"13",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2015$"13", correlation_COD_2015$"14",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2015$"14", correlation_COD_2015$"15",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2015$"15", correlation_COD_2015$"16",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2015$"16", correlation_COD_2015$"17",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2015$"17", correlation_COD_2015$"18",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2015$"18", correlation_COD_2015$"19",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2015$"19", correlation_COD_2015$"20",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2015$"20", correlation_COD_2015$"21",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2015$"21", correlation_COD_2015$"22",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2015$"22", correlation_COD_2015$"23",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2015$"23", correlation_COD_2015$"24",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2015$"24", correlation_COD_2015$"25",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2015$"25", correlation_COD_2015$"26",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2015$"26", correlation_COD_2015$"27",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2015$"27", correlation_COD_2015$"28",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2015$"28", correlation_COD_2015$"29",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2015$"29", correlation_COD_2015$"30",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2015$"30", correlation_COD_2015$"31",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2015$"31", correlation_COD_2015$"32",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2015$"32", correlation_COD_2015$"33",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2015$"33", correlation_COD_2015$"34",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2015$"34", correlation_COD_2015$"35",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2015$"35", correlation_COD_2015$"36",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2015$"36", correlation_COD_2015$"37",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2015$"37", correlation_COD_2015$"38",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2015$"38", correlation_COD_2015$"39",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2015$"39", correlation_COD_2015$"40",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2015$"40", correlation_COD_2015$"41",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2015$"41", correlation_COD_2015$"42",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2015$"42", correlation_COD_2015$"43",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2015$"43", correlation_COD_2015$"44",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2015$"44", correlation_COD_2015$"45",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2015$"45", correlation_COD_2015$"46",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2015$"46", correlation_COD_2015$"47",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2015$"47", correlation_COD_2015$"48",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2015$"48", correlation_COD_2015$"49",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2015$"49", correlation_COD_2015$"50",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2015$"50", correlation_COD_2015$"51",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2015$"51", correlation_COD_2015$"52",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2015$"52", correlation_COD_2015$"53",use="pairwise.complete.obs", method = "pearson"))
write.table(COD_2015, file = "COD_weeks.csv", sep = ",", append = TRUE, col.names = F, row.names = F)

### COD 2016 ###

aggCOD1_2016<-data.frame(filter(aggCOD1, aggCOD1$year=="2016"))
correlation_COD_2016<-spread(aggCOD1_2016, week, NEW_CPUE_COD)

COD_2016<-data.table(
cor(correlation_COD_2016$"1", correlation_COD_2016$"2",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2016$"2", correlation_COD_2016$"3",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2016$"3", correlation_COD_2016$"4",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2016$"4", correlation_COD_2016$"5",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2016$"5", correlation_COD_2016$"6",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2016$"6", correlation_COD_2016$"7",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2016$"7", correlation_COD_2016$"8",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2016$"8", correlation_COD_2016$"9",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2016$"9", correlation_COD_2016$"10",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2016$"10", correlation_COD_2016$"11",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2016$"11", correlation_COD_2016$"12",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2016$"12", correlation_COD_2016$"13",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2016$"13", correlation_COD_2016$"14",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2016$"14", correlation_COD_2016$"15",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2016$"15", correlation_COD_2016$"16",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2016$"16", correlation_COD_2016$"17",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2016$"17", correlation_COD_2016$"18",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2016$"18", correlation_COD_2016$"19",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2016$"19", correlation_COD_2016$"20",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2016$"20", correlation_COD_2016$"21",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2016$"21", correlation_COD_2016$"22",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2016$"22", correlation_COD_2016$"23",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2016$"23", correlation_COD_2016$"24",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2016$"24", correlation_COD_2016$"25",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2016$"25", correlation_COD_2016$"26",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2016$"26", correlation_COD_2016$"27",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2016$"27", correlation_COD_2016$"28",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2016$"28", correlation_COD_2016$"29",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2016$"29", correlation_COD_2016$"30",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2016$"30", correlation_COD_2016$"31",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2016$"31", correlation_COD_2016$"32",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2016$"32", correlation_COD_2016$"33",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2016$"33", correlation_COD_2016$"34",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2016$"34", correlation_COD_2016$"35",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2016$"35", correlation_COD_2016$"36",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2016$"36", correlation_COD_2016$"37",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2016$"37", correlation_COD_2016$"38",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2016$"38", correlation_COD_2016$"39",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2016$"39", correlation_COD_2016$"40",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2016$"40", correlation_COD_2016$"41",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2016$"41", correlation_COD_2016$"42",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2016$"42", correlation_COD_2016$"43",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2016$"43", correlation_COD_2016$"44",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2016$"44", correlation_COD_2016$"45",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2016$"45", correlation_COD_2016$"46",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2016$"46", correlation_COD_2016$"47",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2016$"47", correlation_COD_2016$"48",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2016$"48", correlation_COD_2016$"49",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2016$"49", correlation_COD_2016$"50",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2016$"50", correlation_COD_2016$"51",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2016$"51", correlation_COD_2016$"52",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2016$"52", correlation_COD_2016$"53",use="pairwise.complete.obs", method = "pearson"))
write.table(COD_2016, file = "COD_weeks.csv", sep = ",", append = TRUE, col.names = F, row.names = F)

### COD 2017 ###

aggCOD1_2017<-data.frame(filter(aggCOD1, aggCOD1$year=="2017"))
correlation_COD_2017<-spread(aggCOD1_2017, week, NEW_CPUE_COD)

COD_2017<-data.table(
cor(correlation_COD_2017$"1", correlation_COD_2017$"2",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2017$"2", correlation_COD_2017$"3",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2017$"3", correlation_COD_2017$"4",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2017$"4", correlation_COD_2017$"5",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2017$"5", correlation_COD_2017$"6",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2017$"6", correlation_COD_2017$"7",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2017$"7", correlation_COD_2017$"8",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2017$"8", correlation_COD_2017$"9",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2017$"9", correlation_COD_2017$"10",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2017$"10", correlation_COD_2017$"11",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2017$"11", correlation_COD_2017$"12",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2017$"12", correlation_COD_2017$"13",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2017$"13", correlation_COD_2017$"14",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2017$"14", correlation_COD_2017$"15",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2017$"15", correlation_COD_2017$"16",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2017$"16", correlation_COD_2017$"17",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2017$"17", correlation_COD_2017$"18",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2017$"18", correlation_COD_2017$"19",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2017$"19", correlation_COD_2017$"20",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2017$"20", correlation_COD_2017$"21",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2017$"21", correlation_COD_2017$"22",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2017$"22", correlation_COD_2017$"23",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2017$"23", correlation_COD_2017$"24",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2017$"24", correlation_COD_2017$"25",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2017$"25", correlation_COD_2017$"26",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2017$"26", correlation_COD_2017$"27",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2017$"27", correlation_COD_2017$"28",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2017$"28", correlation_COD_2017$"29",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2017$"29", correlation_COD_2017$"30",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2017$"30", correlation_COD_2017$"31",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2017$"31", correlation_COD_2017$"32",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2017$"32", correlation_COD_2017$"33",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2017$"33", correlation_COD_2017$"34",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2017$"34", correlation_COD_2017$"35",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2017$"35", correlation_COD_2017$"36",use="pairwise.complete.obs", method = "pearson"),
#cor(correlation_COD_2017$"36", correlation_COD_2017$"37",use="pairwise.complete.obs", method = "pearson"),
#cor(correlation_COD_2017$"37", correlation_COD_2017$"38",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2017$"38", correlation_COD_2017$"39",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2017$"39", correlation_COD_2017$"40",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2017$"40", correlation_COD_2017$"41",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2017$"41", correlation_COD_2017$"42",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2017$"42", correlation_COD_2017$"43",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2017$"43", correlation_COD_2017$"44",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2017$"44", correlation_COD_2017$"45",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2017$"45", correlation_COD_2017$"46",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2017$"46", correlation_COD_2017$"47",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2017$"47", correlation_COD_2017$"48",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2017$"48", correlation_COD_2017$"49",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2017$"49", correlation_COD_2017$"50",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2017$"50", correlation_COD_2017$"51",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2017$"51", correlation_COD_2017$"52",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2017$"52", correlation_COD_2017$"53",use="pairwise.complete.obs", method = "pearson"))
write.table(COD_2017, file = "COD_weeks.csv", sep = ",", append = TRUE, col.names = F, row.names = F)

### COD 2018 ###

aggCOD1_2018<-data.frame(filter(aggCOD1, aggCOD1$year=="2018"))
correlation_COD_2018<-spread(aggCOD1_2018, week, NEW_CPUE_COD)

COD_2018<-data.table(
cor(correlation_COD_2018$"1", correlation_COD_2018$"2",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2018$"2", correlation_COD_2018$"3",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2018$"3", correlation_COD_2018$"4",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2018$"4", correlation_COD_2018$"5",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2018$"5", correlation_COD_2018$"6",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2018$"6", correlation_COD_2018$"7",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2018$"7", correlation_COD_2018$"8",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2018$"8", correlation_COD_2018$"9",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2018$"9", correlation_COD_2018$"10",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2018$"10", correlation_COD_2018$"11",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2018$"11", correlation_COD_2018$"12",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2018$"12", correlation_COD_2018$"13",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2018$"13", correlation_COD_2018$"14",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2018$"14", correlation_COD_2018$"15",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2018$"15", correlation_COD_2018$"16",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2018$"16", correlation_COD_2018$"17",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2018$"17", correlation_COD_2018$"18",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2018$"18", correlation_COD_2018$"19",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2018$"19", correlation_COD_2018$"20",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2018$"20", correlation_COD_2018$"21",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2018$"21", correlation_COD_2018$"22",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2018$"22", correlation_COD_2018$"23",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2018$"23", correlation_COD_2018$"24",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2018$"24", correlation_COD_2018$"25",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2018$"25", correlation_COD_2018$"26",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2018$"26", correlation_COD_2018$"27",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2018$"27", correlation_COD_2018$"28",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2018$"28", correlation_COD_2018$"29",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2018$"29", correlation_COD_2018$"30",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2018$"30", correlation_COD_2018$"31",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2018$"31", correlation_COD_2018$"32",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2018$"32", correlation_COD_2018$"33",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2018$"33", correlation_COD_2018$"34",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2018$"34", correlation_COD_2018$"35",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2018$"35", correlation_COD_2018$"36",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2018$"36", correlation_COD_2018$"37",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2018$"37", correlation_COD_2018$"38",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2018$"38", correlation_COD_2018$"39",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2018$"39", correlation_COD_2018$"40",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2018$"40", correlation_COD_2018$"41",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2018$"41", correlation_COD_2018$"42",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2018$"42", correlation_COD_2018$"43",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2018$"43", correlation_COD_2018$"44",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2018$"44", correlation_COD_2018$"45",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2018$"45", correlation_COD_2018$"46",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2018$"46", correlation_COD_2018$"47",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2018$"47", correlation_COD_2018$"48",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2018$"48", correlation_COD_2018$"49",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2018$"49", correlation_COD_2018$"50",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2018$"50", correlation_COD_2018$"51",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2018$"51", correlation_COD_2018$"52",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_COD_2018$"52", correlation_COD_2018$"53",use="pairwise.complete.obs", method = "pearson"))
write.table(COD_2018, file = "COD_weeks.csv", sep = ",", append = TRUE, col.names = F, row.names = F)

### CPUE aggregated by CSquares_mid and weeks ###
aggPLE1catch<- aggregate(list(catch=maps_data$raised_catch_ple),list(CSquare_mid=maps_data$CSquare_mid,week=maps_data$week, year=maps_data$year),sum, na.rm=TRUE)
aggPLE1effort<- aggregate(list(effort=maps_data$Effort_hrs),list(CSquare_mid=maps_data$CSquare_mid,week=maps_data$week, year=maps_data$year),sum, na.rm=TRUE)
aggPLE1<-aggPLE1effort
aggPLE1$NEW_CPUE_PLE<-aggPLE1catch$catch/aggPLE1effort$effort
aggPLE1$effort<-NULL

### PLE 2014 ###
library(dplyr) #filter needs this package
aggPLE1_2014<-data.frame(filter(aggPLE1, aggPLE1$year=="2014"))
correlation_PLE_2014<-spread(aggPLE1_2014, week, NEW_CPUE_PLE)

PLE_2014<-data.table(
cor(correlation_PLE_2014$"1", correlation_PLE_2014$"2",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2014$"2", correlation_PLE_2014$"3",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2014$"3", correlation_PLE_2014$"4",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2014$"4", correlation_PLE_2014$"5",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2014$"5", correlation_PLE_2014$"6",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2014$"6", correlation_PLE_2014$"7",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2014$"7", correlation_PLE_2014$"8",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2014$"8", correlation_PLE_2014$"9",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2014$"9", correlation_PLE_2014$"10",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2014$"10", correlation_PLE_2014$"11",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2014$"11", correlation_PLE_2014$"12",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2014$"12", correlation_PLE_2014$"13",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2014$"13", correlation_PLE_2014$"14",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2014$"14", correlation_PLE_2014$"15",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2014$"15", correlation_PLE_2014$"16",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2014$"16", correlation_PLE_2014$"17",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2014$"17", correlation_PLE_2014$"18",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2014$"18", correlation_PLE_2014$"19",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2014$"19", correlation_PLE_2014$"20",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2014$"20", correlation_PLE_2014$"21",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2014$"21", correlation_PLE_2014$"22",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2014$"22", correlation_PLE_2014$"23",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2014$"23", correlation_PLE_2014$"24",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2014$"24", correlation_PLE_2014$"25",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2014$"25", correlation_PLE_2014$"26",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2014$"26", correlation_PLE_2014$"27",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2014$"27", correlation_PLE_2014$"28",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2014$"28", correlation_PLE_2014$"29",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2014$"29", correlation_PLE_2014$"30",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2014$"30", correlation_PLE_2014$"31",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2014$"31", correlation_PLE_2014$"32",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2014$"32", correlation_PLE_2014$"33",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2014$"33", correlation_PLE_2014$"34",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2014$"34", correlation_PLE_2014$"35",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2014$"35", correlation_PLE_2014$"36",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2014$"36", correlation_PLE_2014$"37",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2014$"37", correlation_PLE_2014$"38",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2014$"38", correlation_PLE_2014$"39",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2014$"39", correlation_PLE_2014$"40",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2014$"40", correlation_PLE_2014$"41",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2014$"41", correlation_PLE_2014$"42",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2014$"42", correlation_PLE_2014$"43",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2014$"43", correlation_PLE_2014$"44",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2014$"44", correlation_PLE_2014$"45",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2014$"45", correlation_PLE_2014$"46",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2014$"46", correlation_PLE_2014$"47",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2014$"47", correlation_PLE_2014$"48",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2014$"48", correlation_PLE_2014$"49",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2014$"49", correlation_PLE_2014$"50",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2014$"50", correlation_PLE_2014$"51",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2014$"51", correlation_PLE_2014$"52",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2014$"52", correlation_PLE_2014$"53",use="pairwise.complete.obs", method = "pearson"))
write.table(PLE_2014, file = "PLE_weeks.csv", sep = ",", append = TRUE, col.names = T, row.names = F)

### PLE 2015 ###

aggPLE1_2015<-data.frame(filter(aggPLE1, aggPLE1$year=="2015"))
correlation_PLE_2015<-spread(aggPLE1_2015, week, NEW_CPUE_PLE)

PLE_2015<-data.table(
cor(correlation_PLE_2015$"1", correlation_PLE_2015$"2",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2015$"2", correlation_PLE_2015$"3",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2015$"3", correlation_PLE_2015$"4",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2015$"4", correlation_PLE_2015$"5",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2015$"5", correlation_PLE_2015$"6",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2015$"6", correlation_PLE_2015$"7",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2015$"7", correlation_PLE_2015$"8",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2015$"8", correlation_PLE_2015$"9",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2015$"9", correlation_PLE_2015$"10",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2015$"10", correlation_PLE_2015$"11",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2015$"11", correlation_PLE_2015$"12",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2015$"12", correlation_PLE_2015$"13",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2015$"13", correlation_PLE_2015$"14",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2015$"14", correlation_PLE_2015$"15",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2015$"15", correlation_PLE_2015$"16",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2015$"16", correlation_PLE_2015$"17",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2015$"17", correlation_PLE_2015$"18",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2015$"18", correlation_PLE_2015$"19",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2015$"19", correlation_PLE_2015$"20",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2015$"20", correlation_PLE_2015$"21",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2015$"21", correlation_PLE_2015$"22",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2015$"22", correlation_PLE_2015$"23",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2015$"23", correlation_PLE_2015$"24",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2015$"24", correlation_PLE_2015$"25",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2015$"25", correlation_PLE_2015$"26",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2015$"26", correlation_PLE_2015$"27",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2015$"27", correlation_PLE_2015$"28",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2015$"28", correlation_PLE_2015$"29",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2015$"29", correlation_PLE_2015$"30",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2015$"30", correlation_PLE_2015$"31",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2015$"31", correlation_PLE_2015$"32",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2015$"32", correlation_PLE_2015$"33",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2015$"33", correlation_PLE_2015$"34",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2015$"34", correlation_PLE_2015$"35",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2015$"35", correlation_PLE_2015$"36",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2015$"36", correlation_PLE_2015$"37",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2015$"37", correlation_PLE_2015$"38",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2015$"38", correlation_PLE_2015$"39",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2015$"39", correlation_PLE_2015$"40",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2015$"40", correlation_PLE_2015$"41",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2015$"41", correlation_PLE_2015$"42",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2015$"42", correlation_PLE_2015$"43",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2015$"43", correlation_PLE_2015$"44",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2015$"44", correlation_PLE_2015$"45",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2015$"45", correlation_PLE_2015$"46",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2015$"46", correlation_PLE_2015$"47",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2015$"47", correlation_PLE_2015$"48",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2015$"48", correlation_PLE_2015$"49",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2015$"49", correlation_PLE_2015$"50",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2015$"50", correlation_PLE_2015$"51",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2015$"51", correlation_PLE_2015$"52",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2015$"52", correlation_PLE_2015$"53",use="pairwise.complete.obs", method = "pearson"))
write.table(PLE_2015, file = "PLE_weeks.csv", sep = ",", append = TRUE, col.names = F, row.names = F)

### PLE 2016 ###

aggPLE1_2016<-data.frame(filter(aggPLE1, aggPLE1$year=="2016"))
correlation_PLE_2016<-spread(aggPLE1_2016, week, NEW_CPUE_PLE)

PLE_2016<-data.table(
cor(correlation_PLE_2016$"1", correlation_PLE_2016$"2",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2016$"2", correlation_PLE_2016$"3",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2016$"3", correlation_PLE_2016$"4",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2016$"4", correlation_PLE_2016$"5",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2016$"5", correlation_PLE_2016$"6",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2016$"6", correlation_PLE_2016$"7",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2016$"7", correlation_PLE_2016$"8",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2016$"8", correlation_PLE_2016$"9",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2016$"9", correlation_PLE_2016$"10",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2016$"10", correlation_PLE_2016$"11",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2016$"11", correlation_PLE_2016$"12",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2016$"12", correlation_PLE_2016$"13",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2016$"13", correlation_PLE_2016$"14",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2016$"14", correlation_PLE_2016$"15",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2016$"15", correlation_PLE_2016$"16",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2016$"16", correlation_PLE_2016$"17",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2016$"17", correlation_PLE_2016$"18",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2016$"18", correlation_PLE_2016$"19",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2016$"19", correlation_PLE_2016$"20",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2016$"20", correlation_PLE_2016$"21",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2016$"21", correlation_PLE_2016$"22",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2016$"22", correlation_PLE_2016$"23",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2016$"23", correlation_PLE_2016$"24",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2016$"24", correlation_PLE_2016$"25",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2016$"25", correlation_PLE_2016$"26",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2016$"26", correlation_PLE_2016$"27",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2016$"27", correlation_PLE_2016$"28",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2016$"28", correlation_PLE_2016$"29",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2016$"29", correlation_PLE_2016$"30",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2016$"30", correlation_PLE_2016$"31",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2016$"31", correlation_PLE_2016$"32",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2016$"32", correlation_PLE_2016$"33",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2016$"33", correlation_PLE_2016$"34",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2016$"34", correlation_PLE_2016$"35",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2016$"35", correlation_PLE_2016$"36",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2016$"36", correlation_PLE_2016$"37",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2016$"37", correlation_PLE_2016$"38",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2016$"38", correlation_PLE_2016$"39",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2016$"39", correlation_PLE_2016$"40",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2016$"40", correlation_PLE_2016$"41",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2016$"41", correlation_PLE_2016$"42",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2016$"42", correlation_PLE_2016$"43",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2016$"43", correlation_PLE_2016$"44",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2016$"44", correlation_PLE_2016$"45",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2016$"45", correlation_PLE_2016$"46",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2016$"46", correlation_PLE_2016$"47",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2016$"47", correlation_PLE_2016$"48",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2016$"48", correlation_PLE_2016$"49",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2016$"49", correlation_PLE_2016$"50",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2016$"50", correlation_PLE_2016$"51",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2016$"51", correlation_PLE_2016$"52",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2016$"52", correlation_PLE_2016$"53",use="pairwise.complete.obs", method = "pearson"))
write.table(PLE_2016, file = "PLE_weeks.csv", sep = ",", append = TRUE, col.names = F, row.names = F)

### PLE 2017 ###

aggPLE1_2017<-data.frame(filter(aggPLE1, aggPLE1$year=="2017"))
correlation_PLE_2017<-spread(aggPLE1_2017, week, NEW_CPUE_PLE)

PLE_2017<-data.table(
cor(correlation_PLE_2017$"1", correlation_PLE_2017$"2",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2017$"2", correlation_PLE_2017$"3",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2017$"3", correlation_PLE_2017$"4",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2017$"4", correlation_PLE_2017$"5",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2017$"5", correlation_PLE_2017$"6",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2017$"6", correlation_PLE_2017$"7",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2017$"7", correlation_PLE_2017$"8",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2017$"8", correlation_PLE_2017$"9",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2017$"9", correlation_PLE_2017$"10",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2017$"10", correlation_PLE_2017$"11",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2017$"11", correlation_PLE_2017$"12",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2017$"12", correlation_PLE_2017$"13",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2017$"13", correlation_PLE_2017$"14",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2017$"14", correlation_PLE_2017$"15",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2017$"15", correlation_PLE_2017$"16",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2017$"16", correlation_PLE_2017$"17",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2017$"17", correlation_PLE_2017$"18",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2017$"18", correlation_PLE_2017$"19",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2017$"19", correlation_PLE_2017$"20",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2017$"20", correlation_PLE_2017$"21",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2017$"21", correlation_PLE_2017$"22",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2017$"22", correlation_PLE_2017$"23",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2017$"23", correlation_PLE_2017$"24",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2017$"24", correlation_PLE_2017$"25",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2017$"25", correlation_PLE_2017$"26",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2017$"26", correlation_PLE_2017$"27",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2017$"27", correlation_PLE_2017$"28",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2017$"28", correlation_PLE_2017$"29",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2017$"29", correlation_PLE_2017$"30",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2017$"30", correlation_PLE_2017$"31",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2017$"31", correlation_PLE_2017$"32",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2017$"32", correlation_PLE_2017$"33",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2017$"33", correlation_PLE_2017$"34",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2017$"34", correlation_PLE_2017$"35",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2017$"35", correlation_PLE_2017$"36",use="pairwise.complete.obs", method = "pearson"),
#cor(correlation_PLE_2017$"36", correlation_PLE_2017$"37",use="pairwise.complete.obs", method = "pearson"),
#cor(correlation_PLE_2017$"37", correlation_PLE_2017$"38",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2017$"38", correlation_PLE_2017$"39",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2017$"39", correlation_PLE_2017$"40",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2017$"40", correlation_PLE_2017$"41",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2017$"41", correlation_PLE_2017$"42",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2017$"42", correlation_PLE_2017$"43",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2017$"43", correlation_PLE_2017$"44",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2017$"44", correlation_PLE_2017$"45",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2017$"45", correlation_PLE_2017$"46",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2017$"46", correlation_PLE_2017$"47",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2017$"47", correlation_PLE_2017$"48",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2017$"48", correlation_PLE_2017$"49",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2017$"49", correlation_PLE_2017$"50",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2017$"50", correlation_PLE_2017$"51",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2017$"51", correlation_PLE_2017$"52",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2017$"52", correlation_PLE_2017$"53",use="pairwise.complete.obs", method = "pearson"))
write.table(PLE_2017, file = "PLE_weeks.csv", sep = ",", append = TRUE, col.names = F, row.names = F)

### PLE 2018 ###

aggPLE1_2018<-data.frame(filter(aggPLE1, aggPLE1$year=="2018"))
correlation_PLE_2018<-spread(aggPLE1_2018, week, NEW_CPUE_PLE)

PLE_2018<-data.table(
cor(correlation_PLE_2018$"1", correlation_PLE_2018$"2",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2018$"2", correlation_PLE_2018$"3",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2018$"3", correlation_PLE_2018$"4",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2018$"4", correlation_PLE_2018$"5",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2018$"5", correlation_PLE_2018$"6",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2018$"6", correlation_PLE_2018$"7",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2018$"7", correlation_PLE_2018$"8",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2018$"8", correlation_PLE_2018$"9",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2018$"9", correlation_PLE_2018$"10",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2018$"10", correlation_PLE_2018$"11",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2018$"11", correlation_PLE_2018$"12",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2018$"12", correlation_PLE_2018$"13",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2018$"13", correlation_PLE_2018$"14",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2018$"14", correlation_PLE_2018$"15",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2018$"15", correlation_PLE_2018$"16",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2018$"16", correlation_PLE_2018$"17",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2018$"17", correlation_PLE_2018$"18",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2018$"18", correlation_PLE_2018$"19",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2018$"19", correlation_PLE_2018$"20",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2018$"20", correlation_PLE_2018$"21",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2018$"21", correlation_PLE_2018$"22",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2018$"22", correlation_PLE_2018$"23",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2018$"23", correlation_PLE_2018$"24",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2018$"24", correlation_PLE_2018$"25",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2018$"25", correlation_PLE_2018$"26",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2018$"26", correlation_PLE_2018$"27",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2018$"27", correlation_PLE_2018$"28",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2018$"28", correlation_PLE_2018$"29",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2018$"29", correlation_PLE_2018$"30",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2018$"30", correlation_PLE_2018$"31",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2018$"31", correlation_PLE_2018$"32",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2018$"32", correlation_PLE_2018$"33",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2018$"33", correlation_PLE_2018$"34",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2018$"34", correlation_PLE_2018$"35",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2018$"35", correlation_PLE_2018$"36",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2018$"36", correlation_PLE_2018$"37",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2018$"37", correlation_PLE_2018$"38",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2018$"38", correlation_PLE_2018$"39",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2018$"39", correlation_PLE_2018$"40",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2018$"40", correlation_PLE_2018$"41",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2018$"41", correlation_PLE_2018$"42",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2018$"42", correlation_PLE_2018$"43",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2018$"43", correlation_PLE_2018$"44",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2018$"44", correlation_PLE_2018$"45",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2018$"45", correlation_PLE_2018$"46",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2018$"46", correlation_PLE_2018$"47",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2018$"47", correlation_PLE_2018$"48",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2018$"48", correlation_PLE_2018$"49",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2018$"49", correlation_PLE_2018$"50",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2018$"50", correlation_PLE_2018$"51",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2018$"51", correlation_PLE_2018$"52",use="pairwise.complete.obs", method = "pearson"),
cor(correlation_PLE_2018$"52", correlation_PLE_2018$"53",use="pairwise.complete.obs", method = "pearson"))
write.table(PLE_2018, file = "PLE_weeks.csv", sep = ",", append = TRUE, col.names = F, row.names = F)

### CPUE aggregated by CSquares_mid and months ###
aggCOD2catch<- aggregate(list(catch=maps_data$raised_catch_cod),list(CSquare_mid=maps_data$CSquare_mid,month=maps_data$month, year=maps_data$year),sum, na.rm=TRUE)
aggCOD2effort<- aggregate(list(effort=maps_data$Effort_hrs),list(CSquare_mid=maps_data$CSquare_mid,month=maps_data$month, year=maps_data$year),sum, na.rm=TRUE)
aggCOD2<-aggCOD2effort
aggCOD2$NEW_CPUE_COD<-aggCOD2catch$catch/aggCOD2effort$effort
aggCOD2$effort<-NULL

### COD 2014 ###
library(dplyr) #filter needs this package
aggCOD2_2014<-data.frame(filter(aggCOD2, aggCOD2$year=="2014"))
correlation_COD2_2014<-spread(aggCOD2_2014, month, NEW_CPUE_COD)

COD2_2014<-data.table(
  cor(correlation_COD2_2014$"1", correlation_COD2_2014$"2",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_COD2_2014$"2", correlation_COD2_2014$"3",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_COD2_2014$"3", correlation_COD2_2014$"4",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_COD2_2014$"4", correlation_COD2_2014$"5",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_COD2_2014$"5", correlation_COD2_2014$"6",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_COD2_2014$"6", correlation_COD2_2014$"7",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_COD2_2014$"7", correlation_COD2_2014$"8",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_COD2_2014$"8", correlation_COD2_2014$"9",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_COD2_2014$"9", correlation_COD2_2014$"10",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_COD2_2014$"10", correlation_COD2_2014$"11",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_COD2_2014$"11", correlation_COD2_2014$"12",use="pairwise.complete.obs", method = "pearson"))
write.table(COD2_2014, file = "COD_correlation_months.csv", sep = ",", append = TRUE, col.names = T, row.names = F)

### COD 2015 ###

aggCOD2_2015<-data.frame(filter(aggCOD2, aggCOD2$year=="2015"))
correlation_COD2_2015<-spread(aggCOD2_2015, month, NEW_CPUE_COD)

COD2_2015<-data.table(
  cor(correlation_COD2_2015$"1", correlation_COD2_2015$"2",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_COD2_2015$"2", correlation_COD2_2015$"3",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_COD2_2015$"3", correlation_COD2_2015$"4",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_COD2_2015$"4", correlation_COD2_2015$"5",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_COD2_2015$"5", correlation_COD2_2015$"6",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_COD2_2015$"6", correlation_COD2_2015$"7",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_COD2_2015$"7", correlation_COD2_2015$"8",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_COD2_2015$"8", correlation_COD2_2015$"9",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_COD2_2015$"9", correlation_COD2_2015$"10",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_COD2_2015$"10", correlation_COD2_2015$"11",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_COD2_2015$"11", correlation_COD2_2015$"12",use="pairwise.complete.obs", method = "pearson"))
write.table(COD2_2015, file = "COD_correlation_months.csv", sep = ",", append = TRUE, col.names = F, row.names = F)

### COD 2016 ###

aggCOD2_2016<-data.frame(filter(aggCOD2, aggCOD2$year=="2016"))
correlation_COD2_2016<-spread(aggCOD2_2016, month, NEW_CPUE_COD)

COD2_2016<-data.table(
  cor(correlation_COD2_2016$"1", correlation_COD2_2016$"2",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_COD2_2016$"2", correlation_COD2_2016$"3",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_COD2_2016$"3", correlation_COD2_2016$"4",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_COD2_2016$"4", correlation_COD2_2016$"5",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_COD2_2016$"5", correlation_COD2_2016$"6",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_COD2_2016$"6", correlation_COD2_2016$"7",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_COD2_2016$"7", correlation_COD2_2016$"8",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_COD2_2016$"8", correlation_COD2_2016$"9",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_COD2_2016$"9", correlation_COD2_2016$"10",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_COD2_2016$"10", correlation_COD2_2016$"11",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_COD2_2016$"11", correlation_COD2_2016$"12",use="pairwise.complete.obs", method = "pearson"))
write.table(COD2_2016, file = "COD_correlation_months.csv", sep = ",", append = TRUE, col.names = F, row.names = F)

### COD 2017 ###

aggCOD2_2017<-data.frame(filter(aggCOD2, aggCOD2$year=="2017"))
correlation_COD2_2017<-spread(aggCOD2_2017, month, NEW_CPUE_COD)

COD2_2017<-data.table(
  cor(correlation_COD2_2017$"1", correlation_COD2_2017$"2",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_COD2_2017$"2", correlation_COD2_2017$"3",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_COD2_2017$"3", correlation_COD2_2017$"4",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_COD2_2017$"4", correlation_COD2_2017$"5",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_COD2_2017$"5", correlation_COD2_2017$"6",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_COD2_2017$"6", correlation_COD2_2017$"7",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_COD2_2017$"7", correlation_COD2_2017$"8",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_COD2_2017$"8", correlation_COD2_2017$"9",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_COD2_2017$"9", correlation_COD2_2017$"10",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_COD2_2017$"10", correlation_COD2_2017$"11",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_COD2_2017$"11", correlation_COD2_2017$"12",use="pairwise.complete.obs", method = "pearson"))
write.table(COD2_2017, file = "COD_correlation_months.csv", sep = ",", append = TRUE, col.names = F, row.names = F)

### COD 2018 ###

aggCOD2_2018<-data.frame(filter(aggCOD2, aggCOD2$year=="2018"))
correlation_COD2_2018<-spread(aggCOD2_2018, month, NEW_CPUE_COD)

COD2_2018<-data.table(
  cor(correlation_COD2_2018$"1", correlation_COD2_2018$"2",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_COD2_2018$"2", correlation_COD2_2018$"3",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_COD2_2018$"3", correlation_COD2_2018$"4",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_COD2_2018$"4", correlation_COD2_2018$"5",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_COD2_2018$"5", correlation_COD2_2018$"6",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_COD2_2018$"6", correlation_COD2_2018$"7",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_COD2_2018$"7", correlation_COD2_2018$"8",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_COD2_2018$"8", correlation_COD2_2018$"9",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_COD2_2018$"9", correlation_COD2_2018$"10",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_COD2_2018$"10", correlation_COD2_2018$"11",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_COD2_2018$"11", correlation_COD2_2018$"12",use="pairwise.complete.obs", method = "pearson"))
write.table(COD2_2018, file= "COD_correlation_months.csv", sep = ",", append = TRUE, col.names = F, row.names = F)

### CPUE aggregated by CSquares_mid and months ###
aggPLE2catch<- aggregate(list(catch=maps_data$raised_catch_ple),list(CSquare_mid=maps_data$CSquare_mid,month=maps_data$month, year=maps_data$year),sum, na.rm=TRUE)
aggPLE2effort<- aggregate(list(effort=maps_data$Effort_hrs),list(CSquare_mid=maps_data$CSquare_mid,month=maps_data$month, year=maps_data$year),sum, na.rm=TRUE)
aggPLE2<-aggPLE2effort
aggPLE2$NEW_CPUE_PLE<-aggPLE2catch$catch/aggPLE2effort$effort
aggPLE2$effort<-NULL

### PLE 2014 ###
aggPLE2_2014<-data.frame(filter(aggPLE2, aggPLE2$year=="2014"))
correlation_PLE2_2014<-spread(aggPLE2_2014, month, NEW_CPUE_PLE)

PLE2_2014<-data.table(
  cor(correlation_PLE2_2014$"1", correlation_PLE2_2014$"2",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_PLE2_2014$"2", correlation_PLE2_2014$"3",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_PLE2_2014$"3", correlation_PLE2_2014$"4",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_PLE2_2014$"4", correlation_PLE2_2014$"5",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_PLE2_2014$"5", correlation_PLE2_2014$"6",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_PLE2_2014$"6", correlation_PLE2_2014$"7",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_PLE2_2014$"7", correlation_PLE2_2014$"8",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_PLE2_2014$"8", correlation_PLE2_2014$"9",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_PLE2_2014$"9", correlation_PLE2_2014$"10",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_PLE2_2014$"10", correlation_PLE2_2014$"11",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_PLE2_2014$"11", correlation_PLE2_2014$"12",use="pairwise.complete.obs", method = "pearson"))
write.table(PLE2_2014, file = "PLE_correlation_months.csv", sep = ",", append = TRUE, col.names = T, row.names = F)

### PLE 2015 ###

aggPLE2_2015<-data.frame(filter(aggPLE2, aggPLE2$year=="2015"))
correlation_PLE2_2015<-spread(aggPLE2_2015, month, NEW_CPUE_PLE)

PLE2_2015<-data.table(
  cor(correlation_PLE2_2015$"1", correlation_PLE2_2015$"2",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_PLE2_2015$"2", correlation_PLE2_2015$"3",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_PLE2_2015$"3", correlation_PLE2_2015$"4",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_PLE2_2015$"4", correlation_PLE2_2015$"5",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_PLE2_2015$"5", correlation_PLE2_2015$"6",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_PLE2_2015$"6", correlation_PLE2_2015$"7",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_PLE2_2015$"7", correlation_PLE2_2015$"8",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_PLE2_2015$"8", correlation_PLE2_2015$"9",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_PLE2_2015$"9", correlation_PLE2_2015$"10",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_PLE2_2015$"10", correlation_PLE2_2015$"11",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_PLE2_2015$"11", correlation_PLE2_2015$"12",use="pairwise.complete.obs", method = "pearson"))
write.table(PLE2_2015, file = "PLE_correlation_months.csv", sep = ",", append = TRUE, col.names = F, row.names = F)

### PLE 2016 ###

aggPLE2_2016<-data.frame(filter(aggPLE2, aggPLE2$year=="2016"))
correlation_PLE2_2016<-spread(aggPLE2_2016, month, NEW_CPUE_PLE)

PLE2_2016<-data.table(
  cor(correlation_PLE2_2016$"1", correlation_PLE2_2016$"2",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_PLE2_2016$"2", correlation_PLE2_2016$"3",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_PLE2_2016$"3", correlation_PLE2_2016$"4",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_PLE2_2016$"4", correlation_PLE2_2016$"5",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_PLE2_2016$"5", correlation_PLE2_2016$"6",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_PLE2_2016$"6", correlation_PLE2_2016$"7",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_PLE2_2016$"7", correlation_PLE2_2016$"8",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_PLE2_2016$"8", correlation_PLE2_2016$"9",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_PLE2_2016$"9", correlation_PLE2_2016$"10",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_PLE2_2016$"10", correlation_PLE2_2016$"11",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_PLE2_2016$"11", correlation_PLE2_2016$"12",use="pairwise.complete.obs", method = "pearson"))
write.table(PLE2_2016, file = "PLE_correlation_months.csv", sep = ",", append = TRUE, col.names = F, row.names = F)

### PLE 2017 ###

aggPLE2_2017<-data.frame(filter(aggPLE2, aggPLE2$year=="2017"))
correlation_PLE2_2017<-spread(aggPLE2_2017, month, NEW_CPUE_PLE)

PLE2_2017<-data.table(
  cor(correlation_PLE2_2017$"1", correlation_PLE2_2017$"2",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_PLE2_2017$"2", correlation_PLE2_2017$"3",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_PLE2_2017$"3", correlation_PLE2_2017$"4",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_PLE2_2017$"4", correlation_PLE2_2017$"5",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_PLE2_2017$"5", correlation_PLE2_2017$"6",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_PLE2_2017$"6", correlation_PLE2_2017$"7",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_PLE2_2017$"7", correlation_PLE2_2017$"8",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_PLE2_2017$"8", correlation_PLE2_2017$"9",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_PLE2_2017$"9", correlation_PLE2_2017$"10",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_PLE2_2017$"10", correlation_PLE2_2017$"11",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_PLE2_2017$"11", correlation_PLE2_2017$"12",use="pairwise.complete.obs", method = "pearson"))
write.table(PLE2_2017, file = "PLE_correlation_months.csv", sep = ",", append = TRUE, col.names = F, row.names = F)

### PLE 2018 ###

aggPLE2_2018<-data.frame(filter(aggPLE2, aggPLE2$year=="2018"))
correlation_PLE2_2018<-spread(aggPLE2_2018, month, NEW_CPUE_PLE)

PLE2_2018<-data.table(
  cor(correlation_PLE2_2018$"1", correlation_PLE2_2018$"2",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_PLE2_2018$"2", correlation_PLE2_2018$"3",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_PLE2_2018$"3", correlation_PLE2_2018$"4",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_PLE2_2018$"4", correlation_PLE2_2018$"5",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_PLE2_2018$"5", correlation_PLE2_2018$"6",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_PLE2_2018$"6", correlation_PLE2_2018$"7",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_PLE2_2018$"7", correlation_PLE2_2018$"8",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_PLE2_2018$"8", correlation_PLE2_2018$"9",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_PLE2_2018$"9", correlation_PLE2_2018$"10",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_PLE2_2018$"10", correlation_PLE2_2018$"11",use="pairwise.complete.obs", method = "pearson"),
  cor(correlation_PLE2_2018$"11", correlation_PLE2_2018$"12",use="pairwise.complete.obs", method = "pearson"))
write.table(PLE2_2018, file= "PLE_correlation_months.csv", sep = ",", append = TRUE, col.names = F, row.names = F)

