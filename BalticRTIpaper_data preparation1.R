### Script to produce C-Square data (0.05 degree lat x 0.05 degree lon)
#   for fleets using mobile bottom contacting gears (mbcg)
# 	with day, metier, effort, landings (total, cod, plaice) and  total revenues (total Euro).
#	The script uses tacsatEflalo and cleanEflalo data that were created from VMS and logbook data 
#   by using standard routines as used within ICES like this one: 
### https://github.com/ices-eg/wg_WGSFD/tree/master/VMS-datacall 

library(vmstools)
library(data.table)
library(stringr)
library(lubridate)

sysPath   <- getwd()
RdataPath   <- paste0(sysPath,"/Rdata/")
dataPath    <- paste0(sysPath,"/Data/")
polygonPath <- paste0(sysPath,"/shapes")
resPath     <- paste0(sysPath,"/Results/")
setwd(sysPath)

# define gears used in workflow
Dredge <- c("DRB", "DRH", "HMD")
BottomTrwl <- c("OTB", "OTT", "PTB", "TBN", "TBS", "TB")
BeamTrwl <- "TBB"
AnchoredSeine <- "SDN"
FlyShootingSeine <- "SSC, SPR"

# combine all mobile bottom contacting gears (mbcg)
All_mbcg <- c(Dredge, BottomTrwl, BeamTrwl, AnchoredSeine, FlyShootingSeine)

# loop through all years
for (Year in c(2013:2018)) {
  load(file=file.path(paste(RdataPath,"tacsatEflalo_",Year,".RData",sep="")))
  load(file.path(paste(RdataPath,"cleanEflalo_",Year,".RData",sep="")))  
  
  ## add icessquares and Csquares to tacsatEflalo
  tacsatEflalo$REF_ICES <- ICESrectangle(tacsatEflalo)
  tacsatEflalo$CSquare  <- CSquare(tacsatEflalo$SI_LONG, tacsatEflalo$SI_LATI, 0.05)
  
  # add relevant information to tacsatEflalo from eflalo
  tacsatEflalo$VE_COU   <- eflalo$VE_COU[match(tacsatEflalo$FT_REF,eflalo$FT_REF)]
  tacsatEflalo$LE_MET   <- eflalo$LE_MET[match(tacsatEflalo$FT_REF,eflalo$FT_REF)]
  tacsatEflalo$LE_GEAR  <- eflalo$LE_GEAR[match(tacsatEflalo$FT_REF,eflalo$FT_REF)]
  
  # make tacsatEflalo into a data.table for faster processing
  te <- data.table(tacsatEflalo) # ; rm(tacsatEflalo); gc()
  
  
  # select only entries for the Baltic (includes Kattegat & Skagerrak)
  te <- te[SI_LATI > 53 & SI_LATI < 66 & SI_LONG > 9 & SI_LONG < 28 & 
                    str_detect(CSquare, "^160", negate = TRUE)  ] # omits positions off Norway
          
  # select only entries with SI_STATE = 1, ie vessels indicated in tacsatEflalo as "fishing"
  te <- te[SI_STATE == 1]
  
  # select only entries for mbcg
  te <- te[LE_GEAR %in% All_mbcg]
  
  # calculate total kg and EURO
  if(!"LE_KG_TOT" %in% colnames(te))
    te[,LE_KG_TOT:=rowSums(.SD, na.rm = TRUE), .SDcols = grep("KG", names(te))]
  if(!"LE_EURO_TOT" %in% colnames(te))
    te[,LE_EURO_TOT:=rowSums(.SD, na.rm = TRUE), .SDcols = grep("EURO", names(te))]
  
  te_test <- mutate(te, ZeroRev = (LE_KG_COD > 0 & LE_EURO_COD == 0))
 
  # aggregate per C-Squares, including number of vessels aggregated and no. of vessels given
   csq_mbcg_aggr <- te[,  c(  Effort_hrs=sum(INTV/60), lapply(.SD, sum), 
                                    NoVessels = length(unique(VE_REF))   )  ,  
                            by=.(CSquare=CSquare, Date=SI_DATE, Gear_code=LE_GEAR, Metier=LE_MET) , 
                            .SDcols=c("LE_KG_COD","LE_EURO_COD", "LE_KG_PLE","LE_KG_TOT", "LE_EURO_TOT")]
    
   # write file with aggregated for  year currently used
   fwrite(csq_mbcg_aggr, file=paste0(resPath, "csq_mbcg_aggr", "_", Year, ".csv"))
}
  
# write C-Square data from all  years into one data table.  
csq_lst <- list.files(path = resPath, pattern= "csq_mbcg_aggr_", full.names = T)
csq_mbcg_allYears <- rbindlist(lapply(csq_lst, fread), fill=T)
fwrite(csq_mbcg_allYears, paste0(resPath, "CSquares_mbcg_aggr_2013-2018.csv"))  
