rm(list=ls(all=TRUE))
library(doParallel) # for parallel computing
library(foreach) # for parallel computing
library(lubridate)
library(raster)
library(dplyr)

cvar = c("pr")

# Model list for pr
model.list <- c("ACCESS-CM2",  "ACCESS-ESM1-5", "CanESM5",       "CESM2-WACCM",      "CMCC-CM2-SR5", 
                "CMCC-ESM2",   "EC-Earth3",     "EC-Earth3-Veg", "EC-Earth3-Veg-LR", "FGOALS-g3", 
                "GFDL-ESM4",   "IITM-ESM",      "INM-CM4-8",     "INM-CM5-0",        "IPSL-CM6A-LR", 
                "KACE-1-0-G",  "MIROC6",        "MPI-ESM1-2-HR", "MPI-ESM1-2-LR",    "MRI-ESM2-0",
                "NorESM2-LM",  "NorESM2-MM",    "TaiESM1")

# Bias correction methods list for precipitation 
bc.list <- c("obs.model","dqm","eqm","gpqm","qdm","scaling","mva","loci")

# SSP scenarios
scenario = c("ssp126","ssp245","ssp370","ssp585")

path.inp      <- "/glade/campaign/collections/rda/data/d010065/CMIP6_downscaled_statistical/AK_10km/"
path.out      <- "/.../AK_10km_Timeseries/"
path.out.1 <- paste0(path.out, cvar,"/")
dir.create(path.out, showWarnings = F)
dir.create(path.out.1, showWarnings = F)

ires <- as.numeric(commandArgs(trailingOnly = TRUE)[1])
res.list.na <- bc.list[ires][[1]][1]

###Historical
yr = c(1981:2014)

mdl.regional.avg.hist <- list()
for(im in 1:length(model.list)){
  
  nday = 365
  if (model.list[im] == "KACE-1-0-G") nday = 360
  
  regional.avg.hist <- list()
  for (iyr in 1:length(yr)){
    print(paste0("m",im,"-",ires,"-",res.list.na,"_historical_",yr[iyr]))
    
    # Load the entire year's data at once, for all days if possible
    file_path <- paste0(patn.inp, model.list[im], "/", cvar,"/historical/bc_pr_day_",model.list[im],"_historical_",yr[iyr],"0101-",yr[iyr],"1231_",res.list.na,".nc")
    res <- raster::brick(file_path)  # Load entire year as a raster brick
    
    # Extract temperature data and calculate the mean over all days
    df.res.1 <- as.data.frame(res, xy=TRUE, na.rm=TRUE)
    
    # Calculate the mean for each pixel over all days of the year
    regional.avg <- round(apply(df.res.1[, 3:ncol(df.res.1)], 2, mean, na.rm=T), 4)
    
    regional.avg.hist[[iyr]] <- regional.avg
    rm(regional.avg, df.res.1)
  }
  df.regional.avg.hist <- do.call(rbind, regional.avg.hist)  
  
  mdl.regional.avg.hist[[im]] <- df.regional.avg.hist
  rm(df.regional.avg.hist)
}
saveRDS(mdl.regional.avg.hist, paste0(path.out.1, "res_regional_avg_historical_",res.list.na,".rds"))
