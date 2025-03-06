rm(list=ls(all=TRUE))
library(doParallel)  # for parallel computing
library(foreach)     # for parallel computing
library(lubridate)
library(raster)
library(dplyr)

cvar = c("tas")

# Model list for tas
model.list <- c("ACCESS-CM2",  "ACCESS-ESM1-5", "CanESM5",       "CESM2-WACCM",      "CMCC-CM2-SR5", 
                "CMCC-ESM2",   "EC-Earth3",     "EC-Earth3-Veg", "EC-Earth3-Veg-LR", "FGOALS-g3", 
                "GFDL-ESM4",   "IITM-ESM",      "INM-CM4-8",     "INM-CM5-0",        "IPSL-CM6A-LR", 
                "KACE-1-0-G",  "MIROC6",        "MPI-ESM1-2-HR", "MPI-ESM1-2-LR",    "MRI-ESM2-0",
                "NorESM2-LM",  "NorESM2-MM")

# Bias correction methods list for temperature 
bc.list <- c("obs.model", "dqm","eqm","qdm","scaling","variance","mva")

# SSP scenarios
scenario = c("ssp126","ssp245","ssp370","ssp585")

path.inp      <- "/glade/campaign/collections/rda/data/d010065/CMIP6_downscaled_statistical/AK_10km/"
path.out      <- "/.../AK_10km_Timeseries/"
path.out.1 <- paste0(path.out, cvar,"/")
dir.create(path.out, showWarnings = F)
dir.create(path.out.1, showWarnings = F)

numCores <- 86  # Use all available cores
cl <- makeCluster(numCores)
registerDoParallel(cl)

# Loop through models
for(im in 1:length(model.list)){
  
  # Model-specific settings
  yr.f = c(2015:2100)
  if (model.list[im] == "IITM-ESM") yr.f <- c(2015:2098)
  
  nday = 365
  if (model.list[im] == "KACE-1-0-G") nday = 360
  
  # Loop for Scenarios
  for(is in 1:length(scenario)){
    
    # Loop through each bias correction method
    for(ires in 1:length(bc.list)){
      
      print(paste0("Processing: m", im, "-", ires, "-", bc.list[ires], "_", scenario[is]))
      
      regional.avg.future_list <- vector("list", length(yr.f))
      
      # Run parallel processing for each year using foreach
      regional.avg.future_list <- foreach(iyr = 1:length(yr.f), .packages = c("raster", "dplyr", "lubridate")) %dopar% { 
        
        # Load the entire year's data at once, for all days if possible
        file_path <- paste0(path.inp, model.list[im], "/",cvar,"/",scenario[is],"/bc_",cvar,"_day_", model.list[im], "_", scenario[is], "_", yr.f[iyr], "0101-", yr.f[iyr], "1231_", bc.list[ires], ".nc")
        res <- raster::brick(file_path)  # Load entire year as a raster brick
        
        # Extract temperature data and calculate the mean over all days
        df.res.1 <- as.data.frame(res, xy=TRUE, na.rm=TRUE)
        
        # Calculate the mean for each pixel over all days of the year
        regional.avg <- round(apply(df.res.1[, 3:ncol(df.res.1)], 2, mean, na.rm=T), 4)
        
        # Return the result for this year
        return(regional.avg)
      }
      
      # Combine all years for this scenario and bias correction method
      df.regional.avg.future <- do.call(rbind, regional.avg.future_list)  
      
      # Save the results
      saveRDS(df.regional.avg.future, paste0(path.out.1, "res_ak_",cvar,"_regional_avg_ts_", model.list[im], "_", scenario[is], "_", bc.list[ires], ".rds"))
    }
    
  } #end of scenario
  
} # end of model.list

# Stop parallel cluster after all models are processed
stopCluster(cl)