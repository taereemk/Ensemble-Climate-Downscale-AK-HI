rm(list=ls(all=TRUE))
library(lubridate)
library(raster)
library(dplyr)

cvar = c("pr")

path.inp      <- "/glade/campaign/collections/rda/data/d010065/CMIP6_downscaled_statistical/AK_10km/OBS_DAYMET/"
path.out      <- "/.../AK_10km_Timeseries/"
path.out.1 <- paste0(path.out, cvar,"/")

dir.create(path.out, showWarnings = F)
dir.create(path.out.1, showWarnings = F)

yr = c(1981:2014) # Historical years
nday = 365

regional.avg.hist <- list()
for (iyr in 1:length(yr)){
  print(paste0("daymet_",yr[iyr]))
  
  # Load the entire year's data at once, for all days if possible
  file_path <- paste0(path.inp, cvar,"/bc_pr_day_historical_",yr[iyr],"0101-",yr[iyr],"1231_obs.daymet.nc")
  
  # Load entire year as a raster brick
  res <- raster::brick(file_path) 
  
  # Extract temperature data and calculate the mean over all days
  df.res.1 <- as.data.frame(res, xy=TRUE, na.rm=TRUE)
  
  # Calculate the mean for each pixel over all days of the year
  regional.avg <- round(apply(df.res.1[, 3:ncol(df.res.1)], 2, mean, na.rm=T), 4)
  
  regional.avg.hist[[iyr]] <- regional.avg
  rm(regional.avg, df.res.1)
}
df.regional.avg.hist <- do.call(rbind, regional.avg.hist)  

saveRDS(df.regional.avg.hist, paste0(path.out.1, "res_regional_avg_historical_obs.daymet.rds"))

