rm(list=ls(all=TRUE))
library(ggplot2)
library(reshape2)
library(dplyr)
library(zoo)
library(plyr)

path.inp  = "/.../AK_10km_Timeseries/"
path.out  = "/.../AK_10km_Timeseries/plot/"

model.list = c("ACCESS-CM2",  "ACCESS-ESM1-5", "CanESM5",       "CESM2-WACCM",      "CMCC-CM2-SR5", 
               "CMCC-ESM2",   "EC-Earth3",     "EC-Earth3-Veg", "EC-Earth3-Veg-LR", "FGOALS-g3", 
               "GFDL-ESM4",   "IITM-ESM",      "INM-CM4-8",     "INM-CM5-0",        "IPSL-CM6A-LR", 
               "KACE-1-0-G",  "MIROC6",        "MPI-ESM1-2-HR", "MPI-ESM1-2-LR",    "MRI-ESM2-0",
               "NorESM2-LM",  "NorESM2-MM")
cvar = c("pr")
scenario = c("ssp126","ssp245","ssp370","ssp585")

# Bias correction methods list for precipitation 
bc.list <- c("obs.model","dqm","eqm","gpqm","qdm","scaling","mva","loci")  

window_size <- 90

ts.hist_origin <- seq(from=as.Date("1981-01-01"), to=as.Date("2014-12-31"), by='day')
ts.hist <- ts.hist_origin[!(format(ts.hist_origin, "%m-%d") == "02-29")]

ts.future_origin <- seq(from=as.Date("2015-01-01"), to=as.Date("2100-12-31"), by='day')
ts.future <- ts.future_origin[!(format(ts.future_origin, "%m-%d") == "02-29")]


obs.ts <- readRDS(paste0(path.inp, 'res_regional_avg_historical_obs.daymet.rds'))
obs.ts.1 <- matrix(t(obs.ts), ncol=1)


# Data arrange for Historical
list_hist.ts.mdl <- list()
for(ires in 1:length(bc.list)){
  hist.ts <- readRDS(paste0(path.inp, 'res_regional_avg_historical_',bc.list[ires],'.rds'))
  
  hist.ts.1 <- lapply(hist.ts, function(x) {
    if (ncol(x) < 365) {
      x <- cbind(x, matrix(NA, nrow = nrow(x), ncol = 365 - ncol(x)))
    }
    return(x)
  })
  
  hist.ts.mdl.all <- NULL
  for(im in 1:length(model.list)){
    hist.ts.mdl <- matrix(t(hist.ts.1[[im]]), ncol=1) 
    hist.ts.mdl.all <- cbind(hist.ts.mdl.all, hist.ts.mdl)
  }
  colnames(hist.ts.mdl.all) <- model.list
  
  ma.obs.ts.1 <- zoo::rollapply(obs.ts.1, window_size, mean, fill = NA, align = "center")
  ma.hist.ts.mdl.all <- zoo::rollapply(hist.ts.mdl.all, window_size, mean, fill = NA, align = "center")
  ma.hist.ens.mdl <- apply(ma.hist.ts.mdl.all, 1, mean, na.rm=T)
  
  df.hist.ts.mdl.all <- data.frame(obs.gmet = ma.obs.ts.1, ma.hist.ts.mdl.all, ens.avg = ma.hist.ens.mdl, timestep=ts.hist)
  colnames(df.hist.ts.mdl.all) <- c('obs.daymet', model.list, 'ens.avg','timestep')

  list_hist.ts.mdl[[ires]] <- df.hist.ts.mdl.all
  
  rm(hist.ts.mdl.all, hist.ts.1)
}

names(list_hist.ts.mdl) <- bc.list


# Data arrange for Future
list_future.ts.mdl <- list()
for(is in 1:length(scenario)){
  
  future.ts.mdl <-list()
  for(im in 1:length(model.list)){
    
    future.ts.2 <- NULL
    for(ires in 1:length(bc.list)){
      future.ts <- readRDS(paste0(path.inp, 'res_ak_pr_regional_avg_ts_',model.list[im],'_',scenario[is],'_',bc.list[ires],'.rds'))
      future.ts.1 <- matrix(t(future.ts), ncol=1)
      future.ts.2 <- cbind(future.ts.2, future.ts.1)
    }
    colnames(future.ts.2) <- bc.list
    future.ts.2 <- data.frame(future.ts.2)
    
    if(nrow(future.ts.2) != c(86*365)){
      future.ts.2[c(nrow(future.ts.2):c(86*365)),] <- NA
    }
    
    ma.future.ts.2 <- zoo::rollapply(future.ts.2, window_size, mean, fill = NA, align = "center")
    
    future.ts.mdl[[im]] <- data.frame(ma.future.ts.2)
    rm(future.ts.2, ma.future.ts.2)
    
    #future.ts.mdl[[im]] <- data.frame(future.ts.2)
    #rm(future.ts.2)
  }
  future.ens.avg <- apply(laply(future.ts.mdl, as.matrix), c(2,3), function(x) mean(x, na.rm=T))
  future.ts.mdl[[im+1]] <- data.frame(future.ens.avg)
  
  future.ts.mdl <- lapply(future.ts.mdl, function(df){
    df$timestep <- ts.future
    return(df)
  })
  
  names(future.ts.mdl) <- c(model.list, 'ens.avg')
  
  list_future.ts.mdl[[is]] <- future.ts.mdl
  rm(future.ts.mdl)
}
names(list_future.ts.mdl) <- scenario


# Data arrange for plotting
melt.list_hist.ts.mdl <- melt(list_hist.ts.mdl, id.vars = c('timestep'))
colnames(melt.list_hist.ts.mdl) <- c('timestep','model.list','value','bc.method')
df_hist.ts.mdl.126 <- data.frame(melt.list_hist.ts.mdl, scenario='ssp126') 
df_hist.ts.mdl.245 <- data.frame(melt.list_hist.ts.mdl, scenario='ssp245')
df_hist.ts.mdl.370 <- data.frame(melt.list_hist.ts.mdl, scenario='ssp370')
df_hist.ts.mdl.585 <- data.frame(melt.list_hist.ts.mdl, scenario='ssp585')


melt.list_future.ts.mdl <- melt(list_future.ts.mdl, id.vars = c('timestep'))
colnames(melt.list_future.ts.mdl) <- c('timestep','bc.method','value','model.list','scenario')

melt.list_future.ts.mdl.1 <- data.frame(melt.list_future.ts.mdl)

common_columns <- colnames(df_hist.ts.mdl.126)
melt.list_future.ts.mdl.2 <- melt.list_future.ts.mdl.1[,common_columns]

df_merge1 <- rbind(df_hist.ts.mdl.126, subset(melt.list_future.ts.mdl.2, scenario %in% 'ssp126'))
df_merge2 <- rbind(df_hist.ts.mdl.245, subset(melt.list_future.ts.mdl.2, scenario %in% 'ssp245'))
df_merge3 <- rbind(df_hist.ts.mdl.370, subset(melt.list_future.ts.mdl.2, scenario %in% 'ssp370'))
df_merge4 <- rbind(df_hist.ts.mdl.585, subset(melt.list_future.ts.mdl.2, scenario %in% 'ssp585'))

df_merge_all <- rbind(df_merge1, df_merge2, df_merge3, df_merge4)

df.melt <- melt(df_merge_all, id.vars=c('scenario','timestep','bc.method','model.list'))
df.melt$model.list <- factor(df.melt$model.list, levels = c(model.list, 'ens.avg','obs.daymet'),
                             labels = c(model.list, 'Ensemble Average','DAYMET'))
df.melt$bc.method <- factor(df.melt$bc.method, levels = bc.list, labels = c('Model',"DQM","EQM","GPQM","QDM","Scaling","MVA","LOCI"))
df.melt$scenario <- factor(df.melt$scenario, levels = scenario, labels = c('SSP126','SSP245','SSP370','SSP585'))


#Plot 
gg <- ggplot()
gg <- gg + geom_line(data= subset(df.melt, bc.method %in% c("DQM","EQM","GPQM","QDM","Scaling","MVA","LOCI")), 
                     aes(x=timestep, y=value, color=model.list, alpha = model.list), size=0.5)
gg <- gg + coord_cartesian(ylim=c(0, 8))
gg <- gg + scale_y_continuous(name='Precipitation (mm)', breaks=seq(0,8,2), labels=seq(0,8,2))
gg <- gg + scale_x_date(name='',
                        breaks = seq(as.Date('1981-01-01'), as.Date('2100-12-31'), by = '34 years'), 
                        labels = format(seq(as.Date('1981-01-01'), as.Date('2100-12-31'), by = '34 years'), "%Y"))
gg <- gg + scale_color_manual(values=c(rep("skyblue",length(model.list)),'blue','black'))
gg <- gg + scale_alpha_manual(values=c(rep(1,length(model.list)), 1, 0.5))
gg <- gg + facet_grid(scenario~bc.method)
gg <- gg + theme_bw()
gg <- gg + theme(legend.position = "none")

png(paste0(path.out, "plot_ts_ak_pr.png"), width=15, height=8, unit='in', res=200)
plot(gg)
dev.off()



