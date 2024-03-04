#streamMetabolism for Qubrada Prieta in LUQ, PR

#Author: Alicia Dixon

#Last edited: 2/25/2024


#You may first need to install the unitted dependency:
    #   
    #   remotes::install_github('appling/unitted')
    # #installing streamMetabolizer  
    #   remotes::install_github(
    #     "DOI-USGS/streamMetabolizer",
    #     build_vignettes = FALSE)
    # 


##loading needed packages
library(ggplot2)
library(dplyr)
library(cowplot)
library(ggpubr)
library(rstatix)
library(stats)
library(agricolae)
library(streamMetabolizer)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
library(lubridate)
library(tidyverse)
library(dygraphs)
library(dataRetrieval)
library(zoo)
library(RCurl)
library(naniar)




##upload csv files
DO_2014 <- read.csv("./data/DO_2014.csv")
temp_2014 <- read.csv("./data/Temp_2014.csv")
#tower_dat <- read.csv("./data/NADPTowerData2015.csv") #calculating light later with calc_light function
#discharge <- read.csv("./data/discharge_2013.csv")
pressure <- read.csv("./data/pressure_data.csv")
depth <- read.csv("./data/depth_2006_2023.csv")
#lux_2016 <- read.csv("light_lux_2016.csv")  ##not needed if we have PAR from tower
      
      ##SPC not needed if we use 0 for streams
      #spc2016 <- read.csv("QP_SPC_2016.csv")
      #spc2019 <- read.csv("QP_SPC_2019.csv")
      #cond_2014 <- read.csv("QP_cond_2014.csv")




#### 1. preparing the input data####

#naming date in pressure dataset correctly
pressure <- pressure %>% 
  rename("local.time" = "date_time_GMT4")


##combining date and time for depth
# single object
depth <- unite(depth, col='local.time', c('Date', 'Time'), sep=' ')
#converting feet to meters for depth
depth$depth <- (depth$stage_ft)*0.3048 
#remove stage (ft) column
depth = subset(depth, select = -c(2) )


##merging all datasets into one table
data2 <- merge(x = DO_2014, y = temp_2014, by = "local.time", all.x = TRUE)
data3 <- merge(x = data2, y = depth, by = "local.time", all.x = TRUE)
metabdata <- merge(x = data3, y = pressure, by = "local.time", all.x = TRUE)
  #metabdata <-  merge(x = data3, y = tower_dat, by = "local.time", all.x = TRUE)    #not using tower data bc using calc_light function


##removing variables I dont want for now
metabdata = subset(metabdata, select = -c(2, 7, 9) )

#converting pressure to right unit
metabdata$press_mb <- (metabdata$abs_pres_psi)*68.9476
#air pressure
calc_air_pressure(15, 350)
##972.065

#converting NAs in pressure column to calculated air pressure
metabdata$press_mb[is.na(metabdata$press_mb)] <- 972.065

##removing more columns I dont need now
metabdata = subset(metabdata, select = -c(6))



##Save USGS gage number and site lat, long for downloading data
#no USGS gage for QP
Latitude <- 18.322222
Longitude <- -65.815278


##elevation
    #lookup_usgs_elevation(18.3222, -65.8153, unites = )
    ###I think this code might not work outside the continental US?? or for territories?
##elevation is about 350 m






#### 1A. Prepping for just 2018####

#select all of 2018
DO_2018 <- subset(metabdata, format(as.Date(local.time),"%Y")==2018)

#remove the first three days of january 2018 bc of NAs
DO_18 = DO_2018[-(1:202),]

#looking at missing values
gg_miss_var(DO_18)
vis_miss(DO_18) 

###taking hourly depth data to fill in 15 min gaps (NAs) by averaging the above and below values
#interpolate, where maxgap sets a week as the maximum gap to fill 
DO_18$depth_interp <- na.approx(DO_18$depth, maxgap = 672 )
DO_18$temp_interp <- na.approx(DO_18$temp.water, maxgap = 672 )


##local time to solar time conversion
local.time <- as.POSIXct(DO_18$local.time, tz='America/Puerto_Rico')
DO_18$solar.time <- calc_solar_time(local.time, longitude=-65.69)



#Generate light column
DO_18$light<-calc_light(DO_18$solar.time,
                        latitude = Latitude,
                        longitude= Longitude)

#Calculate DO at saturation
DO_18$DO.sat <- calc_DO_sat(
  temp=DO_18$temp_interp,
  press=DO_18$press_mb,
  salinity.water = 0,)


##cleaning up DO18
#remove variables i dont need
newDO18 = subset(DO_18, select = -c(1, 3:6))
#remove any dupes
QP18 <- newDO18[!duplicated(newDO18$solar.time), ]
#renaming depth_interp back to depth for model requirements
colnames(QP18)[2] ="depth"
colnames(QP18)[3] ="temp.water"

#looking at missing values
gg_miss_var(QP18)
vis_miss(QP18) 

#remove rows with NAs bc missing data
QP18 <- na.omit(QP18)



#streamMetab specs
bayes_name <- mm_name(
  type='bayes', pool_K600='normal', 
  err_obs_iid=TRUE, err_proc_acor=FALSE, err_proc_iid=TRUE)
bayes_name


bayes_specs <- specs(bayes_name)
bayes_specs

#Alicia specs modified with more burnin steps and with default values
bayes_specs <- specs(bayes_name,
                     burnin_steps = 1000,
                     saved_steps = 1000,
                     K600_daily_meanlog_meanlog = 2.484906649788,
                     K600_daily_meanlog_sdlog = 1.32,
                     K600_daily_sdlog_sigma = 0.05,
                     n_cores=4, verbose=FALSE)



## Fitting the model

met18 <- metab(bayes_specs, data=QP18)

#2018: inspecting the model

met18

# Here are the daily metabolism predictions from the model:
predict_metab(met18)
plot_metab_preds(met18)
get_params(met18)
predict_DO(met18) %>% head()
plot_DO_preds(met18)




#predictions from model
predict_metab(bayes_fit) %>% 
  lapply(function(col) if(is.numeric(col)) round(col, 2) else col ) %>%
  as.data.frame() %>%
  knitr::kable()


# For Bayesian models only, you can dig even deeper using `get_mcmc`, which
# returns a `stanfit` object that can be inspected using the `rstan` package.
# (These traceplots are pretty bad because we used so few MCMC iterations. You
# should strive for better in your final models.)

mcmc <- get_mcmc(met18)
rstan::traceplot(mcmc, pars='K600_daily', nrow=3)



    

#### 1B. Prepping for just 2015####

#just summer 2015#
    DO_15 <- subset(metabdata, format(as.Date(local.time),"%Y")==2015)
    
    #looking at missing values
    gg_miss_var(DO_15)
    vis_miss(DO_15)
    
    
    ###taking hourly depth data to fill in 15 min gaps (NA) by averaging the above and below values
    #delete last three rows to end on an hour
    DO_15 <- DO_15[0:(nrow(DO_15)-3),]
    #interpolate, where max gap sets a week as the maxium gap to fill 
    DO_15$depth_interp <- na.approx(DO_15$depth, maxgap = 672 )
    

    
    ##local time to solar time
    local.time <- as.POSIXct(DO_15$local.time, tz='America/Puerto_Rico')
    DO_15$solar.time <- calc_solar_time(local.time, longitude=-65.69)
    
    # Generate light
    DO_15$light<-calc_light(DO_15$solar.time,
                              latitude = Latitude,
                              longitude= Longitude)
    
    
    #Calculate DO at saturation
    DO_15$DO.sat <- calc_DO_sat(
      temp=DO_15$temp.water,
      press=DO_15$press_mb,
      salinity.water = 0,)

    
    #cleaning up DO15
    newDO15 = subset(DO_15, select = -c(1, 4:6))
    QP15 <- newDO15[!duplicated(newDO15$solar.time), ]
    #renaming depth_interp back to depth for model requirements
    colnames(QP15)[3] ="depth"
    
    #looking at missing values
    gg_miss_var(QP15)
    vis_miss(QP15)
    
    #remove rows with NAs bc missing data?
    #DO_15 <- na.omit(DO_15)
  
    
    ##Alice Carter (AC) Code for graphing data pre-modeling
    QP15 %>% unitted::v() %>%
      mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
      select(solar.time, starts_with('DO')) %>%
      gather(type, DO.value, starts_with('DO')) %>%
      mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
      ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() +
      facet_grid(units ~ ., scale='free_y') + theme_bw() +
      scale_color_discrete('variable')
    
    labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)',
                light='PAR\n(umol m^-2 s^-1)', discharge='Q\n(cms)')
    
    QP15 %>% unitted::v() %>%
      mutate(discharge_cfs = log(discharge_cfs)) %>%
      select(solar.time, depth, temp.water, light, discharge_cfs) %>%
      gather(type, value, depth, temp.water, light, discharge_cfs) %>%
      mutate(
        type=ordered(type, levels=c('depth','temp.water','light','discharge_cfs')),
        units=ordered(labels[type], unname(labels))) %>%
      ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() +
      facet_grid(units ~ ., scale='free_y') + theme_bw() +
      scale_color_discrete('variable')
    
    
    
    
    #streamMetab specs
    bayes_name <- mm_name(
      type='bayes', pool_K600='normal', 
      err_obs_iid=TRUE, err_proc_acor=FALSE, err_proc_iid=TRUE)
    bayes_name
    
    
    bayes_specs <- specs(bayes_name)
    bayes_specs
    
    #Alicia specs modified with more burnin steps and with default values
    bayes_specs <- specs(bayes_name,
                         burnin_steps = 1000,
                         saved_steps = 1000,
                         K600_daily_meanlog_meanlog = 2.484906649788,
                         K600_daily_meanlog_sdlog = 1.32,
                         K600_daily_sdlog_sigma = 0.5,
                         n_cores=4, verbose=FALSE)
        

    

    #fitting the code
    met15 <- metab(bayes_specs, data=QP15)
    
    #viewing
    met15
    predict_metab(met15)
    plot_metab_preds(met15)
    get_params(met15)
    plot_DO_preds(met15)
    
    
    
    
    
    
    

####troubleshooting####
    #taking out duplicate rows in time
    QP15 <- QP15[!duplicated(QP15$solar.time), ]
    

    ##subsetting the year into month chunks using a for loop
    #see below to see for loop that includes metab function
    #creating a month column for the loop
    DO_15$week = strftime(DO_15$local.time, "%V") 
    #loop to choose individual months
    for (i in unique(DO_15$week)){
      print(i)
      weekdf = subset(DO_15, DO_15$week == i)
      newdf = subset(weekdf, select = -c(1, 4, 6, 10))
      cleandf <- newdf[!duplicated(newdf$solar.time), ]
      assign(paste("QP15", i, sep = "_"), cleandf)
    }
    
   #using a for loop with the metab function because of memory (?) issues
      #TROUBLESHOOTING 
    DO_15$week = strftime(DO_15$local.time, "%m")
    big_df = data.frame()
    month = unique(DO_15$month) # this is actually month right now

    
    #loop to choose individual months and then run through metab function
    for (i in week[c(2, 4)]){
      print(i) 
      monthdf = subset(DO_15, DO_15$month == i) 
      newdf = subset(monthdf, select = -c(1, 4, 6, 10))
      cleandf <- newdf[!duplicated(newdf$solar.time), ]
      met15 <- metab(bayes_specs, data = cleandf) #met15 is a object filled with other objects
      #big_df = rbind(met15, big_df)
    }
    
##IF THE ABOVE WORKS, IT IS BECAUSE 3 (MARCH) AND 4 (JUNE) DO NOT HAVE ANY DATA GAPS. METAB FUNCTION HAS A MINIMUM GAP SIZE OF MISSING DATA IT WILL RUN ON. subscript out of bounds error could be due to the function struggling because it wants 15-minute time gaps and when there is too much missing data it does not want to run???
    
    
    #plotting here
    
    plot_metab_preds(big_df)
    plot_DO_preds(big_df)

  

#### 1C. Prepping for just 2017####
    
    DO_17 <- subset(metabdata, format(as.Date(local.time),"%Y")==2017)
    
    ###taking hourly depth data to fill in 15 min gaps (NA) by averaging the above and below values
     #interpolate, where max gap sets a week as the maxium gap to fill 
    DO_17$depth_interp <- na.approx(DO_17$depth, maxgap = 672 )
    
    #looking at missing values
    gg_miss_var(DO_17)
    vis_miss(DO_17)
    
    #remove rows with NAs bc missing data
    #DO_17 <- na.omit(DO_17)
    
    ##local time to solar time
    local.time <- as.POSIXct(DO_17$local.time, tz='America/Puerto_Rico')
    DO_17$solar.time <- calc_solar_time(local.time, longitude=-65.69)
    
    # Generate light
    DO_17$light<-calc_light(DO_17$solar.time,
                            latitude = Latitude,
                            longitude= Longitude)
    
    
    #Calculate DO at saturation
    DO_17$DO.sat <- calc_DO_sat(
      temp=DO_17$temp.water,
      press=DO_17$press_mb,
      salinity.water = 0,)
    
    
    #cleaning up DO17
    newDO17 = subset(DO_17, select = -c(1, 4:6))
    QP17 <- newDO17[!duplicated(newDO17$solar.time), ]
    #renaming depth_interp back to depth for model requirements
    colnames(QP17)[3] ="depth"
    
    
      #looking at missing values
    gg_miss_var(QP17)
    vis_miss(QP17)
    
    
    
    #streamMetab specs
    bayes_name <- mm_name(
      type='bayes', pool_K600='normal', 
      err_obs_iid=TRUE, err_proc_acor=FALSE, err_proc_iid=TRUE)
    bayes_name
    
    
    bayes_specs <- specs(bayes_name)
    bayes_specs
    
    #Alicia specs modified with more burnin steps and with default values
    bayes_specs <- specs(bayes_name,
                         burnin_steps = 1000,
                         saved_steps = 1000,
                         K600_daily_meanlog_meanlog = 2.484906649788,
                         K600_daily_meanlog_sdlog = 1.32,
                         K600_daily_sdlog_sigma = 0.5,
                         n_cores=4, verbose=FALSE)
    
    
    
    #fitting the code
    met17<- metab(bayes_specs, data=QP17)
    
    #viewing
    met17
    predict_metab(met17)
    plot_metab_preds(met17)
    get_params(met17)
    plot_DO_preds(met17)

    
    
    

    

    
    
#### 1D. Prepping for just 2016####
    
    #practicing with just 2016 data#
    
    #select all of 2016
    DO_16 <- subset(metabdata, format(as.Date(local.time),"%Y")==2016)
    
    
   ###taking hourly depth data to fill in 15 min gaps (NA) by averaging the above and below values
    #delete last three rows to end on an hour
    DO_16 <- DO_16[0:(nrow(DO_16)-3),]
    #interpolate, where max gap sets a week as the maxium gap to fill 
    DO_16$depth_interp <- na.approx(DO_16$depth, maxgap = 672 )
    
    
    
    ##local time to solar time conversion
    local.time <- as.POSIXct(DO_16$local.time, tz='America/Puerto_Rico')
    DO_16$solar.time <- calc_solar_time(local.time, longitude=-65.69)
    
    
    
    #Generate light column
    DO_16$light<-calc_light(DO_16$solar.time,
                              latitude = Latitude,
                              longitude= Longitude)
    
    #Calculate DO at saturation
    DO_16$DO.sat <- calc_DO_sat(
      temp=DO_16$temp.water,
      press=DO_16$press_mb,
      salinity.water = 0,)
    
    
    #cleaning up DO16
    newDO16 = subset(DO_16, select = -c(1, 4:6))
    QP16 <- newDO16[!duplicated(newDO16$solar.time), ]
    #renaming depth_interp back to depth for model requirements
    colnames(QP16)[3] ="depth"
    
    
    #looking at missing values
    gg_miss_var(QP16)
    vis_miss(QP16)
    
    
    
    
    #streamMetab specs
    bayes_name <- mm_name(
      type='bayes', pool_K600='normal', 
      err_obs_iid=TRUE, err_proc_acor=FALSE, err_proc_iid=TRUE)
    bayes_name
    
    
    bayes_specs <- specs(bayes_name)
    bayes_specs
    
    #Alicia specs modified with more burnin steps and with default values
    bayes_specs <- specs(bayes_name,
                         burnin_steps = 1000,
                         saved_steps = 1000,
                         K600_daily_meanlog_meanlog = 2.484906649788,
                         K600_daily_meanlog_sdlog = 1.32,
                         K600_daily_sdlog_sigma = 0.5,
                         n_cores=4, verbose=FALSE)
    
    
    
    #fitting the code
    met16<- metab(bayes_specs, data=QP16)
    
    #viewing
    met17
    predict_metab(met16)
    plot_metab_preds(met16)
    get_params(met16)
    plot_DO_preds(met16)
    
    
    
#### 1E. Prepping for just 2014####
    
    #practicing with just 2014 data#
    
    #select all of 2014
    DO_14 <- subset(metabdata, format(as.Date(local.time),"%Y")==2014)
    
    ###taking hourly depth data to fill in 15 min gaps (NA) by averaging the above and below values
    #interpolate, where max gap sets a week as the maxium gap to fill 
    DO_14$depth_interp <- na.approx(DO_14$depth, maxgap = 672 )
    DO_14$temp_interp <- na.approx(DO_14$temp.water, maxgap = 672 )
    
    
    ##local time to solar time conversion
    local.time <- as.POSIXct(DO_14$local.time, tz='America/Puerto_Rico')
    DO_14$solar.time <- calc_solar_time(local.time, longitude=-65.69)
    
    
    
    #Generate light column
    DO_14$light<-calc_light(DO_14$solar.time,
                            latitude = Latitude,
                            longitude= Longitude)
    
    #Calculate DO at saturation
    DO_14$DO.sat <- calc_DO_sat(
      temp=DO_14$temp_interp,
      press=DO_14$press_mb,
      salinity.water = 0,)
    
    
    #cleaning up DO14
    newDO14 = subset(DO_14, select = -c(1, 3:6))
    QP14 <- newDO14[!duplicated(newDO14$solar.time), ]
    #renaming depth_interp and temp_interp back to depth and temp.water for model requirements
    colnames(QP14)[2] ="depth"
    colnames(QP14)[3] ="temp.water"
    
    
    #looking at missing values
    gg_miss_var(QP14)
    vis_miss(QP14)
    
    #remove rows with NAs bc missing data
    QP14 <- na.omit(QP14)
    
    
    
    
    ##Alice Carter (AC) Code for graphing data pre-modeling
    QP14 %>% unitted::v() %>%
      mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
      select(solar.time, starts_with('DO')) %>%
      gather(type, DO.value, starts_with('DO')) %>%
      mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
      ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() +
      facet_grid(units ~ ., scale='free_y') + theme_bw() +
      scale_color_discrete('variable')
    
    ##AC code
    labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)',
                light='PAR\n(umol m^-2 s^-1)', discharge='Q\n(cms)')
    
    QP14 %>% unitted::v() %>%
      mutate(discharge_cfs = log(discharge_cfs)) %>%
      select(solar.time, depth, temp.water, light, discharge_cfs) %>%
      gather(type, value, depth, temp.water, light, discharge_cfs) %>%
      mutate(
        type=ordered(type, levels=c('depth','temp.water','light','discharge_cfs')),
        units=ordered(labels[type], unname(labels))) %>%
      ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() +
      facet_grid(units ~ ., scale='free_y') + theme_bw() +
      scale_color_discrete('variable')
    
    
    

    
    #streamMetab specs
    bayes_name <- mm_name(
      type='bayes', pool_K600='normal', 
      err_obs_iid=TRUE, err_proc_acor=FALSE, err_proc_iid=TRUE)
    bayes_name
    
    
    bayes_specs <- specs(bayes_name)
    bayes_specs
    
    #Alicia specs modified with more burnin steps and with default values
    bayes_specs <- specs(bayes_name,
                         burnin_steps = 1000,
                         saved_steps = 1000,
                         K600_daily_meanlog_meanlog = 2.484906649788,
                         K600_daily_meanlog_sdlog = 1.32,
                         K600_daily_sdlog_sigma = 0.5,
                         n_cores=4, verbose=FALSE)
    
    
    #fitting the code
    met14 <- metab(bayes_specs, data=QP14)
    
    
    
    #viewing
    met14
    predict_metab(met14)
    plot_metab_preds(met14)
    get_params(met14)
    plot_DO_preds(met14)
    
    
    
    
    
    ###Testing new specs
    met_14summer <- subset(QP14, format(as.Date(local.time),"%m") == c("05"))
    #fitting the code
    met_14summer <- metab(bayes_specs, data=met_14summer)
    #viewing
    met_14summer
    predict_metab(met_14summer)
    plot_metab_preds(met_14summer)
    get_params(met_14summer)
    plot_DO_preds(met_14summer)
    
    
    
    
testdf= met14@fit[["daily"]]
    
#### 1F. Prepping for just 2019####
    
    #practicing with just 2019 data#
    
    #select all of 2019 
    DO_19 <- subset(metabdata, format(as.Date(local.time),"%Y")==2019)
    
    
    #remove rows with NAs bc missing data
    ##eventually need to use interpolation to fill in data gaps
    #DO_16 <- na.omit(DO_16)
    
    ##local time to solar time conversion
    local.time <- as.POSIXct(DO_19$local.time, tz='America/Puerto_Rico')
    DO_19$solar.time <- calc_solar_time(local.time, longitude=-65.69)
    
    
    
    #Generate light column
    DO_19$light<-calc_light(DO_19$solar.time,
                            latitude = Latitude,
                            longitude= Longitude)
    
    #Calculate DO at saturation
    DO_19$DO.sat <- calc_DO_sat(
      temp=DO_19$temp.water,
      press=DO_19$press_mb,
      salinity.water = 0,)
    
    
    #cleaning up DO19
    newDO19 = subset(DO_19, select = -c(1, 4, 6))
    QP19 <- newDO19[!duplicated(newDO19$solar.time), ]
    
    #looking at missing values
    gg_miss_var(QP19)
    vis_miss(QP19)
    
    
    
    #streamMetab specs
    bayes_name <- mm_name(
      type='bayes', pool_K600='normal', 
      err_obs_iid=TRUE, err_proc_acor=FALSE, err_proc_iid=TRUE)
    bayes_name
    
    
    bayes_specs <- specs(bayes_name)
    bayes_specs
    
    #Alicia specs modified with more burnin steps and with default values
    bayes_specs <- specs(bayes_name,
                         burnin_steps = 1000,
                         saved_steps = 1000,
                         K600_daily_meanlog_meanlog = 2.484906649788,
                         K600_daily_meanlog_sdlog = 1.32,
                         K600_daily_sdlog_sigma = 0.5,
                         n_cores=4, verbose=FALSE)
    
    
####code that is not needed currently####
    
    #remove all rows with NA because of missing light data
    #not needed when using calc_light function
    # metabdata <- na.omit(metabdata)
    
    
    ##figuring out depth issues due to missing 2017 data because hurricane maria
    # #filter 2017 for depth 
    # depth17 <- subset(depth, format(as.Date(local.time),"%Y")==2017)
    # #making dates
    # data2$local.time.test <- as.POSIXct(data2$local.time, format="%Y-%m-%d %H:%M:%S")
    # depth$local.time.test <- as.POSIXct(depth$local.time, format="%Y-%m-%d %H:%M:%S")
    # ##alternative merging
    # data4 <- full_join(data2, depth, by = "local.time.test"
    
    
    #converting cond to SPC
    
    # cond.to.spc=function(temp,cond){
    #   spc=cond/(1+0.0191*(temp-25))
    #   return(spc)
    # }
    # 
    # cond_2014$SPC <- cond.to.spc(cond_2014$tempB, cond_2014$cond)
    # cond_2014 <- na.omit(cond_2014)
    
    # ##merging SPC files
    # SPC_dat <- merge(x = spc2016, y = spc2019, all.x = TRUE)
    # SPC_dat <- merge(x = data2, y = cond_2014$SPC, all.x = TRUE)
    #   #####THIS ISNT WORKING because 2014 is too big??? need to come back to this
    # 
    # #converting SPC to salinity
    # SPC_dat$salinty <- (SPC_dat$SPC)*0.0006
    
    
    
    
    
    #(AC code) sM model specs 
    ##should also try bayes specs from AA code or investigate differences
    # bayes_name <- mm_name(type='bayes', pool_K600='normal',
    #                       err_obs_iid=TRUE, err_proc_iid=TRUE)
    # 
    # bayes_specs <- specs(bayes_name,
    #                      burnin_steps = 1000,
    #                      saved_steps = 1000,
    #                      K600_daily_meanlog_meanlog = 2.484906649788,
    #                      K600_daily_meanlog_sdlog = 0.75,
    #                      K600_daily_sdlog_sigma = 0.001,
    #                      n_cores=4, verbose=T)
    
    
    
    
    
    #this code if data isnt every 15 min?
    # Make the time steps 15 minutes
    # DO_2018 <- data.frame(solar.time = seq(min(DO_2018$solar.time),
    #                                        max(DO_2018$solar.time),
    #                                        by = '15 min')) %>%
    #   left_join(DO_2018, by = 'solar.time')
    
    
    
    
    
#### 2. Configuring the model ####
    
##There are two steps to configuring a metabolism model in streamMetabolizer: 
###(A) Identify the name of the model structure you want using mm_name(); and 
###(B) Set the specifications for the model using defaults fromspecs() as a starting       point.

      
# (Alice Carter code) Model Metabolism using cleaned and prepped data: 
      # Visualize the data 
      head(QPmet) ; tail(QPmet)
      
      
      ##Alice Carter (AC) Code for viewing DO trends
      QPmet %>% unitted::v() %>%
        mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
        select(solar.time, starts_with('DO')) %>%
        gather(type, DO.value, starts_with('DO')) %>%
        mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
        ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() +
        facet_grid(units ~ ., scale='free_y') + theme_bw() +
        scale_color_discrete('variable')
      
      ##AC code for viewing DO, temp, and discharge trends
      labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)',
                  light='PAR\n(umol m^-2 s^-1)', discharge='Q\n(cms)')
      
      QPmet %>% unitted::v() %>%
        mutate(discharge_cfs = log(discharge_cfs)) %>%
        select(solar.time, depth, temp.water, light, discharge_cfs) %>%
        gather(type, value, depth, temp.water, light, discharge_cfs) %>%
        mutate(
          type=ordered(type, levels=c('depth','temp.water','light','discharge_cfs')),
          units=ordered(labels[type], unname(labels))) %>%
        ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() +
        facet_grid(units ~ ., scale='free_y') + theme_bw() +
        scale_color_discrete('variable')
      
      ##AC code
      #sM model specs--------------- #
      bayes_name <- mm_name(type='bayes', pool_K600='normal',
                            err_obs_iid=TRUE, err_proc_iid=TRUE)
      
      bayes_specs <- specs(bayes_name,
                           burnin_steps = 1000,
                           saved_steps = 1000,
                           K600_daily_meanlog_meanlog = 2.484906649788,
                           K600_daily_meanlog_sdlog = 0.75,
                           K600_daily_sdlog_sigma = 0.001,
                           n_cores=4, verbose=T)
      
      
      
      
      
      ##taking out unneeded columns
      QPmet <- subset(QPmet, select = -c(3, 4))
      
      
      ##troubleshooting
      #taking out duplicate rows in time
      QPmet <- QPmet[!duplicated(QPmet$solar.time), ]
      
      
      #Run model! (AC code)
      fit <- metab(bayes_specs, data=QPmet)
      
      #viewing
      fit
      predict_metab(fit)
      plot_metab_preds(fit)
      get_params(fit)
      plot_DO_preds(fit)
      
      
      
      
      
      

####streamMetabolizer AA github code below

##2A. Choosing a model structure
#For this example, we will specify a Bayesian model with both observation error and process error. We won’t pool K600 here because we don’t have many days of data, but pooling is one feature that makes Bayesian models better than MLE models in general. Another great   feature of Bayesian models is that they produce more accurate and nuanced confidence intervals.

bayes_name <- mm_name(type='bayes', pool_K600='none', err_obs_iid=TRUE, err_proc_iid=TRUE)
bayes_name

##2B Set the specifications
#We now pass the model name to specs() to get a list of default specifications for this model.
bayes_specs <- specs(bayes_name)
bayes_specs

#At this point we can alter some of the specifications if desired.
# one way to alter specifications: call specs() again
bayes_specs <- specs(bayes_name, burnin_steps=100, saved_steps=200, n_cores=1, GPP_daily_mu=3, GPP_daily_sigma=2)
# another way: use revise()
bayes_specs <- revise(bayes_specs, burnin_steps=100, saved_steps=200, n_cores=1, GPP_daily_mu=3, GPP_daily_sigma=2)

bayes_specs




#### 3. Fitting the model ####
##Once a model has been configured, you can fit the model to data with metab(). Bayesian models take a while to run, so be patient.    Or switch to an MLE model if you can afford to sacrifice some accuracy for speed. (This small example usually takes about 30 seconds   on my computer.)

mm <- metab(bayes_specs, data=QPmet)






##### 4. Inspecting the model####

# Once you've fit a model, you can inspect the output with functions including
# `predict_metab()` and `plot_metab_preds()`, `predict_DO()` and `plot_DO_preds()`,
# `get_params()`, and `get_fit()`.
# 
# Start by simply printing the model to the console.

 mm


 
# Here are the daily metabolism predictions from the model:

predict_metab(mm)


plot_metab_preds(mm)

 
# You can inspect more of the fitted daily parameters, including K600, with `get_params()`:

get_params(mm)

 
 
# Here are the first few dissolved oxygen predictions from the model (`DO.mod`).
# They are returned along with the input data for convenience.

predict_DO(mm) %>% head()

# And here are the dissolved oxygen predictions in a figure:

plot_DO_preds(mm)

 
 
 
# For Bayesian models only, you can dig even deeper using `get_mcmc`, which
# returns a `stanfit` object that can be inspected using the `rstan` package.
# (These traceplots are pretty bad because we used so few MCMC iterations. You
# should strive for better in your final models.)

mcmc <- get_mcmc(mm)
rstan::traceplot(mcmc, pars='K600_daily', nrow=3)

 
 
# The `get_fit()` function returns a list of data.frames, one per temporal
# resolution, containing all fitted values and details about their distributions
# and convergence. Here are just the overall metrics of model convergence
# (`Rhat`s, or potential scale reduction statistics; see Gelman and Rubin 1992 or
# Brooks and Gelman 1998):

 get_fit(mm)$overall %>%
   select(ends_with('Rhat'))


 
# And here is a list of all column names available through `get_fit()`:

 get_fit(mm) %>%
   lapply(names)

 
# You're on your way!
