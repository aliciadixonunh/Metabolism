## type the path to where your files are saved
#setwd("C:/Users/dixon/OneDrive - USNH/Research/Metabolism")


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
library(M3JF)



##upload csv file
DO_2014 <- read.csv("./data/DO_2014.csv")
temp_2014 <- read.csv("./data/Temp_2014.csv")
tower_dat <- read.csv("./data/NADPTowerData2015.csv")
discharge <- read.csv("./data/discharge_2013.csv")
pressure <- read.csv("./data/pressure_data.csv")
depth <- read.csv("./data/PrietaDischarge_15min_2006-2023.csv")
#lux_2016 <- read.csv("light_lux_2016.csv")  ##not needed if we have PAR from tower
      
      ##SPC not needed if we use 0 for streams
      #spc2016 <- read.csv("QP_SPC_2016.csv")
      #spc2019 <- read.csv("QP_SPC_2019.csv")
      #cond_2014 <- read.csv("QP_cond_2014.csv")




#### 1. preparing the input data####

pressure <- pressure %>% 
  rename("local.time" = "date_time_GMT4")


#combining date and time for depth
# single object
depth <- unite(depth, col='local.time', c('Date', 'Time'), sep=' ')
#converting feet to meters for depth
depth$depth_m <- (depth$stage_ft)*0.3048 
#remove stage (ft)
depth = subset(depth, select = -c(2) )


##merging into one table
data2 <- merge(x = DO_2014, y = temp_2014, by = "local.time", all.x = TRUE)
data3 <- merge(x = data2, y = depth, by = "local.time", all.x = TRUE)
metabdata <- merge(x = data3, y = pressure, by = "local.time", all.x = TRUE)
  #metabdata <-  merge(x = data3, y = tower_dat, by = "local.time", all.x = TRUE)         #not using tower data bc using calc_light function

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

    # #remove all rows with NA because of missing light data
    #   #not needed when using calc_light function
    # metabdata <- na.omit(metabdata)


##Save USGS gage number and site lat, long for downloading data
   #no USGS gage for QP
Latitude <- 18.322222
Longitude <- -65.815278

##elevation
  #lookup_usgs_elevation(18.3222, -65.8153, unites = )
  ###I think this code might not work outside the continental US?? or for territories?
  ## elevation is about 350 m




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





####practicing with just 2018 data####

#select all of 2018
DO_2018 <- subset(metabdata, format(as.Date(local.time),"%Y")==2018)
     ##not using this because using 0 for SPC
      #SPC_2018 <- subset(SPC_dat, format(as.Date(local.time),"%Y")==2018)
      #prac_dat <- merge(x = DO_2018, y = SPC_2018,  by = "local.time", all.x = TRUE)
      #prac_dat <- na.omit(prac_dat)



#remove rows with NAs bc missing data
DO_2018 <- na.omit(DO_2018)

##local time to solar time
local.time <- as.POSIXct(DO_2018$local.time, tz='America/Puerto_Rico')
DO_2018$solar.time <- calc_solar_time(local.time, longitude=-65.69)

# Generate light
DO_2018$light<-calc_light(DO_2018$solar.time,
                        latitude = Latitude,
                        longitude= Longitude)


#Calculate DO at saturation
DO_2018$DO.sat <- calc_DO_sat(
  temp=DO_2018$temp.water,
  press=DO_2018$press_mb,
  salinity.water = 0,)



  #this code if using tower data but we are using calc_light
  #renaming PAR to light
    #prac_dat <- prac_dat %>% 
    # rename( light = PAR_Den)


#this code if data isnt every 15 min?
      # Make the time steps 15 minutes
      # DO_2018 <- data.frame(solar.time = seq(min(DO_2018$solar.time),
      #                                        max(DO_2018$solar.time),
      #                                        by = '15 min')) %>%
      #   left_join(DO_2018, by = 'solar.time')

##taking out unneeded columns
QPmet <- subset(DO_2018, select = -c(1))


##just summer 2018
    QPmet <- subset(QPmet, format(as.Date(local.time),"%m")== c("06", "07", "08"))
    #remove rows with NAs bc missing data
DO_2018 <- na.omit(DO_2018)

##local time to solar time
local.time <- as.POSIXct(DO_2018$local.time, tz='America/Puerto_Rico')
DO_2018$solar.time <- calc_solar_time(local.time, longitude=-65.69)

# Generate light
DO_2018$light<-calc_light(DO_2018$solar.time,
                        latitude = Latitude,
                        longitude= Longitude)


#Calculate DO at saturation
DO_2018$DO.sat <- calc_DO_sat(
  temp=DO_2018$temp.water,
  press=DO_2018$press_mb,
  salinity.water = 0,)



##adding depth
DO_2018$depth <- 0.25




    
####just summer 2015####
    DO_15 <- subset(metabdata, format(as.Date(local.time),"%Y")==2015)
   
    #remove rows with NAs bc missing data
    DO_15 <- na.omit(DO_15)
    
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
    
    
    
    QP15 <- subset(DO_15, format(as.Date(local.time),"%m")== c("05", "06", "07", "08"))
    QP15 <- subset(QP15, select = -c(1, 4, 5))
    
    
    ##Alice Carter (AC) Code
    QP15 %>% unitted::v() %>%
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
    
    
    

#sM model specs for summer 2015
bayes_name <- mm_name(type='bayes', pool_K600='normal',
                      err_obs_iid=TRUE, err_proc_iid=TRUE)

bayes_specs <- specs(bayes_name,
                     burnin_steps = 1000,
                     saved_steps = 1000,
                     K600_daily_meanlog_meanlog = 2.484906649788,
                     K600_daily_meanlog_sdlog = 0.75,
                     K600_daily_sdlog_sigma = 0.001,
                     n_cores=4, verbose=T)
    


    #troubleshooting
    #taking out duplicate rows in time
    QP15 <- QP15[!duplicated(QP15$solar.time), ]
    
    
    #fitting the code
    met15 <- metab(bayes_specs, data=QP15)
    
    #viewing
    met15
    predict_metab(met15)
    plot_metab_preds(met15)
    get_params(met15)
    plot_DO_preds(met15)
    
    

####summer/fall 2017####
    
    DO_17 <- subset(metabdata, format(as.Date(local.time),"%Y")==2017)
    
    #remove rows with NAs bc missing data
    # DO_15 <- na.omit(DO_15)
    
    ##local time to solar time
    local.time <- as.POSIXct(DO_15$local.time, tz='America/Puerto_Rico')
    DO_17$solar.time <- calc_solar_time(local.time, longitude=-65.69)
    
    # Generate light
    DO_17$light<-calc_light(DO_15$solar.time,
                            latitude = Latitude,
                            longitude= Longitude)
    
    
    #Calculate DO at saturation
    DO_17$DO.sat <- calc_DO_sat(
      temp=DO_17$temp.water,
      press=DO_17$press_mb,
      salinity.water = 0,)
    
    
    
    QP17 <- subset(DO_17, format(as.Date(local.time),"%m")== c("08", "09", "10", "11"))
    QP17 <- subset(QP15, select = -c(1, 4, 5))
    
    
    ##Alice Carter (AC) Code
    QP17 %>% unitted::v() %>%
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
    
    QP17 %>% unitted::v() %>%
      mutate(discharge_cfs = log(discharge_cfs)) %>%
      select(solar.time, depth, temp.water, light, discharge_cfs) %>%
      gather(type, value, depth, temp.water, light, discharge_cfs) %>%
      mutate(
        type=ordered(type, levels=c('depth','temp.water','light','discharge_cfs')),
        units=ordered(labels[type], unname(labels))) %>%
      ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() +
      facet_grid(units ~ ., scale='free_y') + theme_bw() +
      scale_color_discrete('variable')
    
    
    
    
    #sM model specs for summer 2015
    bayes_name <- mm_name(type='bayes', pool_K600='normal',
                          err_obs_iid=TRUE, err_proc_iid=TRUE)
    
    bayes_specs <- specs(bayes_name,
                         burnin_steps = 1000,
                         saved_steps = 1000,
                         K600_daily_meanlog_meanlog = 2.484906649788,
                         K600_daily_meanlog_sdlog = 0.75,
                         K600_daily_sdlog_sigma = 0.001,
                         n_cores=4, verbose=T)
    
    
    
    #troubleshooting
    #taking out duplicate rows in time
    QP17 <- QP17[!duplicated(QP17$solar.time), ]
    
    
    #fitting the code
    met17 <- metab(bayes_specs, data=QP17)
    
    #viewing
    met17
    predict_metab(met17)
    plot_metab_preds(met17)
    get_params(met17)
    plot_DO_preds(met17)
    
    
    
    

#### 2. Configuring the model ####
##There are two steps to configuring a metabolism model in streamMetabolizer: 
###(A) Identify the name of the model structure you want using mm_name(); and 
###(B) Set the specifications for the model using defaults fromspecs() as a starting       point.



# Model Metabolism using cleaned and prepped data: ####
# Visualize the data #####
head(QPmet) ; tail(QPmet)


##Alice Carter (AC) Code
QPmet %>% unitted::v() %>%
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
#sM model specs--------------- ####
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


#troubleshooting
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







####streamMetabolizer github code

##2A. Choosing a model structure
#For this example, we will specify a Bayesian model with both observation error and process error. We won’t pool K600 here because we   don’t have many days of data, but pooling is one feature that makes Bayesian models better than MLE models in general. Another great   feature of Bayesian models is that they produce more accurate and nuanced confidence intervals.

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
