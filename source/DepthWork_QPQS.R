#Using Sonadora depth data to fill in month long gaps in Prieta depth data

#file created 03/19/2024 by Alicia Dixon

##loading needed packages
library(ggplot2)
library(dplyr)
library(cowplot)
library(ggpubr)
library(rstatix)
library(stats)
library(agricolae)
library(flextable)
library(tidyr)
library(viridis)
library(ggforce)
library(ggalt)
library(naniar)

#upload CSV files
QPdepth <- read.csv("./data/depth_2006_2023.csv")

Sonadora1 <- read.csv("./data/Sonadora_depth_18761.csv")
Sonadora2 <- read.csv("./data/Sonadora_depth_17276.csv")


###cleaning QP data
##combining date and time for depth
# single object
QPdepth <- unite(QPdepth, col='local.time', c('Date', 'Time'), sep=' ')
#converting feet to meters for depth
QPdepth$QPdepth <- (QPdepth$stage_ft)*0.3048 
#remove stage (ft) and discharge column
QPdepth = subset(QPdepth, select = -c(2:3) )
#renaming date column
names(QPdepth)[names(QPdepth) == "local.time"] <- "Date"


###cleaning up Sonadora data
#combining Sonadora data
QSdepth <- full_join(Sonadora1, Sonadora2, by = c("Date", "stage_ft"))
#converting feet to meters for depth
QSdepth$QSdepth <- (QSdepth$stage_ft)*0.3048 
#remove stage (ft) column
QSdepth = subset(QSdepth, select = -c(2) )




###Merging the QS and QP data frames 
#combining Sonadora data
LUQ_depth <- full_join(QSdepth, QPdepth, by = c("Date"))
#log transform
LUQ_depth$QP_logdepth <- log(LUQ_depth$QPdepth)
LUQ_depth$QS_logdepth <- log(LUQ_depth$QSdepth)

#looking at missing values
gg_miss_var(LUQ_depth)


#remove rows with NAs bc missing data
LUQ_depth <- na.omit(LUQ_depth)



###plotting the two depths against each other
ggplot(data = LUQ_depth, aes(x = QSdepth, y = QPdepth)) +
  geom_point() +
  scale_y_continuous(trans = "log10") 




#####using a log model####
log.model <-lm(log(QPdepth) ~ QSdepth, LUQ_depth)
exp.model <-lm(QPdepth ~ exp(QSdepth), LUQ_depth)

log.model.df <- data.frame(x = LUQ_depth$QSdepth,
                           y = exp(fitted(log.model)))

#plotting
ggplot(data = LUQ_depth, aes(x = QSdepth, y = QPdepth)) +
  geom_point() +
geom_smooth(method="lm", aes(color="Exp Model"), formula= (y ~ exp(x)), se=FALSE, linetype = 1) +
  geom_line(data = log.model.df, aes(x, y, color = "Log Model"), size = 1, linetype = 2) + 
  guides(color = guide_legend("Model Type"))

#looking at model
exp.model
summary(exp.model)

log.model
summary(log.model)


####creating model####
a <- exp(0.19348)
b <- 0.07867

QP_pred <- a*exp(b*LUQ_depth$QSdepth)



LUQ_data <- full_join(QSdepth, QPdepth, by = c("Date"))
LUQ_pred <- subset(LUQ_data, format(as.Date(Date),"%Y")==2015)


for(i in 1:nrow(LUQ_pred)){
  if(LUQ_pred$var[i]==" NA "){
    df$var[i]="Private"
  }
}




####looking at linear regression####
model <- lm(QPdepth ~ QSdepth, data = LUQ_depth)
#print output
model
#look at summary of model
summary(model)

##looking at regression for log transformed data
log_model <- lm(QP_logdepth ~ QS_logdepth, data = LUQ_depth)
#print output
log_model
#look at summary of model
summary(log_model)

