#metabolism data analysis for Qubrada Prieta in LUQ, PR
##using model estimates and data sets calculated in streamMetabolizer rto do further analysis

#Author: Alicia Dixon

#Last edited: 3/13/2024



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




##upload csv file
met678_data <- read.csv("./data/met678_03072024/met678_model_results.csv")


##cleaning up dataframe
#delete unneeded rows
met678 <- subset(met678_data, select = -c(8:10))

#convert date from character to date object class
met678$Date <- as.Date(met678$date, format= "%m/%d/%Y") 

#create P:R ratio
met678$P_R_ratio <- ((abs(met678$GPP))/(abs(met678$ER)))
                 







####Line graphs for METAB####
class(met678$date)

#create GPP line graph
gpp <- ggplot(data = met678, aes(x=Date, y=GPP)) +
  geom_rect(aes(xmin = as.Date("2017-09-07"), xmax = as.Date("2017-09-23"), ymin = -Inf, ymax = Inf),  fill = "lightblue", alpha = 0.02) +
  geom_rect(aes(xmin = as.Date("2016-01-01"), xmax = as.Date("2016-11-08"), ymin = -Inf, ymax = Inf),  fill = "peachpuff", alpha = 0.05) +
  geom_point(color = "darkgreen") +
  geom_line(color= "darkgreen") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_x_date(breaks= "6 months") +
  theme_bw() +
  labs(x= "Date", y= "GPP (g m^-2 d^-1)") 

#print graph  
gpp  

#create ER line graph
er <- ggplot(data = met678, aes(x=Date, y=ER)) +
  geom_rect(aes(xmin = as.Date("2017-09-07"), xmax = as.Date("2017-09-23"), ymin = -Inf, ymax = Inf),  fill = "lightblue", alpha = 0.02) +
  geom_rect(aes(xmin = as.Date("2016-01-01"), xmax = as.Date("2016-11-08"), ymin = -Inf, ymax = Inf),  fill = "peachpuff", alpha = 0.05) +
  geom_point(color= "sienna4") +
  geom_line(color = "sienna4") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_x_date(breaks= "6 months") +
  theme_bw() +
  labs(x= "Date", y= "ER (g m^-2 d^-1)") 

#print graph
er  

#arrange graphs together
ggarrange(gpp + theme(axis.text.x = element_blank(), axis.title.x = element_blank()), er , 
          nrow=2)








####Line graphs for P:R####
class(met678$date)

#create P:R ratio line graph
P_R <- ggplot(data = met678, aes(x=Date, y=P_R_ratio)) +
  geom_rect(aes(xmin = as.Date("2017-09-07"), xmax = as.Date("2017-09-23"), ymin = -Inf, ymax = Inf),  fill = "lightblue", alpha = 0.02) +
  geom_rect(aes(xmin = as.Date("2016-01-01"), xmax = as.Date("2016-11-08"), ymin = -Inf, ymax = Inf),  fill = "peachpuff", alpha = 0.05) +
  geom_point(shape=16) +
  geom_line() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  scale_x_date(breaks= "6 months") +
  theme_bw() +
  labs(x= "Date", y= "P:R ratio") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16))

#print graph  
P_R  


##GPP vs ER

#creating disturbacne category based on date
met678$DroughtCategory <- ifelse(met678$Date >= as.Date("2016-01-01") & met678$Date <= as.Date("2016-11-08"), "Drought",
       ifelse(met678$Date >= as.Date("2017-09-07") & met678$Date <= as.Date("2017-09-22"), "Hurricane",
              ifelse(met678$Date >= as.Date("2017-09-23") & met678$Date <= as.Date("2017-12-07"), "0-3 months Post Hurricane", 
                     ifelse(met678$Date >= as.Date("2017-12-08") & met678$Date <= as.Date("2018-06-07"), "3-9 months Post Hurricane", "Baseline"))))




#making disturbance category a factor
met678$DroughtCategory <- as.factor(met678$DroughtCategory)


met678$absER <- abs(met678$ER)
                
#plotting
scatterplot <- ggplot(data = met678, aes(x = GPP, y = absER, color = DroughtCategory)) +
  geom_point(shape = 16, size = 1.75) +   
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "maroon1", lwd= 1.25) +  # Add 1-to-1 line
  theme_bw() +
  labs(x = "GPP", y = "ER") +
  scale_color_manual(values = c("deepskyblue", "deepskyblue3", "grey67", "peru", "dodgerblue4"), name = "Disturbance") +
  geom_encircle(expand=0, size=2) +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16), 
        legend.title = element_text(size = 16), 
        legend.text = element_text(size = 14))



#print graph
scatterplot




##just hurricane and drought
dist_scatterplot <- ggplot(data = subset(met678, DroughtCategory != "Baseline"), aes(x = GPP, y = absER, color = DroughtCategory)) +
  geom_point(shape = 16, size = 1.75) +   
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "maroon1", lwd= 1.25) +  # Add 1-to-1 line
  theme_bw() +
  labs(x = "GPP", y = "ER") +
  scale_color_manual(values = c("deepskyblue", "deepskyblue3", "peru", "dodgerblue4"), name = "Disturbance") +
  geom_encircle(expand=0.01, size=2) +
  xlim(-10, 30) +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16), 
        legend.title = element_text(size = 16), 
        legend.text = element_text(size = 14))




dist_scatterplot 




####just 2017####

#subset to get just 2017
met17 <- subset(met678, format(Date, "%Y") == "2017")


##Line graphs
class(met17$date)

#create GPP line graph
gpp17 <- ggplot(data = met17, aes(x=Date, y=GPP)) +
  geom_rect(aes(xmin = as.Date("2017-09-07"), xmax = as.Date("2017-09-23"), ymin = -Inf, ymax = Inf),  fill = "lightblue", alpha = 0.02) +
  geom_point(color = "darkgreen") +
  geom_line(color= "darkgreen") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_x_date(breaks= "3 months") +
  theme_bw() +
  labs(x= "Date", y= "GPP (g m^-2 d^-1)") 

#print graph  
gpp17

#create ER line graph
er17 <- ggplot(data = met17, aes(x=Date, y=ER)) +
  geom_rect(aes(xmin = as.Date("2017-09-07"), xmax = as.Date("2017-09-23"), ymin = -Inf, ymax = Inf),  fill = "lightblue", alpha = 0.02) +
  geom_point(color= "sienna4") +
  geom_line(color = "sienna4") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_x_date(breaks= "3 months") +
  theme_bw() +
  labs(x= "Date", y= "ER (g m^-2 d^-1)") 

#print graph
er17  

#arrange graphs together
ggarrange(gpp17  + theme(axis.text.x = element_blank(), axis.title.x = element_blank()), er17 , 
          nrow=2)

