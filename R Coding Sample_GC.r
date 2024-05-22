#PROJECT: Coding Sample with Machine Learning and Geo-Spatial Heat-maps.

#DESCRIPTION: The following is a blueprint code to learn electricity consumption patterns in India from pre-COVID periods and predict the post-COVID period trends. We then compare these trends with the actual consumption levels, which were obviously much higher. We then mark when the nationwide lockdown was lifted and see the electricity trends (especially where the consumption starts increasing) inferring that to a proxy for economic recovery.

#NOTE: The code would essentially not run as the project data is confidential and publication is still awaited. This is just to showcase my knowlege of coding in R.

#AUTHOR: Geet Chawla
#CONTACT: geet@uchicago.edu
#DATE CREATED: 19th Aug, 2020

#Housekeeping
rm(list=ls())

#Loading libraries
library(tidyverse)
library(stringr)
library(lubridate)
library(plm)
library(broom)
require(plyr)
require(reshape2)
library(readstata13)
library(foreign)
library(sf)
library(sp)
library(rgdal)
library(maptools)

#Setting working directory (essentially to save plots)
setwd("C:/Users/****")
getwd()

#_______________Reading Essential Data_______________

load("C:/Users/*****")

weather_data <- read_csv("C:/Users/****")

print(unique(weather_data$NAME))

electricity_data <- readstata13::read.dta13("C:/Users/****")

states <- read.dbf("C:/Users/****")

#_________________Preparing to merge__________________

states <- states %>% 
  dplyr::select(STATE_ID, NAME)

states$NAME <- gsub("&", "and", states$NAME)

states$NAME <- gsub("Tamilnadu", "TAMIL NADU", states$NAME)

states$NAME <- toupper(states$NAME)

electricity_data$state[electricity_data$state == "J&K(UT) & LADAKH(UT)"] = "JAMMU AND KASHMIR"
electricity_data$state[electricity_data$state == "J&K(UT) AND LADAKH(UT)"] = "JAMMU AND KASHMIR"
electricity_data$state[electricity_data$state == "J&K(UT) &\nLADAKH(UT)"] = "J&K(UT) &\nLADAKH(UT)"

colnames(weather_data)[which(names(weather_data) == "STATE_ID")] <- "sid"

weather_data$date <- ymd(weather_data$date)

weather_data$merge <- paste(weather_data$date, weather_data$sid, sep = "-")
electricity_data$sid <- states$STATE_ID[match(electricity_data$state, states$NAME)]

electricity_data$merge <- paste(electricity_data$date_stata, electricity_data$sid, sep = "-")

weather_data_for_merge <- weather_data %>% 
  group_by(merge) %>% 
  summarise(tavg = round(mean(tavg)))

#____________________Merging and cleaning____________________

electricity_data <- left_join(electricity_data, weather_data_for_merge, by="merge")

#Making week dummy
electricity_data <- electricity_data %>% 
  mutate(month = lubridate::month(ymd(date_stata)),
         week = lubridate::week(ymd(date_stata)))

#Set datatype of sid and week to factors
electricity_data$month = as.factor(electricity_data$month)
electricity_data$week = as.factor(electricity_data$week)

#Create a numeric time ID for plm (date formats don't work well)
electricity_data$time=as.numeric(ymd(electricity_data$date)-ymd("2000-01-01"))
cutoff = as.numeric(ymd("2020-01-01")-ymd("2000-01-01"))

#Create a separate variable to use for time trends 
electricity_data$trend_1=electricity_data$time
electricity_data$trend_2=electricity_data$time^2
electricity_data$trend_3=electricity_data$time^3

#Drop rows with NA - This is often important when predicting with f.e because f.e that are dropped in the model estimation may otherwise appear in the prediction data
electricity_data <- electricity_data %>% 
  dplyr::select(state, date_stata, energy_met, tavg, month, week, time, trend_1, trend_2, trend_3)

electricity_data=na.omit(electricity_data)

#Removing 25th Nov 2014 as it has crazy anomaly

electricity_data <- electricity_data %>% 
  filter(time != 5442)

#########################################################
#-------------------Running State Models-----------------
#########################################################

#Saving state specific models
elec_fit_data <- list()
elec_fit_models <- list()

for( i in unique(electricity_data$state) ){
  elec_fit_data[[i]] <- filter(electricity_data, state == i & time < cutoff)
  elec_fit_models[[i]] <- lm(energy_met~trend_1+trend_2+trend_3+week+tavg, data = elec_fit_data[[i]])
}

#Making a list of dataframes that have only pre cutoff data and predicting
elec_preds_pre <- plyr::mdply(cbind(mod = elec_fit_models, df = elec_fit_data), function(mod, df) {
  mutate(df, value = predict(mod, newdata = df))
})

#Making a list of dataframes that have only post cutoff data 
elec_predict_data <- list()
for (i in electricity_data$state){
  data <- filter(electricity_data, state == i & time >= cutoff)
  elec_predict_data[[i]] <- data
}

elec_preds_post <- plyr::mdply(cbind(mod = elec_fit_models, df = elec_predict_data), function(mod, df) {
  mutate(df, value = predict(mod, newdata = df))
})

elec_plotmat=as.data.frame(rbind(elec_preds_pre,elec_preds_post))
elec_plotmat$month=month(ymd("2000-01-01")+elec_plotmat$time)
elec_plotmat$year=year(ymd("2000-01-01")+elec_plotmat$time)

india_lockdown_start=as.numeric(ymd("2020-03-25")-ymd("2000-01-01"))
india_lockdown_end=as.numeric(ymd("2020-06-01")-ymd("2000-01-01"))
lock_down_window=which( (elec_plotmat$time>=india_lockdown_start)&(elec_plotmat$time<=india_lockdown_end) )

elec_plotmat$india_lockdown=0
elec_plotmat$india_lockdown[lock_down_window]=1

elec_plotmat <- elec_plotmat %>% 
  filter(time < 7518) %>% 
  mutate(last_month_dummy = ifelse(time>=7487,1, 0),
         prediction_error = energy_met-value)

elec_states_time_series_predicted_recorded <- elec_plotmat %>% 
  dplyr::select(date_stata, state, energy_met, value, prediction_error)


write.csv(elec_states_time_series_predicted_recorded, "C:/Users/****")

#_______________Performing Regressions For Each State_________________

elec_plotmat_ex2020 <- elec_plotmat %>% 
  filter(year<2020)

elec_prediction_error_data <- list()
elec_prediction_error_mean <- list()
for (i in unique(elec_plotmat_ex2020$state) ){
  elec_prediction_error_data[[i]] <- filter(electricity_data_ex2020, state == i,)
  elec_prediction_error_mean[[i]] <- mean(elec_prediction_error_data[[i]]$energy_met)
}

elec_prediction_error_mean <- tibble::rownames_to_column(as.data.frame(t(elec_prediction_error_mean)), "NAME")
colnames(elec_prediction_error_mean)[which(names(elec_prediction_error_mean) == "V1")] <- "mean_elec"

#________________PREDICTION ERROR MEDIAN AND LASTMONTH_________________


elec_plotmat_lockdown_median_data <- elec_plotmat %>% 
  filter(india_lockdown==1) %>% 
  group_by(state) %>% 
  summarize(elec_median_lockdown = median(prediction_error))

elec_plotmat_lastmonth_median_data <- elec_plotmat %>% 
  filter(last_month_dummy==1) %>% 
  group_by(state) %>% 
  summarize(elec_median_lastmonth = median(prediction_error))

elec_plotmat_median_data <- merge(elec_plotmat_lockdown_median_data, elec_plotmat_lastmonth_median_data, by = "state")

colnames(elec_plotmat_median_data)[which(names(elec_plotmat_median_data) == "state")] <- "NAME"

elec_plotmat_median_data <- left_join(elec_plotmat_median_data, elec_prediction_error_mean, by = "NAME")

elec_plotmat_median_data$mean_elec = as.numeric(elec_plotmat_median_data$mean_elec)

elec_plotmat_median_data <- elec_plotmat_median_data %>% 
  mutate(elec_lockdown_median.mean = elec_median_lockdown/mean_elec,
         elec_lastmonth_median.mean = elec_median_lastmonth / mean_elec)

elec_plotmat_median_data <- elec_plotmat_median_data %>% 
  mutate(elec_lockdown_median.mean = elec_lockdown_median.mean*100,
         elec_lastmonth_median.mean = elec_lastmonth_median.mean*100)

write.csv(elec_plotmat_median_data, "C:/Users/****")

#_______________________Heatmaps / Spatial Plotting_______________________


india.states <- read_sf(dsn = "C:/Users/****", layer = "STATE_11")
sid_unique=unique(india.states$STATE_ID)

india.states <- india.states %>% 
  dplyr::select(STATE_ID, NAME)

india.states$NAME <- gsub("&", "and", states$NAME)

india.states$NAME <- gsub("Tamilnadu", "TAMIL NADU", states$NAME)

india.states$NAME <- toupper(states$NAME)

elec_heatmap <- left_join(india.states, elec_plotmat_median_data, by="NAME")

elec_median_lockdown_scale <- ggplot(elec_heatmap)+
  geom_sf(aes(fill= elec_lockdown_median.mean))+
  scale_fill_gradient2(low = "darkblue",high = "darkgray",midpoint=0, limits = c(-60,60), breaks= c(60,30,0,-30, -60))+
  theme_classic(base_size = 15)+
  theme(legend.title=element_blank(),
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

ggsave("elec_median_lockdown_scale.pdf", plot = elec_median_lockdown_scale)

elec_median_lastmonth_scale <- ggplot(elec_heatmap)+
  geom_sf(aes(fill= elec_lastmonth_median.mean))+
  scale_fill_gradient2(low = "darkblue",high = "darkgray",midpoint=0, limits = c(-60,60), breaks= c(60,30,0,-30, -60))+
  theme_classic(base_size = 15)+
  theme(legend.title=element_blank(),
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

ggsave("elec_median_lastmonth_scale.pdf", plot = elec_median_lastmonth_scale)


save.image("xyz.Rdata")
