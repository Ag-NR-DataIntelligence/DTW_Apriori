source('https://raw.githubusercontent.com/ZyuAFD/OptiRTCCode/master/Project/Library%20and%20Style%20Load.R')

# Round to closest 5 min step
source('https://raw.githubusercontent.com/ZyuAFD/OptiRTCCode/master/Project/GeneralFuns.R')


library(tidyverse)
library(magrittr)
library(lubridate)
library(RcppRoll)
library(data.table)
library(arules)
library(scales)

# Data load
# Data from Alex on Golda Meir ----------------
path='F:\\Projects\\Opti\\Golda Meir DTW and Apriori\\data\\'
file='gold-meir-2017-feb-download.csv'
filepath=paste0(path,file)

GoldaMeir_Dt_Feb2017=fread(filepath,
                           col.names=c('Time_UTC',
                                       'Inst_Rain_in',
                                       'Temp_F',
                                       'Est_North_SoilM',
                                       'Est_NorthCtr_SoilM',
                                       'Est_SouthCtr_SoilM',
                                       'Est_South_SoilM',
                                       'Est_SoilM')) %>% 
    mutate(Time=ymd_hms(Time_UTC),
           Time_Stamp=ymd_hm(substr(Time_UTC,1,16))) %>% 
    group_by(Time_Stamp) %>%
    summarise(Inst_Rain_in=mean(Inst_Rain_in,na.rm=T),
              Temp_F=mean(Temp_F,na.rm=T),
              Est_North_SoilM=mean(Est_North_SoilM,na.rm=T),
              Est_NorthCtr_SoilM=mean(Est_NorthCtr_SoilM,na.rm=T),
              Est_South_SoilM=mean(Est_South_SoilM,na.rm=T),
              Est_SouthCtr_SoilM=mean(Est_SouthCtr_SoilM,na.rm=T),
              Est_SoilM=mean(Est_SoilM,na.rm=T))