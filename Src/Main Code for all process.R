# Process to Run the DTW with Apriori Algorithm

# Data Loading Functions
source('https://raw.githubusercontent.com/ZyuAFD/DTW_Apriori/master/Src/Data%20Loading.R')
# Data Arrangement Functions
source('https://raw.githubusercontent.com/ZyuAFD/DTW_Apriori/master/Src/Data%20Arrangement.R')
# DTW Functions
source('https://raw.githubusercontent.com/ZyuAFD/DTW_Apriori/master/Src/DTW%20functions.R')
# Paired Event Feature Difference Functions
source('https://raw.githubusercontent.com/ZyuAFD/DTW_Apriori/master/Src/Evt%20Chara%20Extract.R')
# Apriori Functions
source('https://raw.githubusercontent.com/ZyuAFD/DTW_Apriori/master/Src/Apriori%20functions.R')


### Single Probe application with only climatic features ---------------------
dt=Dt_5min_cure %>% filter(Probe=='SouthCtr')
### Multiple Probe application with only climatic features -------------------
dt=Dt_5min_cure 

####  DTW Calculations


Dist.df=DTW_dist_cal(dt)
Dist_threshold=Dist_thresholds(Dist.df)

#### Generate PEFD
Evt_Chara=Event_patterns(dt)
Evt_Dif_quantile=lapply(Evt_Chara$Evt_n,Get_Evt_quantiles,Evt_Chara=Evt_Chara) %>% bind_rows()
#get PC quantile
PCquantile=lapply(Evt_Chara$Evt_n,PC_Dif_quantiles,Evt_Chara=Evt_Chara) %>% 
    bind_rows() %>% 
    pull(PC) %>% 
    max %>% 
    `/`(5) %>% 
    `*`(c(1,2,3,4,5)) %>% 
    signif(digit=4) %>% 
    as.character() %>% 
    paste(collapse=',') %>% 
    rep(.,length(Evt_Chara$Evt_n))


Event_info=lapply(Evt_Chara$Evt_n,
                  Labeling_Evt_Character, 
                  Ref_Evt_Chara=Evt_Chara,
                  Trgt_Evt_Chara=Evt_Chara,
                  Dist.df=Dist.df,
                  Dist_threshold=Dist_threshold,
                  Evt_Dif_quantile=Evt_Dif_quantile) %>% 
    rbindlist(.) 


#### Apriori Algorithm
Patterns=MatchEvtPtn(Event_info) 
Pt_freq_df=Patterns[[1]] %>% 
    mutate(lhs=as.character(lhs),rhs=as.character(rhs))
Soil_Seq_ts=Patterns[[2]] %>% 
    separate(Items,c('AvgTdif','DryPddif','Durhrdif','EvtPdif','Jday','PCdif','RainPdhrdif','Distdif'),",")





### 6 Expand algorithm on test data ------------------------
dt=Dt_5min_4test

New_Evt_Chara=Event_patterns(dt)
New_Event_info=lapply(Evt_Chara$Evt_n,
                  Labeling_Evt_Character, 
                  Ref_Evt_Chara=Evt_Chara,
                  Trgt_Evt_Chara=New_Evt_Chara,
                  Evt_Dif_quantile=Evt_Dif_quantile) %>% 
    rbindlist(.) 



