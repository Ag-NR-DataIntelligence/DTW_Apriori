# Process to Run the DTW with Apriori Algorithm

source('https://raw.githubusercontent.com/ZyuAFD/SWRE_Project/master/General-Functions/EmailCompleteMsg.R')
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

Evt_Differences=lapply(
    Evt_Chara$Evt_n,
    Evt_Difs,
    Ref_Evt_Chara=Evt_Chara,
    Trgt_Evt_Chara=Evt_Chara) %>% 
    bind_rows %>% 
    as_data_frame()


### Quantile group by each event------------
Evt_Dif_quantile_ech_Evts=lapply(
    Evt_Chara$Evt_n,
    Get_Evt_quantiles_byech,
    Evt_Chara=Evt_Chara) %>% 
    bind_rows() %>% 
    {
        #get PC quantile by all events
        df=.
        PC=Evt_Differences %>% 
            pull(PC) %>% 
            abs %>% 
            max %>% 
            `/`(5) %>% 
            `*`(c(0,1,2,3,4,5)) %>% 
            list()
        df$PC_dif=PC
        df
    }

Evt_Dif_quantile_ech_Evts=Get_Evt_quantiles_byech(Evt_Differences)

### Quantile by all events---------

Evt_Dif_quantile=Evt_Dif_quantiles(Evt_Differences)



### Get event information for Apriori
Event_info=lapply(Evt_Chara$Evt_n,
                  Labeling_Evt_Character, 
                  Ref_Evt_Chara=Evt_Chara,
                  Trgt_Evt_Chara=Evt_Chara,
                  Dist.df=Dist.df,
                  Dist_threshold=Dist_threshold,
                  #Evt_Dif_quantile=Evt_Dif_quantile, # Using quantiles for all events
                  Evt_Dif_quantile=Evt_Dif_quantile_ech_Evts,  # Using quantiles grouped by each event
                  Evt_difs=Evt_Differences,
                  AddPC=T) %>% 
    rbindlist(.) 

#### Apriori Algorithm----------
Patterns=MatchEvtPtn(Event_info,AddPC=T) 
Pt_freq_df=Patterns[[1]] %>% 
    mutate(lhs=as.character(lhs),rhs=as.character(rhs))
Soil_Seq_ts=Patterns[[2]] %>% 
    # #Without PC
    # separate(Items,c('AvgTdif','DryPddif','Durhrdif','EvtPdif','Jday','RainPdhrdif','Distdif'),",")
# With PC
separate(Items,c('AvgTdif','DryPddif','Durhrdif','EvtPdif','Jday','PCdif','RainPdhrdif','Distdif'),",")





### 6 Expand algorithm on test data ------------------------
dt=Dt_5min_4test

New_Evt_Chara=Event_patterns(dt)


New_Evt_Differences=lapply(
    Evt_Chara$Evt_n,
    Evt_Difs,
    Ref_Evt_Chara=Evt_Chara,
    Trgt_Evt_Chara=New_Evt_Chara) %>% 
    bind_rows %>% 
    filter(Evt_n!=Ref_Evt)

New_Event_info=lapply(Evt_Chara$Evt_n,
                      Labeling_Evt_Character, 
                      Ref_Evt_Chara=Evt_Chara,
                      Trgt_Evt_Chara=New_Evt_Chara,
                      #Evt_Dif_quantile=Evt_Dif_quantile, # Using quantiles for all events
                      Evt_Dif_quantile=Evt_Dif_quantile_ech_Evts,  # Using quantiles grouped by each event
                      Evt_difs=New_Evt_Differences,
                      AddPC=T) %>% 
    rbindlist(.) 
