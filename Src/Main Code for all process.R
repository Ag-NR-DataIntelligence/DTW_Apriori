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


DTW_Apriori=function (dt,QuantileType="by each event",withPC=TRUE,CalDist=TRUE)
{
    ####  DTW Calculations
    
    if (CalDist) Dist.df=DTW_dist_cal(dt)
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
    Evt_Dif_quantile_ech_Evts=Get_Evt_quantiles_byech(Evt_Differences)
    
    ### Quantile by all events---------
    Evt_Dif_quantile=Evt_Dif_quantiles(Evt_Differences)
    
    if (QuantileType=="by each event") {
        Evt_Dif_qtle=Evt_Dif_quantile_ech_Evts
    } else {
        Evt_Dif_qtle=Evt_Dif_quantile
    }
    
    
    ### Get event information for Apriori
    Event_info=lapply(Evt_Chara$Evt_n,
                      Labeling_Evt_Character, 
                      Ref_Evt_Chara=Evt_Chara,
                      Trgt_Evt_Chara=Evt_Chara,
                      Dist.df=Dist.df,
                      Dist_threshold=Dist_threshold,
                      Evt_Dif_quantile=Evt_Dif_qtle,
                      # #Evt_Dif_quantile=Evt_Dif_quantile, # Using quantiles for all events
                      # Evt_Dif_quantile=Evt_Dif_quantile_ech_Evts,  # Using quantiles grouped by each event
                      Evt_difs=Evt_Differences,
                      AddPC=T) %>% 
        rbindlist(.) 
    
    #### Apriori Algorithm----------
    Patterns<<-MatchEvtPtn(Event_info,AddPC=withPC) 
    Pt_freq_df<<-Patterns[[1]] %>% 
        mutate(lhs=as.character(lhs),rhs=as.character(rhs))
    Soil_Seq_ts<<-Patterns[[2]] %>% 
    {
       
        # #Without PC
        if (!withPC) separate(.,Items,c('AvgTdif','DryPddif','Durhrdif','EvtPdif','Jday','RainPdhrdif','Distdif'),",")
        
        # With PC
        if (withPC) separate(.,Items,c('AvgTdif','DryPddif','Durhrdif','EvtPdif','Jday','PCdif','RainPdhrdif','Distdif'),",")
    }
        
}


### Single Probe application with only climatic features ---------------------
dt=Dt_5min_cure %>% filter(Probe=='SouthCtr')
Dist.df=read_rds("F:\\Projects\\Opti\\DTW_Apriori\\Single Probe Dist.RDS")
### Multiple Probe application with only climatic features -------------------
dt=Dt_5min_cure 
Dist.df=read_rds("F:\\Projects\\Opti\\DTW_Apriori\\Distance.RDS")


DTW_Apriori(dt,CalDist=FALSE)




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


New_Event_info %>% 
    filter(Jday_diff=='Jday_diff=in_season') %>% 
    #unite(pattern,AvgT_diff,DryPd_hr_diff,Dur_hr_diff,EvtP_diff,Jday_diff,RainPd_hr_diff,sep=' ') %>%  # pattern without PC
    unite(pattern,AvgT_diff,DryPd_hr_diff,Dur_hr_diff,EvtP_diff,Jday_diff,PC_diff,RainPd_hr_diff,sep=' ') %>%   #Pattern without PC
    left_join(Pt_freq_df,by=c('pattern'='lhs')) %>% 
    
    #######################################################
# Change these criteria to customize the calculation of overall confidence
    filter(!is.na(rhs),
           lift>1,
           #support>0.0001,
           confidence>=0.6,
           rhs=='Dist=TRUE') %>% 
    group_by(Evt_n,rhs) %>% 
    summarise(Overall_Conf=sum(support*confidence,na.rm=T)/sum(support,na.rm=T),
              Overall_Sup=sum(support,na.rm=T)) ->results