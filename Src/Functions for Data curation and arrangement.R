# Round to closest 5 min step
source('https://raw.githubusercontent.com/ZyuAFD/OptiRTCCode/master/Project/GeneralFuns.R')

FindGaps=function(Dt,minGap=1)
    #Dt dataframe with time column
    #minGap: min gap of hours  (1 for default)
{
    Dt %>% 
        arrange(Time_Stamp) %>% 
        mutate(lagT=lag(Time_Stamp)) %>% 
        filter(as.numeric(Time_Stamp-lagT,unit='hours')>minGap) ->gaps
    
    if (nrow(gaps)==0) return(data.frame(St=NULL,End=NULL))
    
    gaps %>% 
        select(Time_Stamp,lagT) %>% 
        mutate(Time_Stamp=Round_5min(Time_Stamp),
               lagT=Round_5min(lagT)) %>% 
        rename(St=lagT,End=Time_Stamp) %>% 
        return
}

# Regular 5 min time interval

Regular_5min=function(Dt)
    # Dt: data frame with columns
    #       Time_Stamp
    #       Temp_F
    #       Inst_Rain
    #       SoilM
{
    # Aggregate Rain amoutn on curated 5 min interval
    Dt %>% 
        mutate(Time=Round_5min(Time_Stamp)) %>% 
        group_by(Time) %>% 
        summarise(Inst_Rain=sum(Inst_Rain,na.rm=T))->DtRain
    
    #Find the time range of the dataset
    Dt %>% 
        select(Time_Stamp) %>% 
        summarize(min(Time_Stamp),max(Time_Stamp)) -> Dt_rng
    
    colnames(Dt_rng)=c('MinDt','MaxDt')
    
    #Round to the 5 min time step
    Dt_rng %<>% 
        mutate(MinDt=Round_5min(MinDt),
               MaxDt=Round_5min(MaxDt)) %>% 
        mutate(MaxDt=if(max(Dt$Time_Stamp)<MaxDt){
            MaxDt-minutes(5)
        } else {MaxDt})
    
    #form up the complete time step series
    Tm_srs=seq(
        from=Dt_rng$MinDt[1],
        to=Dt_rng$MaxDt[1],
        by=300
    ) 
    rm(Dt_rng)
    
    
    data.frame(
        Time=Tm_srs,
        Temp_F=na.approx(Dt$Temp_F,x=Dt$Time_Stamp,xout=Tm_srs,na.rm=F),
        SoilM=na.approx(Dt$SoilM,x=Dt$Time_Stamp,xout=Tm_srs,na.rm=F)) %>% 
        left_join(.,DtRain,by=c('Time'='Time')) ->Dt_Cure
    
    # Check gaps in original dataset
    
    gaps=FindGaps(Dt)
    
    if (nrow(gaps)>0) 
    {
        for (i in 1:nrow(gaps)){
            Dt_Cure %<>% 
                mutate(Temp_F=ifelse(between(Time,gaps$St[i],gaps$End[i]),NA,Temp_F),
                       SoilM=ifelse(between(Time,gaps$St[i],gaps$End[i]),NA,SoilM),
                       Inst_Rain=ifelse(between(Time,gaps$St[i],gaps$End[i]),NA,Inst_Rain))
        }       
    }
    
    Dt_Cure %>% return
}



# Get precipitation events

Labeling_Evts= function(dt,IntEvt=360)
    # header: 
    # Trnd, 
    # Evt_n,
    # Inst_Rain
    
    #IntEvt : minutes (6 hours by default)
{
    
    dt %>% 
        filter(!is.na(Inst_Rain),
               Inst_Rain>0) %>% 
        arrange(Time) %>% 
        mutate(lag=as.numeric(Time-lag(Time),units='mins'),
               lead=as.numeric(Time-lead(Time),units='mins')) %>% 
        mutate(Evt_n=ifelse(lag>IntEvt,1,0)) %>% 
        mutate(Evt_n=ifelse(is.na(Evt_n),1,Evt_n)) %>% 
        mutate(Evt_n_Ed=ifelse(lead(Evt_n)==1,1,0)) %>% 
        select(Time,
               Evt_n) ->Evt_n
    
    dt %<>% 
        left_join(.,Evt_n,by='Time') %>% 
        mutate(Evt_n=ifelse(is.na(Evt_n),0,Evt_n)) %>% 
        mutate(Evt_n=cumsum(Evt_n)) %>% 
        return
    
}