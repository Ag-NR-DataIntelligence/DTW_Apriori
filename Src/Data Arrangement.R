source('https://raw.githubusercontent.com/ZyuAFD/DTW_Apriori/master/Src/External%20Functions%20and%20librarys.R')

# Loading data arrange functions 
source('https://raw.githubusercontent.com/ZyuAFD/DTW_Apriori/master/Src/Functions%20for%20Data%20curation%20and%20arrangement.R')

# South Center sensor--------
GoldaMeir_Dt_Feb2017 %>% 
    mutate(SoilM=Est_SouthCtr_SoilM,Inst_Rain=Inst_Rain_in) %>% 
    Regular_5min %>% 
    Labeling_Evts -> Dt_5min_cure

# Get all events with gaps
Dt_5min_cure %>% 
    filter(is.na(SoilM)) %>% 
    group_by(Evt_n) %>% 
    tally ->Evt_gap

Dt_5min_cure %<>% 
    filter(!(Evt_n %in% Evt_gap$Evt_n)) 

Dt_5min_cure_SouthCtr=Dt_5min_cure %>% mutate(Probe='SouthCtr')

# North Center sensor--------
GoldaMeir_Dt_Feb2017 %>% 
    mutate(SoilM=Est_NorthCtr_SoilM,Inst_Rain=Inst_Rain_in) %>% 
    Regular_5min %>% 
    Labeling_Evts -> Dt_5min_cure

# Get all events with gaps
Dt_5min_cure %>% 
    filter(is.na(SoilM)) %>% 
    group_by(Evt_n) %>% 
    tally ->Evt_gap

Dt_5min_cure %<>% 
    filter(!(Evt_n %in% Evt_gap$Evt_n)) 

Dt_5min_cure_NorthCtr=Dt_5min_cure %>% 
    filter(Time<ymd('2016-12-20')) %>% 
    mutate(Evt_n=Evt_n+max(Dt_5min_cure_SouthCtr$Evt_n)) %>% 
    filter(Evt_n<max(Evt_n)) %>% 
    mutate(Probe='NorthCtr')

Dt_5min_cure_NorthCtr4test=Dt_5min_cure %>% 
    filter(Time>ymd('2016-12-20')) %>% 
    mutate(Probe='NorthCtr')

# South Center sensor--------
GoldaMeir_Dt_Feb2017 %>% 
    mutate(SoilM=Est_South_SoilM,Inst_Rain=Inst_Rain_in) %>% 
    Regular_5min %>% 
    Labeling_Evts -> Dt_5min_cure

# Get all events with gaps
Dt_5min_cure %>% 
    filter(is.na(SoilM)) %>% 
    group_by(Evt_n) %>% 
    tally ->Evt_gap

Dt_5min_cure %<>% 
    filter(!(Evt_n %in% Evt_gap$Evt_n)) 

Dt_5min_cure_South=Dt_5min_cure %>% 
    filter(Time<ymd('2016-07-01')) %>% 
    mutate(Evt_n=Evt_n+max(Dt_5min_cure_NorthCtr$Evt_n)) %>% 
    filter(Evt_n<max(Evt_n))%>% 
    mutate(Probe='South')


Dt_5min_cure_South4test=Dt_5min_cure %>% 
    filter(Time>ymd('2016-07-01')) %>% 
    mutate(Probe='South')

# Combining data ----------------------------
Dt_5min_cure=rbind(Dt_5min_cure_SouthCtr,
                   Dt_5min_cure_NorthCtr,
                   Dt_5min_cure_South) %>% 
    mutate(Evt_n=as.character(Evt_n))

Dt_5min_4test=rbind(Dt_5min_cure_South4test %>% mutate(Evt_n=paste0('South',as.character(Evt_n))),
                    Dt_5min_cure_NorthCtr4test %>% mutate(Evt_n=paste0('NorthCtr',as.character(Evt_n))))

# Moving average -------------------------------
Dt_5min_cure %<>% 
    group_by(Probe) %>% 
    mutate(SoilM.raw=SoilM) %>% 
    mutate(SoilM=rollmean(SoilM.raw,
                          12*12, #half a day on 5 min interval
                          align='center',
                          fill=NA,
                          na.rm=T)) %>% 
    ungroup %>% 
    filter(!is.na(SoilM)) 


Dt_5min_4test %<>% 
    group_by(Probe) %>% 
    mutate(SoilM.raw=SoilM) %>% 
    mutate(SoilM=rollmean(SoilM.raw,
                          12*12, #half a day on 5 min interval
                          align='center',
                          fill=NA,
                          na.rm=T)) %>% 
    ungroup %>% 
    filter(!is.na(SoilM)) 

rm(Dt_5min_cure_NorthCtr,
   Dt_5min_cure_NorthCtr4test,
   Dt_5min_cure_South,
   Dt_5min_cure_South4test,
   Dt_5min_cure_SouthCtr,
   Evt_gap)