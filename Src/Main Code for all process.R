# Process to Run the DTW with Apriori Algorithm

### 1  Loading data --------------------
source('https://raw.githubusercontent.com/ZyuAFD/DTW_Apriori/master/Src/Data%20Loading.R')

### 2 Arrange Data ----------------------
source('https://raw.githubusercontent.com/ZyuAFD/DTW_Apriori/master/Src/Data%20Arrangement.R')

### 3 DTW Calculations --------------------
source('https://raw.githubusercontent.com/ZyuAFD/DTW_Apriori/master/Src/DTW%20functions.R')

Dist.df=DTW_dist_cal(dt)
Dist_threshold=Dist_thresholds(Dist.df)

### 4 Generate Paired Event Character Difference Pattern----------------------------
source('https://raw.githubusercontent.com/ZyuAFD/DTW_Apriori/master/Src/Evt%20Chara%20Extract.R')

Evt_Chara=Event_patterns(dt)
Event_info=lapply(Evt_Chara$Evt_n,Labeling_Evt_Character, Evt_Chara=Evt_Chara,Dist.df=Dist.df,Dist_threshold=Dist_threshold) %>% 
    rbindlist(.) %>%
    filter(Jday=='in_season') # only included events in season

### 5 Apriori Algorithm-----------------------------
source('https://raw.githubusercontent.com/ZyuAFD/DTW_Apriori/master/Src/Apriori%20functions.R')

Patterns=MatchEvtPtn(Event_info) 
Pt_freq_df=Patterns[[1]]
Soil_Seq_ts=Patterns[[2]]



