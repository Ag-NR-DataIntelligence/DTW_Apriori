source('https://raw.githubusercontent.com/ZyuAFD/OptiRTCCode/master/Project/Library%20and%20Style%20Load.R')
# overall time series plot--------------------------------
GoldaMeir_Dt_Feb2017 %>%
    #filter(between(Time_Stamp,ymd('2016-08-1'),ymd('2016-10-01'))) %>%
    ggplot()+
    #geom_point(aes(x=Time_Stamp,y=Est_North_SoilM,color='North'),alpha=0.4,size=0.5)+
    geom_point(aes(x=Time_Stamp,y=Est_NorthCtr_SoilM,color='North Ctr'),alpha=0.4,size=1)+
    geom_point(aes(x=Time_Stamp,y=Est_South_SoilM,color='South'),alpha=0.4,size=1)+
    geom_point(aes(x=Time_Stamp,y=Est_SouthCtr_SoilM,color='South Ctr'),alpha=0.4,size=1)+
    ylab('Soil Moisture with Imperfect Calibration')+
    ylim(-0.4,0.5)+
    xlab('')+
    scale_color_discrete('Probe')+
    Plot_theme

ggsave('Images\\All Observation.jpg',width = 10, height = 6, units = c("in"))

Dt_5min_cure %>% 
    group_by(Evt_n) %>% 
    tally %>% 
    arrange(n)

# Moving average smooth plot------------------
ggplot()+
    geom_point(data=GoldaMeir_Dt_Feb2017 %>% filter(between(Time_Stamp,ymd('2015-11-1'),ymd('2015-12-15'))),
               aes(x=Time_Stamp,y=Est_SouthCtr_SoilM,color='Observation'),size=2.5,alpha=0.1)+
    geom_point(data=Dt_5min_cure %>% filter(between(Time,ymd('2015-11-1'),ymd('2015-12-15')),Probe=='SouthCtr') %>% mutate(Evt_St=ifelse(lag(Evt_n)<Evt_n,1,0.3)),
               aes(x=Time,y=SoilM,color='Moving Average',size=Evt_St),alpha=0.5)+
    ylab('Soil Moisture with Imperfect Calibration')+
    scale_color_discrete('')+
    scale_size(guide='none')+
    xlab('')+
    Plot_theme

ggsave('Images\\Moving Average.jpg',width = 10, height = 6, units = c("in"))

# Alignment figure in paper------------------------------------
library(dtw)
dtw_cal=dtw(
    (
        Dt_5min_cure %>% 
            filter(Evt_n==57) %>%
            mutate(N=row_number()) %>% 
            filter(N %% 10==0) %>% 
            arrange(Time) %>% 
            pull(SoilM)%>% 
            scale()),
    (Dt_5min_cure %>% 
         filter(Evt_n==81) %>% 
         mutate(N=row_number()) %>% 
         filter(N %% 10==0) %>% 
         arrange(Time) %>% 
         pull(SoilM) %>% 
         scale()),
    
    keep=TRUE,
    #window.type = 'itakura',
    #step=symmetric2,
    open.end = T
)

dtwPlot(dtw_cal, type="twoway",ylab='')

#output   DTW sample events alignment    800*400


# Distance threshold determination charts----------------------------------
Dt_5min_cure %>% 
    group_by(Evt_n) %>% 
    tally->y

#PCA
Dist.df %>% 
    filter(Dist>0) %>% 
    group_by(Ref) %>% 
    summarize(AvgDist=mean(Dist),
              SD=sd(Dist))->pnts

prin_comp <- prcomp(pnts %>% select(-Ref), scale. = T)
prin_comp$rotation

## Generally unmatched events
pnts %>% 
    mutate(PC1=prin_comp$rotation[2,1]*SD+prin_comp$rotation[1,1]*AvgDist,
           PC2=abs(prin_comp$rotation[1,2])*AvgDist-abs(prin_comp$rotation[2,2])*SD) %>% 
    filter(PC2>quantile(PC2,0.95)) %>% 
    pull(Ref) %>% 
    as.character()->False_Ref


## Generally matched events
pnts %>% 
    mutate(PC1=prin_comp$rotation[2,1]*SD+prin_comp$rotation[1,1]*AvgDist,
           PC2=prin_comp$rotation[1,2]*AvgDist+prin_comp$rotation[2,2]*SD) %>% 
    filter(PC1>quantile(PC1,0.95)) %>% 
    pull(Ref) %>% 
    as.character()->True_Ref

# Hisogram to demonstrate the threshold for categorizing distance

Dist.df %>% 
    filter(Ref %in% c(69,44)) %>% 
    mutate(Relationship=case_when(Ref %in% True_Ref ~ 'Generally Matched',
                                  Ref %in% False_Ref ~'Generally Unmatched')) %>% 
    ggplot()+
    geom_histogram(aes(x=Dist,fill=Relationship),position = "identity",alpha=0.6)+
    scale_fill_discrete('')+
    xlab('DTW Distance')+
    theme_bw()+
    theme(legend.position="bottom")

ggsave('Images\\Distance histogram.jpg',width = 8, height = 5, units = c("in"))

# Scatter plot of all Ref event summary measures and categories-----------------------------
PC=c('PC1','PC1','PC2','PC2')
AvgDist=c(prin_comp$center['AvgDist']+prin_comp$rotation['AvgDist','PC1']/40,prin_comp$center['AvgDist'],
          prin_comp$center['AvgDist']+abs(prin_comp$rotation['AvgDist','PC2'])/40,prin_comp$center['AvgDist'])
SD=c(prin_comp$center['SD']+prin_comp$rotation['SD','PC1']/40,prin_comp$center['SD'],
     prin_comp$center['SD']-abs(prin_comp$rotation['SD','PC2'])/40,prin_comp$center['SD'])
DATA <- data.frame(PC, AvgDist, SD)

Dist.df %>% 
    filter(Dist>0) %>% 
    group_by(Ref) %>% 
    summarize(AvgDist=mean(Dist),
              SD=sd(Dist)) %>% 
    mutate(CoV=AvgDist/SD,
           col=sqrt(AvgDist^2+SD^2)) %>% 
    left_join(y,by=c('Ref'='Evt_n')) %>% 
    mutate(col=case_when(Ref %in% True_Ref ~ 'Generally Matched',
                         Ref %in% False_Ref ~'Generally Unmatched',
                         TRUE ~ 'Ambiguous')) ->ScatterPlot_Dt 
# arrange(-CoV)
#filter(AvgDist>0.4)
# .$col %>%
# quantile(.,c(0.95))
# filter(col<=0.2684)

# .$col %>% 
# hist

ScatterPlot_Dt %>% 
    ggplot()+
    #geom_point(aes(x=SD,y=AvgDist,color=AvgDist/SD))+
    geom_point(aes(x=SD,y=AvgDist,color=col))+
    geom_line(data=DATA,
              aes(x=SD, y=AvgDist,linetype=PC),
              alpha=0.4,
              arrow = arrow(length=unit(0.30,"cm"), ends="first", type = "closed"))+
    geom_hline(data=ScatterPlot_Dt %>% filter(col!='Ambiguous') %>% group_by(col) %>% summarize(DistThred=median(AvgDist)),
               aes(yintercept=DistThred))+
    geom_text(data=ScatterPlot_Dt %>% filter(col!='Ambiguous') %>% group_by(col) %>% summarize(DistThred=median(AvgDist)),
              aes(0.02,DistThred,label = round(DistThred,3), vjust = -0.5))+
    scale_color_discrete('')+
    scale_linetype_discrete('')+
    ylab('Average Distance')+
    xlab('Standard Deviation')+
    # xlim(0,0.5)+
    # ylim(0,0.5)+
    theme_bw()+
    theme(legend.position="bottom")


ggsave('Images\\Distance summary with PCA.jpg',width = 8, height = 4.5, units = c("in"))

# Pattern example table --------------------------------
Evt_Chara %>% filter(Evt_n %in% c(60,65)) ->x
x[1,]-x[2,]
Dist.df %>% filter(Evt==65,Ref==60)
Event_info %>% filter(Ref_Evt==65, Evt_n==60) %>% head()

#Demonstrate paired event feature---------------------------------

Target_refEvt=22
ptn='AvgT_diff=0-20 DryPd_hr_diff=40-60 Dur_hr_diff=40-60 EvtP_diff=80-100 Jday_diff=in_season RainPd_hr_diff=80-100'
Pt_freq_df %<>% 
    mutate(lhs=as.character(lhs),rhs=as.character(rhs))
Soil_Seq_ts[grep(ptn,Soil_Seq_ts$Patn),c('Evt_n','Ref_Evt','Dist','Patn')] %>% 
    left_join(Pt_freq_df,by=c('Patn'='lhs','Dist'='rhs')) %>% 
    select(Evt_n,Ref_Evt,Dist) %>% 
    filter( Evt_n==Target_refEvt)-> Evt_Cat


PairEvt_Diff=Event_info %>% 
    filter(Evt_n==Target_refEvt) 

Ref_Evt_Dt=Dt_5min_cure %>% filter(Evt_n==Target_refEvt)

Dt_5min_cure %>% 
    filter(Probe=='SouthCtr',
           Time<ymd('2016-07-01')) %>% 
    mutate(Evt_St=ifelse(lag(Evt_n)<Evt_n,1,0.5)) %>%
    inner_join(PairEvt_Diff,by=c('Evt_n'='Ref_Evt')) %>% 
    select(Time,SoilM,Evt_n,Evt_St,DryPd_hr_diff, RainPd_hr_diff,   Dur_hr_diff,   AvgT_diff, EvtP_diff) %>% 
    mutate(AllPattern=ifelse(Evt_n %in% Evt_Cat$Ref_Evt,'Event of Interested Pattern','Event of Other Patterns') ) %>% 
    gather(Measure,Diff,DryPd_hr_diff:AllPattern) %>%
    mutate(Diff=substr(Diff,nchar(Diff)-1,nchar(Diff)),
           Measure=case_when(Measure=='DryPd_hr_diff'~'(B) Dry Period Difference',
                             Measure=='RainPd_hr_diff'~'(E) Rain Period Difference',
                             Measure=='Dur_hr_diff'~'(C) Event Duration Difference',
                             Measure=='AvgT_diff'~'(A) Average Temperature Difference',
                             Measure=='EvtP_diff'~'(D) Event Precipitation Difference',
                             Measure=='AllPattern'~'(F) SMCEs Matching with PEFD')) %>% 
    ggplot()+
    geom_point(aes(Time,y=SoilM,color=Diff,size=Evt_St),alpha=0.5)+
    geom_point(data=Ref_Evt_Dt,aes(Time,y=SoilM,color='Targeted Event'),size=1)+
    facet_wrap(~Measure)+
    
    scale_color_discrete('',
                         breaks=c('20','40','60','80','00','Targeted Event','rn','ns'),
                         labels=c('0%~20%','20%-40%','40%-60%','60%-80%','80%-100%','Reference Event','Events of Interested PEFD','Events of Other PEFD'))+
    scale_size_continuous(guide='none')+
    labs(y='Soil Moisture with Imperfect Calibration',x='')+
    theme_bw()+
    theme(legend.position="bottom")+
    guides(color = guide_legend(nrow = 2))

ggsave('Images\\Demonstration of pattern match events.jpg',width = 8, height = 5.5, units = c("in"))


# Results plots -------------------------------

#  Match relationship
ptn='AvgT_diff=0-20 DryPd_hr_diff=40-60 Dur_hr_diff=40-60 EvtP_diff=80-100 Jday_diff=in_season .* RainPd_hr_diff=80-100'
Soil_Seq_ts[grep(ptn,Soil_Seq_ts$Patn),c('Evt_n','Ref_Evt','Dist','Patn')] %>% 
    left_join(Pt_freq_df,by=c('Patn'='lhs','Dist'='rhs')) %>% 
    select(Evt_n,Ref_Evt,Dist)  ->Evt_Cat


Dt_5min_cure %>% 
    filter(Probe=='SouthCtr') %>% 
    mutate(Evt_St=ifelse(lag(Evt_n)<Evt_n,1,0.5)) %>% 
    left_join(Evt_Cat[,c('Ref_Evt','Dist')],by=c('Evt_n'='Ref_Evt')) %>% 
    mutate(Dist=ifelse(Evt_n %in% Evt_Cat$Evt_n,'Dist=Targeted Event',Dist)) %>% 
    mutate(Dist=ifelse(is.na(Dist),'Dist=Not of Sample PEFD',Dist)) %>%
    mutate(Dist=ifelse(Dist=='Dist=TRUE','Dist=Match',Dist)) %>% 
    mutate(Dist=ifelse(Dist=='Dist=FALSE','Dist=Unmatch',Dist)) %>% 
    mutate(Dist=substring(Dist,6)) %>% 
    mutate(Dist=factor(Dist,levels=c("Ambiguous", "Match", "Not of Sample PEFD","Targeted Event","Unmatch"))) %>% 
    filter(between(Time,ymd('2015-10-1'),ymd('2015-12-1'))|between(Time,ymd('2016-10-1'),ymd('2016-12-1'))) %>% 
    ggplot()+
    geom_point(aes(x=Time,y=SoilM,color=Dist,size=Evt_St,shape=Probe),alpha=0.4)+
    facet_grid(~year(Time),scales = "free")+
    ylab('Soil Moisture with Imperfect Calibration')+
    xlab('')+
    scale_size(guide = 'none')+
    scale_color_manual('',
                       breaks = c( "Ambiguous", 
                                   "Match", 
                                   "Not of Sample PEFD","Targeted Event"#,"Unmatch"
                                   ),
                       values=c("orange", 
                                "red", 
                                "grey","green"#,"blue"
                                ))+
    theme_bw()+
    theme(legend.position="bottom")


ggsave('Images\\Single Sensor Matched Events.jpg',width = 8, height = 5, units = c("in"))


# Unmatched relationship---------------------------------
ptn='AvgT_diff=40-60 DryPd_hr_diff=0-20 Dur_hr_diff=40-60 EvtP_diff=80-100 Jday_diff=in_season .* RainPd_hr_diff=80-100'

Soil_Seq_ts[grep(ptn,Soil_Seq_ts$Patn),c('Evt_n','Ref_Evt','Dist','Patn')] %>% 
    left_join(Pt_freq_df,by=c('Patn'='lhs','Dist'='rhs')) %>% 
    select(Evt_n,Ref_Evt,Dist) %>% 
    filter(Evt_n=='73') -> Evt_Cat


Dt_5min_cure %>% 
    filter(Probe=='SouthCtr') %>% 
    mutate(Evt_St=ifelse(lag(Evt_n)<Evt_n,1,0.5)) %>% 
    left_join(Evt_Cat[,c('Ref_Evt','Dist')],by=c('Evt_n'='Ref_Evt')) %>% 
    mutate(Dist=ifelse(Evt_n %in% Evt_Cat$Evt_n,'Dist=Targeted Event',Dist)) %>% 
    mutate(Dist=ifelse(is.na(Dist),'Dist=Not of Sample PEFD',Dist)) %>%
    mutate(Dist=ifelse(Dist=='Dist=TRUE','Dist=Match',Dist)) %>% 
    mutate(Dist=ifelse(Dist=='Dist=FALSE','Dist=Unmatch',Dist)) %>% 
    mutate(Dist=substring(Dist,6)) %>% 
    mutate(Dist=factor(Dist,levels=c("Ambiguous", "Match", "Not of Sample PEFD","Targeted Event","Unmatch"))) %>% 
    filter(between(Time,ymd('2016-4-1'),ymd('2016-6-1'))) %>% 
    ggplot()+
    geom_point(aes(x=Time,y=SoilM,color=Dist,size=Evt_St),alpha=0.4)+
    ylab('Soil Moisture with Imperfect Calibration')+
    xlab('')+
    scale_size(guide = 'none')+
    scale_color_manual('',
                       breaks = c( "Not of Sample PEFD","Targeted Event","Unmatch"),
                       values=c( "grey","green","blue"))+
    theme_bw()+
    theme(legend.position="bottom")

ggsave('Images\\Single Sensor UnMatched Events.jpg',width = 8, height = 5, units = c("in"))



# Pattern with all relationships--------------------------
ptn='AvgT_diff=0-20 DryPd_hr_diff=40-60 Dur_hr_diff=20-40 EvtP_diff=0-20 Jday_diff=in_season .* RainPd_hr_diff=0-20'
Soil_Seq_ts[grep(ptn,Soil_Seq_ts$Patn),c('Evt_n','Ref_Evt','Dist','Patn')] %>% 
    left_join(Pt_freq_df,by=c('Patn'='lhs','Dist'='rhs')) %>% 
    select(Evt_n,Ref_Evt,Dist) %>% 
    filter(Evt_n==60)-> Evt_Cat


Dt_5min_cure %>% 
    filter(Probe=='SouthCtr') %>% 
    mutate(Evt_St=ifelse(lag(Evt_n)<Evt_n,1,0.5)) %>% 
    left_join(Evt_Cat[,c('Ref_Evt','Dist')],by=c('Evt_n'='Ref_Evt')) %>% 
    mutate(Dist=ifelse(Evt_n %in% Evt_Cat$Evt_n,'Dist=Targeted Event',Dist)) %>% 
    mutate(Dist=ifelse(is.na(Dist),'Dist=Not of Sample PEFD',Dist)) %>%
    mutate(Dist=ifelse(Dist=='Dist=TRUE','Dist=Match',Dist)) %>% 
    mutate(Dist=ifelse(Dist=='Dist=FALSE','Dist=Unmatch',Dist)) %>% 
    mutate(Dist=substring(Dist,6)) %>% 
    mutate(Dist=factor(Dist,levels=c("Ambiguous", "Match", "Not of Sample PEFD","Targeted Event","Unmatch"))) %>% 
    filter(between(Time,ymd('2016-4-1'),ymd('2016-6-1'))) %>% 
    ggplot()+
    geom_point(aes(x=Time,y=SoilM,color=Dist,size=Evt_St),alpha=0.4)+
    ylab('Soil Moisture with Imperfect Calibration')+
    xlab('')+
    #xlim(ymd('2016-4-1'),ymd('2016-6-1'))+
    scale_size(guide = 'none')+
    scale_color_manual('',
                       breaks = c("Ambiguous", "Match", "Not of Sample PEFD","Targeted Event","Unmatch"),
                       values=c("orange", "red", "grey","green","blue"))+
    theme_bw()+
    theme(legend.position="bottom")

ggsave('Images\\Single Sensor All relationship Events.jpg',width = 8, height = 5, units = c("in"))

# Multiple sensors ----------------------------------


ptn='AvgT_diff=0-20 DryPd_hr_diff=40-60 Dur_hr_diff=40-60 EvtP_diff=80-100 Jday_diff=in_season .* RainPd_hr_diff=80-100'
Soil_Seq_ts[grep(ptn,Soil_Seq_ts$Patn),c('Evt_n','Ref_Evt','Dist','Patn')] %>% 
    left_join(Pt_freq_df,by=c('Patn'='lhs','Dist'='rhs')) %>% 
    select(Evt_n,Ref_Evt,Dist) %>% 
    filter(Evt_n==22)->Evt_Cat


Dt_5min_cure %>% 
    mutate(Evt_St=ifelse(lag(Evt_n)<Evt_n,1,0.5)) %>% 
    left_join(Evt_Cat[,c('Ref_Evt','Dist')],by=c('Evt_n'='Ref_Evt')) %>% 
    mutate(Dist=ifelse(Evt_n %in% Evt_Cat$Evt_n,'Dist=Targeted Event',Dist)) %>% 
    mutate(Dist=ifelse(is.na(Dist),'Dist=Not of Sample PEFD',Dist)) %>%
    mutate(Dist=ifelse(Dist=='Dist=TRUE','Dist=Match',Dist)) %>% 
    mutate(Dist=ifelse(Dist=='Dist=FALSE','Dist=Unmatch',Dist)) %>% 
    mutate(Dist=substring(Dist,6)) %>% 
    mutate(Dist=factor(Dist,levels=c("Ambiguous", "Match", "Not of Sample PEFD","Targeted Event","Unmatch"))) %>% 
    filter(between(Time,ymd('2015-10-15'),ymd('2015-12-1')) | between(Time,ymd('2016-10-1'),ymd('2016-12-1'))) %>% 
    ggplot()+
    geom_point(aes(x=Time,y=SoilM,color=Dist,size=Evt_St,shape=Probe),alpha=0.4)+
    facet_grid(~year(Time),scales = "free")+
    ylab('Soil Moisture with Imperfect Calibration')+
    xlab('')+
    scale_size(guide = 'none')+
    scale_shape_manual('',
                       breaks=c('NorthCtr','South','SouthCtr'),
                       values=c(17,15,16))+
    scale_color_manual('',
                       breaks = c( "Ambiguous","Match", "Not of Sample PEFD","Targeted Event","Unmatch"),
                       values=c("orange", "red", "grey","green","blue"))+
    theme_bw()+
    theme(legend.position="bottom")

ggsave('Images\\Multi Sensors.jpg',width = 8, height = 5, units = c("in"))



## Change of adding PC dimension------------------------

ptn='AvgT_diff=0-20 DryPd_hr_diff=40-60 Dur_hr_diff=40-60 EvtP_diff=80-100 Jday_diff=in_season PC_diff=80-100 RainPd_hr_diff=80-100'
Soil_Seq_ts[grep(ptn,Soil_Seq_ts$Patn),c('Evt_n','Ref_Evt','Dist','Patn')] %>% 
    left_join(Pt_freq_df,by=c('Patn'='lhs','Dist'='rhs')) %>% 
    select(Evt_n,Ref_Evt,Dist) %>% 
    filter(Evt_n=='22') ->Evt_Cat1

ptn='AvgT_diff=0-20 DryPd_hr_diff=40-60 Dur_hr_diff=40-60 EvtP_diff=80-100 Jday_diff=in_season .* RainPd_hr_diff=80-100'
Soil_Seq_ts[grep(ptn,Soil_Seq_ts$Patn),c('Evt_n','Ref_Evt','Dist','Patn')] %>% 
    left_join(Pt_freq_df,by=c('Patn'='lhs','Dist'='rhs')) %>% 
    select(Evt_n,Ref_Evt,Dist) %>% 
    filter(Evt_n=='22') ->Evt_Cat2

Evt_Cat2 = anti_join(Evt_Cat2, Evt_Cat1, by = "Ref_Evt")

rbind(Dt_5min_cure %>% 
        mutate(PC='PC Difference Percentile < 80%') %>% 
        left_join(Evt_Cat2[,c('Ref_Evt','Dist')],by=c('Evt_n'='Ref_Evt')),
    Dt_5min_cure %>% 
        mutate(PC='PC Difference Percentile >= 80%') %>% 
        left_join(Evt_Cat1[,c('Ref_Evt','Dist')],by=c('Evt_n'='Ref_Evt'))
    ) %>% 
    mutate(Evt_St=ifelse(lag(Evt_n)<Evt_n,1,0.5)) %>% 
    mutate(Dist=ifelse(Evt_n %in% Evt_Cat1$Evt_n,'Dist=Targeted Event',Dist)) %>% 
    mutate(Dist=ifelse(is.na(Dist),'Dist=Not of Sample PEFD',Dist)) %>%
    mutate(Dist=ifelse(Dist=='Dist=TRUE','Dist=Match',Dist)) %>% 
    mutate(Dist=ifelse(Dist=='Dist=FALSE','Dist=Unmatch',Dist)) %>% 
    mutate(Dist=substring(Dist,6)) %>% 
    mutate(Dist=factor(Dist,levels=c("Ambiguous", "Match", "Not of Sample PEFD","Targeted Event","Unmatch"))) %>% 
    filter(between(Time,ymd('2015-10-15'),ymd('2015-12-1'))|between(Time,ymd('2016-10-1'),ymd('2016-12-1'))) %>% 
    ggplot()+
    geom_point(aes(x=Time,y=SoilM,color=Dist,size=Evt_St,shape=Probe),alpha=0.4)+
    facet_grid(PC~year(Time),scales = "free")+
    ylab('Soil Moisture with Imperfect Calibration')+
    xlab('')+
    scale_size(guide = 'none')+
    scale_shape_manual('',
                       breaks=c('NorthCtr','South','SouthCtr'),
                       values=c(17,15,16))+
    scale_color_manual('',
                       breaks = c( "Ambiguous",
                                   "Match", 
                                   "Not of Sample PEFD",
                                   "Targeted Event",
                                   "Unmatch"),
                       values=c("orange", 
                                "red", 
                                "grey",
                                "green",
                                "blue"))+
    theme_bw()+
    theme(legend.position="bottom")

ggsave('Images\\Multi Sensors with PC.jpg',width = 8, height = 8, units = c("in"))

#  View impact on PC dimension ------------------------


ptn='AvgT_diff=0-20 DryPd_hr_diff=40-60 Dur_hr_diff=40-60 EvtP_diff=80-100 Jday_diff=in_season PC_diff=80-100 RainPd_hr_diff=80-100'
Soil_Seq_ts[grep(ptn,Soil_Seq_ts$Patn),c('Evt_n','Ref_Evt','Dist','Patn')] %>% 
    left_join(Pt_freq_df,by=c('Patn'='lhs','Dist'='rhs')) %>% 
    select(Evt_n,Ref_Evt,Dist) %>% 
    filter(Evt_n=='22') ->Evt_Cat1

ptn='AvgT_diff=0-20 DryPd_hr_diff=40-60 Dur_hr_diff=40-60 EvtP_diff=80-100 Jday_diff=in_season PC_diff=60-80 RainPd_hr_diff=80-100'
Soil_Seq_ts[grep(ptn,Soil_Seq_ts$Patn),c('Evt_n','Ref_Evt','Dist','Patn')] %>% 
    left_join(Pt_freq_df,by=c('Patn'='lhs','Dist'='rhs')) %>% 
    select(Evt_n,Ref_Evt,Dist) %>% 
    filter(Evt_n=='22') ->Evt_Cat2

ptn='AvgT_diff=0-20 DryPd_hr_diff=40-60 Dur_hr_diff=40-60 EvtP_diff=80-100 Jday_diff=in_season PC_diff=40-60 RainPd_hr_diff=80-100'
Soil_Seq_ts[grep(ptn,Soil_Seq_ts$Patn),c('Evt_n','Ref_Evt','Dist','Patn')] %>% 
    left_join(Pt_freq_df,by=c('Patn'='lhs','Dist'='rhs')) %>% 
    select(Evt_n,Ref_Evt,Dist) %>% 
    filter(Evt_n=='22') ->Evt_Cat3

ptn='AvgT_diff=0-20 DryPd_hr_diff=40-60 Dur_hr_diff=40-60 EvtP_diff=80-100 Jday_diff=in_season PC_diff=20-40 RainPd_hr_diff=80-100'
Soil_Seq_ts[grep(ptn,Soil_Seq_ts$Patn),c('Evt_n','Ref_Evt','Dist','Patn')] %>% 
    left_join(Pt_freq_df,by=c('Patn'='lhs','Dist'='rhs')) %>% 
    select(Evt_n,Ref_Evt,Dist) %>% 
    filter(Evt_n=='22') ->Evt_Cat4

ptn='AvgT_diff=0-20 DryPd_hr_diff=40-60 Dur_hr_diff=40-60 EvtP_diff=80-100 Jday_diff=in_season PC_diff=0-20 RainPd_hr_diff=80-100'
Soil_Seq_ts[grep(ptn,Soil_Seq_ts$Patn),c('Evt_n','Ref_Evt','Dist','Patn')] %>% 
    left_join(Pt_freq_df,by=c('Patn'='lhs','Dist'='rhs')) %>% 
    select(Evt_n,Ref_Evt,Dist) %>% 
    filter(Evt_n=='22') ->Evt_Cat5

rbind(Dt_5min_cure %>% 
          mutate(PC='PC Difference > 80%') %>% 
          left_join(Evt_Cat1[,c('Ref_Evt','Dist')],by=c('Evt_n'='Ref_Evt')),
      Dt_5min_cure %>% 
          mutate(PC='PC Difference 60% ~ 80%') %>% 
          left_join(Evt_Cat2[,c('Ref_Evt','Dist')],by=c('Evt_n'='Ref_Evt')),
      Dt_5min_cure %>% 
          mutate(PC='PC Difference 40% ~ 60%') %>% 
          left_join(Evt_Cat3[,c('Ref_Evt','Dist')],by=c('Evt_n'='Ref_Evt')),
      Dt_5min_cure %>% 
          mutate(PC='PC Difference 20% ~ 40%') %>% 
          left_join(Evt_Cat4[,c('Ref_Evt','Dist')],by=c('Evt_n'='Ref_Evt')),
      Dt_5min_cure %>% 
          mutate(PC='PC Difference 0% ~ 20%') %>% 
          left_join(Evt_Cat5[,c('Ref_Evt','Dist')],by=c('Evt_n'='Ref_Evt'))
) %>% 
    mutate(Evt_St=ifelse(lag(Evt_n)<Evt_n,1,0.5)) %>% 
    mutate(Dist=ifelse(Evt_n %in% Evt_Cat1$Evt_n,'Dist=Targeted Event',Dist)) %>% 
    mutate(Dist=ifelse(is.na(Dist),'Dist=Not of Sample PEFD',Dist)) %>%
    mutate(Dist=ifelse(Dist=='Dist=TRUE','Dist=Match',Dist)) %>% 
    mutate(Dist=ifelse(Dist=='Dist=FALSE','Dist=Unmatch',Dist)) %>% 
    mutate(Dist=substring(Dist,6)) %>% 
    mutate(Dist=factor(Dist,levels=c("Ambiguous", "Match", "Not of Sample PEFD","Targeted Event","Unmatch"))) %>% 
    filter(between(Time,ymd('2015-10-15'),ymd('2015-12-1'))|between(Time,ymd('2016-10-1'),ymd('2016-12-1'))) %>% 
    ggplot()+
    geom_point(aes(x=Time,y=SoilM,color=Dist,size=Evt_St,shape=Probe),alpha=0.4)+
    facet_grid(PC~year(Time),scales = "free")+
    ylab('Soil Moisture with Imperfect Calibration')+
    xlab('')+
    scale_size(guide = 'none')+
    scale_shape_manual('',
                       breaks=c('NorthCtr','South','SouthCtr'),
                       values=c(17,15,16))+
    scale_color_manual('',
                       breaks = c( "Ambiguous",
                                   "Match", 
                                   "Not of Sample PEFD",
                                   "Targeted Event",
                                   "Unmatch"),
                       values=c("orange", 
                                "red", 
                                "grey",
                                "green",
                                "blue"))+
    theme_bw()+
    theme(legend.position="bottom")



# Multi sensor test data applied with algorithm ---------------------------

New_Event_info %>% 
    filter(Jday_diff=='Jday_diff=in_season') %>% 
    unite(pattern,AvgT_diff,DryPd_hr_diff,Dur_hr_diff,EvtP_diff,Jday_diff,PC_diff,RainPd_hr_diff,sep=' ') %>% 
    left_join(Pt_freq_df,by=c('pattern'='lhs')) %>% 
    filter(!is.na(rhs),
           lift>1,
           support>0.0001,
           confidence>=0.8,
           rhs=='Dist=TRUE') %>% 
    group_by(Evt_n,rhs) %>% 
    summarise(Overall_Conf=sum(support*confidence,na.rm=T)/sum(support,na.rm=T),
              Overall_Sup=sum(support,na.rm=T)) ->results




Dt_5min_4test %>% 
    #Dt_5min_4test2 %>% 
    left_join(results,by=c('Evt_n'='Evt_n')) %>% #select(-Overall_Conf,-Overall_Sup) %>% 
    #rbind(data.frame(Dt_5min_cure,rhs='Training Data')) %>% 
    mutate(Evt_St=ifelse(lag(Evt_n)<Evt_n,1,0.5)) %>% 
    mutate(rhs=ifelse(is.na(rhs),'Not of existing PEFDs',rhs)) %>% 
    mutate(rhs=ifelse(rhs=='Dist=TRUE','Match',rhs)) %>% 
    mutate(rhs=ifelse(rhs=='Dist=FALSE','Unmatch',rhs)) %>% 
    mutate(rhs=ifelse(rhs=='Dist=Ambiguous','Ambiguous',rhs)) %>% 
    #mutate(rhs=factor(rhs,levels=c( "Match","Unmatch", "Not Found Pattern","Training Data","Ambiguous"))) %>% 
    #filter(between(Time,ymd('2016-07-10'),ymd('2016-09-11'))) %>% 
    mutate(Confidence=ifelse(is.na(Overall_Conf),'Unconfident to be match',paste0(as.character(Overall_Conf %/% 0.05*5),'%'))) %>% #distinct(Confidence)

    ggplot()+
    #geom_line(data=data.frame(Dt_5min_cure,rhs='Training data'),aes(x=Time,y=SoilM,linetype=Probe,color=rhs),size=1)+
    geom_point(aes(x=Time,y=SoilM,shape=Probe,color=Confidence,size=Evt_St),alpha=0.5)+
    labs(y='Soil Moisture with Imperfect Calibration',
         x='')+
    scale_size(guide='none')+
    # scale_color_manual('',
    #                    breaks = c( "Match", "Not Found Pattern","Training Data","Unmatch"),
    #                    values=c( "red", "grey","palegreen3","blue")
    # )+
    scale_linetype_manual(guide='none',
                          breaks = c( "NorthCtr","SouthCtr", "South"),
                          values=c( 1,1,1)
    )+
    theme_bw()+
    theme(legend.position="bottom")

ggsave('Images\\Multi sensor test data applied with algorithm.jpg',width = 8, height = 5.5, units = c("in"))





Dt_5min_4test %>% 
    left_join(results,by=c('Evt_n'='Evt_n')) %>% 
    mutate(Evt_St=ifelse(lag(Evt_n)<Evt_n,1,0.5)) %>% 
    mutate(rhs=ifelse(is.na(rhs),'Not of existing PEFDs',rhs)) %>% 
    mutate(rhs=ifelse(rhs=='Dist=TRUE','Match',rhs)) %>% 
    mutate(rhs=ifelse(rhs=='Dist=FALSE','Unmatch',rhs)) %>% 
    mutate(rhs=ifelse(rhs=='Dist=Ambiguous','Ambiguous',rhs)) %>% 
    mutate(Confidence=ifelse(is.na(Overall_Conf),'Unconfident to be match',paste0(as.character(Overall_Conf %/% 0.05*5),'% onfidence'))) %>% #distinct(Evt_n)
    group_by(Evt_n) %>% 
    mutate(SoilM_z=(SoilM-mean(SoilM))) %>% 
    filter(between(Time,ymd('2016-7-1'),ymd('2016-8-30'))) %>% 
    ggplot()+
    geom_point(data=Dt_5min_cure %>% 
                   filter(Probe=='SouthCtr',between(Time,ymd('2016-7-1'),ymd('2016-8-30'))) %>% 
                   group_by(Evt_n) %>% 
                   mutate(SoilM_z=(SoilM-mean(SoilM))),aes(x=Time,y=SoilM_z+0.1))+
    geom_point(aes(x=Time,y=SoilM_z,shape=Probe,color=Confidence,size=Evt_St),alpha=0.5)

#data after 2017 feb
path='F:\\Projects\\Opti\\Golda Meir DTW and Apriori\\data\\'
file='test-data-for-paper.csv'
filepath=paste0(path,file)

newdt=fread(filepath,
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

newdt %>% 
    ggplot()+
    geom_point(aes(Time_Stamp,Est_SouthCtr_SoilM,color='southctr'))+
    geom_point(aes(Time_Stamp,Est_South_SoilM,color='south'))+
    geom_point(aes(Time_Stamp,Est_NorthCtr_SoilM,color='northctr'))
