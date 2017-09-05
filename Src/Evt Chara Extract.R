Event_patterns=function(dt)
{
    
    as_data_frame(dt) %>% 
        select(Evt_n,Time,SoilM) %>% 
        arrange(Evt_n,Time) %>% 
        group_by(Evt_n) %>% 
        mutate(Indx=row_number()) %>% 
        split(.$Evt_n) %>%
        map_dbl(~prcomp(~Indx+SoilM, data = .)$rotation['SoilM','PC2']*prcomp(~Indx+SoilM, data = .)$rotation['SoilM','PC1']) %>% 
        data_frame(Evt_n=as.numeric(attributes(.)$names),PC=.)->Evt_PCs
    
    
    as_data_frame(dt) %>% 
        #Structure(ifelse,class=class())  force the results as the class on different conditions
        mutate(Rain_Tm=structure(ifelse(Inst_Rain==0,NA,Time), class = class(Time))) %>% 
        mutate(Rain_Tm=with_tz(Rain_Tm,'UTC'))%>% 
        group_by(Evt_n) %>% 
        mutate(#DryPd_hr=as.numeric(max(Time,na.rm=T)-max(Rain_Tm,na.rm=T),units='hours')+1/12,
            maxRainTm=max(Rain_Tm,na.rm=T),
            minTm=min(Time,na.rm=T),
            RainPd_hr=as.numeric(max(Rain_Tm,na.rm=T)-min(Time,na.rm=T),units='hours')+1/12) %>% 
        summarise(#DryPd_hr=max(DryPd_hr),
            RainPd_hr=max(RainPd_hr),
            Dur_hr=n()/12,
            St=min(Time),
            AvgT=mean(Temp_F),
            EvtP=sum(Inst_Rain,na.rm=T),
            SoilM_SD=sd(SoilM,na.rm=TRUE),
            SoilM_Avg=mean(SoilM,na.rm=TRUE)
        ) %>%
        mutate(DryPd_hr=Dur_hr-RainPd_hr) %>% 
        mutate(Jday=round(as.numeric(St-ymd(year(St)*10000+101),unit='days'))) %>% 
        left_join(Evt_PCs) %>% 
        select(-St)-> Evt_Chara
    
    return(Evt_Chara)
}





Labeling_Evt_Character=function(RefID,Evt_Chara,Dist.df,Dist_threshold)
{
    #print (RefID)
    #Constant
    #In season Julian date difference 45 (90 days a season)
    Season_Diff=45
    
    ApplyQuintiles <- function(x) {
        cut(x, breaks=c(quantile(unique(x), probs = seq(0, 1, by = 0.20))), 
            labels=c("0-20","20-40","40-60","60-80","80-100"), include.lowest=TRUE)
    }
    
    Dist_Cut <- function(x) {
        cut(x, breaks=c(0,Dist_threshold %>% pull(Dist_threshold_T),Dist_threshold %>% pull(Dist_threshold_F),max(x)), 
            labels=c("TRUE","Ambiguous","FALSE"), include.lowest=TRUE)
    }
    
    
    Ref=NULL
    for (j in Evt_Chara$Evt_n)
    {
        Ref=rbind(Ref,Evt_Chara %>% filter(Evt_n==RefID) %>% rename(Ref_Evt=Evt_n))
    }
    
    data.frame(Evt_n=Evt_Chara$Evt_n,
               Ref_Evt=Ref$Ref_Evt,
               Evt_Chara[,-1]-Ref[,-1]) %>% 
        left_join(Dist.df,by=c('Evt_n'='Evt','Ref_Evt'='Ref')) %>% 
        filter(Dist>0) %>% 
        mutate(Jday=ifelse(abs(Jday)<=Season_Diff,'in_season','out_season')) %>% 
        filter(Jday=='in_season') %>% 
        mutate(DryPd_hr=ApplyQuintiles(abs(DryPd_hr)),
               RainPd_hr=ApplyQuintiles(abs(RainPd_hr)),
               Dur_hr=ApplyQuintiles(abs(Dur_hr)),
               AvgT=ApplyQuintiles(abs(AvgT)),
               EvtP=ApplyQuintiles(abs(EvtP)),
               SoilM_SD=ApplyQuintiles(abs(SoilM_SD)),
               SoilM_Avg=ApplyQuintiles(abs(SoilM_Avg)),
               PC=ApplyQuintiles(abs(PC)),
               Dist=Dist_Cut(Dist)) %>% 
        mutate(DryPd_hr_diff=paste0('DryPd_hr_diff=',DryPd_hr),
               RainPd_hr_diff=paste0('RainPd_hr_diff=',RainPd_hr),
               Dur_hr_diff=paste0('Dur_hr_diff=',Dur_hr),
               AvgT_diff=paste0('AvgT_diff=',AvgT),
               EvtP_diff=paste0('EvtP_diff=',EvtP),
               Jday_diff=paste0('Jday_diff=',Jday),
               SoilM_SD_diff=paste0('SoilM_SD_diff=',SoilM_SD),
               SoilM_Avg_diff=paste0('SoilM_Avg_diff=',SoilM_Avg),
               PC_diff=paste0('PC_diff=',PC),
               Dist=paste0('Dist=',Dist)) %>% 
        return
}
