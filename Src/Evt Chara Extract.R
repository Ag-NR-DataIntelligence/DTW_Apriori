Event_patterns=function(dt)
{
    
    as_data_frame(dt) %>% 
        select(Evt_n,Time,SoilM) %>% 
        arrange(Evt_n,Time) %>% 
        group_by(Evt_n) %>% 
        mutate(Indx=row_number()) %>% 
        split(.$Evt_n) %>%
        map_dbl(~prcomp(~Indx+SoilM, data = .)$rotation['SoilM','PC1']) %>% 
        data_frame(Evt_n=attributes(.)$names,PC=.)->Evt_PCs
    
    
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
            AvgT=mean(Temp_F,na.rm=T),
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



Get_Evt_quantiles=function(RefID,Evt_Chara)
{
    #print (RefID)
    #Constant
    #In season Julian date difference 45 (90 days a season)
    Season_Diff=45
    
    
    GetQuantiles <- function(x) {
        #x=abs(x)
        quants=as.character(signif(quantile(unique(x), probs = seq(0.2, 1, by = 0.20)),digits=4))
        return(paste(quants,collapse=','))
    }
    
    Ref=map(Evt_Chara$Evt_n,~Evt_Chara %>% filter(Evt_n==RefID) %>% rename(Ref_Evt=Evt_n)) %>% 
        rbindlist %>% 
        data.frame() 
    
    data.frame(Evt_n=Evt_Chara$Evt_n,
                           Ref_Evt=Ref$Ref_Evt,
                           Evt_Chara[,-1]-Ref[,-1]) %>% 
        mutate(Jday=ifelse(abs(Jday)<=Season_Diff,'in_season','out_season')) %>% 
        filter(Jday=='in_season') %>% 
        group_by(Ref_Evt) %>% 
        summarise(DryPd_dif=GetQuantiles(DryPd_hr),
                  RainPd_dif=GetQuantiles(RainPd_hr),
                  Dur_dif=GetQuantiles(Dur_hr),
                  AvgT_dif=GetQuantiles(AvgT),
                  EvtP_dif=GetQuantiles(EvtP),
                  SoilM_SD_dif=GetQuantiles(SoilM_SD),
                  SoilM_Avg_dif=GetQuantiles(SoilM_Avg)) %>% 
        ungroup %>% 
        data.frame %>% 
        return
}



PC_Dif_quantiles=function(RefID,Evt_Chara)
{
    Ref=map(Evt_Chara$Evt_n,~Evt_Chara %>% filter(Evt_n==RefID) %>% rename(Ref_Evt=Evt_n)) %>% 
        rbindlist %>% 
        data.frame()
    
    Evt_Chara[,'PC']-Ref[,'PC'] %>% 
        return
}


Labeling_Evt_Character=function(RefID,
                                Ref_Evt_Chara,
                                Trgt_Evt_Chara,
                                Dist.df=NULL,
                                Dist_threshold=NULL,
                                Evt_Dif_quantile)
{
    #Constant
    #In season Julian date difference 45 (90 days a season)
    Season_Diff=45
    
    
    ApplyQuantiles <- function(x,quantls) {
        
        quantls %>% 
            strsplit(split=',') %>% 
            unlist %>% 
            as.numeric %>% 
            .[1:4] %>% 
            c(-Inf,.,Inf) ->    bin_vec
        
        cut(x, breaks=bin_vec, 
            labels=c("0-20","20-40","40-60","60-80","80-100"), include.lowest=TRUE,right=TRUE)
    }
    
    Dist_Cut <- function(x) {
        cut(x, breaks=c(0,Dist_threshold %>% pull(Dist_threshold_T),Dist_threshold %>% pull(Dist_threshold_F),max(x)), 
            labels=c("TRUE","Ambiguous","FALSE"), include.lowest=TRUE)
    }
    
    

    Ref=map(Trgt_Evt_Chara$Evt_n,~Ref_Evt_Chara %>% filter(Evt_n==RefID) %>% rename(Ref_Evt=Evt_n)) %>% 
        rbindlist%>% 
        data.frame()
    
    Quantls=Evt_Dif_quantile %>% filter(Ref_Evt==RefID)
    
    data.frame(Evt_n=Trgt_Evt_Chara$Evt_n,
               Ref_Evt=Ref$Ref_Evt,
               Trgt_Evt_Chara[,-1]-Ref[,-1]) %>%  
        mutate(Jday=ifelse(abs(Jday)<=Season_Diff,'in_season','out_season')) %>% 
        filter(Jday=='in_season') %>%     
        mutate(DryPd_hr=ApplyQuantiles(DryPd_hr,Quantls$DryPd_dif[1]),
               RainPd_hr=ApplyQuantiles(RainPd_hr,Quantls$RainPd_dif[1]),
               Dur_hr=ApplyQuantiles(Dur_hr,Quantls$Dur_dif[1]),
               AvgT=ApplyQuantiles(AvgT,Quantls$AvgT_dif[1]),
               EvtP=ApplyQuantiles(EvtP,Quantls$EvtP_dif[1]),
               SoilM_SD=ApplyQuantiles(SoilM_SD,Quantls$SoilM_SD_dif[1]),
               SoilM_Avg=ApplyQuantiles(SoilM_Avg,Quantls$SoilM_Avg_dif[1]),
               PC=ApplyQuantiles(abs(PC),Quantls$PC_dif[1])
               ) %>% 
        mutate(DryPd_hr_diff=paste0('DryPd_hr_diff=',DryPd_hr),
               RainPd_hr_diff=paste0('RainPd_hr_diff=',RainPd_hr),
               Dur_hr_diff=paste0('Dur_hr_diff=',Dur_hr),
               AvgT_diff=paste0('AvgT_diff=',AvgT),
               EvtP_diff=paste0('EvtP_diff=',EvtP),
               Jday_diff=paste0('Jday_diff=',Jday),
               SoilM_SD_diff=paste0('SoilM_SD_diff=',SoilM_SD),
               SoilM_Avg_diff=paste0('SoilM_Avg_diff=',SoilM_Avg),
               PC_diff=paste0('PC_diff=',PC)) %>% 
        select(-DryPd_hr,-RainPd_hr,-Dur_hr,-AvgT,-EvtP,-Jday,-SoilM_SD,-SoilM_Avg,PC) %>% 
               {
                   if(sum(is.na(.$DryPd_hr))) 
                       {print(RefID)}
                   if (!is.null(Dist.df))
                   {
                       left_join(.,Dist.df,by=c('Evt_n'='Evt','Ref_Evt'='Ref')) %>%
                           filter(.,Dist>0) %>%
                           mutate(.,Dist=paste0('Dist=',Dist_Cut(Dist)))
                   }
                   else {data.frame(.)}
               } %>%
        return
}
