Event_patterns=function(dt)
    # Calculating all features for each event
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
        mutate(Rain_Tm=structure(ifelse(Inst_Rain==0,NA,Time), class = class(Time))) %>% 
        mutate(Rain_Tm=with_tz(Rain_Tm,'UTC'))%>% 
        group_by(Evt_n) %>% 
        mutate(
            maxRainTm=max(Rain_Tm,na.rm=T),
            minTm=min(Time,na.rm=T),
            RainPd_hr=as.numeric(max(Rain_Tm,na.rm=T)-min(Time,na.rm=T),units='hours')+1/12) %>% 
        summarise(
            RainPd_hr=max(RainPd_hr),
            Dur_hr=n()/12,
            St=min(Time),
            AvgT=mean(Temp_F,na.rm=T),
            EvtP=sum(Inst_Rain,na.rm=T),
            SoilM_SD=sd(SoilM,na.rm=TRUE),
            SoilM_Avg=mean(SoilM,na.rm=TRUE)
        ) %>%
        mutate(DryPd_hr=Dur_hr-RainPd_hr) %>% 
        mutate(Jday=yday(as.Date(St))) %>% 
        left_join(Evt_PCs) %>% 
        select(-St)-> Evt_Chara
    
    return(Evt_Chara)
}


Evt_Difs=function(RefID,Ref_Evt_Chara,Trgt_Evt_Chara)
    # Calculating all Paired Event Features Difference 
{
    Ref=map(Trgt_Evt_Chara$Evt_n,~Evt_Chara %>% filter(Evt_n==RefID) %>% rename(Ref_Evt=Evt_n)) %>% 
        rbindlist %>% 
        data.frame(stringsAsFactors=F)
    
    data.frame(Evt_n=Trgt_Evt_Chara$Evt_n,
               Ref_Evt=Ref$Ref_Evt,
               Trgt_Evt_Chara[,-1]-Ref[,-1],
               stringsAsFactors=F) %>% 
        filter(Evt_n!=Ref_Evt)
}




## Different methods to get quantiles for PEFD ---------

GetQuantiles <- function(x) {
    #x=abs(x)
    quants=quantile(unique(x), probs = seq(0, 1, by = 0.20))
    return(list(quants))
}

GetEvenQuantiles <- function(x) {
    #x=abs(x)
    quants=max(abs(x))*seq(0, 1, by = 0.20)
    return(list(quants))
}


# Functions to get PEFD quantile ------------------------

Get_Evt_quantiles_byech=function(Evt_Dif)
    #Get quantile for each PEFD by each reference event 
    #EXCEPT PC
{
    #Constant
    #In season Julian date difference 45 (90 days a season)
    Season_Diff=45
    
    
    Evt_Dif %>% 
        mutate(Jday=ifelse((abs(Jday)<=Season_Diff #& abs(Jday)>=(366-Season_Diff)
                            ),
                           'in_season','out_season')) %>% 
        filter(Jday=='in_season') %>% 
        group_by(Ref_Evt) %>%
        summarise(DryPd_dif=GetQuantiles(DryPd_hr),
                  RainPd_dif=GetQuantiles(RainPd_hr),
                  Dur_dif=GetQuantiles(Dur_hr),
                  AvgT_dif=GetQuantiles(AvgT),
                  EvtP_dif=GetQuantiles(EvtP),
                  SoilM_SD_dif=GetQuantiles(SoilM_SD),
                  SoilM_Avg_dif=GetQuantiles(SoilM_Avg),
                  PC_dif=GetEvenQuantiles(PC)) %>% 
        ungroup %>% 
        return
}




Evt_Dif_quantiles=function(Evt_Dif)
    #Get quantile for each PEFD for all events 
{

    Evt_Dif %>% 
        summarise(DryPd_dif=GetEvenQuantiles(DryPd_hr),
                  RainPd_dif=GetEvenQuantiles(RainPd_hr),
                  Dur_dif=GetEvenQuantiles(Dur_hr),
                  AvgT_dif=GetEvenQuantiles(AvgT),
                  EvtP_dif=GetEvenQuantiles(EvtP),
                  SoilM_SD_dif=GetEvenQuantiles(SoilM_SD),
                  SoilM_Avg_dif=GetEvenQuantiles(SoilM_Avg),
                  PC_dif=GetEvenQuantiles(PC)
                  ) %>% 
        return
    
}

Labeling_Evt_Character=function(RefID,
                                Ref_Evt_Chara,
                                Trgt_Evt_Chara,
                                Dist.df=NULL,
                                Dist_threshold=NULL,
                                Evt_Dif_quantile,
                                Evt_difs,
                                AddPC=TRUE)
{
    #Constant
    #In season Julian date difference 45 (90 days a season)
    Season_Diff=45
    
    ApplyQuantiles <- function(x,quantls) {
        
        quantls %>% 
            unlist  ->    bin_vec
        
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
    
    Quantls=Evt_Dif_quantile %>% 
        {
            x=.
            if("Ref_Evt" %in% colnames(x))  # Estimate by each event
            {
                filter(.,Ref_Evt==RefID)    # Estimate by all events
            } else {
                x
            }
        }
    
 
        
    if (AddPC) {
        # Estimate by all events
        dfdt=Evt_difs 
    } else { # Estimate by each event
        dfdt=data.frame(Evt_n=Trgt_Evt_Chara$Evt_n,
                   Ref_Evt=Ref$Ref_Evt,
                   Trgt_Evt_Chara[,-1]-Ref[,-1]) 
    } 
    
    dfdt %>% 
        filter(Ref_Evt==RefID) %>% 
        mutate(Jday=ifelse(abs(Jday)<=Season_Diff,'in_season','out_season')) %>% 
        filter(Jday=='in_season') %>%     
        mutate(DryPd_hr=ApplyQuantiles(DryPd_hr,Quantls$DryPd_dif),
               RainPd_hr=ApplyQuantiles(RainPd_hr,Quantls$RainPd_dif),
               Dur_hr=ApplyQuantiles(Dur_hr,Quantls$Dur_dif),
               AvgT=ApplyQuantiles(AvgT,Quantls$AvgT_dif),
               EvtP=ApplyQuantiles(EvtP,Quantls$EvtP_dif),
               SoilM_SD=ApplyQuantiles(SoilM_SD,Quantls$SoilM_SD_dif),
               SoilM_Avg=ApplyQuantiles(SoilM_Avg,Quantls$SoilM_Avg_dif)) %>% 
        #mutate(PC=ApplyQuantiles(abs(PC),Quantls$PC_dif[1]))
               {
                   df=.
                   if (AddPC) {mutate(df,PC=ApplyQuantiles(abs(PC),Quantls$PC_dif))
                   }else{
                           select(df,-PC)
                       }
               } %>% 
        mutate(DryPd_hr_diff=paste0('DryPd_hr_diff=',DryPd_hr),
               RainPd_hr_diff=paste0('RainPd_hr_diff=',RainPd_hr),
               Dur_hr_diff=paste0('Dur_hr_diff=',Dur_hr),
               AvgT_diff=paste0('AvgT_diff=',AvgT),
               EvtP_diff=paste0('EvtP_diff=',EvtP),
               Jday_diff=paste0('Jday_diff=',Jday),
               SoilM_SD_diff=paste0('SoilM_SD_diff=',SoilM_SD),
               SoilM_Avg_diff=paste0('SoilM_Avg_diff=',SoilM_Avg)) %>% 
               {
                   df=.
                   if (AddPC) {mutate(df,PC_diff=paste0('PC_diff=',PC))
                   }else{df} 
               } %>% 
        select(-DryPd_hr,-RainPd_hr,-Dur_hr,-AvgT,-EvtP,-Jday,-SoilM_SD,-SoilM_Avg) %>% 
               {
                   df=.
                   if(sum(is.na(df$DryPd_hr))) 
                       {print(RefID)}
                   if (!is.null(Dist.df))   
                   {
                       # For generate model
                       left_join(.,Dist.df,by=c('Evt_n'='Evt','Ref_Evt'='Ref')) %>%
                           filter(Dist>0) %>%
                           mutate(Dist=paste0('Dist=',Dist_Cut(Dist)))
                   }
                   else {
                       as_data_frame(df)  # for testing new data
                       }
               } %>%
        return
}
