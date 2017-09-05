
MatchEvtPtn=function(Event_info)
{
    library(arules)
    library(stringr)
    
    # min confidence and length need to be specified based on model
    minConf=0
    minLength=8 
    
    Event_info %>%
        mutate(Items=paste(DryPd_hr_diff,RainPd_hr_diff,Dur_hr_diff,AvgT_diff,EvtP_diff,Jday_diff,
                           #SoilM_SD_diff,SoilM_Avg_diff,
                           PC_diff,
                           Dist,sep=','),
               Patn=paste(AvgT_diff,DryPd_hr_diff,Dur_hr_diff,EvtP_diff,Jday_diff,RainPd_hr_diff,
                          #SoilM_SD_diff,SoilM_Avg_diff,
                          PC_diff
               )) %>% 
        select(Evt_n,Ref_Evt,Items,Patn,Dist)->Soil_Seq_ts
    
    
    Soil_Pt=as(lapply(Soil_Seq_ts %>%
                          select(Items) %>%
                          unlist %>%
                          as.list,
                      function(x) strsplit(x,',') %>% unlist),'transactions')
    
    
    Pt_feq = apriori(Soil_Pt, parameter=list(support=1/nrow(Soil_Seq_ts), confidence=minConf,target='rules',minlen=minLength), 
                     appearance = list(rhs = c("Dist=TRUE","Dist=FALSE",'Dist=Ambiguous'),
                                       default="lhs"))
    
    
    
    Pt_freq_df=data.frame(
        lhs=sapply(1:Pt_feq@lhs@data@Dim[2],function(x) paste(Pt_feq@lhs@itemInfo$labels[Pt_feq@lhs@data[,x]],collapse = ' ')),
        rhs=sapply(1:Pt_feq@rhs@data@Dim[2],function(x) paste(Pt_feq@rhs@itemInfo$labels[Pt_feq@rhs@data[,x]],collapse = ' '))) %>% 
        cbind(Pt_feq@quality) %>% 
        mutate(lhs=as.character(lhs),rhs=as.character(rhs))
    list(Pt_freq_df,Soil_Seq_ts)%>% 
        return
}