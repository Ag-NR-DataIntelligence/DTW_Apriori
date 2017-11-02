
MatchEvtPtn=function(Event_info,AddPC=TRUE)
{
    library(arules)
    library(stringr)
    
    # min confidence and length need to be specified based on model
    minConf=0
    minLength=7+AddPC 
    
    Event_info %>%
       
        mutate(PC_diff=ifelse(rep(AddPC,nrow(.)),PC_diff,"")) %>% 
        # These columns has to be ordered alphabetically
        mutate(Items=paste(AvgT_diff,DryPd_hr_diff,Dur_hr_diff,EvtP_diff,Jday_diff,PC_diff,
                           RainPd_hr_diff,
                           #SoilM_SD_diff,SoilM_Avg_diff,
                           Dist,sep=','),
               Patn=paste(AvgT_diff,DryPd_hr_diff,Dur_hr_diff,EvtP_diff,Jday_diff,PC_diff,
                          RainPd_hr_diff
                          #SoilM_SD_diff,SoilM_Avg_diff,
               )) %>% 
        select(Evt_n,Ref_Evt,Items,Patn,Dist) %>% 
        mutate(Items=gsub(",,",",",Items),
               Patn=gsub("  "," ",Patn)) ->Soil_Seq_ts
    
    Event_info %>%
        # These columns has to be ordered alphabetically
        unite(Items,sort(noquote(colnames(Event_info[,-c('Evt_n','Ref_Evt')]))),sep=',',remove=F) %>% 
        unite(Patn,sort(noquote(colnames(Event_info[,-c('Evt_n','Ref_Evt','Dist','Items')]))),sep=',')
    
    Soil_Pt=as(lapply(Soil_Seq_ts %>%
                          select(Items) %>%
                          unlist %>%
                          as.list,
                      function(x) strsplit(x,',') %>% unlist),'transactions')
    
    
    Pt_feq = apriori(Soil_Pt, parameter=list(support=1/nrow(Soil_Seq_ts), confidence=minConf,target='rules',minlen=minLength), 
                     appearance = list(rhs = c("Dist=TRUE","Dist=FALSE","Dist=Ambiguous"),
                                       default="lhs"))
    
    
    
    Pt_freq_df=data.frame(
        lhs=sapply(1:Pt_feq@lhs@data@Dim[2],function(x) paste(Pt_feq@lhs@itemInfo$labels[Pt_feq@lhs@data[,x]],collapse = ' ')),
        rhs=sapply(1:Pt_feq@rhs@data@Dim[2],function(x) paste(Pt_feq@rhs@itemInfo$labels[Pt_feq@rhs@data[,x]],collapse = ' '))) %>% 
        cbind(Pt_feq@quality) %>% 
        mutate(lhs=as.character(lhs),rhs=as.character(rhs))
    list(Pt_freq_df,Soil_Seq_ts)%>% 
        return
}






