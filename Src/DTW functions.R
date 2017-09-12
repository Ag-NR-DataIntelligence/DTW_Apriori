
# Parallel calculate alignment
align_pal=function(Dt,Tslist_val)
{
    library(foreach)
    library(doParallel)
    
    #setup parallel backend to use many processors
    cores=detectCores()
    cl <- makeCluster(cores[1]-2) #not to overload your computer
    registerDoParallel(cl)
    
    Val_convert=function(x,method='Zscore')
    {
        if (method=='Zscore') 
        {
            return(zscore(x))
            
        } else if (method=='RMAvg')
        {
            return(x-mean(x))
        } else return(x)
    }
    
    Dists=foreach(n=  1:length(Tslist_val),
                  .combine=list,
                  .multicombine = TRUE,
                  .packages=c('dtw','dtwclust')) %dopar%
                  {
                      Dist=dtw(Val_convert(Dt,'Zscore'),Val_convert(Tslist_val[[n]],'Zscore'),keep=TRUE
                               ,open.end = T
                      )$normalizedDistance  # Distance per step
                      
                      Dist
                  } 
    
    #stop cluster
    stopCluster(cl)
    
    Dists %>% 
        unlist %>% 
        return
}

EvtDist_Cal=function(Tslist,Evt_ID)
{
    Dim=length(Tslist)
    
    lapply(Tslist,align_pal,Tslist_val=Tslist) %>% 
        unlist(.) %>% 
        matrix(.,nrow=Dim,ncol=Dim) %>% 
        data.frame(.) -> dist.df
    
    colnames(dist.df)= as.character(Evt_ID)
    dist.df$Evt=Evt_ID
    dist.df %>% 
        gather(.,'Ref','Dist',1:Dim) %>% 
        mutate(Ref=as.numeric(Ref)) %>% 
        return
}


DTW_dist_cal=function(dt)
    #Time_Stamp
    #Temp_F
    #Inst_Rain
    #SoilM
{
    
    dt %>%
        select(Evt_n,SoilM) %>% 
        group_by(Evt_n) %>% 
        do(vals=.$SoilM) ->Evt_SoilM_Ts
    
    # Evt_SoilM_Ts$vals->Tslist
    # 
    # Evt_SoilM_Ts$Evt_n->Evt_ID 
    
    Dist.df=EvtDist_Cal(Evt_SoilM_Ts$vals,Evt_SoilM_Ts$Evt_n)
    
    return(Dist.df)
}


# Determine Distance thresholds
Dist_thresholds=function(Dist.df){
    
    
    #PCA
    Dist.df %>% 
        filter(Dist>0) %>% 
        group_by(Ref) %>% 
        summarize(AvgDist=mean(Dist),
                  SD=sd(Dist))->pnts
    
    prin_comp <- prcomp(pnts %>% select(-Ref), scale. = T)
    prin_comp$rotation
    
    #Generally Unmatched
    pnts %>% 
        mutate(PC1=prin_comp$rotation[2,1]*SD+prin_comp$rotation[1,1]*AvgDist,
               PC2=abs(prin_comp$rotation[1,2])*AvgDist-abs(prin_comp$rotation[2,2])*SD) %>% 
        filter(PC2>quantile(PC2,0.95)) %>% 
        pull(Ref) %>% 
        as.character()->False_Ref
    
    Dist.df %>% 
        filter(Ref %in% False_Ref) %>% 
        pull(Dist) %>% 
        median ->Dist_threshold_F
    
    #Generally Matched
    pnts %>% 
        mutate(PC1=prin_comp$rotation[2,1]*SD+prin_comp$rotation[1,1]*AvgDist,
               PC2=prin_comp$rotation[1,2]*AvgDist+prin_comp$rotation[2,2]*SD) %>% 
        filter(PC1>quantile(PC1,0.95)) %>% 
        pull(Ref) %>% 
        as.character()->True_Ref
    
    Dist.df %>% 
        filter(Ref %in% True_Ref) %>% 
        pull(Dist) %>% 
        median ->Dist_threshold_T
    
    
    data.frame(Dist_threshold_T=Dist_threshold_T,
               Dist_threshold_F=Dist_threshold_F) %>% 
        return
}