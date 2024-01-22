# load packages --------------------------------------------------------
packs <- c('tidyverse', "data.table", "lubridate",'viridis','ggplot2','dplyr','stringr','plotly')
InstIfNec<-function (pack) {
  if (!do.call(require,as.list(pack))) {
    do.call(install.packages,as.list(pack))  }
  do.call(require,as.list(pack)) }
lapply(packs, InstIfNec)

myTheme_multi=theme_minimal() +
  theme(text = element_text( face = "plain",  size = 12,
                             angle = 0, lineheight = 0.9),
        plot.title = element_text(size = rel(1.2)),
        axis.text = element_text(face = "plain", size = 12)
  )


# import data -------------------------------------------------------------

dir_outputs='./2-outputs/Run_simu/output/'



# resLeaflet_raw=NULL
resLeaf_raw=NULL

files=c('DA1_Average_MAP_90_inter12-intra7_NS_meteo21february','DA1_Average_MAP_90_inter7.97-intra9.21_NS_meteo21february','DA1_Average_MAP_90_inter14-intra8_NS_meteo21february','DA1_Average_MAP_90_inter9-intra7_NS_meteo21february','DA1_Average_MAP_90_inter14-intra7_NS_meteo21february','DA1_Average_MAP_90_inter12-intra8_NS_meteo21february','DA1_Average_MAP_90_inter10-intra8_NS_meteo21february','DA1_Average_MAP_90_inter10-intra7_NS_meteo21february','DA1_Average_MAP_90_inter9-intra8_NS_meteo21february')



for (file in files){
  # file=files[1]
  print(file)
  df = data.table::fread(paste0(dir_outputs,file,"/component_values.csv"), data.table = FALSE)
  
  meteo = data.table::fread(paste0(dir_outputs,file,"/meteo.csv"), data.table = FALSE)
  meteo$step_number = 1:nrow(meteo)
  ###get Leaf rank
  
  # for (i in unique(df$item_id)[unique(df$item_id)!=-1]){
  i=1
  print(paste('item id ',i))
  
  df_L =
    df%>%
    mutate(Design=file)%>%
    filter(type=="Leaflet")%>%
    filter(step_number==0)%>%
    filter(item_id==i & step_number==0)%>%
    arrange(component_id)%>%
    mutate(dif=abs(component_id-lag(component_id)),
           dif=ifelse(is.na(dif),0,dif))
  
  # test$cluster=kmeans(x=na.omit(test$dif),centers = 2)$cluster
  
  lim=quantile(df_L$dif,probs = 0.99,na.rm=T)
  
  df_L=df_L%>%
    mutate(cluster=ifelse(dif>lim,1,0),
           LeafRank=cumsum(cluster))%>%
    select(component_id,LeafRank)
  
  
  # data per leaflet (only with plasticity) -----------------------------------------------------------
  
  # 
  # df_leaflet = merge(df%>%filter(type=="Leaflet" & item_id==i),df_L)%>%
  #   mutate(Design=file)%>%
  #   group_by(Design,LeafRank,item_id,component_id,barycentre_x,barycentre_y,barycentre_z)%>%
  #   summarise(Global_Intercepted_leaflet= sum(.data$Ra_PAR_q+.data$Ra_NIR_q,na.rm=T)*10^-6,
  #             # Global intercepted radiation in MJ leaflet-1 day-1
  #             # An= sum(.data$area*.data$An_f*.data$step_duration*10^-6,na.rm=T),
  #             # umol m-2 leaf s-1 -> mol leaflet-1 day-1
  #             # transpiration= sum(.data$Tr_q,na.rm=T),
  #             # Total transpiration in the plot in mm leaf-1 timestep-1
  #             Leaflet_area= mean(.data$area, na.rm=T),
  #             N= n()
  #             # Gs = mean(.data$Gs, na.rm=T),
  #             
  #             # temp = mean(.data$`T`,na.rm=T),
  #             # H_q = sum(.data$H_q,na.rm=T)*10^-6,
  #             # LE_q = sum(.data$LE_q,na.rm=T)*10^-6,
  #             # Ra_TIR_q = sum(.data$Ra_TIR_q,na.rm=T)*10^-6
  #   )%>%
  #   ungroup()
  # 
  # resLeaflet_raw=rbind(resLeaflet_raw,df_leaflet)
  
  
  # data per leaf -----------------------------------------------------------
  
  
  df_leaf = merge(df%>%filter(type=="Leaflet" & item_id==i),df_L)%>%
    mutate(Design=file)%>%
    group_by(Design,LeafRank,step_number,item_id)%>%
    summarise(Global_Intercepted_leaves= sum(.data$Ra_PAR_q+.data$Ra_NIR_q,na.rm=T)*10^-6,
              # Global intercepted radiation in MJ leaf-1 timestep-1
              An= sum(.data$area*.data$An_f*.data$step_duration*10^-6,na.rm=T),
              # umol m-2 leaf s-1 -> mol leaf-1 timestep-1
              transpiration= sum(.data$Tr_q,na.rm=T),
              # Total transpiration in the plot in mm leaf-1 timestep-1
              Leaf_Area= sum(.data$area, na.rm=T),
              Gs = mean(.data$Gs, na.rm=T),
              N= n(),
              temp = mean(.data$`T`,na.rm=T),
              H_q = sum(.data$H_q,na.rm=T)*10^-6,
              LE_q = sum(.data$LE_q,na.rm=T)*10^-6,
              Ra_TIR_q = sum(.data$Ra_TIR_q,na.rm=T)*10^-6
    )%>%
    ungroup()
  
  resLeaf_raw=rbind(resLeaf_raw,df_leaf)
  }
# }

tableDensity=data.frame(Design=files,Density=c(119,136,89,159,102,104,125,139,143))

resLeaf=merge(resLeaf_raw,tableDensity)

# data.table::fwrite(resLeaf,file ='2-outputs/Archimed_outputs_rev.csv' )


# graphics ----------------------------------------------------------------

# leaflet scale -----------------------------------------------------------
###distribution of light

# resLeaflet%>%
#   mutate(irrad=Global_Intercepted_leaflet/Leaflet_area)%>%
#   ggplot(aes(x=irrad,col=LeafRank,group=paste(Design,item_id,LeafRank)))+
#   geom_density()+
#   facet_grid(item_id~Design)

# leaf scale --------------------------------------------------------------

# 
# resLeaf%>%
#   filter(step_number==0 )%>%
#   ggplot(aes(x= LeafRank,y=Leaf_Area))+
#   geom_line()+
#   facet_wrap(~Design)
# 
# ### light intercepted
# resLeaf%>%
#   group_by(Design,LeafRank)%>%
#   summarize(Daily_Intercepted_leaves=sum(Global_Intercepted_leaves))%>%
#   ungroup()%>%
#   ggplot(aes(x=LeafRank,y=Daily_Intercepted_leaves,col=Design,group=paste(Design)))+
#   geom_line(alpha=0.5)+
#   geom_smooth(aes(group=Design))
# 
# 
# 
# # Assimilation
# resLeaf%>%
#   group_by(Design,LeafRank)%>%
#   summarize(An_leaves=sum(An))%>%
#   ungroup()%>%
#   ggplot(aes(x=LeafRank,y=An_leaves,col=Design,group=paste(Design)))+
#   geom_line(alpha=0.5)+
#   geom_smooth(aes(group=Design))

### plant and plot scale
resPlant=resLeaf%>%
  group_by(Design,step_number)%>%
  mutate(PLA=sum(Leaf_Area))%>%
  group_by(Design,Density)%>%
  summarize(PLA=mean(PLA),
            total_Intercepted=sum(Global_Intercepted_leaves),
            total_irradiance=total_Intercepted/PLA,
            total_An=sum(An*12/1000),
            total_transpiration=sum(transpiration),
            WUE=total_An*1000/total_transpiration)%>%
  ungroup()

resPlot=resPlant%>%
  group_by(Design,Density)%>%
  mutate(LAI=mean(PLA)*Density/10000,
         LI=mean(total_Intercepted)*Density,
         An=mean(total_An)*Density,
         Tr=mean(total_transpiration)*Density,
         WUE=total_An/total_transpiration)%>%
  ungroup()%>%
  mutate(Group=paste(Design))


# 
# f.bar=function(data=data,var='var'){
#   
#   # var='total_Intercepted'
#   
#   donRes=data%>%
#     group_by(Design,Density)%>%
#     summarize(m=mean(get(var)))%>%
#     ungroup()
#   
#   perct=donRes%>%
#     mutate(Rank=abs(Density-136))%>%
#     arrange(Rank)%>%
#     mutate(dif_m=(m-first(m))/first(m)*100)
#   
#   
#   donResPerc=merge(donRes,perct,all.x=T)%>%
#     mutate(Den=paste(sprintf("%03d",Density)))
#   
#   gr=ggplot()+
#     geom_col(data=donResPerc,aes(x=Den,y=m,fill=Den),position='dodge')+
#     geom_label(data=donResPerc%>%filter(!is.na(dif_m) & Design!='Design136'),aes(x=Den,y=0.5*m,label=paste(round(dif_m),'%')),size=4,fill=NA,col='white')+
#     xlab('')+
#     scale_fill_grey(name='density (plants.ha-1)')+
#     myTheme_multi+
#     theme(legend.position = 'none')
#   
#   return(gr)
# }
# 
# 
# plot_grid(
#   f.bar(data=resPlant,var = 'total_Intercepted')+
#     ylab(expression('Light interception '*(MJ[' ']*' plant'^-1*day^-1)))+coord_flip(),
#   f.bar(data=resPlot,var = 'LI')+
#     ylab(expression('Light interception '*(MJ[' ']*' ha'^-1*day^-1)))+coord_flip(),
#   f.bar(data=resPlant,var = 'total_An')+theme(legend.position='none')+ylab(expression('Carbon assimilation '*(' kg'[C]*' plant'^-1*day^-1)))+coord_flip(),
#   f.bar(data=resPlot,var = 'An')+theme(legend.position='none')+ylab(expression('Carbon assimilation '*(' kg'[C]*' ha'^-1*day^-1)))+coord_flip(),
#   f.bar(data=resPlant,var = 'total_transpiration')+theme(legend.position='none')+
#     ylab(expression('Transpiration '*('L'[H2O]*' plant'^-1*day^-1)))+coord_flip(),
#   f.bar(data=resPlot,var = 'Tr')+theme(legend.position='none')+
#     ylab(expression('Transpiration '*('L'[H2O]*' ha'^-1*day^-1)))+coord_flip(),
#   
#   f.bar(data=resPlant,var = 'WUE')+theme(legend.position='none')+
#     ylab(expression('Water use efficiency '*(' g'[C]*' '*L[H2O]^-1)))+ theme(legend.position='none')+coord_flip(),
#   f.bar(data=resPlot,var = 'WUE')+theme(legend.position='none')+
#     ylab(expression('Water use efficiency '*(' g'[C]*' '*L[H2O]^-1*' ha'^-1)))+theme(legend.position='none')+coord_flip(),
#   f.bar(data=resPlot,var = 'LAI')+
#     ylab(expression('LAI '*(m^2*m^-1)))+ theme(legend.position=c(1.5,0.3),legend.background = element_blank())+coord_flip(),
#   labels = c('a','A','b','B','c','C','d','D','E'),ncol=2)
