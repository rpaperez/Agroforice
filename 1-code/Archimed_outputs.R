### Visualisation of archimed outputs###
##R Perez, 21 Decembre 2023


# Load packages -----------------------------------------------------------
packs <- c("lubridate", "stringr", "ggplot2",'dplyr','Vpalmr','ggpmisc','plotly','archimedR','viridis','ggrepel')
InstIfNec<-function (pack) {
  if (!do.call(require,as.list(pack))) {
    do.call(install.packages,as.list(pack))  }
  do.call(require,as.list(pack)) }
lapply(packs, InstIfNec)

source('./1-code/helpers_archimed.R')
source('./1-code/load_map_light.R')


# inputs ------------------------------------------------------------------

paramFileName='DA1_Average_MAP_90'
# d_inter=12
# d_intra=7
path_designs='./2-outputs/Run_simu/planting_designs/'
# pathVpalmParam='./2-outputs/Generate_VPalm_param/'
pathArchimed='./1-code/archimed-phys.jar'
# pathVpalmJar='./1-code/vpalm_biomech.jar'
pathOpf='./2-outputs/Run_simu/ops/opf/'
pathOPS='./2-outputs/Run_simu/ops/'
opfStepExport=14
orientation='NS'
# meteoFileName='meteo20march'



dynAll=NULL
gridAll=NULL

for (d_inter in c(9,10,12,14)){
  for (d_intra in c(7,8)){
    for (met in c('meteo20march','meteo21december','meteo21october','meteo21november','meteo21january','meteo21february')){
      
      sub=load_map(d_inter =d_inter ,d_intra = d_intra,path_designs = path_designs,paramFileName=paramFileName,orientation = orientation,meteoFileName = met)
      dynAll=rbind(dynAll,sub$don_crop_fin)
      gridAll=rbind(gridAll,sub$grid_fin)
    }
  }  
}

dynAll=dynAll%>%
  mutate(Hour=hms(Hour))

ggplot()+
  geom_line(data=dynAll,aes(x=hms(Hour),y=ppfd_mean_rel,col=meteoFileName,group=paste(meteoFileName,density)),lwd=1.5)+
  scale_color_viridis_d()+
  scale_x_time()+
  facet_wrap(~density)+
  ylab('% transmission')+
  xlab('Hour of the day')

ggplot()+
  geom_line(data=dynAll,aes(x=hms(Hour),y=ppfd_mean,col=paste(density,'plt.ha-1'),group=paste(meteoFileName,density)),lwd=1.5)+
  scale_color_viridis_d()+
  scale_fill_viridis_d()+
  scale_x_time()+
  ylab(expression ('PPFD '*(mu*mol*' '*m**-2*' '*s**-1)))+
  xlab('Hour of the day')


conv=60/((10**6)*4.6)

donComp=dynAll%>%
  mutate( date=str_remove(meteoFileName,"meteo")) %>% 
  group_by(density,design,date)%>%
  summarize(PAR_day=sum(ppfd_mean*30*conv),
           )%>%
  mutate(dens=as.numeric(str_sub(string = density,start = 0,end = 3)),
         ratio=PAR_day/dens)

ratiomean=mean(donComp$ratio)



ggplot()+
  geom_smooth(data=donComp,aes(x=dens,y=PAR_day),method='lm',se=F,lty=2,col="lightgrey")+
  geom_x_margin_arrow(xintercept = 136,arrow.length	
                      =0.1)+
  geom_point(data=donComp,aes(x=dens,y=PAR_day,col=paste(density,'plt.ha-1'),shape=date),size=5)+
  # geom_label_repel(data=donComp,aes(x=dens,y=PAR_day,label=design,fill=paste(density,'plt.ha-1')),col='white')+
  scale_color_viridis_d()+
  scale_fill_viridis_d()+
  theme(legend.position='none')+
  ylab(expression ('Transmitted PAR '*(MJ*' '*m**-2*' '*day**-1)))+
  xlab(expression ('Palm density '*(plants*' '*ha**-1)))


don_m=donComp%>%
  group_by(density,design,dens)%>%
  summarize(PAR_day_m=mean(PAR_day),
            PAR_day_sd=sd(PAR_day)) %>% 
  ungroup()

ggplot()+
  geom_smooth(data=don_m,aes(x=dens,y=PAR_day_m),method='lm',se=F,lty=2,col="lightgrey")+
  geom_x_margin_arrow(xintercept = 136,arrow.length	
                  =0.1)+
  geom_point(data=don_m,aes(x=dens,y=PAR_day_m,col=paste(density,'plt.ha-1')),size=2)+
  geom_errorbar(data=don_m,aes(x=dens,ymax=PAR_day_m+PAR_day_sd,ymin=PAR_day_m-PAR_day_sd,col=paste(density,'plt.ha-1')))+
  geom_label_repel(data=don_m,aes(x=dens,y=PAR_day_m,label=design,fill=paste(density,'plt.ha-1')),col='white')+
  scale_color_viridis_d()+
  scale_fill_viridis_d()+
  theme(legend.position='none')+
  ylab(expression ('Transmitted PAR '*(MJ*' '*m**-2*' '*day**-1)))+
  xlab(expression ('Palm density '*(plants*' '*ha**-1)))




min=30

map=gridAll%>%
  
  mutate(density=as.numeric(density))%>%
  arrange(density)%>%
  filter(x<=min & y<=min)


ggplot()+
  geom_point(data=map,aes(x=x, y=y,fill=Intercepted_rel,col=Intercepted_rel),pch=22)+
  geom_point(data=map%>%filter(x_tree_1<=min & y_tree_1<=min)
             ,aes(x=x_tree_1,y=y_tree_1),col=2,pch=0)+
  geom_point(data=map%>%filter(x_tree_2<=min & y_tree_2<=min),aes(x=x_tree_2,y=y_tree_2),col=2,pch=0)+
  facet_wrap(~paste(sprintf("%03d",density),'plant.ha-2')) +
  coord_fixed()+
  labs(x = 'x (m)', y = 'y (m)',fill= '%',col='%') +
  # theme(legend.position="bottom")+
  scale_fill_viridis()+
  scale_color_viridis()+
  theme(legend.direction = 'horizontal',legend.position = c(0.83,0.12))
