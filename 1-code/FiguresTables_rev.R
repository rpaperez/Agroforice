#### Analysis of Agroforice data - exportation of Tables and Figures### Revision for JexBot
##R Perez january 2024

# Load packages  -----------------------------------------------------------

packs <- c('tidyverse', "data.table", "lubridate",'remotes','viridis','ggpmisc',"ggrepel",'png','magick','agricolae','cowplot','RColorBrewer','ade4','ggcorrplot','car','lmtest')
InstIfNec<-function (pack) {
  if (!do.call(require,as.list(pack))) {
    do.call(install.packages,as.list(pack))  }
  do.call(require,as.list(pack)) }
lapply(packs, InstIfNec)

# source('1-code/Mapping_light.R')
source('1-code/helpers.R')
source('./1-code/helpers_archimed.R')
source('./1-code/load_map_light.R')

#  inputs ---------------------------------------------------------

myTheme=theme_minimal() %+replace% 
  theme( 
    panel.background = element_rect(fill = "white", 
                                    colour = NA), panel.border = element_rect(fill = NA, 
                                                                              colour = "grey20"),
    text = element_text( face = "plain",  size = 14,
                         angle = 0, lineheight = 0.9),
    plot.title = element_text(size = rel(1.2)),
    axis.text = element_text(face = "plain", size = 10)
  )


myTheme_multi=theme_minimal() +
  theme(text = element_text( face = "plain",  size = 12,
                             angle = 0, lineheight = 0.9),
        plot.title = element_text(size = rel(1.2)),
        axis.text = element_text(face = "plain", size = 12)
  )

color_Trt=brewer.pal(n = 3, name = 'Set2')
names(color_Trt)=c('C','S-AF','S')

color_Var=brewer.pal(n = 4, name = 'BrBG')
names(color_Var)=c('V3','V5','V7','V8')

shape_Trt=c(0,1,2)
names(shape_Trt)=c('C','S-AF','S')



mT=data.frame(Treatment=c('C','D','L'),new_Treatment=c('C','S-AF','S'))

Vprod=c('V3','V5','V7','V8')



# map of light ------------------------------------------------------------

paramFileName='DA1_Average_MAP_90'
path_designs='./2-outputs/Run_simu/planting_designs/'
pathOpf='./2-outputs/Run_simu/ops/opf/'
pathOPS='./2-outputs/Run_simu/ops/'
orientation='NS'

grid_df_day_comp=NULL
for (d_inter in c(9,10,12,14)){
  for (d_intra in c(7,8)){
    for (met in c('meteo20march','meteo21december','meteo21october','meteo21november','meteo21january','meteo21february')){
      
      sub=load_map(d_inter =d_inter ,d_intra = d_intra,path_designs = path_designs,paramFileName=paramFileName,orientation = orientation,meteoFileName = met)
      grid_df_day_comp=rbind(grid_df_day_comp,sub$grid_fin)
    }  
  }
}
min=30

map=grid_df_day_comp%>%
  mutate(density=as.numeric(density))%>%
  arrange(meteoFileName,density)%>%
  filter(x<=min & y<=min)%>%
  group_by(density,x,y,x_tree_1,x_tree_2,y_tree_2,y_tree_1)%>%
  summarize(Intercepted_rel=mean(Intercepted_rel))


map_ligth_rel=ggplot()+
  geom_point(data=map,aes(x=x, y=y,fill=Intercepted_rel,col=Intercepted_rel),pch=22)+
  geom_point(data=map%>%filter(x_tree_1<=min & y_tree_1<=min)
             ,aes(x=x_tree_1,y=y_tree_1),col=2,pch=0)+
  geom_point(data=map%>%filter(x_tree_2<=min & y_tree_2<=min),aes(x=x_tree_2,y=y_tree_2),col=2,pch=0)+
  facet_wrap(~paste(sprintf("%03d",density),'palms ha-1')) +
  coord_fixed()+
  labs(x = 'x (m)', y = 'y (m)',fill= '%',col='%') +
  # theme(legend.position="bottom")+
  scale_fill_viridis()+
  scale_color_viridis()

map_ligth_rel+myTheme+theme(legend.direction = 'horizontal',legend.position = c(0.83,0.12))
ggsave(file = "2-outputs/Figures_rev/Fig_light_planting.png")

# table planting designs --------------------------------------------------


table_design=grid_df_day_comp%>%
  group_by(Design)%>%
  summarize(dist_inter=unique(d_inter),
            dist_intra=unique(d_intra),
            density=unique(density))%>%
  select(dist_intra,dist_inter,density)%>%
  arrange(as.numeric(density))


colnames(table_design)=c('dist intra (m)','dist inter (m)','density (palms ha-1)')

print(table_design)

data.table::fwrite(table_design,file = '2-outputs/Tables_rev/Design.csv',row.names =T )


# simulated dynamic of light ----------------------------------------------


dynAll=NULL

for (d_inter in c(9,10,12,14)){
  for (d_intra in c(7,8)){
    for (met in c('meteo20march','meteo21december','meteo21october','meteo21november','meteo21january','meteo21february')){
      
      sub=load_map(d_inter =d_inter ,d_intra = d_intra,path_designs = path_designs,paramFileName=paramFileName,orientation = orientation,meteoFileName = met)
      dynAll=rbind(dynAll,sub$don_crop_fin)
    }
  }  
}

tableDate=data.frame(meteoFileName=unique(dynAll$meteoFileName),Date=c(dmy('21-03-2020'),dmy('21-12-2019'),dmy('21-10-2019'),dmy('21-11-2019'),dmy('21-01-2020'),dmy('21-02-2020')))

dynAll=merge(dynAll%>%
               mutate(Hour=hms(Hour)),tableDate)


ggplot()+
  geom_line(data=dynAll,aes(x=hms(Hour),y=ppfd_mean_rel,col=Date,group=Date),lwd=1.5)+
  ylab(expression ('Light transmission (%)'))+
  xlab('Hour of the day')+
  scale_x_time()+
  facet_wrap(~paste(sprintf("%03d",density),'palms ha-1')) +
  labs(col='Date') +
  myTheme

ggsave(filename = '2-outputs/Figures_rev/FigS_light_date.png',width = 12,height = 8)


grid_lab2=dynAll%>%
  filter(meteoFileName=='meteo20march')%>%
  group_by(design)%>%
  summarize(Hour=hms('12:00:00'),
            ppfd_mean_rel=max(ppfd_mean_rel,na.rm=T),
            density=unique(density),
            dist_inter=unique(dist_inter),
            dist_intra=unique(dist_intra),
            design=unique(design))

dynAv=dynAll%>%
  group_by(design,Hour,density)%>%
  summarize(ppfd_mean_m=mean(ppfd_mean),
            ppfd_mean_rel_m=mean(ppfd_mean_rel),
            ppfd_mean_rel_sd=sd(ppfd_mean_rel))%>%
  ungroup()

gr2a=ggplot()+
  geom_line(data=dynAv,aes(x=hms(Hour),y=ppfd_mean_rel_m,col=paste(sprintf("%03d",density),'palms ha-1'),group=density),lwd=1.5)+
  geom_label_repel(data=grid_lab2,aes(label=design,x=hms(Hour),y=ppfd_mean_rel,fill=paste(sprintf("%03d",density),'palms ha-1')),col='white')+
  scale_color_viridis_d(direction = -1,name='density')+
  scale_fill_viridis_d(direction = -1,name='density')+
  ylab(expression ('Light transmission (%)'))+
  xlab('Hour of the day')+
  scale_x_time()+
  myTheme+
  theme(legend.position = c(0.8,0.8))



conv=60/((10**6)*4.6)

donComp=dynAll%>%
  group_by(density,design,Date)%>%
  summarize(PAR_day=sum(ppfd_mean*30*conv),
  )%>%
  mutate(dens=as.numeric(str_sub(string = density,start = 0,end = 3)),
         ratio=PAR_day/dens)

ratiomean=mean(donComp$ratio)


don_m=donComp%>%
  group_by(density,design,dens)%>%
  summarize(PAR_day_m=mean(PAR_day),
            PAR_day_sd=sd(PAR_day)) %>% 
  ungroup()

gr2b=ggplot()+
  geom_smooth(data=don_m,aes(x=dens,y=PAR_day_m),method='lm',se=F,lty=2,col="lightgrey")+
  geom_x_margin_arrow(xintercept = 136,arrow.length	
                      =0.1)+
  geom_point(data=don_m,aes(x=dens,y=PAR_day_m,col=paste(sprintf("%03d",density),'palms ha-1')),size=2)+
  geom_errorbar(data=don_m,aes(x=dens,ymax=PAR_day_m+PAR_day_sd,ymin=PAR_day_m-PAR_day_sd,col=paste(sprintf("%03d",density),'palms ha-1')))+
  geom_label_repel(data=don_m,aes(x=dens,y=PAR_day_m,label=design,fill=paste(sprintf("%03d",density),'palms ha-1')),col='white')+
  scale_color_viridis_d(direction = -1)+
  scale_fill_viridis_d(direction = -1)+
  myTheme+
  theme(legend.position='none')+
  ylab(expression ('Transmitted PAR '*(MJ*' '*m**-2*' '*day**-1)))+
  xlab(expression ('Palm density '*(palms*' '*ha**-1)))


cowplot::plot_grid(gr2a,gr2b,labels=c('A','B'),rel_widths = c(0.5,0.5))

ggsave(filename = '2-outputs/Figures_rev/Fig_simul_Light.png',width = 12,height = 8)


# palm physiol vs light transmission --------------------------------------

resLeaf=data.table::fread(file ='2-outputs/Archimed_outputs_rev.csv' )
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


color_spe=c("#440154FF","#46337EFF","#365C8DFF",'grey',"#277F8EFF","#1FA187FF","#4AC16DFF","#9FDA3AFF","#FDE725FF")
names(color_spe)=paste(sprintf("%03d",sort(unique(resPlant$Density),decreasing = T)))

resPlot=resPlant%>%
  group_by(Design,Density)%>%
  mutate(LAI=mean(PLA)*Density/10000,
         LI=mean(total_Intercepted)*Density,
         An=mean(total_An)*Density,
         Tr=mean(total_transpiration)*Density,
         WUE=total_An/total_transpiration)%>%
  ungroup()%>%
  mutate(Group=paste(Design))

f.bar=function(data=data,var='var'){
  
  # var='total_Intercepted'
  
  donRes=data%>%
    group_by(Design,Density)%>%
    summarize(m=mean(get(var)))%>%
    ungroup()
  
  perct=donRes%>%
    mutate(Rank=abs(Density-136))%>%
    arrange(Rank)%>%
    mutate(dif_m=(m-first(m))/first(m)*100)
  
  
  donResPerc=merge(donRes,perct,all.x=T)%>%
    mutate(Den=paste(sprintf("%03d",Density)))
  
  gr=ggplot()+
    geom_col(data=donResPerc,aes(x=Den,y=m,fill=Den),position='dodge')+
    geom_text(data=donResPerc%>%filter(!is.na(dif_m) & Design!='Design136'),aes(x=Den,y=0.5*m,label=paste(round(dif_m),'%')),size=4,col='white')+
    xlab('')+
    scale_fill_manual(name='density (plants.ha-1)',values =color_spe )+
    myTheme_multi+
    theme(legend.position = 'none')
  
  return(gr)
}



plot_grid( 
  
  f.bar(data=resPlot,var = 'LI')+
    ylab(expression('Light interception '*(MJ[' ']*' ha'^-1*day^-1)))+coord_flip()+
    xlab('  '),
  f.bar(data=resPlant,var = 'total_Intercepted')+
    ylab(expression('Light interception '*(MJ[' ']*' palm'^-1*day^-1)))+coord_flip()+xlab('  '),
  
  f.bar(data=resPlot,var = 'An')+theme(legend.position='none')+ylab(expression('Carbon assimilation '*(' kg'[C]*' ha'^-1*day^-1)))+coord_flip()+xlab(' '),
  f.bar(data=resPlant,var = 'total_An')+theme(legend.position='none')+ylab(expression('Carbon assimilation '*(' kg'[C]*' palm'^-1*day^-1)))+coord_flip()+xlab(' '),
  
  f.bar(data=resPlot,var = 'Tr')+theme(legend.position='none')+
    ylab(expression('Transpiration '*('L'[H2O]*' ha'^-1*day^-1)))+coord_flip()+xlab('                      Density (palms ha-1)'),
  f.bar(data=resPlant,var = 'total_transpiration')+theme(legend.position='none')+
    ylab(expression('Transpiration '*('L'[H2O]*' palm'^-1*day^-1)))+coord_flip()+xlab('                     Density (palms ha-1)'),
  
  f.bar(data=resPlot,var = 'LAI')+
    ylab(expression('LAI '*(m^2*m^-1)))+ theme(legend.position='none')+coord_flip()+xlab(' '),
  
  f.bar(data=resPlant,var = 'WUE')+theme(legend.position='none')+
    ylab(expression('Water use efficiency '*(' g'[C]*' '*L[H2O]^-1)))+ theme(legend.position='none')+coord_flip()+xlab(' '),
  
 
  labels = c('A','B',' ',' ',' ',' ',' ',' '),ncol=2)


ggsave(filename = '2-outputs/Figures_rev/Fig_palmOutputs.png',width = 10,height = 10)


# phyto Climate----------------------------------------------------------------

load('0-data/Climate.RData')


met_raw=met

met_raw=met_raw%>%
  mutate(month=str_sub(string = time,start = 6,end = 7),
         hms=hms(str_sub(string = time,start = 11,end = str_length(string = time))))%>%
  mutate(HM=str_sub(string = time,start = 11,end = 16),
         hm=hms(paste0(HM,':00')))

met_raw=merge(met_raw,mT)%>%
  select(-Treatment)%>%
  mutate(Treatment=new_Treatment)

met_phyto=met_raw%>%
  filter(date>'2020-09-01' & Chamber!='Chamber2')%>%
  group_by(Treatment,HM,hm)%>%
  summarize(Rad=mean(Radiation,na.rm=T),
            Rad_c=mean(SP.Radiation,na.rm=T),
            Rd_sd=sd(Radiation,na.rm=T))%>%
  ungroup()

rad1=met_phyto%>%
  mutate(Hour=paste0('2020-05-06 ',HM,':00'))%>%
  select(Hour,Rad,Treatment)

rad2=dynAv%>%filter(design=="7 m x 12 m")%>%
  mutate(Treatment='sim',
         Rad=ppfd_mean_m,
         Hour=as.character(Hour))%>%
  select(Hour,Rad,Treatment)



fig3a=ggplot()+
  geom_point(data=rad1,aes(x=ymd_hms(Hour)-3600,y=Rad,shape=Treatment,col=Treatment),size=2)+
  geom_line(data=rad2,aes(x=ymd_hms(paste('2020-05-06',Hour)),y=Rad),lwd=1.5)+
  geom_point(data=rad1,aes(x=ymd_hms(Hour)-3600,y=Rad,shape=Treatment,col=Treatment),size=2)+
  geom_label(data=data.frame(label='simulation',x=ymd_hms('2020-05-06  12:00:00'),y=1.02*max(rad2$Rad)),aes(x=x,y=y,label=label),col='white',fill=1)+
  scale_x_datetime(date_labels ='%R',limits=c(ymd_hms('2020-05-06  05:00:00'),ymd_hms('2020-05-06  19:00:00')))+
  ylab(expression ('PPFD '*(mu*mol*' '*m**-2*' '*s**-1)))+
  xlab('Hour of the day')+
  ylim(c(0,1.1*max(rad1$Rad)))+
  scale_color_manual(name='Treatment',values = color_Trt)+
  scale_shape_manual(name='Treatment',values = shape_Trt)+
  scale_fill_manual(name='Treatment',values = color_Trt)+
  myTheme+
  theme(legend.position = c(0.15,0.8))


metsum=met_raw%>%
  filter(date>'2020-08-04' & date<Sys.Date() &  Chamber!='Chamber2')%>%
  group_by(date,Treatment)%>%
  summarize(totalRad=sum(Radiation*conv),
            totalRad_cons=sum(SP.Radiation*conv))%>%
  ungroup()%>%
  group_by(date)%>%
  mutate(totalRad_rel=totalRad/max(totalRad),
         totalRad_cons_rel=totalRad_cons/max(totalRad_cons))%>%
  ungroup()%>%
  group_by(Treatment)%>%
  summarize(total=mean(totalRad),
            total_sd=sd(totalRad))%>%
  ungroup()

### calcul of DLI (daily light integral in mol.m-2.day-1)

donDLI=met_raw%>%
  filter(date>'2020-08-04' & date<Sys.Date() &  Chamber!='Chamber2')%>%
  group_by(date,Treatment)%>%
  summarize(dli=sum(Radiation*60)/1000000,
            dlf=max(Radiation)-min(Radiation),
            dlfcv=sd(Radiation)/mean(Radiation))%>%
  ungroup()%>%
  group_by(Treatment)%>%
  summarize(dli_m=mean(dli),
            dli_sd=sd(dli),
            dlf_m=mean(dlf),
            dlf_sd=sd(dlf),
            dlfcv_m=mean(dlfcv),
            dlfcv_sd=sd(dlfcv))%>%
  ungroup()


rad_int=rad2%>%
  group_by(Treatment)%>%
  summarize(total=sum(Rad*conv*60*0.48),total_sd=0)%>%
  ungroup()



metsum=rbind(metsum,rad_int)




fig3b=donDLI%>%
  ggplot()+
  geom_col(aes(x=Treatment,y=dli_m,fill=Treatment,group=Treatment),position = "dodge",alpha=0.8)+
  geom_errorbar(aes(x=Treatment,ymax=dli_m+dli_sd,ymin=dli_m-dli_sd,col=Treatment,group=Treatment),width=0.2)+
  scale_color_manual(name='Treatment',values = color_Trt)+
  scale_fill_manual(name='Treatment',values = color_Trt)+
  xlab('')+
  ylab(expression('DLI '*(mol*' '*m**-2*day**-1)))+
  myTheme+
  theme_classic()+
  theme(legend.position='none',plot.background = element_rect(fill='transparent'))+
  coord_flip()

fig3c=donDLI%>%
  ggplot()+
  geom_col(aes(x=Treatment,y=dlf_m,fill=Treatment,group=Treatment),position = "dodge",alpha=0.8)+
  geom_errorbar(aes(x=Treatment,ymax=dlf_m+dlf_sd,ymin=dlf_m-dlf_sd,col=Treatment,group=Treatment),width=0.2)+
  scale_color_manual(name='Treatment',values = color_Trt)+
  scale_fill_manual(name='Treatment',values = color_Trt)+
  xlab('')+
  ylab(expression('DLF '*(mu*mol[photon]*' '*m**-2*day**-1)))+
  myTheme+
  theme_classic()+
  theme(legend.position='none',plot.background = element_rect(fill='transparent'))+
  coord_flip()

fig3d=donDLI%>%
  ggplot()+
  geom_col(aes(x=Treatment,y=dlfcv_m,fill=Treatment,group=Treatment),position = "dodge",alpha=0.8)+
  geom_errorbar(aes(x=Treatment,ymax=dlfcv_m+dlfcv_sd,ymin=dlfcv_m-dlfcv_sd,col=Treatment,group=Treatment),width=0.2)+
  scale_color_manual(name='Treatment',values = color_Trt)+
  scale_fill_manual(name='Treatment',values = color_Trt)+
  xlab('')+
  ylab(expression('CV'))+
  myTheme+
  theme_classic()+
  theme(legend.position='none',plot.background = element_rect(fill='transparent'))+
  coord_flip()


cowplot::plot_grid(fig3a,cowplot::plot_grid(fig3b,fig3c,ncol=1,labels=c('B','C')),labels=c('A',''),rel_widths = c(0.6,0.4))
ggsave(filename = '2-outputs/Figures_rev/Fig_Light_chambers.png',width = 8,height = 6)


### daily mean range of light per treatment

donAmp=rad1%>%
  group_by(Treatment)%>%
  summarize(min=min(Rad),
            max=max(Rad),
            DLF=max-min)%>%
  ungroup()%>%
  select(Treatment,DLF)


lightCarac=merge(donDLI%>%filter(Treatment!='sim'),donAmp)%>%
  rename(DLI=dli_m)%>%
  mutate(DLI=round(DLI,2))



# Temprature and HR in chambers --------------------------------------------------


graphDynTemp=met_raw%>%
  filter(date>'2020-09-01' & Chamber!='Chamber2')%>%
  group_by(Treatment,HM,hm)%>%
  summarize(Temp=mean(Temperature,na.rm=T),
            Temp_c=mean(SP.Temperature,na.rm=T),
            Temp_sd=sd(Temperature,na.rm=T))%>%
  ungroup()%>%
  ggplot()+
  geom_line(aes(x=hm,y=Temp,col=Treatment),size=1.5)+
  scale_x_time()+
  scale_color_manual(name='Treatment',values = color_Trt)+
  ylab(expression('Temperature '*(degree*C)))+
  xlab('Hour of the day')+
  myTheme

graphDynHR=met_raw%>%
  filter(date>'2020-09-01' & Chamber!='Chamber2')%>%
  group_by(Treatment,HM,hm)%>%
  summarize(HR=mean(Humidity,na.rm=T),
            HR_c=mean(SP.Humidity,na.rm=T),
            HR_sd=sd(Humidity,na.rm=T))%>%
  ungroup()%>%
  ggplot()+
  geom_line(aes(x=hm,y=HR,col=Treatment),size=1.5)+
  
  scale_x_time()+
  scale_color_manual(name='Treatment',values = color_Trt)+
  ylab(expression('Relative humidity '*('%')))+
  xlab('Hour of the day')+
  myTheme

cowplot::plot_grid(graphDynTemp+theme(legend.position='none'),graphDynHR+theme(legend.position=c(0.8,0.2)),labels=c('A','B'))

ggsave(filename = '2-outputs/Figures_rev//Fig_TempHR_chambers.png',width = 8,height = 6)



# Spectro -----------------------------------------------------------------


spec=data.table::fread(input =  './0-data/Spectro.csv',dec=',')%>%
  mutate(Time=ymd_hms(Time))

graph_spectro1=spec%>%
  ggplot(aes(x=nm,y=intensity,col=Treatment))+
  geom_line(aes(group=paste(sp_pos,Treatment)))+
  ylab('mW.m-2')+
  scale_color_manual(name='Treatment',values = color_Trt)+
  myTheme+
  theme(legend.position = 'none')

graph_spectro2=spec%>%
  group_by(Treatment,sp_pos)%>%
  mutate(intensity_rel=intensity/max(intensity))%>%
  ungroup()%>%
  ggplot(aes(x=nm,y=intensity_rel,col=Treatment))+
  geom_line(aes(group=paste(sp_pos,Treatment)))+
  ylab('relative intensity')+
  scale_color_manual(name='',values = color_Trt)+
  myTheme+
  theme(legend.position = c(0.2,0.8))


cowplot::plot_grid(graph_spectro1,graph_spectro2,labels=c('A','B'))
ggsave(filename = '2-outputs/Figures_rev//Fig_light_spectr_chambers.png',width = 8)


# Images analysis -------------------------------------------------


sto=readPNG("2-outputs/Figures/stomate.png")
cmp=readPNG("2-outputs/Figures/Compactness.png")
stg=readPNG("2-outputs/Figures/StayGreen.png")


cowplot::plot_grid(ggdraw() +
                     draw_image(sto),
                   ggdraw() +
                     draw_image(cmp),
                   ggdraw() +
                     draw_image(stg),labels=c('A','B','C'),ncol=1)

ggsave(filename = '2-outputs/Figures_rev/FigImageAnalysis.png')


### load phenotypic data

qual=fread(file = '0-data/GrainQuality.csv')

don=fread(input = '0-data/Data_agroforice.csv')%>%
  filter(!Plant %in% c('D_V3_3','L_V3_3'))

morphoL=fread(file = './0-data/Morpho_leaf.csv')%>%
  mutate(Date=dmy(Date))%>%
  mutate(ratioWL=Length/Width)%>%
  filter(Variety %in% Vprod)%>%
  filter(!Plant %in% c('D_V3_3','L_V3_3'))

pheno=data.table::fread(input =  '0-data/PhenologyVeg.csv')%>%
  mutate(DAS_FL=ifelse(Event=='FL',DAS,NA))%>%
  mutate(Date=dmy(Date))%>%
  filter(!Plant %in% c('D_V3_3','L_V3_3'))

don_all=merge(don,qual,all=T)%>%
  data.frame()


donMod=merge(don_all,lightCarac%>%select(Treatment,DLI,DLF),all.x =T,all.y=F)%>%
  filter(!Plant %in% c('D_V3_3','L_V3_3'))


# dynamic of plant developpment ------------------------------------------------------------------


dynL=pheno%>%
  filter(Variety %in% Vprod & !is.na(nb_Leaves))%>%
  group_by(Variety,Treatment,DAS)%>%
  summarize(nbL=mean(nb_Leaves),
            nbL_sd=sd(nb_Leaves))%>%
  ungroup()%>%
  ggplot(aes(x=DAS,y=nbL,col=Treatment))+
  geom_hline(yintercept = median(pheno$nb_Leaves,na.rm=T),lty=2)+
  geom_point()+
  geom_smooth(se=F)+
  geom_errorbar(aes(x = DAS,y=nbL,ymax=nbL+nbL_sd,ymin=nbL-nbL_sd))+
  facet_wrap(~Variety,ncol=1,scales='free_y')+
  scale_color_manual(values = color_Trt,name='')+
  myTheme+
  labs(x='Days after sowing',
       y='Number of leaves')

dynT=pheno%>%
  filter(Variety %in% Vprod & !is.na(nb_tillers))%>%
  group_by(Variety,Treatment,DAS)%>%
  summarize(nbT=mean(nb_tillers),
            nbT_sd=sd(nb_tillers))%>%
  ungroup()%>%
  ggplot(aes(x=DAS,y=nbT,col=Treatment))+
  geom_hline(yintercept = median(pheno$nb_tillers,na.rm=T),lty=2)+
  geom_point()+
  geom_smooth(se=F)+
  geom_errorbar(aes(x = DAS,y=nbT,ymax=nbT+nbT_sd,ymin=nbT-nbT_sd))+
  facet_wrap(~Variety,ncol=1,scales='free_y')+
  scale_color_manual(values = color_Trt,name='')+
  myTheme+
  labs(x='Days after sowing',
       y='Number of tillers')

morphoL_rel=merge(morphoL%>%
                    filter(Id %in% c(2,3,4,7,8,9))%>%
                    filter(!is.na(Length)),
                  pheno%>%
                    filter(!is.na(Event))%>%
                    select(Treatment,Variety,Id,Plant,nb_Leaves),all.x=T,all.y=F,by =c('Plant','Treatment','Variety') )%>%
  mutate(Leaf=as.factor(Leaf))

levels(morphoL_rel$Leaf)=seq(0,length(unique(morphoL$Leaf))-1,1)

morphoL_rel=morphoL_rel%>%
  mutate(Leaf=nb_Leaves-as.numeric(Leaf))%>%
  group_by(Variety,Treatment,Plant)%>%
  mutate(Leaf_rel=Leaf/max(Leaf),
         length_rel=Length/max(Length))%>%
  ungroup()

dynl=morphoL_rel%>%
  group_by(Variety,Treatment,Leaf)%>%
  mutate(rep=n())%>%
  ungroup()%>%
  filter(rep>2 & Leaf>=6)%>%
  group_by(Variety,Treatment,Leaf)%>%
  summarize(Lm=mean(Length),
            L_sd=sd(Length))%>%
  ungroup()%>%
  ggplot(aes(x=Leaf,y=Lm,col=Treatment))+
  geom_hline(yintercept = median(morphoL_rel$Length,na.rm=T),lty=2)+
  geom_point()+
  geom_smooth(aes(group=Treatment),se=F)+
  geom_errorbar(aes(x = Leaf,y=Lm,ymax=Lm+L_sd,ymin=Lm-L_sd),width=0.2)+
  facet_wrap(~Variety,ncol=1,scales='free_y')+
  scale_color_manual(name='',values = color_Trt)+
  myTheme+
  labs(y='Leaf length (cm)',
       x='Leaf rank')


cowplot::plot_grid(dynL+theme(legend.position = c(0.8,0.85)),dynT+theme(legend.position = 'none'),dynl+theme(legend.position = 'none'),ncol=3,labels=c('A','B','C'))

ggsave(filename = '2-outputs/Figures_rev/FigDynDev.png',width = 8,height = 8)


# tables model LRT and tuckey ------------------------------------------------------------

var_rdt=c('Yield','Nb_Pan','Nb_Kern','TKW','SF','HI')
var_M=c('Length','Width','LWratio','Area','Incli','StoDen')
var_pheno=c('DaysFL','phylochron')
var_archi=c('Nb_Lea',"Nb_Til",'PLA','CPN','SLA','VDW','PH')
var_Eco=c('SGA','SPAD','Lcp','Asat','Aqe','Rd','ETRm',"alpha",'Ik')
var_qual=c('M','m','starch_content','amylose_content','porosity')

tableNames=data.frame(var=c(var_rdt,var_pheno,var_M,var_archi,var_Eco,var_qual),
                      type=c(rep('Yield components',length(var_rdt)),
                             rep('Phenology & growth',length(var_pheno)),
                             rep('Flag leaf morphology',length(var_M)),
                             rep('Plant architecture',length(var_archi)),
                             rep('Ecophysiology',length(var_Eco)),
                             rep('Quality',length(var_qual))
                      ))


sqrt_var=c('Width','LWratio','Area','Lcp','SGA','m')
boxcox_var=c('TKW','Incli','alpha','DaysFL','Nb_Lea','Nb_Til','PLA','SF','starch_content','porosity')

tableNames=tableNames%>%
  mutate(transfo='no')

tableNames=tableNames%>%
  mutate(transfo=ifelse(test = var %in% sqrt_var,yes = 'sqrt',no = ifelse(test = var %in% boxcox_var,yes = 'bc',no = transfo)))

tableMod=NULL
sumTab=f.barplot(data=don,var=tableNames$var[1])$tab%>%select(Variety,Treatment)

for (i in 1:nrow(tableNames)){
  
  var=tableNames$var[i]
  coef_bc=NA
  print(paste('______',var,'_____________'))
  
  sub=donMod%>%filter(!is.na(get(var)))
  
  if (tableNames$transfo[i]=='no') {
    mod=lm(data=sub,formula = get(var)~Variety+as.numeric(DLI)+Variety:as.numeric(DLI)+as.numeric(DLF)+Variety:as.numeric(DLF))
    mod2=lm(data=sub,formula = get(var)~Variety+as.numeric(DLI)+Variety:as.numeric(DLI))
  }
  
  if (tableNames$transfo[i]=='log') {
    mod=lm(data=sub,formula = log(get(var))~Variety+as.numeric(DLI)+Variety:as.numeric(DLI)+as.numeric(DLF)+Variety:as.numeric(DLF))
    
    mod2=lm(data=sub,formula = log(get(var))~Variety+as.numeric(DLI)+Variety:as.numeric(DLI))
  }
  
  if (tableNames$transfo[i]=='sqrt') {
    mod=lm(data=sub,formula = sqrt(get(var))~Variety+as.numeric(DLI)+Variety:as.numeric(DLI)+as.numeric(DLF)+Variety:as.numeric(DLF))
    
    mod2=lm(data=sub,formula = sqrt(get(var))~Variety+as.numeric(DLI)+Variety:as.numeric(DLI))
  }
  
  if (tableNames$transfo[i]=='bc') {
    mod=lm(data=sub,formula = get(var)~Variety+as.numeric(DLI)+Variety:as.numeric(DLI)+as.numeric(DLF)+Variety:as.numeric(DLF))
    p1 <- powerTransform(mod)
    print(p1)
    coef_bc=coef(p1)
    sub <- transform(sub, var_bc=bcPower(get(var),coef(p1)))
    
    mod=lm(data=sub,formula = var_bc~Variety+as.numeric(DLI)+Variety:as.numeric(DLI)+as.numeric(DLF)+Variety:as.numeric(DLF))
    
    mod2=lm(data=sub,formula = var_bc~Variety+as.numeric(DLI)+Variety:as.numeric(DLI))
  }
  
  tableMod_sub=data.frame(var= tableNames$var[i],
                          type=tableNames$type[i],
                          Variety=f.sig(p=anova(mod)["Pr(>F)"]['Variety',1]),
                          DLI=f.sig(p=anova(mod)["Pr(>F)"]['as.numeric(DLI)',1]),
                          DLF=f.sig(p=anova(mod)["Pr(>F)"]['as.numeric(DLF)',1]),
                          `Variety x DLI`=f.sig(p=anova(mod)["Pr(>F)"]['Variety:as.numeric(DLI)',1]),
                          `Variety x DLF`=f.sig(p=anova(mod)["Pr(>F)"]['Variety:as.numeric(DLF)',1]),
                          r2=  round(summary(mod)$adj.r.squared,2),
                          `LRtest`=f.sig(p=lrtest(mod, mod2)["Pr(>Chisq)"][2,]),
                          transfo=tableNames$transfo[i],
                          coeff_bc=coef_bc)
  
  tableMod=rbind(tableMod,tableMod_sub)
  
  out=f.barplot(data=sub,var=tableMod_sub$var)
  
  out$tab$Transfo=NA
  if (!is.na(tableMod_sub$transfo) & tableMod_sub$transfo=='bc'){
    sub <- transform(sub, var_bc=bcPower(get(var),coef_bc))
    out_transfo=f.barplot(data=sub,var='var_bc',var_name =var )
    out$tab$Transfo=out_transfo$tab[,3]
  }
  
  if (!is.na(tableMod_sub$transfo) &tableMod_sub$transfo=='sqrt'){
    sub <- transform(sub, var_bc=sqrt(get(var)))
    out_transfo=f.barplot(data=sub,var='var_bc',var_name =var)
    out$tab$Transfo=out_transfo$tab[,3]
  }
  
  
  sumTab=cbind(sumTab,out$tab[,c(3,4)])
  
}

### export tables
data.table::fwrite(tableMod,file = '2-outputs/Tables_rev/Table_LRT.csv',row.names = F)

tabExp=t(sumTab)%>%data.frame()
colnames(tabExp)=tabExp['Variety',]


data.table::fwrite(tabExp,file = '2-outputs/Tables_rev/TableTuckey.csv',row.names =T )



# response curves Licor---------------------------------------------------------


lic_raw=fread(file = '0-data/CurveLightLicor.csv')%>%
  mutate(Date=dmy(Date),
         Time=hms(Time),
         ETRm_FL=ETRm,alpha_FL=alpha,Ik_FL=Ik,
         SPAD_FL=SPAD,
         PARm=round(PAR))%>%
  filter(!Plant %in% c('D_V3_3','L_V3_3'))


ggplot()+
  geom_smooth(data=lic_raw%>%
                filter(Variety %in% Vprod ),aes(x=PAR,y=A,group=paste(Treatment,Variety),col=Variety,fill=Variety),se=T)+
  geom_rect(data=lightCarac%>%mutate(xmin=0,ymin=-Inf,ymax=Inf),aes(x=DLF,y=10,xmin=xmin,xmax=DLF,ymin=ymin,ymax=ymax,fill=Treatment),alpha=0.1)+
  facet_wrap(~Treatment,scale='free_x')+
  scale_color_manual(values = color_Var)+
  scale_fill_manual(values = color_Var)+
  myTheme+
  theme(legend.position=c(0.95,0.2))+
  ylab(expression('A '*(mu*mol[C02]*' '*m**-2*' '*s**-1)))+
  xlab(expression('PPFD '*(mu*mol[photons]*' '*m**-2*' '*s**-1)))

ggsave(filename = '2-outputs/Figures_rev/FigResponseCurves.png',width = 8,height = 4)



# Boxplot -------------------------------------------------------------------

### yield

### tuckey for each Variety

donV_all=NULL
for (t in unique(don$Variety)){
  modelTuckeyT<-aov(Yield~Treatment, data=don%>%
                      filter(Variety==t))
  
  outV <- HSD.test(modelTuckeyT,"Treatment", group=TRUE,console=F,
                   main="")
  
  resV=data.frame(outV$means)
  resGV=data.frame(outV$groups)
  colnames(resV)[1]='Yield'
  colnames(resGV)[1]='Yield'
  
  donV=merge(resV%>%
               mutate(Variety=t,
                      Treatment=row.names(data.frame(outV$means))),resGV)
  
  donV_all=rbind(donV_all,donV)
  
  
}
plotYield=ggplot()+
  geom_boxplot(data=don,aes(x=Treatment,y=Yield,group=paste(Variety,Treatment),col=Treatment),notch=F,outlier.colour=NA)+
  geom_jitter(data=don,aes(x=Treatment,y=Yield,group=paste(Variety,Treatment),col=Treatment),height=0)+
  geom_text(data=donV_all,aes(x=Treatment,y=1.1*Yield+1.8*std,label=groups,col=Treatment))+
  facet_grid(~Variety)+
  scale_color_manual(name='Treatment',values = color_Trt)+
  ylab(expression('Yield'*' '*(g.plant**-1)))+
  xlab('')+
  myTheme+
  theme(legend.position='none')


### SGA

### tuckey for each Variety
donSGA=NULL
for (t in unique(don$Variety)){
  modelTuckeyT<-aov(SGA~Treatment, data=don%>%
                      filter(Variety==t))
  
  outV <- HSD.test(modelTuckeyT,"Treatment", group=TRUE,console=F,
                   main="")
  
  resV=data.frame(outV$means)
  resGV=data.frame(outV$groups)
  colnames(resV)[1]='SGA'
  colnames(resGV)[1]='SGA'
  
  donT=merge(resV%>%
               mutate(Variety=t,
                      Treatment=row.names(data.frame(outV$means))),resGV)
  
  donSGA=rbind(donSGA,donT)
  
  
}


plotSGA=ggplot()+
  geom_boxplot(data=don,aes(x=Treatment,y=SGA,group=paste(Variety,Treatment),col=Treatment),notch=F,outlier.colour=NA)+
  geom_jitter(data=don,aes(x=Treatment,y=SGA,group=paste(Variety,Treatment),col=Treatment),height=0)+
  geom_text(data=donSGA,aes(x=Treatment,y=1.1*SGA+1.8*std,label=groups,col=Treatment))+
  facet_grid(~Variety)+
  scale_color_manual(name='Treatment',values = color_Trt)+
  ylab(expression('Stay grean area'*' '*(cm**2*'plant'**-1)))+
  xlab('')+
  myTheme+
  theme(legend.position='none')


cowplot::plot_grid(plotSGA,plotYield,ncol=1,labels=c('A','B'))

ggsave(filename = '2-outputs/Figures_rev/Boxplots.png',width = 10,height = 8)


# Trait Correlations -------------------------------------------------------------

computepvalue = function(cormatrice) {
  mat <- as.matrix(cormatrice)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j])
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

var_corr=c("Yield",'Nb_Pan','Nb_Kern','TKW','HI','SF','DaysFL','phylochron','Width','Length','Nb_Til','Nb_Lea','SGA','PH','VDW','Asat','Aqe','Lcp','ETRm','alpha','Ik')


donCorr=na.omit(don%>%
                  select(c('Id','Plant','Treatment','Variety',var_corr)))



donCor_C=donCorr%>%filter(Treatment=='C')%>%select(-c(Treatment,Variety,Plant,Id))
donCor_S=donCorr%>%filter(Treatment=='S')%>%select(-c(Treatment,Variety,Plant,Id))
donCor_SAF=donCorr%>%filter(Treatment=='S-AF')%>%select(-c(Treatment,Variety,Plant,Id))
don_cor_all=donCorr%>%select(-c(Treatment,Variety,Plant,Id))


corC=ggcorrplot(cor(donCor_C)
                , hc.order = F, type = 'lower', tl.srt = 45,
                lab = T,p.mat =computepvalue(cor(donCor_C)), sig.level = 0.01,insig = "blank")+
  ggtitle('C')


corS=ggcorrplot(cor(donCor_S)
                , hc.order = F, type = 'lower', tl.srt = 45,
                lab = T,p.mat =computepvalue(cor(donCor_C)), sig.level = 0.01,insig = "blank")+
  ggtitle('S')

corSAF=ggcorrplot(cor(donCor_SAF)
                  , hc.order = F, type = 'lower', tl.srt = 45,
                  lab = T,p.mat =computepvalue(cor(donCor_C)), sig.level = 0.01,insig = "blank")+
  ggtitle('S-AF')

corAll=ggcorrplot(cor(don_cor_all)
                  , hc.order = F, type = 'lower', tl.srt = 45,
                  lab = T,p.mat =computepvalue(cor(donCor_C)), sig.level = 0.01,insig = "blank")+
  ggtitle('All treatments')



cowplot::plot_grid(hjust = -0.2,corC,corS,corSAF,corAll,
                   ncol=2,
                   labels=c('A','B','C','D'))

ggsave(filename = '2-outputs/Figures_rev/FigCorrelations.png',width = 18,height =18)




# PCA ---------------------------------------------------------------------



set.seed(1)
var_ACP=c("Yield",'Nb_Kern','TKW','HI','SF','DaysFL','phylochron','Width','Length','Nb_Til','Nb_Lea','SGA','VDW','Asat','Lcp','Ik')
donACP=na.omit(don%>%
                 select(c('Id','Plant','Treatment','Variety',var_ACP)))


acpAll=f.ACP(don=donACP,vars = var_ACP,table_type = tableNames)

acpAll$gr_vp

tableACP=round(acpAll$table[,c(1,2,3)],2)
data.table::fwrite(tableACP,file = '2-outputs/Tables_rev/Table_PCA.csv',row.names =T )


cowplot::plot_grid(hjust = -0.2,
                   acpAll$gr_corr12+myTheme+theme(legend.position = 'none'),
                   acpAll$gr_proj12+myTheme+theme(legend.position=c(0.8,0.9),legend.title=element_blank()),
                   labels=c('A','B'),
                   ncol=2)


ggsave(filename = '2-outputs/Figures_rev/FigACP.png',width = 10,height =5)


### LRT test on PCA components


donModAll=merge(acpAll$data%>%
                  rename(PC1=x,PC2=y,PC3=z)%>%
                  select(Variety,Plant,Id,Treatment,PC1,PC2,PC3),lightCarac%>%select(Treatment,DLI,DLF),all.x =T,all.y=F)%>%
  filter(!Plant %in% c('D_V3_3','L_V3_3'))

tableModACP=NULL
for (var in c('PC1','PC2','PC3')){
  # var='PC1'
  
  sub=donModAll%>%filter(!is.na(get(var)))
  
  mod=lm(data=sub,formula = get(var)~Variety+as.numeric(DLI)+Variety:as.numeric(DLI)+as.numeric(DLF)+Variety:as.numeric(DLF))
  mod2=lm(data=sub,formula = get(var)~Variety+as.numeric(DLI)+Variety:as.numeric(DLI))
  
  
  
  tableModACP_sub=data.frame(var= var,
                             Variety=f.sig(p=anova(mod)["Pr(>F)"]['Variety',1]),
                             DLI=f.sig(p=anova(mod)["Pr(>F)"]['as.numeric(DLI)',1]),
                             DLF=f.sig(p=anova(mod)["Pr(>F)"]['as.numeric(DLF)',1]),
                             `Variety x DLI`=f.sig(p=anova(mod)["Pr(>F)"]['Variety:as.numeric(DLI)',1]),
                             `Variety x DLF`=f.sig(p=anova(mod)["Pr(>F)"]['Variety:as.numeric(DLF)',1]),
                             r2=  round(summary(mod)$adj.r.squared,2),
                             `LRtest`=f.sig(p=lrtest(mod, mod2)["Pr(>Chisq)"][2,]))
  
  tableModACP=rbind(tableModACP,tableModACP_sub)
}


print(tableModACP)

data.table::fwrite(tableModACP,file = '2-outputs/Tables_rev/Table_LRT_PCA.csv',row.names =T )


