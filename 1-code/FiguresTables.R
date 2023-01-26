#### Analysis of Agroforice data - exportation of Tables and Figures###
##R Perez 06/01/2022

# Load packages  -----------------------------------------------------------

packs <- c('tidyverse', "data.table", "lubridate",'remotes','viridis','ggpmisc',"ggrepel",'png','magick','agricolae','cowplot','RColorBrewer','ade4','ggcorrplot','car','lmtest')
InstIfNec<-function (pack) {
  if (!do.call(require,as.list(pack))) {
    do.call(install.packages,as.list(pack))  }
  do.call(require,as.list(pack)) }
lapply(packs, InstIfNec)

# source('1-code/Mapping_light.R')
source('1-code/helpers.R')

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


color_Trt=brewer.pal(n = 3, name = 'Set2')
names(color_Trt)=c('C','S-AF','S')

color_Var=brewer.pal(n = 4, name = 'BrBG')
names(color_Var)=c('V3','V5','V7','V8')

shape_Trt=c(0,1,2)
names(shape_Trt)=c('C','S-AF','S')

mT=data.frame(Treatment=c('C','D','L'),new_Treatment=c('C','S-AF','S'))

Vprod=c('V3','V5','V7','V8')


# table planting designs --------------------------------------------------


table_design=grid_df_day%>%
  group_by(Treatment)%>%
  summarize(dist_inter=unique(str_sub(string =Treatment,start = str_locate(string = Treatment,pattern = '_inter')[1,2]+1,end = str_locate(string = Treatment,pattern = 'xintra')[1,1]-1)),
            dist_intra=unique(str_sub(string =Treatment,start = str_locate(string = Treatment,pattern = 'xintra')[1,2]+1,end = str_locate(string = Treatment,pattern = '_den')[1,1]-1)),
            density=unique(str_sub(string =Treatment,start = str_locate(string = Treatment,pattern = '_den')[1,2]+1,end = str_length(Treatment))))%>%
  select(dist_intra,dist_inter,density)%>%
  arrange(as.numeric(density))


colnames(table_design)=c('dist intra (m)','dist inter (m)','density (plant.ha-1)')

print(table_design)

data.table::fwrite(table_design,file = '2-outputs/Tables/Design.csv',row.names =T )



# map of light ------------------------------------------------------------
###relatively to incident PAR0
load(file = '0-data/MapLight.RData')

lims=as.numeric(grid_df_day_comp%>%
                  group_by(Treatment)%>%
                  summarize(x=max(x,na.rm=T),
                            y=max(y,na.rm=T))%>%
                  ungroup()%>%
                  summarize(x=min(x),y=min(y)))

map_ligth_rel=grid_df_day_comp%>%
  mutate(density=as.numeric(density))%>%
  arrange(density)%>%
  filter(x<=lims[1] & y<=lims[2])%>%
  ggplot(aes(x=x, y=y,fill=Intercepted_rel,col=Intercepted_rel))+
  geom_point(pch=22)+
  geom_point(aes(x=x_tree_1,y=y_tree_1),col=2,pch=0)+
  geom_point(aes(x=x_tree_2,y=y_tree_2),col=2,pch=0)+
  xlim(c(0,lims[1]))+
  ylim(c(0,lims[2]))+
  facet_wrap(~paste(sprintf("%03d",density),'plant.ha-2')) +
  coord_fixed()+
  labs(x = 'x (m)', y = 'y (m)',fill= '%',col='%') +
  # theme(legend.position="bottom")+
  scale_fill_viridis()+
  scale_color_viridis()

map_ligth_rel+myTheme+theme(legend.direction = 'horizontal',legend.position = c(0.83,0.12))
ggsave(file = "2-outputs/Figures/Fig_light_planting.png")



# simulated dynamic of light ----------------------------------------------


###coversion factor micromol.m-2.s-1 --> MJ.m-2
conv=60/((10**6)*4.6)

###coversion factor W.m-2.30mn-->micromol.m-2.s-1 of PAR
conv0=0.48*4.6

load('0-data/meteo.RData')


load('0-data/SimuLight.RData')

don_crop=don_crop%>%
  mutate(density=str_replace(string =density,pattern = 'm-2',replacement = 'ha-1'))


inc=meteo%>%
  mutate(Hour=ymd_hms(str_replace(string = date,pattern = '2019-03-20',replacement = '2020-05-06')),ppfd_inc=conv0*`globalIrradiance (W/m2)`)%>%
  select(Hour,ppfd_inc)


inc_all=merge(inc%>%
                mutate(Hour=as.character(Hour)),don_crop%>%
                mutate(Hour=as.character(Hour)))%>%
  mutate(Hour=ymd_hms(Hour),
         trans=ifelse(ppfd_inc==0,0,ppfd_mean/ppfd_inc))

grid_lab2=inc_all%>%
  group_by(Treatment)%>%
  summarize(Hour=mean(Hour,na.rm=T),
            ppfd_mean=max(ppfd_mean),
            trans=max(trans,na.rm=T),
            density=unique(density),
            dist_inter=unique(dist_inter),
            dist_intra=unique(dist_intra),
            design=unique(design))



gr2a=ggplot()+
  geom_line(data=inc_all,aes(x=Hour,y=trans*100,col=density),lwd=1.5)+
  geom_label_repel(data=grid_lab2,aes(label=design,x=Hour,y=trans*100,fill=density),col='white')+
  scale_color_viridis_d(name='density')+
  scale_fill_viridis_d(name='density')+
  ylab(expression ('Light transmission (%)'))+
  ylim(c(0,50))+
  xlab('Hour of the day')+
  myTheme+
  theme(legend.position = c(0.8,0.8))


donComp=don_crop%>%
  group_by(Treatment,density,design)%>%
  summarize(PAR_day=sum(ppfd_mean*60*0.48)*conv)%>%
  mutate(dens=as.numeric(str_sub(string = density,start = 0,end = 3)),
         ratio=PAR_day/dens)

ratiomean=mean(donComp$ratio)

gr2b=ggplot()+
  geom_smooth(data=donComp,aes(x=dens,y=PAR_day),method='lm',se=F,lty=2,col="lightgrey")+
  geom_x_margin_arrow(xintercept = 136,arrow.length	
                      =0.1)+
  geom_point(data=donComp,aes(x=dens,y=PAR_day,col=density),size=5)+
  geom_label_repel(data=donComp,aes(x=dens,y=PAR_day,label=design,fill=density),col='white')+

  scale_color_viridis_d()+
  scale_fill_viridis_d()+
  myTheme+
  theme(legend.position='none')+
  ylab(expression ('Transmitted PAR '*(MJ*' '*m**-2*' '*day**-1)))+
  xlab(expression ('Palm density '*(plants*' '*ha**-1)))


cowplot::plot_grid(gr2a,gr2b,labels=c('A','B'),rel_widths = c(0.5,0.5))

ggsave(filename = '2-outputs/Figures/Fig_simul_Light.png',width = 12,height = 8)


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

rad2=inc_all%>%filter(design=="07 m x 12 m")%>%
  mutate(Treatment='sim',
         Rad=ppfd_mean,
         Hour=as.character(Hour))%>%
  select(Hour,Rad,Treatment)



fig3a=ggplot()+
  geom_point(data=rad1,aes(x=ymd_hms(Hour),y=Rad,shape=Treatment,col=Treatment),size=2)+
  geom_line(data=rad2,aes(x=ymd_hms(Hour),y=Rad),lwd=1.5)+
  geom_point(data=rad1,aes(x=ymd_hms(Hour),y=Rad,shape=Treatment,col=Treatment),size=2)+
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
ggsave(filename = '2-outputs/Figures/Fig_Light_chambers.png',width = 8,height = 6)


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

ggsave(filename = '2-outputs/Figures/Fig_TempHR_chambers.png',width = 8,height = 6)


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
ggsave(filename = '2-outputs/Figures/Fig_light_spectr_chambers.png',width = 8)


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

ggsave(filename = '2-outputs/Figures/FigImageAnalysis.png')


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

ggsave(filename = '2-outputs/Figures/FigDynDev.png',width = 8,height = 8)



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
data.table::fwrite(tableMod,file = '2-outputs/Tables/Table_LRT.csv',row.names = F)

tabExp=t(sumTab)%>%data.frame()
colnames(tabExp)=tabExp['Variety',]


data.table::fwrite(tabExp,file = '2-outputs/Tables/TableTuckey.csv',row.names =T )



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

ggsave(filename = '2-outputs/Figures/FigResponseCurves.png',width = 8,height = 4)



# yield -------------------------------------------------------------------

### tuckey for each treatment
donT_all=NULL
for (t in unique(don$Treatment)){
  modelTuckeyT<-aov(Yield~Variety, data=don%>%
                      filter(Treatment==t))

  outV <- HSD.test(modelTuckeyT,"Variety", group=TRUE,console=F,
                   main="")

  resV=data.frame(outV$means)
  resGV=data.frame(outV$groups)
  colnames(resV)[1]='Yield'
  colnames(resGV)[1]='Yield'

  donT=merge(resV%>%
               mutate(Treatment=t,
                      Variety=row.names(data.frame(outV$means))),resGV)

  donT_all=rbind(donT_all,donT)


}


ggplot()+
  geom_boxplot(data=don,aes(x=Variety,y=Yield,group=paste(Variety,Treatment),col=Variety),notch=F,outlier.colour=NA)+
  geom_jitter(data=don,aes(x=Variety,y=Yield,group=paste(Variety,Treatment),col=Variety),height=0)+
  geom_text(data=donT_all,aes(x=Variety,y=1.1*Yield+1.8*std,label=groups,col=Variety))+
  facet_grid(~Treatment)+
  scale_color_manual(name='Variety',values = color_Var)+
  ylab(expression('Yield'*' '*(g.plant**-1)))+
  xlab('')+
  myTheme+
  theme(legend.position='none')

ggsave(filename = 'Figures/Yield.png',width = 8,height = 4)

# PCA ---------------------------------------------------------------------

set.seed(1)
var_ACP=c('Nb_Pan','Nb_Kern','TKW','HI','SF','SGA','phylochron','Width','Length','Area','Nb_Lea','Nb_Til','PH','VDW','Asat','Lcp')

donACP=na.omit(don%>%
                 select(c('Id','Plant','Treatment','Variety',var_ACP)))





acpC=f.ACP(don=donACP%>%filter(Treatment=='C'),vars = var_ACP,table_type = tableNames)


acpD=f.ACP(don=donACP%>%filter(Treatment=='S-AF'),vars = var_ACP,table_type = tableNames)

acpL=f.ACP(don=donACP%>%filter(Treatment=='S'),vars = var_ACP,table_type = tableNames)

print(cowplot::plot_grid(acpC$gr_vp,acpD$gr_vp,acpL$gr_vp,labels=c('C','S-AF','S'),ncol=1))

cowplot::plot_grid(hjust = -0.2,acpC$gr_corr12+myTheme+theme(legend.position='none')+geom_text(aes(x=-1,y=1,label='C')),
                   acpC$gr_proj12+myTheme+theme(legend.position='none')+geom_text(aes(x=-3.5,y=4.5,label='C')),
                   acpD$gr_corr12+myTheme+theme(legend.position='none')+geom_text(aes(x=-0.96,y=1,label='S-AF')),
                   acpD$gr_proj12+myTheme+theme(legend.position='none')+geom_text(aes(x=-3.75,y=3.5,label='S-AF')),
                   acpL$gr_corr12+myTheme+theme(legend.position='none')+geom_text(aes(x=-1,y=1,label='S')),
                   acpL$gr_proj12+myTheme+theme(legend.position='none')+geom_text(aes(x=-3.75,y=3,label='S')),
                   ncol=2,
                   labels=c('A','B','B','','C','')

)


ggsave(filename = 'Figures/FigACP.png',width = 8,height = 10)





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

var_ACP2=c('Yield','Nb_Pan','Nb_Kern','TKW','HI','SF','SGA','phylochron','Width','Length','Area','Nb_Lea','Nb_Til','PH','VDW','Asat','Aqe','Lcp')


donACP2=na.omit(don%>%
                 select(c('Id','Plant','Treatment','Variety',var_ACP2)))



donCor_C=donACP2%>%filter(Treatment=='C')%>%select(-c(Treatment,Variety,Plant,Id))
donCor_S=donACP2%>%filter(Treatment=='S')%>%select(-c(Treatment,Variety,Plant,Id))
donCor_SAF=donACP2%>%filter(Treatment=='S-AF')%>%select(-c(Treatment,Variety,Plant,Id))

match=merge(data.frame(var=colnames(donCor_C)),tableNames,all.x=T,sort = F)

colnames(donCor_C)=match$var_names
colnames(donCor_S)=match$var_names
colnames(donCor_SAF)=match$var_names


corC=ggcorrplot(cor(donCor_C)
           , hc.order = F, type = 'lower', tl.srt = 45,
           lab = T,p.mat =computepvalue(cor(donCor_C)), sig.level = 0.05,insig = "blank")+
  ggtitle('C')


corS=ggcorrplot(cor(donCor_S)
           , hc.order = F, type = 'lower', tl.srt = 45,
           lab = T,p.mat =computepvalue(cor(donCor_C)), sig.level = 0.05,insig = "blank")+
  ggtitle('S')

corSAF=ggcorrplot(cor(donCor_SAF)
           , hc.order = F, type = 'lower', tl.srt = 45,
           lab = T,p.mat =computepvalue(cor(donCor_C)), sig.level = 0.05,insig = "blank")+
  ggtitle('S-AF')

cowplot::plot_grid(hjust = -0.2,corC,corS,corSAF,
                   ncol=2,
                   labels=c('A','B','C'))


ggsave(filename = 'Figures/FigCorrelations.png',width = 8,height = 10)

