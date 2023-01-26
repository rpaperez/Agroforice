#### Visualisation of archimed outputs###
##R Perez & R Vezy, 06/01/2022


# Load packages -----------------------------------------------------------

packs <- c("lubridate", "stringr", "ggplot2",'dplyr','ggpmisc','plotly','archimedR','viridis','ggrepel')
InstIfNec<-function (pack) {
  if (!do.call(require,as.list(pack))) {
    do.call(install.packages,as.list(pack))  }
  do.call(require,as.list(pack))}
lapply(packs, InstIfNec)

# install archimedR
if (require('archimedR')==F){
  install.packages('remotes')
  remotes::install_github('VEZY/archimedR')
}

# import data -------------------------------------------------------------

###designs
design_folder='0-data/planting_design/'
planting_designs=list.files(path = design_folder,pattern = '.csv')


###archimed outputs
sim_folder='0-data/Simulation_outputs/'

files_sim= 
  sim_folder%>%
  list.files(recursive = TRUE, full.names = TRUE)

###meteo
path_meteo= files_sim[grep("meteo.csv",files_sim)]
# Importing the meteorology file from the first simulation (all are the same):
meteo= archimedR::import_meteo(x = file.path(path_meteo[1]))%>%
  mutate(date= as.POSIXct(date)+ seconds(hour_start))


###conversion factor W.m-2.30mn of GR-->MJ.m-2.day-1 of PAR
conMJday=0.48*1800*10**-6

##"incident PAR in MJ.m-2.day-1
PARinc=as.numeric(meteo%>%
                    summarize(PARinc=sum(`globalIrradiance (W/m2)`*conMJday)))


###incident radiation
###coversion factor W.m-2.30mn-->micromol.m-2.s-1 of PAR
conv0=0.48*4.6

graph_PPFD_inc=meteo%>%
  mutate(PAR=conv0*`globalIrradiance (W/m2)`)%>%
  ggplot(aes(x=date ,y=PAR))+
  geom_line()+
  ylab(expression ('Incident '~"PPFD "*(mu*mol*' '*m**-2*' '*s**-1)))+
  xlab('Hour of the day')


###import summary outputs
path_sum= files_sim[grep("summary.csv",files_sim)]
# Importing the meteorology file from the first simulation (all are the same):
summ=data.table::fread(file =  (path_sum[1]))

stepDuration=unique(summ$`stepDuration (sec)`)

# Importing the node values (main output):
path_nodes= files_sim[grep("nodes_values.csv",files_sim)]

nodes= 
  lapply(path_nodes, function(x){
    name= 
      x%>%dirname()%>%strsplit(split = "/")%>%unlist()%>%
      head(-1)%>%tail(1)%>%strsplit(split = " ")%>%unlist()%>%head(1)
    data.table::fread(x)%>%mutate(Treatment= name)
  })%>%data.table::rbindlist()%>%tibble::as_tibble()


ngrid= 
  nodes%>%
  ungroup()%>%
  filter(type=="Cobblestone")%>%
  filter(stepNumber==stepNumber[1])%>%
  group_by(Treatment)%>%
  summarise(ngrid= n(), area_grid= sum(.data$meshArea))

# Grid index: 
grid_dim= 
  lapply(planting_designs, function(x){
    cbind(
      Treatment= x%>%basename%>%gsub(".csv","",.),
      data.table::fread(paste0(design_folder,'/',x), data.table = FALSE)%>%
        summarise_at(vars(matches("max|min")),unique)
    )
  })%>%data.table::rbindlist(use.names=TRUE)%>%as_tibble()

grid_dim= 
  dplyr::right_join(grid_dim,ngrid%>%mutate(Treatment= gsub("DA1_MAP_[0-9][0-9]_","",Treatment)))%>%
  mutate(
    surf_grid= area_grid/ngrid,
    x_length= xmax-xmin, y_length= ymax-ymin,
    grid_n_x= round(x_length/sqrt(surf_grid)),
    grid_n_y= round(y_length/sqrt(surf_grid))
  )
grid_dim$Treatment= ngrid$Treatment


grid_index= 
  lapply(grid_dim$Treatment, function(z){
    grid_= grid_dim[grid_dim$Treatment==z,]
    expand.grid(y= (1:grid_$grid_n_y)*sqrt(grid_$surf_grid),
                x= (1:grid_$grid_n_x)*sqrt(grid_$surf_grid))%>%
      mutate(Treatment= z)%>%
      mutate(id= 1:nrow(.))
  })%>%data.table::rbindlist()%>%as_tibble()



# Reading the trees positions:
trees_positions= 
  lapply(planting_designs, function(x){
    treatment= basename(x)%>%gsub(".csv","",.)
    des_= data.table::fread(paste0(design_folder,'/',x),data.table = FALSE)%>%select(x,y)%>%unlist(.)
    des_%>%matrix(nrow = 1)%>%as.data.frame()%>%setNames(names(des_))%>%
      mutate(Treatment= treatment)
  })%>%data.table::rbindlist()%>%as_tibble()

trees_positions= 
  lapply(trees_positions$Treatment, function(x){
    found= grep(x,grid_dim$Treatment)
    trees_positions[trees_positions$Treatment==x,]%>%
      .[rep(1,length(found)),]%>%
      mutate(Treatment= grid_dim$Treatment[grep(x,grid_dim$Treatment)])
  })%>%data.table::rbindlist()%>%as_tibble()


grid_dist= 
  merge(grid_index,trees_positions,by = "Treatment")%>%
  mutate(dist_tree_1= sqrt((x - x1)^2 + (y - y1)^2),
         dist_tree_2= sqrt((x - x2)^2 + (y - y2)^2),
         dist_tree_x1= abs(x - x1),
         dist_tree_x2= abs(x - x2),
         x_tree_1= x1, x_tree_2= x2,
         y_tree_1=y1,y_tree_2=y2)%>%
  mutate(dist_tree= pmin(dist_tree_1,dist_tree_2),
         dist_tree_x= pmin(dist_tree_x1,dist_tree_x2))

plane_df= 
  nodes%>%
  filter(type=="Cobblestone")%>%
  mutate(nodeId= nodeId-1)%>% # id 1 was the scene
  # dplyr::left_join(Area_plots, by= "Treatment")%>%
  dplyr::left_join(meteo%>%select(date,step), by= c("stepNumber"= "step"))%>%
  dplyr::left_join(grid_dist, by= c("Treatment","nodeId"= "id"))

plane_df$Treatment= as.factor(plane_df$Treatment)

plane_df=plane_df%>%
  group_by(Treatment)%>%
  mutate(MAP=str_sub(string =Treatment,start = str_locate(string = Treatment,pattern = 'MAP_')[1,2]+1,end = str_locate(string = Treatment,pattern = '_inter')[1,1]-1),
         dist_inter=str_sub(string =Treatment,start = str_locate(string = Treatment,pattern = '_inter')[1,2]+1,end = str_locate(string = Treatment,pattern = 'xintra')[1,1]-1),
         dist_intra=str_sub(string =Treatment,start = str_locate(string = Treatment,pattern = 'xintra')[1,2]+1,end = str_locate(string = Treatment,pattern = '_den')[1,1]-1),
         density=str_sub(string =Treatment,start = str_locate(string = Treatment,pattern = '_den')[1,2]+1,end = str_length(Treatment)))%>%
  ungroup()%>%
  mutate(dist_inter=sprintf("%02d", as.numeric(dist_inter)),
         dist_intra=sprintf("%02d", as.numeric(dist_intra)),
         density=sprintf("%03d", as.numeric(density)))


plane_df_step= 
  plane_df%>%
  dplyr::mutate(Date= date,
                irradiation= absEnergy_withScattering_PAR*meshArea*stepDuration,
                irradiance= absEnergy_withScattering_PAR)







# Graphics ----------------------------------------------------------------

### Light transmitted to the soil according to the distance to the tree

grid_df_day= 
  plane_df_step%>%
  filter(Treatment %in% unique(plane_df_step$Treatment))%>%
  group_by(Treatment,nodeId)%>%
  summarise(Date= mean(.data$date),
            Intercepted= sum(absEnergy_withScattering_PAR/meshArea*10**-6),
            # absEnergy_withScattering_PAR J grid-1
            # Global intercepted radiation in J grid-1 d-1
            # Area_plot= mean(.data$Area_plot),
            dist_tree= mean(dist_tree),
            dist_tree_x= mean(dist_tree_x),
            density= unique(density),
            dist_inter=unique(dist_inter),
            dist_intra=unique(dist_intra),
            MAP= unique(MAP),
            x= unique(x),
            y=unique(y),
            x_tree_1= unique(x_tree_1), 
            x_tree_2= unique(x_tree_2),
            y_tree_1= unique(y_tree_1), 
            y_tree_2= unique(y_tree_2))%>%
  ungroup()



rep1=grid_df_day%>%
  group_by(Treatment)%>%
  mutate(xmax=max(x,na.rm=T),
         x=x+xmax,
         x_tree_1=x_tree_1+xmax,
         x_tree_2=x_tree_2+xmax,
         y=y)%>%
  ungroup()%>%
  select(colnames(grid_df_day))

rep2=grid_df_day%>%
  group_by(Treatment)%>%
  mutate(ymax=max(y,na.rm=T),
         x=x,
         y=y+ymax,
         y_tree_1=y_tree_1+ymax,
         y_tree_2=y_tree_2+ymax)%>%
  ungroup()%>%
  select(colnames(grid_df_day))

rep3=grid_df_day%>%
  group_by(Treatment)%>%
  mutate(xmax=max(x,na.rm=T),
         ymax=max(y,na.rm=T),
         x=x+xmax,
         y=y+ymax,
         x_tree_1=x_tree_1+xmax,
         x_tree_2=x_tree_2+xmax,
         y_tree_1=y_tree_1+ymax,
         y_tree_2=y_tree_2+ymax)%>%
  ungroup()%>%
  select(colnames(grid_df_day))




grid_df_day_comp=rbind(grid_df_day,rep1,rep2,rep3)%>%
  mutate(Intercepted_rel=Intercepted/PARinc*100)

lims=as.numeric(grid_df_day_comp%>%
                  group_by(Treatment)%>%
                  summarize(x=max(x,na.rm=T),
                            y=max(y,na.rm=T))%>%
                  ungroup()%>%
                  summarize(x=min(x),y=min(y)))


map_light=grid_df_day_comp%>%
  mutate(density=as.numeric(density))%>%
  arrange(density)%>%
  filter(x<=lims[1] & y<=lims[2])%>%
  ggplot(aes(x=x, y=y,fill=Intercepted,col=Intercepted))+
  geom_point(pch=22)+
  geom_point(aes(x=x_tree_1,y=y_tree_1),col=2,pch=0)+
  geom_point(aes(x=x_tree_2,y=y_tree_2),col=2,pch=0)+
  xlim(c(0,lims[1]))+
  ylim(c(0,lims[2]))+
  facet_wrap(~paste(sprintf("%03d",density),'plant.ha-2')) +
  coord_fixed()+
  labs(x = 'x (m)', y = 'y (m)',fill= expression (MJ*' '*m**-2*' '*day**-1 ),col=expression (MJ*' '*m**-2*' '*day**-1 )) +
  scale_fill_viridis()+
  scale_color_viridis()

###relatively to incident PAR
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

