#### Visualisation of archimed outputs###
##R Perez 12/06/2023

# Load packages -----------------------------------------------------------

packs <- c("lubridate", "stringr", "ggplot2",'dplyr','ggpmisc','plotly','archimedR','viridis','ggrepel','cowplot')
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

#' Create_map
#'
#' @param d_inter distance between rows of palm trees
#' @param d_intra distance within rows of palm trees
#' @param path_designs path to save planting designs
#' @param paramFileName name of the Vpalm param file (.txt)
#' @param orientation orientation of the scene (NS: North-South or EW: East-West)
#' @param meteoFileName name of meteo file
#'
#' @return map of transmitted light to the ground
#' @export csv file of the map
#'
#' @examples
load_map=function(d_inter=d_inter,d_intra=d_intra,path_designs=path_designs,paramFileName=paramFileName,orientation=orientation,meteoFileName=meteoFileName){
  
  # d_inter=12
  # d_intra=7
  # path_designs='2-outputs/Run_simu/planting_designs/'
  # paramFileName='DA1_Average_MAP_90'
  # orientation='NS'
  # meteoFileName='meteo20march'
  design_name=paste0('inter',d_inter,'-intra',d_intra)
  
  ### checking for existing map
  existing_files=list.files(path ='./2-outputs/Mapping_Light',pattern = '.csv',full.names = T)
  fileMap=paste0('./2-outputs/Mapping_Light/',paramFileName,'_',design_name,'_',orientation,'_',meteoFileName,'.csv')
  dyn=paste0('./2-outputs/Mapping_Light/',paramFileName,'_',design_name,'_',orientation,'_',meteoFileName,'_dyn.csv')
  
  if (!(fileMap %in% existing_files) & !(dyn %in% existing_files)){
    
    print(paste("generating the file:",fileMap))
    
    ###archimed outputs

    
    sim_folder=paste0('./2-outputs/Run_simu/output/',paramFileName,'_',design_name,'_',orientation,'_',meteoFileName)
    
    files_sim= 
      sim_folder%>%
      list.files(recursive = T, full.names = TRUE)
    
    
    
    # ###meteo
    # path_meteo= files_sim[grep("meteo.csv",files_sim)]
    # 
    # # Importing the meteorology file from the first simulation (all are the same):
    # meteo= archimedR::import_meteo(x = file.path(path_meteo[1]))%>%
    #   mutate(date= as.POSIXct(date)+ seconds(hour_start))
    # 
    # 
    # ###conversion factor W.m-2.30mn of GR-->MJ.m-2.day-1 of PAR
    # conMJday=0.48*1800*10**-6
    # 
    # ##"incident PAR in MJ.m-2.day-1
    # PARinc=as.numeric(meteo%>%
    #                     summarize(PARinc=sum(`RI_TIR_f`*conMJday)))
    
    
    
    # Importing the meteorology file from the first simulation (all are the same):
    meteo=data.table::fread(input = paste0('2-outputs/Run_simu/',meteoFileName,'.csv'),skip = 4) %>%
      mutate(step=row_number()-1)
    
    ###conversion factor W.m-2.30mn of GR-->MJ.m-2.day-1 of PAR
    conMJday=1800*10**-6
    
    ##"incident PAR in MJ.m-2.day-1
    PARinc=as.numeric(meteo%>%
                        summarize(PARinc=sum(`Ri_PAR_f`*conMJday)))
    
    
    
    ###import summary outputs
    path_sum= files_sim[grep("summary.csv",files_sim)]
    
    # Importing the meteorology file from the first simulation (all are the same):
    summ=data.table::fread(file =  (path_sum[1]))
    
    stepDuration=unique(summ$`step_duration`)
    
    # Importing the node values (main output):
    path_nodes= files_sim[grep("component_values.csv",files_sim)]
    
    nodes= 
      lapply(path_nodes, function(x){
        name= 
          x%>%dirname()%>%strsplit(split = "/")%>%unlist()%>%
          tail(1)
        data.table::fread(x)%>%mutate(Design= name)
      })%>%data.table::rbindlist()%>%tibble::as_tibble()
    
    ngridT= 
      nodes%>%
      ungroup()%>%
      filter(type=="Cobblestone")%>%
      filter(step_number==step_number[1])%>%
      group_by(Design)%>%
      summarise(ngrid= n(), area_grid= sum(.data$area))%>%
      ungroup()%>%
      mutate(Design=design_name)%>%
      data.frame()
    
    
    
    # Grid index: 
    grid_dim= data.table::fread(paste0(path_designs,design_name,'.csv'))%>%
      mutate(Design=design_name,
             ngrid=ngridT$ngrid,
             area_grid=ngridT$area_grid,
             surf_grid= area_grid/ngrid,
             x_length= xmax-xmin,
             y_length= ymax-ymin,
             grid_n_x= round(x_length/sqrt(surf_grid)),
             grid_n_y= round(y_length/sqrt(surf_grid))
      )%>%
      select(Design,  xmin ,xmax,ymin,ymax,ngrid,area_grid,Design,surf_grid,x_length,y_length ,grid_n_x,grid_n_y)%>%
      distinct()

      
    grid_index= expand.grid(y= (1:grid_dim$grid_n_y)*sqrt(grid_dim$surf_grid),
                            x= (1:grid_dim$grid_n_x)*sqrt(grid_dim$surf_grid))%>%
      mutate(id= 1:nrow(.))%>%
      mutate(Design=grid_dim$Design)
    
    des_=  data.table::fread(paste0(path_designs,design_name,'.csv'))%>%select(x,y)%>%unlist(.)
    trees_positions= des_%>%matrix(nrow = 1)%>%as.data.frame()%>%setNames(names(des_))%>%
      mutate(Design= design_name)
    
    
    
    x=trees_positions$Design
    found= grep(x,grid_dim$Design)
    trees_positions= trees_positions[trees_positions$Design==x,]%>%
      .[rep(1,length(found)),]%>%
      mutate(Design= grid_dim$Design[grep(x,grid_dim$Design)])
    
    ## get 4 trees positions to be generic
    nbT=(length(trees_positions)-1)/2
    trees_positions=trees_positions%>%
      mutate(x3=ifelse(nbT>2,x3,NA),
             x4=ifelse(nbT>3,x4,NA),
             y3=ifelse(nbT>2,y3,NA),
             y4=ifelse(nbT>3,y4,NA))
    
    
    grid_dist= 
      merge(grid_index,trees_positions,by = "Design",all.x=F)%>%
      mutate(dist_tree_1= sqrt((x - x1)^2 + (y - y1)^2),
             dist_tree_2= sqrt((x - x2)^2 + (y - y2)^2),
             dist_tree_3= sqrt((x - x3)^2 + (y - y3)^2),
             dist_tree_4= sqrt((x - x4)^2 + (y - y4)^2),
             dist_tree_x1= abs(x - x1),
             dist_tree_x2= abs(x - x2),
             dist_tree_x3= abs(x - x3),
             dist_tree_x4= abs(x - x4),
             x_tree_1= x1, x_tree_2= x2,
             x_tree_3= x3, x_tree_4= x4,
             y_tree_1=y1,y_tree_2=y2,
             y_tree_3=y3,y_tree_4=y4)%>%
      mutate(dist_tree= pmin(dist_tree_1,dist_tree_2,dist_tree_3,dist_tree_4,na.rm = T),
             dist_tree_x= pmin(dist_tree_x1,dist_tree_x2,dist_tree_x3,dist_tree_x4,na.rm = T))
    
    
    plane_df= 
      nodes%>%
      filter(type=="Cobblestone")%>%
      mutate(Design=design_name)%>%
      mutate(component_id= component_id-1)%>% # id 1 was the scene
      # dplyr::left_join(Area_plots, by= "Design")%>%
      dplyr::left_join(meteo%>%select(date,step,hour_start), by= c("step_number"= "step"))%>%
      dplyr::left_join(grid_dist, by= c("Design","component_id"= "id"))
    
    
    plane_df$Design= as.factor(plane_df$Design)
    
    plane_df=plane_df%>%
      filter(Design==unique(grid_dist$Design))
    
    
    plane_df_step= 
      plane_df%>%
      dplyr::mutate(Date= date,
                    irradiation= Ri_PAR_q*area*step_duration,
                    irradiance= Ri_PAR_q)
    
    # dynamic of ppfd over the day --------------------------------------------
    
    ###coversion factor W.m-2.30mn-->micromol.m-2.s-1 of PAR
    conv0=4.6
    
    meteoHour=meteo%>%
      mutate(PARinc=Ri_PAR_f*conv0)%>%
      mutate(Hour=hms(hour_start))%>%
      select(PARinc,Hour)
    
    grid_df_hour=
      plane_df_step%>%
      mutate(Hour=hms(hour_start))%>%
      group_by(component_id,Hour)%>%
      summarise(
        Intercepted= sum(Ri_PAR_q/area*10**-6),
        dist_tree= mean(dist_tree),
        dist_tree_x= mean(dist_tree_x),
        dist_inter=d_inter,
        dist_intra=d_intra,
        x= unique(x),
        y=unique(y),
        x_tree_1= unique(x_tree_1),
        x_tree_2= unique(x_tree_2),
        x_tree_3= unique(x_tree_3),
        x_tree_4= unique(x_tree_4),
        y_tree_1= unique(y_tree_1),
        y_tree_2= unique(y_tree_2),
        y_tree_3= unique(y_tree_3),
        y_tree_4= unique(y_tree_4))%>%
      ungroup()%>%
      as_tibble()
    
   
    
    

    ####keep a band of crop 3m away from palm trees
    ###coversion factor MJ.m-2.30mn-->micromol.m-2.s-1
    conv=((10**6)*4.6)/(30*60)
    harvest_path=3
    
    don_crop= grid_df_hour%>%
      filter(dist_tree_x>harvest_path)%>%
      group_by(Hour)%>%
      summarise(Intercepted_mean= mean(Intercepted),
                Intercepted_sd= sd(Intercepted),
                dist_intra=unique(dist_intra),
                dist_inter=unique(dist_inter),
                crop_band=paste('rice band = ',as.numeric(dist_inter)-2*harvest_path,'m'))%>%
      ungroup()%>%
      mutate(ppfd_mean=Intercepted_mean*conv,
             ppfd_sd=Intercepted_sd*conv,
             design=paste(dist_intra,'m x',dist_inter,'m'))


    
    ###add %transmission
    
    
    don_crop_fin=merge(don_crop,meteoHour)%>%
      mutate(ppfd_mean_rel=ifelse(PARinc>0,ppfd_mean/PARinc*100,0))
    
    grid_df_day= 
      plane_df_step%>%
      filter(Design %in% unique(plane_df_step$Design))%>%
      group_by(Design,component_id)%>%
      summarise(Date= mean(.data$date),
                Intercepted= sum(Ri_PAR_q/area*10**-6),
                # absEnergy_withScattering_PAR J grid-1
                # Global intercepted radiation in J grid-1 d-1
                # Area_plot= mean(.data$Area_plot),
                dist_tree= mean(dist_tree),
                dist_tree_x= mean(dist_tree_x),
                # density= unique(density),
                x= unique(x),
                y=unique(y),
                x_tree_1= unique(x_tree_1), 
                x_tree_2= unique(x_tree_2),
                x_tree_3= unique(x_tree_3), 
                x_tree_4= unique(x_tree_4),
                y_tree_1= unique(y_tree_1), 
                y_tree_2= unique(y_tree_2),
                y_tree_3= unique(y_tree_3), 
                y_tree_4= unique(y_tree_4))%>%
      ungroup()
    
    
    
    reps=NULL
    
    nbReps=4  
    for (r_x in 0:nbReps){
      for (r_y in 0:nbReps){
        
        # print(paste('r_x:',r_x,' r_y:',r_y))
        
        subRep=grid_df_day%>%
          group_by(Design)%>%
          mutate(xmax=max(x,na.rm=T),
                 ymax=max(y,na.rm=T),
                 x=x+r_x*xmax,
                 y=y+r_y*ymax,
                 x_tree_1=x_tree_1+r_x*xmax,
                 x_tree_2=x_tree_2+r_x*xmax,
                 # x_tree_3=x_tree_3+r_x*xmax,
                 # x_tree_4=x_tree_4+r_x*xmax,
                 y_tree_1=y_tree_1+r_y*ymax,
                 y_tree_2=y_tree_2+r_y*ymax
                 # y_tree_3=y_tree_3+r_y*ymax,
                 # y_tree_4=y_tree_4+r_y*ymax
                 )%>%
          ungroup()%>%
          select(colnames(grid_df_day))
        
        reps=rbind(reps,subRep)
      }
    }
    
    
    
    grid_fin=reps%>%
      mutate(Intercepted_rel=Intercepted/PARinc*100)

    
    
    ### export
    
    grid= data.table::fread(paste0(path_designs,design_name,'.csv'))
    density=round(nrow(grid)*10000/(unique(grid$xmax)*unique(grid$ymax)))
    
    
    grid_fin=grid_fin%>%
      mutate(density=density,
             d_inter=d_inter,
             d_intra=d_intra,
             meteoFileName=meteoFileName)
    
    don_crop_fin=don_crop_fin%>%
      mutate(Hour=as.character(Hour), ## format to export in character
             density=density,
             d_inter=d_inter,
             d_intra=d_intra,
             meteoFileName=meteoFileName)
    
    
    ### save the map
    data.table::fwrite(x = grid_fin,file =fileMap)
    data.table::fwrite(x = don_crop_fin,file =dyn)
    
    
    grid_fin%>%
      mutate(density=as.numeric(density))%>%
      arrange(density)%>%
      # filter(x<=lims[1] & y<=lims[2])%>%
      ggplot(aes(x=x, y=y,fill=Intercepted_rel,col=Intercepted_rel))+
      geom_point(pch=22)+
      geom_point(aes(x=x_tree_1,y=y_tree_1),col=2,pch=0)+
      geom_point(aes(x=x_tree_2,y=y_tree_2),col=2,pch=0)+
      facet_wrap(~paste(sprintf("%03d",density),'plant.ha-2')) +
      coord_fixed()+
      labs(x = 'x (m)', y = 'y (m)',fill= '%',col='%') +
      # theme(legend.position="bottom")+
      scale_fill_viridis()+
      scale_color_viridis()+
      theme(legend.direction = 'horizontal',legend.position = c(0.83,0.12))

  }

    if (fileMap %in% existing_files & dyn %in% existing_files){
    print(paste('loading existing file',fileMap))
    grid_fin=data.table::fread(file =fileMap,dec = '.',sep = ',')
    don_crop_fin=data.table::fread(file =dyn,dec = '.',sep = ',')
  }
  
  
  list(grid_fin=grid_fin,don_crop_fin=don_crop_fin)
  
  
  # plot0=grid_fin%>%
  #   ggplot(aes(x=x, y=y,fill=Intercepted,col=Intercepted))+
  #   geom_point(pch=22)+
  #   geom_point(aes(x=x_tree_1,y=y_tree_1),pch=8,col='green',size=2)+
  #   geom_point(aes(x=x_tree_2,y=y_tree_2),pch=8,col='green',size=2)+
  #   # geom_point(aes(x=x_tree_3,y=y_tree_3),pch=8,col=2,size=2)+
  #   # geom_point(aes(x=x_tree_4,y=y_tree_4),pch=8,col=2,size=2)+
  #   # coord_fixed()+
  #   ylim(low= 0, high= min(c(max(grid_fin$y),max(grid_fin$x))))+
  #   xlim(low= 0, high=min(c(max(grid_fin$y),max(grid_fin$x))))+
  #   # labs(x = 'x (m)', y = 'y (m)',fill= expression (MJ*' '*m**-2*' '*day**-1 ),col=expression (MJ*' '*m**-2*' '*day**-1 )) +
  #   # labs(x = 'x (m)', y = 'y (m)',fill= expression ('Transmitted light'),col=expression ('Transmitted light ')) +
  #   scale_fill_viridis(option = 'plasma')+
  #   scale_color_viridis(option = 'plasma')+
  #   ggtitle(paste("Inter row:",d_inter,'m','- Intra row:',d_intra,'m -',orientation),
  #           paste(paramFileName,' density:',density,'palms ha-1'))
  # 
  # plot=grid_fin%>%
  #   mutate(Intercepted_rel=ifelse(Intercepted_rel>100,100,Intercepted_rel))%>%
  #   ggplot(aes(x=x, y=y,fill=Intercepted_rel,col=Intercepted_rel))+
  #   geom_point(pch=22)+
  #   geom_point(aes(x=x_tree_1,y=y_tree_1),pch=8,col='green',size=2)+
  #   geom_point(aes(x=x_tree_2,y=y_tree_2),pch=8,col='green',size=2)+
  #   # geom_point(aes(x=x_tree_3,y=y_tree_3),pch=8,col=2,size=2)+
  #   # geom_point(aes(x=x_tree_4,y=y_tree_4),pch=8,col=2,size=2)+
  #   # coord_fixed()+
  #   ylim(low= 0, high= min(c(max(grid_fin$y),max(grid_fin$x))))+
  #   xlim(low= 0, high=min(c(max(grid_fin$y),max(grid_fin$x))))+
  #   # labs(x = 'x (m)', y = 'y (m)',fill= expression (MJ*' '*m**-2*' '*day**-1 ),col=expression (MJ*' '*m**-2*' '*day**-1 )) +
  #   labs(x = 'x (m)', y = 'y (m)',fill= expression ('Transmitted light (%)'),col=expression ('Transmitted light (%)')) +
  #   scale_fill_viridis(option = 'plasma',limits = c(0, 100))+
  #   scale_color_viridis(option = 'plasma',limits = c(0, 100))+
  #   ggtitle(paste("Inter row:",d_inter,'m','- Intra row:',d_intra,'m -',orientation),
  #           paste(paramFileName,' density:',density,'palms ha-1'))
  # 
  # 
  # plot2=grid_fin%>%
  #   group_by(x)%>%
  #   summarize(Intercepted_rel=mean(Intercepted_rel),
  #             x_tree_1=mean(x_tree_1),
  #             x_tree_2=mean(x_tree_2))%>%
  #   ungroup()%>%
  #   ggplot(aes(x=x, y=Intercepted_rel,col=Intercepted_rel))+
  #   geom_line()+
  #   geom_vline(aes(xintercept = x_tree_1),col='green')+
  #   geom_vline(aes(xintercept = x_tree_2),col='green')+
  #   scale_color_viridis(option = 'plasma',limits = c(0, 100))+
  #   xlim(low= 0, high=min(c(max(grid_fin$y),max(grid_fin$x))))+
  #   # ggtitle(paste("Inter row:",d_inter,'m','- Intra row:',d_intra,'m -',orientation),
  #   #         paste('density:',density,'palms ha-1'))+
  # labs(x = 'x (m)', y =  expression ('Transmitted light (%)'),col=expression ('Transmitted light (%)'))
  # 
  # 
  # list(plot0=plot0,plot=plot,plot2=plot2)
  
 

}
