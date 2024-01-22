packs <- c("lubridate", "stringr", 'tidyverse','viridis','Vpalmr','data.table','yaml','archimedR')
InstIfNec<-function (pack) {
  if (!do.call(require,as.list(pack))) {
    do.call(install.packages,as.list(pack))  }
  do.call(require,as.list(pack)) }
lapply(packs, InstIfNec)

# DESIGN ------------------------------------------------------------------


#' Get quinconx design for Archimed scene
#'
#' @param dist_inter : inter row distance (m)
#' @param dist_intra : intra row distance (m)
#'
#' @return 
#' @export
#'
#' @examples
design=function(dist_inter= NULL, dist_intra= NULL){
  
  # Voronoi of the quincunx design:
  voronoi_plot= data.frame(x= c(dist_inter/2,dist_inter/2+dist_inter),
                           y= c(dist_intra/4,dist_intra/4+dist_intra/2),
                           xmin= c(0,0), xmax= rep(dist_inter*2,2),
                           ymin= c(0,0), ymax= rep(dist_intra,2))
  
  
  # number of raow and columns in 1 ha
  nbRow=ceiling(100/dist_inter)
  nbCol=ceiling(100/dist_intra)
  
  # Matrix of the design (each cell is a Voronoi):
  mat_plot= expand.grid(nbRow= 1:nbRow, nbCol= 1:nbCol)
  
  # density
  density=floor(nrow(voronoi_plot)/(unique(voronoi_plot$xmax)*unique(voronoi_plot$ymax))*10000)
  
  
  # Full design:
  design=
    mapply(function(nbRow,nbCol){
      voronoi_plot%>%
        dplyr::select(x,y,xmax,ymax,xmin,ymin)%>%
        dplyr::mutate(xmin= xmax*(nbRow-1), ymin= ymax*(nbCol-1),
                      x= x+xmin, y= y+ymin,
                      xmax= xmax*nbCol, ymax= ymax*nbRow,
                      nbCol= nbCol, nbRow= nbRow)
    }, nbRow= mat_plot$nbRow, nbCol= mat_plot$nbCol)%>%t()%>%
    dplyr::as_tibble()%>%
    tidyr::unnest(cols = c(x, y, xmax, ymax, xmin, ymin, nbCol, nbRow))%>%
    dplyr::mutate(xmax= max(xmax), ymax= max(ymax),
                  xmin= min(xmin), ymin= min(ymin))
  
  # plot:
  plot_bounds=
    design%>%
    ggplot2::ggplot(ggplot2::aes(x= x, y= y))+
    ggplot2::geom_point(shape=8)+
    ggplot2::ylim(low= 0, high= 100)+
    ggplot2::xlim(low= 0, high=100)+
    ylab('Intra row distance (m)')+
    xlab('Inter row distance (m)')+
    ggtitle(paste(density,' plants.ha-1'))
  
  # result to export scene pattern
  result=
    voronoi_plot%>%
    dplyr::mutate(z= 0.0, scale= 1.0,
                  inclinationAzimut= 0.0, inclinationAngle= 0.0,
                  stemTwist= 0.0)
  
  list(design=design,result= result, plot= plot_bounds)
  
}



# CREATE OPS --------------------------------------------------------------


#' Function to generate ops
#'
#' @param opfname name of the opf to call 
#' @param dist_inter cf design
#' @param dist_intra cf design
#'
#' @return ops file
#' @export
#'
#' @examples
create.ops=function(opfname='opfname',dist_inter= NULL, dist_intra= NULL,writeOPS=T,pathOPS=NULL){
  
  
  ### generate the design
  
  des=design(dist_inter =dist_inter, dist_intra=dist_intra)$result
  
  
  xmin=unique(des$xmin)
  xmax=unique(des$xmax)
  ymin=unique(des$ymin)
  ymax=unique(des$ymax)
  zmin=0
  
  
  ###generate ligne of config file for each tree
  
  table=NULL
  for (t in 1:nrow(des)){
    tableSub=paste(1,t,paste('opf/',opfname,'.opf',sep=''),des$x[t],des$y[t],des$z[t],des$scale[t],des$inclinationAzimut [t],des$inclinationAngle[t],des$stemTwist[t],sep='	')
    table=rbind(table,tableSub)
  }
  
  if (nrow(des)==2){
    ops.file=c(
      paste('# T xmin ymin zmin xmax ymax flat'),
      paste('T', xmin, ymin, zmin, xmax,ymax,'flat'),
      paste('#[Archimed] elaeis'),
      paste('#sceneId objectId FilePath x y z scale inclinationAzimut inclinationAngle rotation'),
      paste(table[1,]),
      paste(table[2,],collapse=' ')
    )
  }
  
  if (nrow(des)==3){
    ops.file=c(
      paste('# T xmin ymin zmin xmax ymax flat'),
      paste('T', xmin, ymin, zmin, xmax,ymax,'flat'),
      paste('#[Archimed] elaeis'),
      paste('#sceneId objectId FilePath x y z scale inclinationAzimut inclinationAngle rotation'),
      paste(table[1,]),
      paste(table[2,],collapse=' '),
      paste(table[3,],collapse=' ')
    )
  }
  if (nrow(des)==4){
    ops.file=c(
      paste('# T xOrigin yOrigin zOrigin xSize ySize flat'),
      paste('T', xmin, ymin, zmin, xmax,ymax,'flat'),
      paste('#[Archimed] elaeis'),
      paste('#sceneId plantId plantFileName x y z scale inclinationAzimut inclinationAngle stemTwist'),
      paste(table[1,]),
      paste(table[2,],collapse=' '),
      paste(table[3,],collapse=' '),
      paste(table[4,],collapse=' ')
    )
  }
  
  if (nrow(des)>4){
    print('more than 4 opfs are not implemented')
    return(NULL)
  }
  if (writeOPS==T){
    write(ops.file,file= paste0(pathOPS,'/',opfname,'_inter',dist_inter,'-intra',dist_intra,'.ops')) ###wrtie the ops
  }
  return(ops.file)
}



# RUN SIMU ----------------------------------------------------------------

#' Run VPalm and Archimed simulation
#'@param d_inter: distance between rows of palm trees
#'#'@param d_intra: distance within rows of palm trees
#' @param  path_designs path to save planting designs
#' @param  paramFileName name of paramerter or opf name
#' @param  meteoFileName name of the meteoFile
#' @param pathArchimed path to archimed jar
#' @param pathVpalmJar path to Vpalm Jar
#' @param pathOpf Path where opf is written
#' @param pathOPS path where ops is written
#' @param run_photosynthesis run phototsynthesis (T or F)
#' @param opfStepExport step of simulation when the opf is exported (numeric 0-28)
#' @param overwrite overwrite existing simulation
#' @param orientation orientation of the scene (NS: North-South or EW: East-West)
#'
#' @return
#' @export
#'
#' @examples
RunSimu=function(d_inter=d_inter,d_intra=d_intra,paramFileName=paramFileName,meteoFileName=meteoFileName,pathArchimed=pathArchimed,path_designs=path_designs,pathVpalmJar=pathVpalmJar,pathOpf=pathOpf,pathOPS=pathOPS,run_photosynthesis=T,opfStepExport=14,overwrite=F,orientation='NS'){
  
  ### debug
  # paramFileName ='DA1_Average_MAP_90'
  # pathArchimed='./1-code/archimed-phys.jar'
  # pathOPS= './2-outputs/Run_simu/ops/'
  #  d_intra=7
  # d_inter=12
  # overwrite=F
  # run_photosynthesis=F
  # orientation='NS'
  # opfStepExport=14
  
  # generate ops ------------------------------------------------------------
  print('writting ops')
  create.ops(opfname =paramFileName ,dist_inter = d_inter,dist_intra = d_intra,writeOPS = T,pathOPS = pathOPS)
 
   ## save planting design
  print('saving planting design')
  design_name=paste0('inter',d_inter,'-intra',d_intra,'.csv')
  data.table::fwrite(x=design(dist_inter =d_inter,dist_intra =d_intra)$result,file = paste0(path_designs,design_name))
  
  
    # generate config file ----------------------------------------------------
    
    ##"load template
    
    configYml=read_yaml(file =  '0-data/Archimed_inputs/config_template.yml')
  
    ## change names & dirs

    opsName=paste0(paramFileName,'_inter',d_inter,'-intra',d_intra,'.ops')
    configYml$scene=paste0('ops/',opsName)
    configYml$simulation_directory=paste0(str_remove(string = opsName,pattern = '.ops'),'_',orientation,'_',meteoFileName)
    configYml$export_ops=as.character(round(opfStepExport))
    configYml$meteo=paste0(meteoFileName,'.csv')
    
    if (!(orientation %in% c('NS','EW'))){
      print('orientation must be NS or EW')
    }
    
    if (orientation=='EW'){
      configYml$scene_rotation=90
    }
    
    
    if (run_photosynthesis==T){
      # Absorbed light
      configYml$component_variables$Ra_PAR_0_f=T
      configYml$component_variables$Ra_NIR_0_f=T
      configYml$component_variables$Ra_PAR_0_q=T
      configYml$component_variables$Ra_NIR_0_q=T
      configYml$component_variables$Ra_PAR_f=T
      configYml$component_variables$Ra_NIR_f=T
      configYml$component_variables$Ra_TIR_f=T
      configYml$component_variables$Ra_PAR_q=T
      configYml$component_variables$Ra_NIR_q=T
      configYml$component_variables$Ra_TIR_q=T
      # Assimilation
      configYml$component_variables$An_f=T
      configYml$component_variables$An_q=T
      configYml$component_variables$Gs=T
      # Energy
      configYml$component_variables$H_q=T
      configYml$component_variables$H_f=T
      configYml$component_variables$LE_q=T
      configYml$component_variables$LE_f=T
      configYml$component_variables$Tr_f=T
      configYml$component_variables$Tr_q=T
      configYml$component_variables$T=T
    }
    
    pathConfig=paste0('2-outputs/Run_simu/',str_replace(string = opsName,pattern = '.ops',replacement = paste0('_',orientation,'_',meteoFileName,'.yml'))) 
    
    yaml::write_yaml(x =  configYml,file =pathConfig)
    
    debut=Sys.time()
    system(command =paste0('java -jar ',pathArchimed,' ',pathConfig))
    
    print(paste("compute time :",difftime(time1 = Sys.time(),time2 = debut,units = "mins"),'mins'))
  
}
