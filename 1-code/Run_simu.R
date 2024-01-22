# Load packages -----------------------------------------------------------
# install.packages("devtools")
# devtools::install_github("VEZY/Vpalmr")
packs <- c("lubridate", "stringr", 'tidyverse','viridis','Vpalmr','data.table','yaml')
InstIfNec<-function (pack) {
  if (!do.call(require,as.list(pack))) {
    do.call(install.packages,as.list(pack))  }
  do.call(require,as.list(pack)) }
lapply(packs, InstIfNec)

source('./1-code/helpers_archimed.R')
# source('./1-code/Mapping_light_V2.R')


# inputs ------------------------------------------------------------------

paramFileName='DA1_Average_MAP_90'
d_inter=9
d_intra=8
path_designs='./2-outputs/Run_simu/planting_designs/'
# pathVpalmParam='./2-outputs/Generate_VPalm_param/'
pathArchimed='./1-code/archimed-phys.jar'
# pathVpalmJar='./1-code/vpalm_biomech.jar'
pathOpf='./2-outputs/Run_simu/ops/opf/'
pathOPS='./2-outputs/Run_simu/ops/'
opfStepExport=14
orientation='NS'
# meteoFileName='meteo20march'
meteoFileName='meteo21february'


RunSimu(d_inter=d_inter,d_intra=d_intra,paramFileName=paramFileName,pathArchimed=pathArchimed,meteoFileName = meteoFileName,path_designs=path_designs,pathVpalmJar=pathVpalmJar,pathOpf=pathOpf,pathOPS=pathOPS,run_photosynthesis=T,opfStepExport=14,overwrite=T,orientation=orientation)

# for (d_inter in c(9,10,12,14)){
#   for (d_intra in c(7,8)){
# RunSimu(d_inter=d_inter,d_intra=d_intra,paramFileName=paramFileName,pathArchimed=pathArchimed,meteoFileName = meteoFileName,path_designs=path_designs,pathVpalmJar=pathVpalmJar,pathOpf=pathOpf,pathOPS=pathOPS,run_photosynthesis=T,opfStepExport=14,overwrite=T,orientation=orientation)
#   }
# }
#### map
# 
# Create_map(d_inter =12 ,d_intra = 7,path_designs = path_designs,paramFileName=paramFileName,orientation = orientation,meteoFileName = meteoFileName)


