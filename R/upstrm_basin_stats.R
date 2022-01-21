


hydro_condition_dem <- function(dem,outdir,env,minslope=0.1)
{
  
  #Load grid into SAGA format 
  RSAGA::rsaga.import.gdal(in.grid = dem,
                           out.grid = file.path(outdir,'DEM'),
                           show.output.on.console=FALSE,
                           env=env,
                           warn=FALSE,
                           flags=c('s'))
  
  dem<-file.path(outdir,'DEM.sgrd')
  
  
  
  RSAGA::rsaga.geoprocessor(lib='ta_preprocessor',
                            module = 5,
                            param = list(ELEV=dem,
                                         FILLED=file.path(outdir,'FILLED'),
                                         MINSLOPE=minslope),
                            show.output.on.console=FALSE,
                            env=env,
                            warn=FALSE,
                            flags=c('s'))
  
  
  return(file.path(outdir,'FILLED.sgrd'))
}



gen_upstream_basin <-function(dem,x_coord,y_coord,method=2,converge=1.1,outdir,basin_id,env)
{
  
  RSAGA::rsaga.geoprocessor(lib='ta_hydrology',
                            module=4,
                            param=list(TARGET_PT_X=x_coord,
                                       TARGET_PT_Y=y_coord,
                                       ELEVATION=dem,
                                       AREA=file.path(outdir,paste('BASIN',basin_id,sep='')),
                                       METHOD=method,
                                       CONVERGE=converge),
                            show.output.on.console=FALSE,
                            env=env,
                            warn=FALSE)
  
  RSAGA::rsaga.geoprocessor(lib='grid_calculus',
                            module=1,
                            param =list(FORMULA='g1 > 50.0',
                                        GRIDS=file.path(outdir,paste('BASIN',basin_id,'.sgrd',sep='')),
                                        RESULT=file.path(outdir,paste('BASIN_BNRY_',basin_id,sep='')),
                                        TYPE=0),
                            show.output.on.console=FALSE,
                            env=env,
                            warn=FALSE)
  
  return(file.path(outdir,paste0('BASIN_BNRY_',basin_id,'.sgrd')))
  
}

get_basin_stats<-function(zone_rast,catlist=NULL,statlist,outdir,basin_id,env)
{
  outtab<-file.path(outdir,paste0('basinstats_',basin_id))
  
  if(!is.null(catlist))
  {
    RSAGA::rsaga.geoprocessor(lib='statistics_grid',
                              module = 5,
                              param = list(ZONES=zone_rast,
                                           CATLIST=catlist,
                                           STATLIST=statlist,
                                           OUTTAB=outtab),
                              show.output.on.console=F,
                              env=env,
                              warn=FALSE)
    
  }else{
    RSAGA::rsaga.geoprocessor(lib='statistics_grid',
                              module = 5,
                              param = list(ZONES=zone_rast,
                                           STATLIST=statlist,
                                           OUTTAB=outtab),
                              show.output.on.console=F,
                              env=env,
                              warn=FALSE)
  }
  
  return(read.delim(outtab))
  
}

env<-RSAGA::rsaga.env(parallel = T)


hcd<-hydro_condition_dem(dem='/home/hunter/R/x86_64-pc-linux-gnu-library/4.0/wetlandmapR/extdata/DEM.tif',
                         outdir=tempdir(),
                         env=env)



# library(doParallel)
# 
# cl<-parallel::makeCluster(32)
# 
# doParallel::registerDoParallel(cl)
# 
# 
# yup<-foreach(i = 1:100,.packages = 'RSAGA') %dopar%
# {
# gub<-gen_upstream_basin(dem=hcd,
#                    x_coord = 1246640,
#                    y_coord = 1080390,
#                    outdir = tempdir(),
#                    basin_id = i,
#                    env=env)
# 
# 
# 
# 
# hm<-get_basin_stats(zone_rast=gub,
#                     statlist=paste0(hcd,";","/tmp/RtmplQ7zOE/DEM.sgrd"),
#                     outdir=tempdir(),
#                     basin_id = i,
#                     env=env)
# 
# 
# }
#stopCluster(cl)




