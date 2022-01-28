

#' Provided a DEM function will apply hydrologic pre-processing outputting a 
#' hydrologic conditioned DEM, free of sinks, using methods defined in Wang, L. & H. Liu (2006).
#'
#' @param dem (character) Filepath to input digital elevation model (DEM). 
#' @param outdir (character) Filepath for output hydrologic conditioned DEM, will be named 'FILLED.sgrd'
#' @param env A RSAGA environment,i.e., RSAGA::rsaga.env()
#' @param minslope (float) Minimum slope gradient (degrees) to preserve from cell to cell; with a value of zero
#' sinks are filled up to the spill elevation (which results in flat areas).
#' @return (character) File path to FILLED.sgrd. 
#' @export
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

#' Provided a digital elevation model (DEM), X-Y coordinates (in map units), using RSAGA function uses flow tracing 
#' algorithms to determine the upstream contributing area. Function returns path to binary grid defining
#' the upstream catchment for the provided coordinates. 
#' 
#' @param dem (character) Filepath to digital elevation model, assumed to be hydrologic conditioned.  
#' @param x_coord (flaot) The X coordinate in map units, must be withing extent of DEM.  
#' @param y_coord (flaot) The Y coordinate in map units, must be withing extent of DEM.
#' @param method (integer) [0] Deterministic 8; [1] Deterministic Infinity;[2] Multiple Flow Direction (default)
#' @param converge (flaot) Convergence factor for Multiple Flow Direction algorithm (default 1.1)
#' @param outdir (character) Filepath for binary grid output.
#' @param basin_id (integer) A unique basin identifier, will be appended to output binary grid name.
#' @param env A RSAGA environment,i.e., RSAGA::rsaga.env()
#' @return (character) Filepath to output binary upstream catchment grid, will match extent of DEM.  
#' @export
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
                            warn=FALSE,
                            flags=c('s'))
  
  RSAGA::rsaga.geoprocessor(lib='grid_calculus',
                            module=1,
                            param =list(FORMULA='g1 > 50.0',
                                        GRIDS=file.path(outdir,paste('BASIN',basin_id,'.sgrd',sep='')),
                                        RESULT=file.path(outdir,paste('BASIN_BNRY_',basin_id,sep='')),
                                        TYPE=0),
                            show.output.on.console=FALSE,
                            env=env,
                            warn=FALSE,
                            flags=c('s'))
  
  return(file.path(outdir,paste0('BASIN_BNRY_',basin_id,'.sgrd')))
  
}


#' Provided a zonal raster and input layers, using RSAGA this function will return a table
#' of zonal statistics as a data.frame.
#'  
#' @param zone_rast (character) Filepath to SAGA grind defining the zones to analyse, acts as a mask,i.e., output from 'gen_upstream_basin'.
#' @param catlist (character; optional) Filepath(s) (Seperated by ';') used to delineate unique condition units (UCUs). (default NULL).
#' @param statlist (character) Filepath(s) (Seperated by ';') to SAGA grids with continuous data, statistics are calculated for each grid.
#' @param outdir (character) Filepath to location for SAGA statistics table to be written. 
#' @param basin_id (integer) A unique basin identifier, will be appended to output statistics table name.
#' @param env A RSAGA environment,i.e., RSAGA::rsaga.env()    
#' @return A data.frame of statistics for each input grid, by zone. 
#' @export
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
                              warn=FALSE,
                              flags=c('s'))
    
  }else{
    RSAGA::rsaga.geoprocessor(lib='statistics_grid',
                              module = 5,
                              param = list(ZONES=zone_rast,
                                           STATLIST=statlist,
                                           OUTTAB=outtab),
                              show.output.on.console=F,
                              env=env,
                              warn=FALSE,
                              flags=c('s'))
  }
  
  return(read.delim(outtab))
  
}



#' Provided a DEM (or any raster layer), function returns the XY location
#' of the minimum (or maximum) raster value within a provided polygon area
#' of interest.
#'
#' @param lyr Typically a digital elevation model, but can be any layer of type 'raster' or 'stars'.
#' @param poly An sf polygon object representing the area of interest in which to return the location 
#' of the minimum (or maximum) raster value. 
#' @param low Should the location of the  lowest or highest raster value be returned, defaults to true. 
#' @return A numeric vector with he XY coordinated of the highest or lowest raster value in map units. 
#' @export
get_low_high_loc<-function(lyr,poly,low=TRUE)
{
  if(class(lyr)==class(rast()))
  {
    lyr <- stars::st_as_stars(lyr)
  }
  
  masked <- as.data.frame(lyr[poly])
  
  masked <- masked[complete.cases(masked),]
  
  if(low==T)
  {
    xy<-as.vector(masked[which.min(masked[,3]),c('x','y')])
  }else{
    xy<-as.vector(masked[which.max(masked[,3]),c('x','y')])
  }
  
  return(xy)
}




# env<-RSAGA::rsaga.env(parallel = T)
# 
# 
# hcd<-hydro_condition_dem(dem='/home/hunter/R/x86_64-pc-linux-gnu-library/4.0/wetlandmapR/extdata/DEM.tif',
#                          outdir=tempdir(),
#                          env=env)
# 
# 
# 
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
# stopCluster(cl)
# 



