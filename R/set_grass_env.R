#' Function initializes a temporary GRASS-GIS environment, sets the region
#' parameters to match those of user provided DEM, writes input
#' raster to the GRASS environment. 
#'
#' This function relies on 'rgrass7' to initialize and populate a 
#' temporary GRASS-GIS environment. The user must have GRASS-GIS
#' binaries installed, and provide the path to these, e.g., "/usr/lib/grass78".
#' User must provide a DEM, which will be used for subsequent processing,
#' the GRASS environment will be set to match the DEM extent and CRS.
#' A list of 'raster' objects representing input layers must be 
#' provided, as each layer will be written to the GRASS environment. 
#' For each input raster, a character vector of desired raster 
#' layer names must be provided. Both a accumulation ('acc') and drainage 
#' direction ('dir') raster are computed. A stream network ('stream_r') is derived and
#' terrain derivatives are generated using 'r.watershed'. In addition, 
#' strahler stream order ('strahler') is computed using 'r.stream.order'. For systems 
#' with limited resources, the 'seg' parameter can be set to 'T" with a maximum 
#' RAM memory MB defined by 'memory_mb'.
#'
#' @param gisBase The directory path to GRASS binaries and libraries, 
#' containing bin and lib sub directories among others, see rgrass7::initGRASS
#' @param DEM A digital elevation model as a 'raster' object with an 
#' extent encompassing all pour points of interest. Note, all layers
#' must match the CRS of this layer. 
#' @param lyr_list A list of 'raster' objects representing the input 
#' raster layers that will be written to the GRASS environment. 
#' @param lyr_names A character vector corresponding to 'lyr_list' defining
#' the names of each input raster layer.
#' @param acc_thresh The accumulation threshold to be passed to 'r.stream.extract'.
#' @param seg Should data be segmented to disk to save RAM on resource limited systems,
#' defaults to FALSE. 
#' @param memory_mb Maximum memory in MB to allocate if 'seg' is equal to TRUE.
#' @param convergence `integer` Convergence factor fo MFD (1-10) (Default 5) 
#' @return NULL, only initializes and populates a GRASS-GIS environment, see description.  
#' @export
set_grass_env<-function(gisbase,DEM,lyr_list,lyr_names,acc_thresh,seg=F,memory_mb=NULL,convergence=5)
{
  #Set up temporary GRASS Environment, must provide gisBase, e.g., gisBase="/usr/lib/grass78"#
  cat("Initializing GRASS ...")
  rgrass7::initGRASS(gisBase = gisbase,
                     home=tempdir(),
                     override = T,
                     remove_GISRC=T,
                     mapset = 'PERMANENT')
  
  #Use sp
  rgrass7::use_sp()
  
  #write DEM to GRASS env. 
  cat("Writing DEM to GRASS env ...")
  rgrass7::writeRAST(as(DEM,"SpatialGridDataFrame"),
                     'dem',
                     ignore.stderr = T,
                     overwrite = T)
  
  rgrass7::execGRASS('g.proj',
                     parameters = list(georef=DEM@file@name),
                     flags = c('t','c'))
  
  #Set grass region parameters to 'dem' layer 
  rgrass7::execGRASS('g.region',
                     parameters = list(raster='dem'),
                     flags = c('d'))
  
  #Write each disturbance layer to GRASS environment 
  for(lyr in c(1:length(lyr_list)))
  {
    cat("Writing layer to GRASS env ...")
    rgrass7::writeRAST(as(lyr_list[[lyr]],"SpatialGridDataFrame"),
                       lyr_names[lyr],
                       ignore.stderr = T,
                       overwrite = T)
  }
  
  #Run r.watershed in GRASS to create DEM derivatives and stream network, 31 MB of RAM for 1 million cells
  if(seg==F)
  {
    cat("Extracting streams from DEM, generating GRASS derivatives ...")
    rgrass7::execGRASS("r.watershed",
                       parameters = list(elevation='dem',
                                         threshold=2,
                                         accumulation='acc',
                                         tci='topo_idx',
                                         spi='strm_pow',
                                         drainage='dir',
                                         stream='stream_r',
                                         length_slope='slope_lngth',
                                         slope_steepness='steepness',
                                         convergence=convergence,
                                         threshold=acc_thresh),
                       flags = c('overwrite','quiet','a'))
    
    rgrass7::execGRASS('r.stream.order',
                       parameters = list(stream_rast='stream_r',
                                         direction='dir',
                                         strahler='strahler'),
                       flags = c('overwrite'))
    
  }else{
    cat("Extracting streams from DEM, generating GRASS derivatives ...")
    rgrass7::execGRASS("r.watershed",
                       parameters = list(elevation='dem',
                                         threshold=2,
                                         accumulation='acc',
                                         tci='topo_idx',
                                         spi='strm_pow',
                                         drainage='dir',
                                         stream='stream_r',
                                         length_slope='slope_lngth',
                                         slope_steepness='steepness',
                                         memory=memory_mb,
                                         convergence=convergence,
                                         threshold=acc_thresh),
                       flags = c('overwrite','quiet','a','m'))
    
    rgrass7::execGRASS('r.stream.order',
                       parameters = list(stream_rast='stream_r',
                                         direction='dir',
                                         strahler='strahler',
                                         memory=memory_mb),
                       flags = c('overwrite','m'))
    
  }
  cat("GRASS-GIS env initialized and populated ...")
}
