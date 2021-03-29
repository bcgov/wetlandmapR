#' @title gdal_polygonizeR
#' @description R Wrapper for the gdal_polygonize.py python script (http://www.gdal.org/gdal_polygonize.html)
#' This utility creates vector polygons for all connected regions of pixels in the raster sharing
#' a common pixel value. Each polygon is created with an attribute indicating the pixel value of
#' that polygon. Can be userful for example to create fishnet polygons (see "create_fishnet")
#'
#' @param x          `character`filename of a raster file, or "R" raster object
#' @param outshape   `character`filename of the desired output polygon shapefile
#' @param gdalformat  defaults to ESRI - don't change
#' @param pypath     `character` path of python  - if `NULL` (the default) the script tries to
#' automatically retrieve it
#' @param quiet      `logical` if TRUE, limit the messages (defaults to TRUE)
#' @param overwrite  `logical` If TRUE overwrite shapefile if already existing (defaults to FALSE)
#' @param python3 `logical` If TRUE uses python3 instead of python2.x (defaults to TRUE)
#' @return A sf polygon object.
#' @export
#' @importFrom raster writeRaster
#' @importFrom methods is
#' @author Original code by Jonh Baumgartner
#' [https://johnbaumgartner.wordpress.com/2012/07/26/getting-rasters-into-shape-from-r/](), with
#' slight modifications by L.Busetto & H.Gleason 
gdal_polygonizeR <- function(x, outshape=NULL, gdalformat = 'ESRI Shapefile', 
                             pypath=NULL, quiet=TRUE,python3=T) {
  if (is.null(pypath)) {
    pypath <- Sys.which('gdal_polygonize.py')
  }
  if (!file.exists(pypath)) stop("Can't find gdal_polygonize.py on your system.") 
  owd <- getwd()
  on.exit(setwd(owd))
  setwd(dirname(pypath))
  if (!is.null(outshape)) {
    outshape <- sub('\\.shp$', '', outshape)
    f.exists <- file.exists(paste(outshape, c('shp', 'shx', 'dbf'), sep='.'))
    if (any(f.exists)) 
      stop(sprintf('File already exists: %s', 
                   toString(paste(outshape, c('shp', 'shx', 'dbf'), 
                                  sep='.')[f.exists])), call.=FALSE)
  } else outshape <- tempfile()
  if (is(x, 'Raster')) {
    require(raster)
    writeRaster(x, {f <- tempfile(fileext='.asc')})
    rastpath <- normalizePath(f)
  } else if (is.character(x)) {
    rastpath <- normalizePath(x)
  } else stop('x must be a file path (character string), or a Raster object.')
  if(python3==T)
  {
    system2('python3', args=(sprintf('"%1$s" "%2$s" -f "%3$s" "%4$s.shp"', pypath, rastpath, gdalformat,outshape)))
  }else{
    system2('python', args=(sprintf('"%1$s" "%2$s" -f "%3$s" "%4$s.shp"', pypath, rastpath, gdalformat,outshape)))
  }
  
  shp <- sf::read_sf(paste0(outshape,".shp"))
  
  return(shp) 
}





#' Polygonize a classified raster layer, dropping crumbs and filling holes. 
#'
#' Must provide a classified raster, minimum area for which polygons should
#' be dropped (meteres sqaured), and maximum area for which polygon holes
#' should be filled. Optionally, polygons may be simplified using \code{\link{smoothr}} 
#' function, a smoothing method must be specified. 
#'
#' @param r Classified Raster or STARS object.
#' @param min_area_drop_m2 Numeric. Minimum area in square meters for which polygons will be dropped
#' @param max_area_fill_m2 Numeric. Maximum area in square meters of internal polygon holes for which
#' holes will be remove. 
#' @param smooth Boolean. Should the resulting polygons geometry be simplified. 
#' @param smooth_method String. Smoothing method to pass to \code{\link{smoothr}},
#' smooth must be TRUE.
#' @param workers Integer, number of threads to apply for parallel processing. 
#' @param ... Any arguments to be passed to gdal_polgonizeR  
#'
#' @return Polygonized sf object representing the input raster categories. 
#'
#' @examples
#' \dontrun{
#'#Create a classified *stars* object 
#'r <- stars::read_stars('CategoryPredictionMap.tif')
#'
#'#Polygonize classified raster
#'p <- raster_to_clean_polygon(r,2500,2500,TRUE,'chaikin')
#' }
#' @export
raster_to_clean_polygon<-function(r,min_area_drop_m2,max_area_fill_m2,smooth,smooth_method,workers=1,...)
{
  
  max_area_fill_m2<-units::set_units(max_area_fill_m2, "m2")
  min_area_drop_m2<-units::set_units(min_area_drop_m2, "m2")
  
  # POLYGONIZE (AND DISSOLVE/MERGE BY CLASS)
  cat("Converting raster cetegories to polygon ... \n")
  p <- gdal_polygonizeR(x=r,...)
  
  sf::st_crs(p) <- sf::st_crs(r)
  
  # RENAME FIELDS
  names(p) <- c("class","geometry")
  
  # CALCULATE AREA (m2) (based on trim 25x25 data, so 625 m2 is one pixel)
  cat("Computing polygon areas ... \n")
  p <- p %>% 
    dplyr::mutate(area = sf::st_area(.))
  
  # FILTER AREA (i.e. DROP CRUMBS)
  cat("Filtering polygons by area ... \n")
  p <- p %>%
    dplyr::filter(area > min_area_drop_m2)
  
  # FILL HOLES
  cat("Filling polygon holes and smoothing, may take some time... \n")
  
  #Break polygons up into chunks for parallel processing, store within list
  n_poly <- nrow(p)
  proc_list <- list()
  delta<-ceiling(n_poly/workers)
  strt_idx<-1
  end_idx<-delta
  
  i<-1
  
  while(strt_idx<=n_poly)
  {
    if(end_idx<n_poly)
    {
      proc_list[[i]]<-p[c(strt_idx:end_idx),]
    }else{
      proc_list[[i]]<-p[c(strt_idx:n_poly),]
    }
    
    strt_idx<-end_idx+1
    end_idx<-end_idx+delta
    i<-i+1
  }
  
  #Function to fill and smooth a polygon
  fill_smooth<-function(p)
  {
    p_fill <- p %>% 
      smoothr::fill_holes(threshold = max_area_fill_m2)
    
    # # SMOOTH POLYGONS
    if(smooth == T){
      p_sm <- smoothr::smooth(p_fill, method = smooth_method)
    }
    
    return(p_sm)
  }
  
  #Map fill_smooth in parallel over polygon chunks
  cl<-parallel::makeCluster(workers)
  doParallel::registerDoParallel(cl)
  fut<-foreach::foreach(sub_p=c(1:length(proc_list)),.packages=c('dplyr')) %dopar% {fill_smooth(proc_list[[sub_p]])}
  parallel::stopCluster(cl)
  
  #Merge back to one sf object 
  p_merge=do.call(rbind,fut)
  
  return(p_merge)
  
}






