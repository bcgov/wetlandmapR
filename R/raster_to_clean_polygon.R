#' @title gdal_polygonizeR
#' @description R Wrapper for the gdal_polygonize.py python script (http://www.gdal.org/gdal_polygonize.html)
#' This utility creates vector polygons for all connected regions of pixels in the raster sharing
#' a common pixel value. Each polygon is created with an attribute indicating the pixel value of
#' that polygon. Can be useful for example to create fishnet polygons (see "create_fishnet")
#'
#' @param x          `character`filename of a raster file, or "SpatRaster" raster object
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
  if (is(x, 'SpatRaster')) {
    require('terra')
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










