#' Polygonize a classified raster layer, dropping crumbs and fill holes. 
#'
#' Must provide a classified raster, minimum area for which polygons should
#' be dropped (meteres sqaured), and maximum area for which polygon holes
#' should be filled. Optionally, polygons may be simplified using *smoothr*
#' 'smooth'[https://cran.r-project.org/web/packages/smoothr/index.html] function, 
#' a smoothing method must be specified. 
#'
#' @param r Classified Raster object, STARS object or Spatial Grid.
#' @param min_area_drop_m2 Numeric. Minimum area in square meters for which polygons will be dropped
#' @param max_area_fill_m2 Numeric. Maximum area in square meters of internal polygon holes for which
#' holes will be remove. 
#' @param smooth Boolean. Should the resulting polygons geometry be simplified. 
#' @param smooth_method String. Smoothing method to pass to smoothr::smooth,
#' smooth must be TRUE.  
#'
#' @return Polygonized sf object representing the input raster categories. 
#'
#' @examples
#' \dontrun{
#'#Create a classified *stars* object 
#'r <- stars::read_stars('CategoryPredictionMap.tif')
#'
#'#Polygonize classified raster
#'p <- raster_to_clean_polygon`(`r,2500,2500,TRUE,'chaikin'`)`
#' }
#' @export
raster_to_clean_polygon<-function(r,min_area_drop_m2,max_area_fill_m2,smooth,smooth_method)
{
  max_area_fill_m2<-units::as_units(max_area_fill_m2, "m2")
  min_area_drop_m2<-units::as_units(min_area_drop_m2, "m2")
  
  # POLYGONIZE (AND DISSOLVE/MERGE BY CLASS)
  p <- sf::st_as_sf(r,as_points = F, merge = T)
  
  # RENAME FIELDS
  names(p) <- c("class","geometry")
  
  # CALCULATE AREA (m2) (based on trim 25x25 data, so 625 m2 is one pixel)
  p <- p %>% dplyr::mutate(area = sf::st_area(.))
  
  # FILTER AREA (i.e. DROP CRUMBS)
  p <- p %>%
    dplyr::filter(area > min_area_drop_m2)
  
  # FILL HOLES
  p <- p %>%
    smoothr::fill_holes(threshold = max_area_fill_m2)
  
  # SMOOTH POLYGONS
  if(smooth == T){
    p_sm <- smoothr::smooth(p, method = smooth_method)
  }
  
  return(p_sm)
  
}