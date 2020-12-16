#' Retrieves BC Data layers in batch.
#'
#' This function downloads layers from the BC Geographic Warehouse
#' using the R bcdata package. The function takes in one input
#' as a data frame with three columns named 'code','geom_ext'
#' & 'filter_exp'. Each row represents a new BC Data layer,
#' and its associated parameters. See table parameter
#' descriptions below.
#'
#' @param layer_tbl A data frame with three columns named 'code','geom_ext'
#' & 'filter_exp'. The 'code' field represents a character string denoting
#' the URL of the BC Geographic Warehouse BCGW entry. The 'geom_ext' represents 
#' the name of the SF object as a string for which to intersect the layer data with, 
#' which must be present in the global environment. The 'filter_exp' field represents 
#' the filter expression as a string to be passed to bcdata::filter evaluated by bcdata::CQL.
#' @return A list of the resulting BC Data layers defined by 'layer_tbl'.
#' @export
batch_bcdata<-function(layer_tbl) 
{
  
  #Create a list for holding layer results
  lyr_lst<-list()
  
  #For each layer in layer_tbl
  for(row in c(1:nrow(layer_tbl)))
  {
    #Get BC Catalog permanent ID code
    code<-layer_tbl$code[row]
    
    #Get the geometry, may be NA
    geom<-layer_tbl$geom_ext[row]
    
    #Get filter expression, may be NA
    filter_exp<-layer_tbl$filter_exp[row]
    
    #No geometry or filter is provided, just retrive per norm
    if(is.na(geom)==T && is.na(filter_exp)==T)
    {
      lyr_lst[[row]]<-bcdata::bcdc_query_geodata(code) %>%
        bcdata::collect() %>%
        sf::st_as_sf()
    }
    
    #If only a geometry is provided, intersect and retrieve
    if(is.na(geom)==F && is.na(filter_exp)==T)
    {
      lyr_lst[[row]]<-bcdata::bcdc_query_geodata(code) %>%
        bcdata::filter(bcdata::INTERSECTS((!!rlang::sym(geom)))) %>%
        bcdata::collect() %>%
        sf::st_as_sf()
    }
    
    #If both geometry and filter expression are provided, intersect filter and retrieve
    if(is.na(geom)==F && is.na(filter_exp)==F)
    {
      lyr_lst[[row]]<-bcdata::bcdc_query_geodata(code) %>%
        bcdata::filter(bcdata::INTERSECTS((!!rlang::sym(geom)))) %>%
        bcdata::filter(bcdata::CQL(filter_exp)) %>%
        bcdata::collect() %>%
        sf::st_as_sf()
    }
    
    #If only filter is provided, filter and retrieve
    if(is.na(geom)==T && is.na(filter_exp)==F)
    {
      lyr_lst[[row]]<-bcdata::bcdc_query_geodata(code) %>%
        bcdata::filter(bcdata::CQL(filter_exp)) %>%
        bcdata::collect() %>%
        sf::st_as_sf()
    }
    
  }
  
  #Return list of layer bcdata results
  return(lyr_lst)
}
