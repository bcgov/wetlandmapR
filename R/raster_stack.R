#' Create products from a DEM using RSAGA
#'
#' Creates raster derivatives (products) from an input Digital Elevation Model
#' (DEM) using SAGA-GIS.
#'
#' This function uses RSAGA which requires having SAGA-GIS (v2.3+) installed.
#' The input DEM can be in any format that can be read by \code{terra::rast}.
#' Output rasters are saved in the SAGA Grid format, including a SAGA Grid
#' version of the input DEM, saved as "ELEV.sgrd".
#'
#' The following products can be created by using the \code{products} parameter
#' (if \code{products = NULL} all products will be created by default):
#'   SLOPE:   Slope (percent).
#'   ASPECT:  Aspect (degrees).
#'   DAH:     Diurnal Anisotrophic Heating.
#'   MRVBF:   Multiresolution Index of Valley Bottom Flatness.
#'   TPI:     Topographic Position Index.
#'   CPLAN:   Plan Curvature.
#'   CPROF:   Profile Curvature.
#'   TOPOWET: SAGA Wetness Index.
#'   CAREA:   Top-Down Flow Accumulation.
#'
#'
#' @param dem filename (character) to input DEM raster.
#' @param stream_vec (Optional) filename (character) or SpatVector of existing vector stream network to burn into DEM, e.g., Fresh Water Atlas.  
#' @param burn_val (Optional) double. The value of Epsilon for burning stream network into DEM.  
#' @param outdir output folder path (character).
#' @param products character. The DEM products to be created. Can be a vector
#'   with any of the following: c("SLOPE", "ASPECT", "DAH", "MRVBF", "TPI",
#'   "CPLAN", "CPROF", "TOPOWET", "CAREA"). If not provided, this parameter
#'   defaults to generating all products.
#' @param param_vect vector (character). Similar to the \code{products} parameter, this is a vector of
#' the SAGA module names (see Details section below) for which non-default
#' parameters will be passed. Defaults to NULL.
#' @param param_values This is a list of lists, and the order corresponding to \code{param_vect}. For each
#' parameter specified in \code{param_vect}, the additional parameters for the corresponding
#' SAGA module must be specified using a list of those additional SAGA parameters and their values. 
#' Defaults to NULL.    
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#' create_dem_products(dem = "data/dem.tif", outdir = "output",
#'                     products = c("SLOPE", "CAREA"))
#' }
#' @export
create_dem_products <- function(dem,stream_vec = NULL,burn_val=NULL,outdir, products = NULL,param_vect=NULL,param_values=NULL) {
  
  #Check if 'param_vect' provided
  if(is.null(param_vect))
  {
    #Set as char
    param_vect<-c(NULL)
  }else{
    #Add NULL as first list entry 
    param_values<-append(list(NULL),param_values)
  }
  
  #Find SAGA installation 
  env <- RSAGA::rsaga.env()
  
  
  # Convert DEM to SAGA grid
  dem.sgrd <- file.path(outdir, "ELEV.sgrd")
  dem.sdat <- file.path(outdir, "ELEV.sdat")
  RSAGA::rsaga.import.gdal(in.grid=dem,out.grid=dem.sgrd)
  
  #Optionally burn in a existing stream network
  if(!is.null(stream_vec))
  {
    
    #Rasterize stream lyr
    streams_r <- file.path(tempdir(), "streams_r.tif")
    lyr_name <- tools::file_path_sans_ext(basename(stream_vec))
    
    
    if(class(stream_vec)=="character")
    {
      stream_vec<-terra::vect(stream_vec)
    }
    
    terra_dem<- terra::rast(dem.sdat)
    
    #Reproject streams if necessary
    if(terra::crs(stream_vec)!=terra::crs(terra_dem))
    {
      stream_vec<-terra::project(stream_vec,terra::crs(terra_dem))
    }
    
    #Rasterize stream layer
    terra::rasterize(stream_vec, terra_dem,filename=streams_r, overwrite=TRUE)
    
    #Load streams as '.sgrd'
    streams_saga <- file.path(tempdir(), "streams_saga_r.sgrd")
    streams_resamp <- file.path(tempdir(), "streams_resamp.sgrd")
    RSAGA::rsaga.import.gdal(in.grid = streams_r, out.grid = streams_saga, env = env)
    
    #Resample stream raster to dem.sgrd
    RSAGA::rsaga.geoprocessor(lib = 'grid_tools', module = 0,
                              param = list(INPUT=streams_saga,
                                           OUTPUT=streams_resamp,
                                           TARGET_DEFINITION=1,
                                           TARGET_TEMPLATE=dem.sgrd), env=env)
    #Burn streams into DEM by 'burn_val' in meters 
    RSAGA::rsaga.geoprocessor(lib = "ta_preprocessor", module = 6, 
                              param = list(DEM = dem.sgrd,
                                           STREAM = streams_resamp, 
                                           EPSILON = burn_val),env=env)
  }
  
  #Create a binary 'SINKS' layer 
  dem.filled<-file.path(outdir, "ELEV_NoSink.sgrd")
  RSAGA::rsaga.geoprocessor(lib='ta_preprocessor',module=4,param=list(ELEV=dem.sgrd,FILLED=dem.filled), env = env)
  
  RSAGA::rsaga.grid.calculus(c(dem.sgrd,dem.filled),file.path(outdir,"SINKS.sgrd"),~abs(a-b)>0)
  
  #Format user provided 'products', otherwise specify all modules 
  if (is.null(products)) {
    products <- c("SLOPE", "ASPECT", "DAH", "MRVBF", "TPI", "CPLAN", "CPROF",
                  "TOPOWET", "CAREA","SINKS")
  } else {
    products <- toupper(products)
  }
  
  #Vector of product out paths 
  products.out <- paste(file.path(outdir,products),".sgrd",sep="")
  
  #Function for associating 'param_vect' entries with corresponding 'product' index
  get_param_lst_idx <- function(product)
  {
    idx<-1
    if(!is.null(param_vect[1])){
      for(i in c(1:length(param_vect)))
      {
        if(product == param_vect[i])
        {
          idx<-i+1
        }
        i<-i+1
      }
    }
    return(idx)
  }
  
  #Use RSAGA to create specified product layers in 'outdir'
  for (i in 1:length(products)) {
    
    strt<-Sys.time()
    p <- products[i]
    
    if (p == "SLOPE") {
      RSAGA::rsaga.slope.asp.curv(in.dem = dem.sgrd,
                                  out.slope = products.out[i],
                                  unit.slope = "percent",
                                  env = env)
    } else if (p == "ASPECT") {
      RSAGA::rsaga.slope.asp.curv(in.dem = dem.sgrd,
                                  out.aspect = products.out[i],
                                  unit.aspect = "radians",
                                  env = env)
      
      RSAGA::rsaga.grid.calculus(file.path(outdir,'ASPECT.sgrd'),file.path(outdir,"NORTHNESS.sgrd"),~cos(a))
      
      RSAGA::rsaga.geoprocessor(lib='grid_calculus',
                                module = 1,
                                param=list(FORMULA='ifelse(eq(g1,nodata()),0.0,g1)',
                                           USE_NODATA=1,
                                           GRIDS=file.path(outdir,'NORTHNESS.sgrd'),
                                           RESULT=file.path(outdir,'NORTHNESS.sgrd')))
      
      
      RSAGA::rsaga.grid.calculus(file.path(outdir,'ASPECT.sgrd'),file.path(outdir,"EASTNESS.sgrd"),~sin(a))
      
      RSAGA::rsaga.geoprocessor(lib='grid_calculus',
                                module = 1,
                                param=list(FORMULA='ifelse(eq(g1,nodata()),0.0,g1)',
                                           USE_NODATA=1,
                                           GRIDS=file.path(outdir,'EASTNESS.sgrd'),
                                           RESULT=file.path(outdir,'EASTNESS.sgrd')))
      
      
      
    } else if (p == "CPLAN") {
      RSAGA::rsaga.slope.asp.curv(in.dem = dem.sgrd,
                                  out.cplan = products.out[i],
                                  env = env)
    } else if (p == "CPROF") {
      RSAGA::rsaga.slope.asp.curv(in.dem = dem.sgrd,
                                  out.cprof = products.out[i],
                                  env = env)
    } else if (p == "DAH") {
      RSAGA::rsaga.geoprocessor(lib = "ta_morphometry",
                                module = 12,
                                param = append(list(DEM = dem.sgrd,
                                                    DAH = products.out[i]),param_values[[get_param_lst_idx("DAH")]]),
                                env = env)
    } else if (p == "MRVBF") {
      RSAGA::rsaga.geoprocessor(lib = "ta_morphometry",
                                module = 8,
                                param = append(list(DEM = dem.sgrd,
                                                    MRVBF = products.out[i]),param_values[[get_param_lst_idx("MRVBF")]]),
                                env = env)
    } else if (p == "TPI") {
      RSAGA::rsaga.geoprocessor(lib = "ta_morphometry",
                                module = 18,
                                param = append(list(DEM = dem.sgrd,
                                                    TPI = products.out[i]),param_values[[get_param_lst_idx("TPI")]]),
                                env = env)
    } else if (p == "TOPOWET") {
      RSAGA::rsaga.wetness.index(in.dem = dem.sgrd,
                                 out.wetness.index = products.out[i],
                                 env = env)
    } else if (p == "CAREA") {
      RSAGA::rsaga.geoprocessor(lib = "ta_hydrology",
                                module = 0,
                                param = append(list(ELEVATION = dem.sgrd,
                                                    FLOW = products.out[i]),param_values[[get_param_lst_idx("CAREA")]]),
                                env = env)
    }
    end<-Sys.time()
    dur<-end-strt
    cat(p,dur)
  }
}


#' Create a stack of input rasters
#'
#' Aligns input raster(s) to a target raster so that extent, cell size, and
#' cell origin are the same, returning a Terra raster object.
#'
#' This function will reproject and or resample the input rasters (using bilinear resampling)
#' to the same projection as the target, if necessary.
#'
#' If \code{outdir} is given an GTIFF (.tif) file is output for each
#' aligned raster.
#'
#' This function can also output a "rastLUT" CSV file for use as input to
#' \code{\link{wetland_model}}, where the raster names are used as the predictor
#' name.
#'
#' If you have a set of rasters that are already aligned, you can use this
#' function to create a RasterStack without creating duplicate output rasters
#' by using the \code{aligned} parameter.
#'
#' @param rasters filename(s) (character) of raster files to be
#'   stacked with the target_raster. Input rasters can be any format read by
#'   \code{terra::rast}.
#' @param target_raster filename (character) of target raster used to
#'   align all other rasters. 
#' @param outdir (optional) output folder path (character) where .tif files of the
#'   stacked rasters are saved. If no value is provided, no .tif files are saved. Outputs will be
#'   placed in a directory names 'resampl'.
#' @param rastLUTfn (optional) Directory path (character) for output 'rastLUT.csv' file,
#'   for use in \code{wetland_map}. Default NULL.
#' @param bands (optional) character vector corresponding to rastLUT indicating which band to use as model input.
#' Defaults to first band for each layer specified in rastLUT. 
#' @param rastNames (optional) (character) If rastLUTfn is provided, must provide 
#' corresponding names of each raster in 'rasters'.
#' @param NAval NA value of input raster if not NaN (must be same for all), defaults to NaN.
#' @param aligned (Boolean) If true, function acts as wrapper for terra::rast, default false. 
#' @return Multiband Terra raster object 
#'
#' @examples
#' \dontrun{
#' # Create raster list from output files generated by create_dem_products()
#' raster_list <- list.files("output", "sdat$", full.names = TRUE)
#'
#' raster_stack <- stack_rasters(rasters = raster_list,
#'                               target_raster = "data/dem.tif",
#'                               outdir = "output",
#'                               rastLUTfn = "output/rastLUT.csv")
#'
#' }
#' @export
stack_rasters <- function(rasters,target_raster,outdir=NULL,rastLUTfn=NULL,rastNames=NULL,NAval=NaN,bands=1,aligned=FALSE)
{  
  
  if(aligned==F)
  {
  
  #Read target_raster as terra
  target_raster<-terra::rast(target_raster)
  
  #Get current NA flag of target raster
  tr_naflag<-terra::NAflag(target_raster)
  
  #If current flag is NaN
  if(is.nan(tr_naflag))
  {
    #replace with 'NAval'
    target_raster[is.nan(target_raster)]<-NAval
  }else{
    #replace with 'NAval'
    target_raster[target_raster==tr_naflag]<-NAval
  }
  
  #Set the NAflag to 'NAval' for target raster 
  terra::NAflag(target_raster)<-NAval
  
  
  #Vector of aligned raster paths 
  aligned<-c()
  
  #For each raster in 'rasters'
  for(i in c(1:length(rasters)))
  {
    #Read in raster as terra
    test<-terra::rast(rasters[i])
    
    #Check to see if 'test' NAflag is same as NAval
    na_match<-FALSE
    
    if(is.nan(terra::NAflag(test)))
    {
      if(is.nan(NAval))
      {
        na_match<-TRUE
      }
    }else{
      if(terra::NAflag(test)==NAval)
      {
        na_match=TRUE
      }
    }
    
    #Cehck if the geometry (and crs), NAflag match 'target_raster'
    if(terra::compareGeom(test, target_raster,stopOnError=F)==FALSE || na_match==FALSE)
    {
      
      #Repalce test NAflag with 'NAval'
      if(is.nan(terra::NAflag(test)))
      {
        test[is.nan(test)]<-NAval
        terra::NAflag(test)<-NAval
      }else{
        test[terra::NAflag(test)]<-NAval
        terra::Naflag(test)<-NAval
      }
      
      #Reproject if necessary 
      if(terra::crs(test) != terra::crs(target_raster))
      {
        test <- terra::project(test,target_raster)
      }
      
      #If 'outdir' is provided, else write to 'tempdir()'
      if(!is.null(outdir))
      {
        #Check if sub-dir 'resampl' already exists within 'output' dir
        if(!dir.exists(file.path(outdir,"resampl/")))
        {
          #If not, create it
          dir.create(file.path(outdir,"resampl/"))
        }
        
        #If not create a new raster resampled (and possibly projeted) to 'target_raster'
        aligned[i]<-paste(outdir,"/resampl/",tools::file_path_sans_ext(basename(rasters[i])),".tif",sep="")
        terra::resample(test,target_raster,filename=aligned[i],overwrite=T,filetype="GTIFF")
      }else{
        #If not create a new raster resampled (and possibly projeted) to 'target_raster'
        aligned[i]<-paste(tempdir(),"/",tools::file_path_sans_ext(basename(rasters[i])),".tif",sep="")
        terra::resample(test,target_raster,filename=aligned[i],overwrite=T,filetype="GTIFF")
      }
      
    }else{
      #Otherwise just add raster path to 'aligned' vector
      aligned[i]<-rasters[i]
      
      #If 'outdir' provided, write to 'outdir'
      if(!is.null(outdir))
      {
        #Check if sub-dir 'resampl' already exists within 'output' dir
        if(!dir.exists(file.path(outdir,"resampl/")))
        {
          #If not, create it
          dir.create(file.path(outdir,"resampl/"))
        }
        
        #Update 'aligned' path
        aligned[i]<-paste(outdir,"/resampl/",tools::file_path_sans_ext(basename(rasters[i])),".tif",sep="")
        #Write out resampled raster 
        terra::writeRaster(test,filename=aligned[i],overwrite=T,filetype="GTIFF")
      }
    }
    
  }
  
  #Write rastLUT.csv if 'rastLUTfn' path provided
  if(!is.null(rastLUTfn))
  {
    #Check to see if user provided names for input rasters 
    if(!is.null(rastNames))
    {
      #Create rastLUT
      
      rasterLUT<-data.frame(file=character(length(rastNames)),
                            predictor=character(length(rastNames)),
                            band=integer(length(rastNames)),
                            stringsAsFactors = F)
      
      
      
      
      rasterLUT$file<-aligned
      rasterLUT$predictor<-rastNames
      if(bands==1)
      {
        rasterLUT$band<-rep(1,nrow(rasterLUT))
      }else{
        rasterLUT$band<-bands
      }
      
      #write table to rastLUTfn 
      utils::write.table(rasterLUT,
                         file.path(rastLUTfn,'rastLUT.csv'),
                         row.names = F,
                         col.names = F,
                         sep=",")
      
    }else{
      cat("Error: Please provide names of raster for rastLUTfn table output.")
    }
    
  }
  
  
  #Return all rasters in 'aligned' vector as terra raster
  return(terra::rast(aligned))
  }else{
    return(terra::rast(rasters))
  }
  
}

#' Extracts raster values at points
#'
#' Adds cell values from a Raster *object as attributes to a SpatVector*
#' object.
#'
#' If the SpatRaster object contains multiple bands or layers, an attribute for
#' each band/layer will be added to the point attributes table. Raster names are used as attribute
#' column names; for multiband rasters, the column name is
#' \code{<name>.<band number>}.
#'
#' An optional polygon Area of Interest (AOI) object can be provided which will
#' intersect the points with the AOI polygons, adding the AOI attributes to the
#' points.
#'
#' The optional output CSV can be used as input training data for
#' \code{\link{wetland_model}}. Raster layer names will be used as the column
#' names in the output.
#'
#' @param raster_stack SpatRaster* object.
#' @param points SpatVector* object.
#' @param filename optional output CSV filename.
#' @param aoi optional SpatVector object, used to intersect with input
#'   points. If a point intersects more than one polygon, that point will be
#'   duplicated (once for each polygon it intersects) in the output. If a point
#'   doesn't intersect any AOI polygon it will be excluded from the output.
#'
#' @return SpatialPoints* object with values added to it's data frame
#'
#' @examples
#' \dontrun{
#' # Shapefile points:
#' input_points <- raster::shapefile("data/training_points.shp")
#'
#' # AOI layer:
#' aoi_polys <- raster::shapefile("data/aoi.shp")
#'
#' # Stack existing, aligned, rasters
#' raster_list <- c("output/ELEV.img", "output/SLOPE.img", "output/ASPECT.img")
#' raster_stack <- stack_rasters(rasters = raster_list,
#'                               aligned = TRUE)
#'
#' # Add raster values to training points; attribute points by AOI
#' qdatafn <- "../../testdata/training_points_attd.csv"
#' input_points_withvalues <- grid_values_at_sp(raster_stack,
#'                                             input_points,
#'                                             filename = qdatafn,
#'                                             aoi = aoi_polys)
#' }
#' @export
grid_values_at_sp <- function(raster_stack,points,filename = NULL,aoi = NULL) {
  
  #Confirm that CRS match 
  if(terra::crs(raster_stack)==terra::crs(points))
  {
    #Generate join column (ID)
    points$ID<-c(1:nrow(points))
    
    # Extract raster cell values for each point and add them as an attribute
    rast.values <- terra::extract(raster_stack, points)
    
    #Join rast.values to points by ID
    points<-terra::merge(points,rast.values,by="ID")
    
    # If AOI is provided, intersect AOI with points
    if (!is.null(aoi)) {
      #Check that CRS match 
      if(terra::crs(points)==terra::crs(aoi))
      {
        #Carry out intersection with AOI
        points.aoi <- terra::intersect(points, aoi)
      }else{
        cat("Sample points / raster and AOI CRS do not match...")
      }
    } else {
      #Consistent name
      points.aoi <- points
    }
    if (!is.null(filename)) {
      #write out dataframe  
      
      points.aoi<-terra::as.data.frame(points.aoi,row.names = F)
      
      utils::write.csv(points.aoi, filename)
    }
    return(points.aoi)
  }else{
    cat("Sample points and raster CRS do not match...")
  }
}


