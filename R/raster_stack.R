#' Create products from a DEM using RSAGA
#'
#' Creates raster derivatives (products) from an input Digital Elevation Model
#' (DEM) using SAGA-GIS.
#'
#' This function uses RSAGA which requires having SAGA-GIS (v2.3+) installed.
#' The input DEM can be in any format that can be read by \code{raster::raster}.
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
#' @param dem filename (character) of input DEM raster.
#' @param stream_vec (Optional) filename (character) to existing vector stream network to burn into DEM, e.g., Fresh Water Atlas.  
#' @param burn_val (Optional) double. The value of Epsilon for burning stream network into DEM.  
#' @param outdir output folder path (character).
#' @param products character. The DEM products to be created. Can be a vector
#'   with any of the following: c("SLOPE", "ASPECT", "DAH", "MRVBF", "TPI",
#'   "CPLAN", "CPROF", "TOPOWET", "CAREA"). If not provided, this parameter
#'   defaults to generating all products.
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#' create_dem_products(dem = "data/dem.tif", outdir = "output",
#'                     products = c("SLOPE", "CAREA"))
#' }
#' @export
create_dem_products <- function(dem,stream_vec = NULL,burn_val=NULL,outdir, products = NULL) {
  env <- RSAGA::rsaga.env()
  
  #Get dem raster params
  r<-raster::raster(dem)
  
  x_res<-res(r)[1]
  y_res<-res(r)[2]
  
  x_min<-extent(r)[1]
  x_max<-extent(r)[2]
  y_min<-extent(r)[3]
  y_max<-extent(r)[4]
  
  
  # Convert DEM to SAGA grid
  dem.sgrd <- file.path(outdir, "ELEV.sgrd")
  RSAGA::rsaga.import.gdal(in.grid=dem,out.grid=dem.sgrd)
  
  #Optionally burn in a existing stream network
  if(!is.null(stream_vec))
  {
    streams_r <- file.path(tempdir(), "streams_r.tif")
    lyr_name <- tools::file_path_sans_ext(basename(stream_vec))
    cmd <- paste("gdal_rasterize -l ", lyr_name, " -burn 1 -tr ", 
                 x_res, " ", y_res, " -te ", x_min, " ", y_min, " ", 
                 x_max, " ", y_max, " -ot Byte ", stream_vec, " ", 
                 streams_r, sep = "")
    system(cmd)
    streams_saga <- file.path(tempdir(), "streams_saga_r.sgrd")
    streams_resamp <- file.path(tempdir(), "streams_resamp.sgrd")
    RSAGA::rsaga.import.gdal(in.grid = streams_r, out.grid = streams_saga, env = env)
    
    RSAGA::rsaga.geoprocessor(lib = 'grid_tools', module = 0,
                              param = list(INPUT=streams_saga,
                                           OUTPUT=streams_resamp,
                                           TARGET_DEFINITION=1,
                                           TARGET_TEMPLATE=dem.sgrd), env=env)
    
    RSAGA::rsaga.geoprocessor(lib = "ta_preprocessor", module = 6, 
                              param = list(DEM = dem.sgrd,
                                           STREAM = streams_resamp, 
                                           EPSILON = burn_val),env=env)
  }
  
  dem.filled<-file.path(outdir, "ELEV_NoSink.sgrd")
  RSAGA::rsaga.geoprocessor(lib='ta_preprocessor',module=4,param=list(ELEV=dem.sgrd,FILLED=dem.filled), env = env)
  
  RSAGA::rsaga.grid.calculus(c(dem.sgrd,dem.filled),file.path(outdir,"SINKS.sgrd"),~abs(a-b)>0)
  
  
  if (is.null(products)) {
    products <- c("SLOPE", "ASPECT", "DAH", "MRVBF", "TPI", "CPLAN", "CPROF",
                  "TOPOWET", "CAREA","SINKS")
  } else {
    products <- toupper(products)
  }
  
  products.out <- file.path(outdir, products)
  raster::extension(products.out) <- "sgrd"
  
  for (i in 1:length(products)) {
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
                                param = list(DEM = dem.sgrd,
                                             DAH = products.out[i]),
                                env = env)
    } else if (p == "MRVBF") {
      RSAGA::rsaga.geoprocessor(lib = "ta_morphometry",
                                module = 8,
                                param = list(DEM = dem.sgrd,
                                             MRVBF = products.out[i]),
                                env = env)
    } else if (p == "TPI") {
      RSAGA::rsaga.geoprocessor(lib = "ta_morphometry",
                                module = 18,
                                param = list(DEM = dem.sgrd,
                                             TPI = products.out[i]),
                                env = env)
    } else if (p == "TOPOWET") {
      RSAGA::rsaga.wetness.index(in.dem = dem.sgrd,
                                 out.wetness.index = products.out[i],
                                 env = env)
    } else if (p == "CAREA") {
      RSAGA::rsaga.geoprocessor(lib = "ta_hydrology",
                                module = 0,
                                param = list(ELEVATION = dem.sgrd,
                                             FLOW = products.out[i]),
                                env = env)
    }
  }
}


#' Create a stack of rasters
#'
#' Aligns input raster(s) to a target raster so that extent, cell size, and
#' cell origin are the same, returning a RasterStack object.
#'
#' This function will reproject the input rasters (using bilinear resampling)
#' to the same projection as the target if necessary.
#'
#' If \code{outdir} is given an ERDAS Imagine .img file is output for each
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
#'   \code{raster::raster}.
#' @param aligned optional boolean, indicates whether input rasters are already
#'   aligned to the same grid. Use \code{aligned = TRUE} if \code{stack_rasters}
#'   has already been run and you want to create a RasterStack object from an
#'   existing list of aligned rasters. If \code{aligned = FALSE} (default),
#'   \code{target_raster} must be provided.
#' @param target_raster optional filename (character) of target raster used to
#'   align all other rasters. Required if \code{aligned = FALSE}.
#' @param outdir optional output folder path (character) where .img files of the
#'   stacked rasters are saved. If no value is provided, no .img files are saved.
#'   If \code{aligned = TRUE}, no output .img files are saved, even if an output
#'   folder is given.
#' @param rastLUTfn optional filename (character) of output rastLUT .csv file,
#'   for use in \code{wetland_map}.
#'
#' @return RasterStack object
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
#' # Stack existing, aligned, rasters
#' raster_list <- c("output/ELEV.img", "output/SLOPE.img", "output/ASPECT.img")
#' raster_stack_2 <- stack_rasters(rasters = raster_list,
#'                                 aligned = TRUE)
#' }
#' @export
stack_rasters <- function(rasters,
                          aligned = FALSE,
                          target_raster = NULL,
                          outdir = NULL,
                          rastLUTfn = NULL) {
  if (aligned) {
    outdir <- NULL
  } else {
    t <- raster::raster(target_raster)
  }
  
  files <- basename(rasters)
  lyr_names<-sub(pattern = "(.*)\\..*$", replacement = "\\1",files)
  
  if (is.null(outdir)) {
    outfiles <- files
    outfiles[] <- ""
  } else {
    outfiles <- file.path(outdir, files)
    raster::extension(outfiles) <- "img"
  }
  
  # Create the list, of correct length, first rather than appending as
  # part of the for loop - avoids issues in R with memory use
  # See: https://stackoverflow.com/questions/14801035/growing-a-list-with-variable-names-in-r
  rp <- vector(mode = "list", length = length(rasters))
  rasterLUT <- data.frame(file = character(),
                          predictor = character(),
                          band = integer(),
                          stringsAsFactors = FALSE)
  
  for (i in 1:length(rasters)) {
    # TO DO:
    # Check resolution of raster and, if target raster resolution is much
    # larger, aggregate the input raster to a similar resolution using
    # raster::aggregate
    # ...
    
    r <- raster::brick(rasters[i])
    
    if (is.na(raster::crs(r)) | is.null(raster::crs(r))) {
      r.prj <- rasters[i]
      raster::extension(r.prj) <- "prj"
      if (file.exists(r.prj)) {
        r.crs <- rgdal::showP4(readLines(r.prj))
        raster::crs(r) <- sp::CRS(r.crs)
      } else {
        stop(paste0(rasters[i], " has no defined CRS"))
      }
    }
    
    # TO DO:
    # Need to output nodata values as -9999; required for ModelMap.
    # ...
    
    if (!aligned) {
      # Align raster to target
      rp[[i]] <- raster::projectRaster(r, t,
                                       method = "bilinear",
                                       filename = outfiles[i],
                                       overwrite = TRUE)
    } else {
      names(r)<-lyr_names[i]
      rp[[i]] <- r
    }
    
    # Add rows to rastLUT
    
    # If no output .img files are being saved, add the input raster filename
    # to the rastLUT
    if (is.null(outdir)) {
      file = normalizePath(rasters[i], winslash = "/")
    } else {
      file = normalizePath(outfiles[i], winslash = "/")
    }
    rasterLUT <- rbind(rasterLUT,
                       data.frame(file = file,
                                  predictor = lyr_names[i],
                                  band = 1))
    
  }
  
  # Write rasterLUT to csv
  if (!is.null(rastLUTfn)) {
    write.table(rasterLUT, rastLUTfn,
                row.names = FALSE,
                col.names = FALSE,
                sep=",")
  }
  
  return(raster::stack(rp))
}


#' Extract raster values at points
#'
#' Adds cell values from a Raster* object as attributes to a SpatialPoints*
#' object.
#'
#' If the Raster* object contains multiple bands or layers, an attribute for
#' band/layer will be added to the points. Raster names are used as attribute
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
#' @param x Raster* object.
#' @param y SpatialPoints* object.
#' @param filename optional output CSV filename.
#' @param aoi optional SpatialPolygon object, used to intersect with input
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
grid_values_at_sp <- function(x, y,
                              filename = NULL,
                              aoi = NULL) {
  
  if(sf::st_crs(x)==sf::st_crs(y))
  {
    
    # Extract raster cell values for each point and add them as an attribute
    shp.values <- raster::extract(x, y, sp = TRUE)
    
    # If AOI is provided, intersect AOI with points
    if (!is.null(aoi)) {
      if(sf::st_crs(shp.values)==sf::st_crs(aoi))
      {
        shp.values.aoi <- raster::intersect(shp.values, aoi)
      }else{
        cat("Sample points / raster and AOI CRS do not match...")
      }
    } else {
      shp.values.aoi <- shp.values
    }
    if (!is.null(filename)) {
      utils::write.csv(shp.values.aoi, filename)
    }
    return(shp.values.aoi)
  }else{
    cat("Sample points and raster CRS do not match...")
  }
}

