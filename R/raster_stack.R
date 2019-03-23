#' Create products from a DEM using RSAGA
#'
#' @param dem filename (character) of input DEM raster.
#' @param outdir output folder path (character).
#' @param products character. The DEM products to be created. Can be a vector
#'   with any of the following: c("SLOPE", "ASPECT", "DAH", "MRVBF", "TPI",
#'   "CPLAN", "CPROF", "TOPOWET", "CAREA").
create_dem_products <- function(dem, outdir, products = "ALL") {
  env <- RSAGA::rsaga.env()

  # Convert DEM to SAGA grid
  dem.r <- raster::raster(dem)
  names(dem.r) <- "ELEV"
  dem.sgrd <- file.path(outdir, "ELEV.sgrd")
  raster::writeRaster(dem.r, dem.sgrd,
                      format = "SAGA",
                      prj = TRUE,
                      overwrite = TRUE)

  if (toupper(products) == "ALL") {
    products <- c("SLOPE", "ASPECT", "DAH", "MRVBF", "TPI", "CPLAN", "CPROF",
                  "TOPOWET", "CAREA")
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
                                  unit.aspect = "degrees",
                                  env = env)
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
#' @param rasters filename(s) (character) of raster files to be
#'   stacked with the target_raster. Input rasters can be any format read by
#'   \code{raster}.
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
#' @return RasterStack object
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
  rasterLUT <- data.frame(file=character(),
                          predictor=character(),
                          band=integer(),
                          stringsAsFactors=FALSE)
  for (i in 1:length(rasters)) {
    # TO DO:
    # Check resolution of raster and, if target raster resolution is much
    # larger, aggregate the input raster to a similar resolution using
    # raster::aggregate

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
    # Need to output nodata values as -9999

    if (!aligned) {
      # Align raster to target
      rp[[i]] <- raster::projectRaster(r, t,
                                       method = "bilinear",
                                       filename = outfiles[i],
                                       overwrite = TRUE)
    } else {
      rp[[i]] <- r
    }

    # Add rows to rastLUT
    for (b in 1:raster::nbands(r)) {
      # If no output .img files are being saved, add the input raster filename
      # to the rastLUT
      if (is.null(outdir)) {
        file = normalizePath(rasters[i], winslash = "/")
      } else {
        file = normalizePath(outfiles[i], winslash = "/")
      }
      rasterLUT <- rbind(rasterLUT,
                         data.frame(file = file,
                                    predictor = names(r)[b],
                                    band = b))
    }
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
#' @param x Raster* object.
#' @param y SpatialPoints* object.
#' @param filename optional output CSV filename.
#' @param aoi optional SpatialPolygon object, used to intersect with input
#'   points. If a point intersects more than one polygon, that point will be
#'   duplicated (once for each polygon it intersects) in the output. If a point
#'   doesn't intersect any AOI polygon it will be excluded from the output.
#' @return SpatialPoints* object with values added to it's data frame
grid_values_at_sp <- function(x, y,
                              filename = NULL,
                              aoi = NULL) {
  # Extract raster cell values for each point and add them as an attribute
  shp.values <- raster::extract(x, y, sp = TRUE)

  # If AOI is provided, intersect AOI with points
  if (!is.null(aoi)) {
    shp.values.aoi <- raster::intersect(shp.values, aoi)
  } else {
    shp.values.aoi <- shp.values
  }

  if (!is.null(filename)) {
    write.csv(shp.values.aoi, filename)
  }
  return(shp.values.aoi)
}

