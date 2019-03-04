#' Create products from a DEM using RSAGA
#'
#' @param dem filename (character) of input DEM raster.
#' @param outdir output folder path (character).
#' @param products character. The DEM products to be created. Can be a vector
#'   with any of the following: c("SLOPE", "ASPECT", "DAH", "MRVBF", "TPI", "CPLAN",
#'   "CPROF", "TOPOWET", "CAREA")
create_dem_products <- function(dem, outdir, products = "ALL") {
  env <- RSAGA::rsaga.env()

  # Convert DEM to SAGA grid
  dem.r <- raster::raster(dem)
  dem.sgrd <- file.path(outdir, "ELEV.sgrd")
  raster::writeRaster(dem.r, dem.sgrd, format = "SAGA", prj = TRUE, overwrite = TRUE)

  if (toupper(products) == "all") {
    products <- c("SLOPE", "ASPECT", "DAH", "MRVBF", "TPI", "CPLAN", "CPROF", "TOPOWET", "CAREA")
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
                                param = list(DEM = dem.sgrd, DAH = products.out[i]),
                                env = env)
    } else if (p == "MRVBF") {
      RSAGA::rsaga.geoprocessor(lib = "ta_morphometry",
                                module = 8,
                                param = list(DEM = dem.sgrd, MRVBF = products.out[i]),
                                env = env)
    } else if (p == "TPI") {
      RSAGA::rsaga.geoprocessor(lib = "ta_morphometry",
                                module = 18,
                                param = list(DEM = dem.sgrd, TPI = products.out[i]),
                                env = env)
    } else if (p == "TOPOWET") {
      RSAGA::rsaga.wetness.index(in.dem = dem.sgrd,
                                 out.wetness.index = products.out[i],
                                 env = env)
    } else if (p == "CAREA") {
      RSAGA::rsaga.geoprocessor(lib = "ta_hydrology",
                                module = 0,
                                param = list(ELEVATION = dem.sgrd, FLOW = products.out[i]),
                                env = env)
    }
  }
}


#' Create a stack of rasters
#'
#' @param rasters filename(s) (character) of raster files to be
#'   stacked with the target_raster. Input rasters can be any format read by
#'   \code{raster}
#' @param target_raster filename (character) of target raster used to align all
#'   other rasters.
#' @param outdir optional output folder path (character) where .img files of the stacked
#'   rasters are saved.
#' @return RasterStack object
stack_rasters <- function(rasters, target_raster, outdir = NULL) {
  # TO DO:
  # Use folder as input for rasters; stack all rasters in that folder

  t <- raster::raster(target_raster)
  files <- basename(rasters)

  if (is.null(outdir)) {
    outfiles <- files
    outfiles[] <- ""
  } else {
    outfiles <- file.path(outdir, files)
    raster::extension(outfiles) <- "img"
  }

  rp <- list()
  for (i in 1:length(rasters)) {
    # TO DO:
    # Check resolution of raster and, if target raster resolution is much larger,
    # aggregate the input raster to a similar resolution using raster::aggregate

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

    r1 <- raster::projectRaster(r, t, method = "bilinear", filename = outfiles[i], overwrite = TRUE)
    rp <- append(rp, r1)
  }

  return(raster::stack(rp))
}


#' Extract raster values at points
#'
#' @param x Raster* object
#' @param y SpatialPoints* object
#' @param filename optional output CSV filename
#' @return SpatialPoints* object with values added to it's data frame
grid_values_at_sp <- function(x, y, filename = NULL) {
  shp.values <- raster::extract(x, y, sp = TRUE)
  if (!is.null(filename)) {
    write.csv(shp.values, filename)
  }
  return(shp.values)
}

