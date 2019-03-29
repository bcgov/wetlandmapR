rm(list=ls(all=TRUE)) #start with empty workspace

#------------------------------------------------------------------------------
# Raster data set-up...
#------------------------------------------------------------------------------
# SAGA processing
create_dem_products(dem = "../../testdata/dem.tif",
                    outdir = "data")

# List the output from create_dem_products... this will form the input to the
# raster stack
raster_list <- list.files("data", "sdat$", full.names = TRUE)

# Add Landsat and Sentinel to the list (and any other rasters to be included in
# the stack)
raster_list <- append(raster_list, "../../testdata/landsat.tif")
raster_list <- append(raster_list, "../../testdata/sentinel2.tif")

# Add ClimateBC rasters to the list
raster_list <- append(raster_list,
                      list.files("../../testdata/ClimateBC",
                                 ".img$", full.names = TRUE))

# Stack all raster inputs
raster_stack <- stack_rasters(rasters = raster_list,
                              target_raster = "../../testdata/dem.tif",
                              outdir = "../../testdata/raster_stack",
                              rastLUTfn = "../../testdata/raster_stack/rastLUT.csv")

# If you already have a folder containing rasters that have been aligned to the
# same grid, i.e. previous output of stack_rasters(), you can use those rasters
# to create a RasterStack object and rastLUT, e.g:
#   raster_list <- list.files("../../testdata/raster_stack", "img$", full.names = TRUE)
#   raster_stack <- stack_rasters(rasters = raster_list,
#                                 aligned = TRUE,
#                                 rastLUTfn = "../../testdata/raster_stack/rastLUT.csv")

#------------------------------------------------------------------------------
# Add raster values to training points...
#------------------------------------------------------------------------------
# Shapefile points:
input_points <- raster::shapefile("../../testdata/training_points.shp")

# File Geodatabase points:
#   input_points <- rgdal::readOGR(dsn = "../../WillistonWetlands.gdb",
#                                  layer = "AttributedPoints_FWCP_Master_20April2018")

# AOI layer:
aoi_polys <- raster::shapefile("../../testdata/dinosaur_aoi_basins.shp")

# Add raster values to training points; attribute points by AOI
qdatafn <- "../../testdata/training_points_attd.csv"
input_points_withvalues <- grid_values_at_sp(raster_stack,
                                             input_points,
                                             filename = qdatafn,
                                             aoi = aoi_polys)


#------------------------------------------------------------------------------
# Setup predictor list and raster LUT...
#------------------------------------------------------------------------------
rastLUT <- read.csv("../../testdata/raster_stack/rastLUT.csv",
                    header = FALSE,
                    stringsAsFactors = FALSE)

# NOTE:
# Edit rastLUT (or edit the .csv created by stack_rasters() before reading it) to remove any rows that aren't required.
# i.e. exclude rows 42 to 43
rastLUT <- rastLUT[-(42:43),]

# Character vector of the predictor names, used as input to the model
predList <- rastLUT[, 2]


#------------------------------------------------------------------------------
# Run model and diagnostics...
#------------------------------------------------------------------------------
model.out <- wetland_model(qdatafn = qdatafn,
                          model.type = "RF",
                          model.folder = "./output",
                          unique.rowname = "OBJECTID",
                          predList = predList,
                          predFactor = FALSE,
                          response.name = "T_W_Class",
                          response.type = "categorical",
                          seed = 44,
                          aoi.col = "BASIN")

#------------------------------------------------------------------------------
# Create map from model...
#------------------------------------------------------------------------------

wetland_map(model.out = model.out,
            model.folder = "./output",
            rastLUTfn = rastLUT,
            aoi = aoi_polys,
            aoi.col = "BASIN")
