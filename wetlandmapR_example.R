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

#------------------------------------------------------------------------------
# Add raster values to training points...
#------------------------------------------------------------------------------
# Shapefile points:
input_points <- raster::shapefile("../../testdata/training_points.shp")

# File Geodatabase points:
#input_points <- rgdal::readOGR(dsn = "../../WillistonWetlands.gdb",
#                               layer = "AttributedPoints_FWCP_Master_20April2018")

# Add raster values to training points
qdatafn <- "../../testdata/training_points_attd.csv"
input_points_withvalues <- grid_values_at_sp(raster_stack,
                                             input_points,
                                             filename = qdatafn)


#------------------------------------------------------------------------------
# Setup predictor list and raster LUT...
#------------------------------------------------------------------------------
#
# NOTE:
# Edit .csv created by stack_rasters() to remove any rows that aren't required.
#
rastLUT <- read.csv("../../testdata/raster_stack/rastLUT.csv", header = FALSE, stringsAsFactors = FALSE)

# Character vector of the predictor names, used as input to the model
predList <- rastLUT[, 2]


#------------------------------------------------------------------------------
# Run model and diagnostics...
#------------------------------------------------------------------------------
out.list <- wetland_model(qdatafn = qdatafn,
                          model.type = "RF",
                          model.folder = "./output",
                          unique.rowname = "OBJECTID",
                          predList = predList,
                          predFactor = FALSE,
                          response.name = "T_W_Class",
                          response.type = "categorical",
                          seed = 44)

#------------------------------------------------------------------------------
# Create map from model...
#------------------------------------------------------------------------------
wetland_map(model.obj = out.list[[1]],
            folder = out.list[[2]],
            MODELfn = basename(out.list[[2]]),
            rastLUTfn = rastLUT)
