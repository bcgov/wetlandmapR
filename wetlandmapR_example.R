rm(list=ls(all=TRUE)) #start with empty workspace

#------------------------------------------------------------------------------
# Raster data set-up...
#------------------------------------------------------------------------------
# SAGA processing
create_dem_products("../../testdata/dem.tif",
                    "data")

# List the output from create_dem_products... this will form the input to the
# raster stack
raster_list <- list.files("data", "sdat$", full.names = TRUE)

# Add Landsat and Sentinel to the list
raster_list <- append(raster_list, "../../testdata/landsat.tif")
raster_list <- append(raster_list, "../../testdata/sentinel2.tif")

# Add ClimateBC rasters to the list
raster_list <- append(raster_list,
                      list.files("../../testdata/ClimateBC",
                                 ".img$", full.names = TRUE))

# Stack all raster inputs
raster_stack <- stack_rasters(raster_list,
                              "../../testdata/dem.tif",
                              "../../testdata/raster_stack")

#------------------------------------------------------------------------------
# Add raster values to training points...
#------------------------------------------------------------------------------
# Shapefile points:
input_points <- raster::shapefile("../../testdata/training_pts.shp")

# File Geodatabase points:
#input_points <- rgdal::readOGR(dsn = "../../WillistonWetlands.gdb",
#                               layer = "AttributedPoints_FWCP_Master_20April2018")

# Add raster values to training points
qdatafn <- "../../testdata/training_pts.csv"
input_points_withvalues <- grid_values_at_sp(raster_stack,
                                             input_points,
                                             qdatafn)

# Character vector of the predictor names, used as input to the model
predList <- names(r_stack)
# Or, if using a previously generated set of attributed training points...
# qdata <- read.csv(qdatafn)
# predList <- names(qdata)
predList

# Remove any values from predList that aren't to be used as predictors
predList <- predList[!predList %in% c("sentinel2.14", "sentinel2.15")]

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

# TO DO:
# Create function to generate rasterLUT... maybe output from stack_rasters...
# return list of outfiles[i]...

rastLUT <- "../../testdata/raster_stack/rastLUT.csv"

wetland_map(model.obj = out.list[[1]],
            folder = out.list[[2]],
            MODELfn = basename(out.list[[2]]),
            rastLUTfn = rastLUT)
