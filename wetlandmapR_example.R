
#Load dependencies 
library(wetlandmapR)
library(rgeos)
library(rgdal)
library(tidyverse)
library(bcmaps)
library(raster)
library(sf)
#------------------------------------------------------------------------------
# Raster data set-up...
#------------------------------------------------------------------------------

set.seed(44)

#Specify a output directory 
out_dir<-paste(tempdir(),"/wetlandmapR_expl",sep="")
dir.create(out_dir)

#Get digital elevation model DEM
target_dem <- system.file('extdata','DEM.tif',package = 'wetlandmapR')

#Create SAGA elevation derivatives 
create_dem_products(dem = target_dem, outdir = out_dir)

#Get list of raster input paths
raster_list <- list.files(out_dir, "sdat$", full.names = TRUE)
#Drop DEM w/ sinks 
raster_list<-raster_list[!raster_list %in% file.path(out_dir,'ELEV_NoSink.sdat')]

#E.g., Add external Sentinel-2 Band 4 data
raster_list<-append(raster_list,system.file("extdata", "B4_SENT2.tif", package = "wetlandmapR"))

#Stack all raster inputs
raster_stack <- stack_rasters(rasters = raster_list, target_raster = target_dem, outdir = out_dir,rastLUTfn = file.path(out_dir,'rastLUT.csv'), aligned=FALSE)

#Plot the new stack of raster objects (co-variate layers)
plot(raster_stack)

#------------------------------------------------------------------------------
# Add raster values to training points...
#------------------------------------------------------------------------------

#Load training data as st  
training_points<-st_read(system.file('extdata', 'TrainingPoints.shp', package = 'wetlandmapR'))

#Load BEC zones from bcmaps as AOIs
aoi_polys<-bec() %>% st_crop(training_points)


aoi_polys<-as_Spatial(aoi_polys)


attributed_csv <- file.path(out_dir,"train_pnts_attributed.csv")

#Attribute training points with values at intersection with co-variate layers
grid_values_at_sp(x=raster_stack,
                  y=training_points,
                  filename = attributed_csv,
                  aoi = aoi_polys)



#------------------------------------------------------------------------------
# Setup predictor list and raster LUT...
#------------------------------------------------------------------------------

#Read in rastLUT table as data.frame 
rastLUT <- read.csv(file.path(out_dir,'rastLUT.csv'),
                    header = FALSE,
                    stringsAsFactors = FALSE)

#Get the list of co-variate predictor names (2nd column)
predList <- rastLUT[,2]


#Create an output directory within 'out_dir'
dir.create(file.path(out_dir,"output"))


#------------------------------------------------------------------------------
# Run model and diagnostics...
#------------------------------------------------------------------------------

#Fit a random forest model 
model.out <- wetland_model(qdatafn = attributed_csv,
                           model.type = "RF",
                           model.folder = file.path(out_dir,"output"),
                           unique.rowname = "OBJECTID",
                           predList = predList,
                           predFactor = c('SINKS'),
                           response.name = "T_W_Class",
                           response.type = "categorical",
                           seed = 44,
                           response.target = as.vector(unique(training_points$T_W_Class)),
                           aoi.col = "ZONE")

#------------------------------------------------------------------------------
# Create map(s) from model...
#------------------------------------------------------------------------------

#Map random forest results 
wetland_map(model.out = model.out,
            model.folder = file.path(out_dir,"output"),
            rastLUTfn = rastLUT,
            aoi = aoi_polys,
            aoi.col = "ZONE")



