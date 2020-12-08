
#Load dependencies 
library(wetlandmapR)
library(rgeos)
library(rgdal)
library(tidyverse)
library(bcmaps)
library(raster)
library(sf)
library(doParallel)
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

head(as.data.frame(readr::read_csv(attributed_csv)))

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
                           aoi.col = "BGC_ZONE")

#------------------------------------------------------------------------------
# Create map(s) from model...
#------------------------------------------------------------------------------

#Map random forest results 
wetland_map(model.out = model.out,
            model.folder = file.path(out_dir,"output"),
            rastLUTfn = rastLUT,
            aoi = aoi_polys,
            aoi.col = "ZONE")

#------------------------------------------------------------------------------
# Set up a GRASS-GIS Environment for attributing upstream basin stats...
#------------------------------------------------------------------------------

#!!Change for your current GRASS-GIS installation!! 
gisbase<-'/usr/lib/grass76/'

#Get list of raster layers and names 
lyr_lst<-list()
lyr_names<-c()
for(lyr in  c(1:dim(raster_stack)[3]))
{
  lyr_lst[[lyr]]<-raster(rastLUT[lyr,1])
  lyr_names[lyr]<-rastLUT[lyr,2]
}

#Write the raster objects to a GRASS-GIS environment, derive streams and create GRASS DEM derivatives "r.watershed" 
set_grass_env(gisbase=gisbase,
              DEM=raster(target_dem),
              lyr_list=lyr_lst,
              lyr_names=lyr_names,
              acc_thresh = 1000)

#Create a data frame of pour points for purpose of example 
pour_pnts<-as.data.frame(raster(target_dem),xy=T)
pour_pnts<-pour_pnts[complete.cases(pour_pnts),]
colnames(pour_pnts)<-c('X','Y','UID')
pour_pnts$UID<-c(1:nrow(pour_pnts))

#Take a sample, processing can take a long time when n is large 
pour_pnts<-pour_pnts[sample(c(1:nrow(pour_pnts)),50),]

cores<-parallel::detectCores()-1


#For the random pour points calculate upstream basin mean and standard deviation for elevation and topographic wetness.
basin_stats<-run_basin_stats(pour_pnts = pour_pnts,
                             covar_rast = c('ELEV','TOPOWET','ELEV','TOPOWET'),
                             stat_vect = c('MEAN','MEAN','STDDEV','STDDEV'),
                             procs = cores)

#View results, columns are defined by 'covar_rast' and 'stat_vec' parameters.
head(basin_stats)
