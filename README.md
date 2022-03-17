# wetlandmapR
[![Lifecycle:Maturing](https://img.shields.io/badge/Lifecycle-Maturing-007EC6)](<Redirect-URL>)
R package for mapping wetland ecosystems using data driven R statistical methods like Random Forests and open source GIS.

## Introduction
This package (in development) provides tools for running the `ModelMap::model.build` and `ModelMap::model.mapmake` R functions, specifically for modeling and mapping wetland ecosystems. Additional functions help generate the necessary input training data and raster look up table inputs.


wetlandmapR depends on the package RSAGA for processing. RSAGA depends on [SAGA GIS](http://www.saga-gis.org/en/index.html) being installed and accessible on your computer. Please see the RSAGA documentation for instructions on how to do this.

## Functions
### create_dem_products
Creates raster derivatives (products) from an input Digital Elevation Model (DEM) using SAGA-GIS.

### stack_rasters
Aligns input raster(s) to a target raster so that extent, cell size, and cell origin are the same, returning a SpatRaster object.

### grid_values_at_sp
Adds cell values from a Raster object as attributes to a SpatVector object.

### wetland_model
This function runs `ModelMap::model.build` to build a wetland model using training data attributed with predictor values.

### wetland_map
This function runs `ModelMap::model.mapmake` to generate raster prediction surfaces using model output from `wetland_model`.

### raster_to_clean_polygon
This function reads in classified raster values and creates polygons from contiguous cell values, these polygons are then simplified.

### hydro_condition_dem
This function hydrologicaly conditions an input digital elevation model using RSAGA.

### gen_upstream_basin
This function creates delineates an upstream basin provided a DEM and pour point using RSAGA, outputting a binary grid.

### get_basin_stats
This function computes upstream basin statistics provided input grids and and a zonal basin grid.  

## Installation
Get the latest version from GitHub with:
```r
remotes::install_github("bcgov/wetlandmapR")
```

## Or with Docker 
Download a Docker image based on *rocker/geospatial:4.1.2* with all the required dependencies and packages pre-installed with:
```bash
docker pull huntgdok/wetlandmapr:latest
```
The image can then be run by passing the command:
```bash
docker run -e PASSWORD=URPassword -p 8787:8787 --rm huntgdok/wetlandmapr:latest 
``` 
Where `URPassword` is any password of your choice, and username `rstudio`. The running container can be viewed by passing [localhost](http://localhost:8787/) to your browser. Be sure to copy all outputs locally before exiting as all data will be lost.  

PLEASE NOTE: A Docker subscription is required if using the *wetlandmapR* Docker image for commercial use, see service agreement [Docker Subscription Service Agreement](https://www.docker.com/legal/docker-subscription-service-agreement?utm_campaign=2021-08-31-business-tier-launch&utm_medium=email&utm_source=mailgun&utm_content=service-agreement) for more information. 

## Examples
See the example vignette which describes [wetlandmapR_example](https://htmlpreview.github.io/?https://github.com/bcgov/wetlandmapR/blob/migrate_stars/vignettes/wetlandmapR_example.html) how to use the functions in this package together for mapping wetlands.
