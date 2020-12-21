# wetlandmapR
R package for mapping wetland ecosystems using data driven R statistical methods like Random Forests and open source GIS.

## Introduction
This package (in development) provides tools for running the `ModelMap::model.build` and `ModelMap::model.mapmake` R functions, specifically for modelling and mapping wetland ecosystems. Additional functions help generate the necessary input training data and raster look up table inputs.

Wetland models can be run using area of interest polygons, restricting output to specific drainage basins, for example.

wetlandmapR depends on RSAGA for some raster processing. RSAGA depends on [SAGA GIS](http://www.saga-gis.org/en/index.html) being installed and accessible on your computer. Please see the RSAGA documentation for instructions on how to do this. Optionally, if the user wishes to attribute upstream basin statistics to provided pour points [GRASS-GIS](https://grass.osgeo.org/) version 7.6.0 or higher must also be installed in addition to the [rgrass7](https://cran.r-project.org/web/packages/rgrass7/index.html) R package. 

## Functions
### create_dem_products
Creates raster derivatives (products) from an input Digital Elevation Model (DEM) using SAGA-GIS.

### stack_rasters
Aligns input raster(s) to a target raster so that extent, cell size, and cell origin are the same, returning a RasterStack object.

### grid_values_at_sp
Adds cell values from a Raster object as attributes to a SpatialPoints object.

### wetland_model
This function runs `ModelMap::model.build` to build a wetland model using training data attributed with predictor values.

### wetland_map
This function runs `ModelMap::model.mapmake` to generate raster prediction surfaces using model output from `wetland_model`.

### set_grass_env
This function initializes a GRASS-GIS environment using the [rgrass7](https://cran.r-project.org/web/packages/rgrass7/index.html) package in order to calculate upstream basin statistics of provided pour points.

### run_basin_stats
This function assumes `set_grass_env` has been called. This function attributes upstream basin statistics calculated from provided input raster layers for specified pour point locations. 

## Installation
Get the latest version from GitHub with:
```r
devtools::install_github("bcgov/wetlandmapR")
```

## Or with Docker 
Download a Docker image based on rocker/geospatial with all the required dependencies and packages pre-installed with:
```bash
docker pull huntgdok/geospat:4.0.3
```
The image can then be run by passing the command:
```bash
docker run -e PASSWORD=URPassword -p 8787:8787 --rm huntgdok/geospat:4.0.3 
``` 
Where `URPassword` is any password of your choice, and username `rstudio`. The running container can be viewed by passing [localhost](http://localhost:8787/) to your browser. Be sure to copy all outputs locally before exiting as all data will be lost.  

## Examples
See the example code in [wetlandmapR_example.Rmd](https://github.com/HunterGleason/wetlandmapR/blob/hg_wetlandmapR/vignettes/wetlandmapR_example.Rmd) for how to use the functions in this package together for mapping wetlands.
