# wetlandmapR
R package for mapping wetland ecosystems using data driven R statistical methods like Random Forests and open source GIS.

## Introduction
This package (in development) provides tools for running the `ModelMap::model.build` and `ModelMap::model.mapmake` R functions, specifically for modelling and mapping wetland ecosystems. Additional functions help generate the necessary input training data and raster look up table inputs.

Wetland models can be run using area of interest polygons, restricting output to specific drainage basins, for example.

wetlandmapR depends on RSAGA for some raster processing. RSAGA depends on [SAGA GIS](http://www.saga-gis.org/en/index.html) being installed and accessible on your computer. Please see the RSAGA documentation for instructions on how to do this.

## Functions
### create_dem_products
Creates raster derivitives (products) from an input Digital Elevation Model (DEM) using SAGA-GIS.

### stack_rasters
Aligns input raster(s) to a target raster so that extent, cell size, and cell origin are the same, returning a RasterStack object.

### grid_values_at_sp
Adds cell values from a Raster object as attributes to a SpatialPoints object.

### wetland_model
This function runs `ModelMap::model.build` to build a wetland model using training data attributed with predictor values.

### wetland_map
This function runs `ModelMap::model.mapmake` to generate raster prediction surfaces using model output from `wetland_model`.

## Installation
Get the latest version from GitHub with:
```r
devtools::install_github("bcgov/wetlandmapR", dependencies = TRUE)
```

## Examples
See the example code in [wetlandmapR_example.R](R/wetlandmapR_example.R) for how to use the functions in this package together for mapping wetlands.
