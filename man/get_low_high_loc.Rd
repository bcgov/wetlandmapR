% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/upstrm_basin_stats.R
\name{get_low_high_loc}
\alias{get_low_high_loc}
\title{Provided a DEM (or any raster layer), function returns the XY location
of the minimum (or maximum) raster value within a provided polygon area
of interest.}
\usage{
get_low_high_loc(lyr, poly, low = TRUE)
}
\arguments{
\item{lyr}{Typically a digital elevation model, but can be any layer of type 'raster' or 'stars'.}

\item{poly}{An sf polygon object representing the area of interest in which to return the location 
of the minimum (or maximum) raster value.}

\item{low}{Should the location of the  lowest or highest raster value be returned, defaults to true.}
}
\value{
A numeric vector with he XY coordinated of the highest or lowest raster value in map units.
}
\description{
Provided a DEM (or any raster layer), function returns the XY location
of the minimum (or maximum) raster value within a provided polygon area
of interest.
}
