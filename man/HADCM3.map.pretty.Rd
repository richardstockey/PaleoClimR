% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HADCM3.map.pretty.R
\name{HADCM3.map.pretty}
\alias{HADCM3.map.pretty}
\title{HADCM3.map.pretty}
\usage{
HADCM3.map.pretty(
  experiment,
  depth.level = 1,
  dims = 3,
  land.opt = "default",
  min.value,
  max.value,
  intervals,
  continents.outlined,
  scale.label,
  unit.factor = 1,
  time.present = FALSE,
  scale = "viridis",
  projection = "ESRI:54012",
  calcs = TRUE,
  plot = TRUE,
  for.app = FALSE,
  polygons
)
}
\arguments{
\item{experiment}{A string specifying the path to the experiment directory.}

\item{depth.level}{An integer specifying the depth level to be visualized. Default is 1.}

\item{dims}{An integer specifying the dimensions of the netCDF file being read in. Default is 3.}

\item{land.opt}{A string specifying land options. Default is "default".}

\item{min.value}{Minimum value for the color scale.}

\item{max.value}{Maximum value for the color scale.}

\item{intervals}{Intervals for the color scale.}

\item{continents.outlined}{Logical indicating whether continents should be outlined.}

\item{scale.label}{Label for the color scale.}

\item{unit.factor}{A numeric factor to adjust units. Default is 1.}

\item{time.present}{Logical indicating whether time is present in the data. Default is FALSE.}

\item{scale}{A string specifying the color scale to use. Default is "viridis".}

\item{projection}{A string specifying the map projection to use. Default is 'ESRI:54012'.}

\item{calcs}{Logical indicating whether calculations should be performed. Default is TRUE.}

\item{plot}{Logical indicating whether to plot the map. Default is TRUE.}

\item{for.app}{Logical indicating whether the map is for an app. Default is FALSE.}

\item{polygons}{Spatial polygons data frame if calcs is FALSE.}
}
\value{
If plot is FALSE, returns a spatial polygons data frame. If plot is TRUE, returns a ggplot object.
}
\description{
This function generates visually appealing maps from imported .nc files, such as those from Valdes et al. 2021. It aims to include key information to visualize as much of the earth system as possible at one time.
}
