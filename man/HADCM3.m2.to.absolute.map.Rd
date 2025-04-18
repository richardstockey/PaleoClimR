% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HADCM3.m2.to.absolute.map.R
\name{HADCM3.m2.to.absolute.map}
\alias{HADCM3.m2.to.absolute.map}
\title{HADCM3.m2.to.absolute.map}
\usage{
HADCM3.m2.to.absolute.map(
  var,
  file,
  experiment,
  depth.level = 1,
  dims = 2,
  min.value,
  max.value,
  intervals,
  continents.outlined,
  scale.label,
  unit.factor = 1,
  time.present = FALSE,
  projection = "ESRI:54012",
  calcs = TRUE,
  plot = TRUE,
  palette_name = pals::parula(1000),
  polygons,
  na.colour = "grey80"
)
}
\arguments{
\item{var}{Character. The name of the variable to extract from the NetCDF file.}

\item{file}{Character. The name of the NetCDF file (without extension).}

\item{experiment}{Character. The path to the experiment directory containing the NetCDF file.}

\item{depth.level}{Numeric. The depth level to extract (default is 1).}

\item{dims}{Numeric. The dimensions of the NetCDF file (default is 2).}

\item{min.value}{Numeric. The minimum value for the color scale.}

\item{max.value}{Numeric. The maximum value for the color scale.}

\item{intervals}{Numeric. The intervals for the color scale.}

\item{continents.outlined}{Logical. Whether to outline continents (default is FALSE).}

\item{scale.label}{Character. The label for the color scale.}

\item{unit.factor}{Numeric. A factor to scale the variable values (default is 1).}

\item{time.present}{Logical. Whether the time dimension is present in the NetCDF file (default is FALSE).}

\item{projection}{Character. The projection to use for the map (default is 'ESRI:54012').}

\item{calcs}{Logical. Whether to perform calculations (default is TRUE).}

\item{plot}{Logical. Whether to generate a plot (default is TRUE).}

\item{palette_name}{Character. The name of the color palette to use (default is pals::parula(1000)).}

\item{polygons}{List. A list of polygons to overlay on the map.}

\item{na.colour}{Character. The color to use for NA values (default is "grey80").}
}
\value{
A ggplot object representing the map.
}
\description{
This function is designed to upscale per area estimates of variables such as TRIFFID outputs to global values.
}
