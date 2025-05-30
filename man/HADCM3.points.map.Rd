% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HADCM3.points.map.R,
%   R/HADCM3.points.map.benthic.R
\name{HADCM3.points.map}
\alias{HADCM3.points.map}
\title{HADCM3.map with Points Matching}
\usage{
HADCM3.points.map(
  var,
  file,
  experiment,
  depth.level = 1,
  dims = 3,
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
  na.colour = "grey80",
  coord.dat = NULL,
  lat.name = "p_lat",
  lng.name = "p_lng",
  darkmode = FALSE,
  bg.colour = "black",
  fg.colour = "white"
)

HADCM3.points.map(
  var,
  file,
  experiment,
  depth.level = 1,
  dims = 3,
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
  na.colour = "grey80",
  coord.dat = NULL,
  lat.name = "p_lat",
  lng.name = "p_lng",
  darkmode = FALSE,
  bg.colour = "black",
  fg.colour = "white"
)
}
\arguments{
\item{var}{Character. The variable to be extracted from the netCDF file.}

\item{file}{Character. The name of the netCDF file (without extension).}

\item{experiment}{Character. The path to the experiment directory containing the netCDF file.}

\item{depth.level}{Numeric. The depth level to be extracted for 3D netCDF files. Default is 1.}

\item{dims}{Numeric. The number of dimensions in the netCDF file (2 or 3). Default is 3.}

\item{min.value}{Numeric. The minimum value for the color scale.}

\item{max.value}{Numeric. The maximum value for the color scale.}

\item{intervals}{Numeric. The intervals for the color scale.}

\item{continents.outlined}{Logical. Whether to outline continents. (Not used in the current implementation)}

\item{scale.label}{Character. The label for the color scale.}

\item{unit.factor}{Numeric. A factor to multiply the variable values by. Default is 1.}

\item{time.present}{Logical. Whether the netCDF file includes a time dimension. Default is FALSE.}

\item{projection}{Character. The projection to be used for the map. Default is 'ESRI:54012'.}

\item{calcs}{Logical. Whether to perform calculations to generate the spatial polygons. Default is TRUE.}

\item{plot}{Logical. Whether to generate and return the plot. Default is TRUE.}

\item{palette_name}{Function. The color palette function to be used for the map. Default is \code{pals::parula(1000)}.}

\item{na.colour}{Character. The color to be used for NA values. Default is "grey80".}

\item{coord.dat}{Data frame. Any data frame with the lat long column names assigned - cGENIE data will be added to this and returned.}

\item{lat.name}{Character. The name of the latitude column in \code{coord.dat}. Default is "p_lat".}

\item{lng.name}{Character. The name of the longitude column in \code{coord.dat}. Default is "p_lng".}

\item{darkmode}{Logical. Whether to use dark mode for the plot. Default is FALSE.}

\item{bg.colour}{Character. Background color for the plot in dark mode. Default is "black".}

\item{fg.colour}{Character. Foreground color for the plot in dark mode. Default is "white".}
}
\value{
Returns a ggplot object.

If \code{plot} is TRUE, returns a ggplot object. If \code{plot} is FALSE and \code{calcs} is TRUE, returns a SpatialPolygonsDataFrame. If \code{plot} is FALSE and \code{calcs} is FALSE, returns the input \code{polygons}.
}
\description{
This function generates maps from imported .nc files, such as those from Valdes et al. 2021. It can handle both 2D and 3D netCDF files and provides options for various customizations including projections, color palettes, and more.
Additionally, it matches and plots specific points from a provided data frame.

This function generates maps from imported .nc files, such as those from Valdes et al. 2021. It can handle both 2D and 3D netCDF files and provides options for various customizations including projections, color palettes, and more.
Additionally, it matches and plots specific points from a provided data frame.
}
