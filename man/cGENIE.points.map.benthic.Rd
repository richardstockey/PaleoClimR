% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cGENIE.points.map.benthic.R
\name{cGENIE.points.map.benthic}
\alias{cGENIE.points.map.benthic}
\title{Generate Maps from cGENIE Model Output with Points Matching (for bottom water conditions)}
\usage{
cGENIE.points.map.benthic(
  var,
  experiment,
  dims = 3,
  depth.level = 1,
  year = "default",
  unit.factor = NULL,
  min.value = NULL,
  max.value = NULL,
  intervals = NULL,
  continents.outlined = TRUE,
  scale.label = NULL,
  model = "biogem",
  palette_name = pals::parula(1000),
  projection = "ESRI:54012",
  line.thickness = 1,
  coord.dat = NULL,
  lat.name = "p_lat",
  lng.name = "p_lng",
  darkmode = FALSE,
  bg.colour = "black",
  fg.colour = "white"
)
}
\arguments{
\item{var}{(character) The variable from the NetCDF file to be visualized (e.g., "ocn_temp", "ocn_sal", "ocn_O2").}

\item{experiment}{(character) The path or name of the experiment used to locate the NetCDF file.}

\item{dims}{(numeric) The dimensionality of the data (default is 3 for 3D; can be 2D or 3D).}

\item{depth.level}{(numeric) Depth layer to visualize (default is 1 for the surface layer). Note this is only for the map visualization, not for the points.}

\item{year}{(numeric or character) Time step to visualize (default uses the final time step if "default").}

\item{unit.factor}{(numeric) A scaling factor for the variable values (default is 1).}

\item{min.value}{(numeric) Minimum value for the color scale (used to set scale limits).}

\item{max.value}{(numeric) Maximum value for the color scale.}

\item{intervals}{(numeric) Step intervals for the color scale.}

\item{continents.outlined}{(logical) Logical value to control whether to outline continents.}

\item{scale.label}{(character) Label for the color bar.}

\item{model}{(character) The model type (default is 'biogem'; can be extended for other models).}

\item{palette_name}{(character) Color palette to be used for the plot (default is \code{pals::parula(1000)}).}

\item{projection}{(character) Map projection to use (default is ESRI:54012 for Equal Earth).}

\item{line.thickness}{(numeric) Thickness of the lines outlining continents (default is 1).}

\item{coord.dat}{(data.frame) Data frame with latitude and longitude columns to which cGENIE data will be added and returned.}

\item{lat.name}{(character) Name of the latitude column in \code{coord.dat} (default is "p_lat").}

\item{lng.name}{(character) Name of the longitude column in \code{coord.dat} (default is "p_lng").}

\item{darkmode}{(logical) Logical value to control whether to use dark mode (default is FALSE).}

\item{bg.colour}{(character) Background color for the map when in dark mode (default is "black").}

\item{fg.colour}{(character) Foreground color for the map when in dark mode (default is "white").}
}
\value{
A ggplot object representing the generated map with the specified variable visualized across geographical coordinates.
}
\description{
This function generates maps from imported .nc (NetCDF) files containing data from the cGENIE model outputs.
It can handle both 2D and 3D data, visualizing variables across specified depth levels and time steps.
Additionally, it matches and plots specific points from a provided data frame.
}
\details{
This function reads 2D or 3D data from cGENIE model output NetCDF files and produces a map visualization.
Default settings are defined for several commonly used variables, and users can specify their own scaling and color settings.
The function also matches specific points from a provided data frame and plots them on the map.
}
