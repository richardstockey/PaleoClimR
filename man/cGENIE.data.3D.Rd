% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cGENIE.data.3D.R
\name{cGENIE.data.3D}
\alias{cGENIE.data.3D}
\title{Extract and Process 3D Data from cGENIE NetCDF Files}
\usage{
cGENIE.data.3D(var, experiment, year = "default", model = "biogem")
}
\arguments{
\item{var}{A string specifying the variable name to extract from the NetCDF file.
Examples include "ocn_temp", "ocn_O2", etc.}

\item{experiment}{A string specifying the path to the folder or file prefix
where the experiment's data is stored.}

\item{year}{A numeric value or the string "default" indicating the time step
to extract data for. If "default", the latest time step will be selected.}

\item{model}{A string indicating the model type (default is "biogem").
Currently supports the "biogem" model.}
}
\value{
A dataframe (\code{df.sum}) containing:
\itemize{
\item \code{lon.mid}: Midpoint of the longitude.
\item \code{lon.min}: Minimum longitude boundary.
\item \code{lon.max}: Maximum longitude boundary.
\item \code{lat.mid}: Midpoint of the latitude.
\item \code{lat.min}: Minimum latitude boundary.
\item \code{lat.max}: Maximum latitude boundary.
\item \code{var}: Extracted variable values (e.g., temperature, salinity).
\item \code{depth.level}: Depth level index (1 for surface, increasing with depth).
\item \code{depth}: Actual depth value in meters.
\item \code{depth.min}: Minimum depth boundary.
\item \code{depth.max}: Maximum depth boundary.
\item \code{lon.range}: Range of longitude for handling map boundaries.
}
}
\description{
This function extracts and processes 3D data from cGENIE model NetCDF files
based on latitude, longitude, depth, and time. It converts the 3D array into a
2D dataframe with geographical coordinates and depth levels, making the data
easier to visualize or further process.
}
\details{
\enumerate{
\item The function assumes a 3D NetCDF file (longitude, latitude, depth) is used.
\item Default projection adjustments are made for handling longitudes that cross
0 degrees.
\item The data is reshaped from a 3D array into a 2D dataframe for easier analysis.
}
}
