# import NetCDF from the following URL
# https://www.ngdc.noaa.gov/thredds/fileServer/global/ETOPO2022/60s/60s_bed_elev_netcdf/ETOPO_2022_v1_60s_N90W180_bed.nc
# Load necessary libraries
library(RNetCDF)  # For handling netCDF files
library(dplyr)    # For data manipulation
library(sf)       # For handling spatial data
library(sp)       # For spatial data classes and methods
library(reshape2) # For reshaping data
library(ggplot2)  # For plotting
library(pals)     # For color palettes
# Define the local file path to save the NetCDF file
local_file <- "/Users/rgs1e22/Downloads/ETOPO_2022_v1_60s_N90W180_bed.nc"

# Open the NetCDF file
nc <- open.nc(local_file)

# Extract latitude values and calculate edges
lat <- var.get.nc(nc, "lat") # units: degrees north
lat.edges <- c(lat - mean(diff(lat)/2), lat[length(lat)] + mean(diff(lat)/2)) # should work for any evenly spaced grid (although note we have values outside reality! removed later...)

# Extract longitude values and calculate edges
lon <- var.get.nc(nc, "lon") # units: degrees east
lon.edges <- c(lon - mean(diff(lon)/2), lon[length(lon)] + mean(diff(lon)/2)) # should work for any evenly spaced grid (although note we have values outside reality! removed later...)

# Extract the specified variable from the netCDF file
var.arr <- var.get.nc(nc, "z")

# Generate dataframe of 2D slice from 2D array
df <- as.data.frame(cbind(
  rep(lon, times = length(lat), each = 1),
  rep(lon.edges[1:(length(lon.edges)-1)], times = length(lat), each = 1),
  rep(lon.edges[2:(length(lon.edges))], times = length(lat), each = 1),
  rep(lat, times = 1, each = length(lon)),
  rep(lat.edges[1:(length(lat.edges)-1)], times = 1, each = length(lon)),
  rep(lat.edges[2:(length(lat.edges))], times = 1, each = length(lon)),
  as.data.frame(melt(var.arr))$value))

# Assign column names to the dataframe
names(df) <- c("lon.mid",
               "lon.min",
               "lon.max",
               "lat.mid",
               "lat.min",
               "lat.max",
               "var"
)

# Initialize lists to store polygons and their names
library(parallel)

# Define the number of cores to use
num_cores <- detectCores() - 1

# Function to create polygons
create_polygon <- function(poly) {
    polygon.code <- Polygon(cbind(
        c(df$lon.min[poly], df$lon.max[poly], df$lon.max[poly], df$lon.min[poly]),
        c(df$lat.min[poly], df$lat.min[poly], df$lat.max[poly], df$lat.max[poly])))
    polygons.code <- Polygons(list(polygon.code), paste0("p", poly))
    return(list(polygons.code, paste0("p", poly)))
}

# Use mclapply to parallelize the loop
results <- mclapply(1:nrow(df), create_polygon, mc.cores = num_cores)

# Extract polygons and names from the results
poly.list <- lapply(results, `[[`, 1)
poly.names.list <- lapply(results, `[[`, 2)

# Create SpatialPolygons object
SpP <- SpatialPolygons(poly.list)

# Create a dataframe for the attributes
attr <- data.frame(var = df$var, row.names = paste(poly.names.list))

# Create SpatialPolygonsDataFrame object
SpDf <- SpatialPolygonsDataFrame(SpP, attr)

# Convert to sf object and set CRS
SpDfSf <- st_as_sf(SpDf)
st_crs(SpDfSf) = '+proj=longlat +ellps=sphere'



## Outline of map using a framing line
l1 <- cbind(c(-180, 180, rep(180, 1801), 180, -180, rep(-180, 1801), -180), c(-90, -90, seq(-90,90,0.1),  90, 90, seq(90,-90,-0.1), -90))
L1 <- Polygon(l1)
Ls1 <- Polygons(list(L1), ID="a")
SLs1 <-  SpatialPolygons(list(Ls1))

df1 <- data.frame(rep(2,1), row.names = rep("a",  1))
names(df1)[1] <- "var"
SLs1df = SpatialPolygonsDataFrame(SLs1, data = df1)
SLs1dfSf <- st_as_sf(SLs1df)
st_crs(SLs1dfSf) = '+proj=longlat +ellps=sphere'
# Create the ggplot object for the map
map <- ggplot() +
  # Add the main spatial data layer, transforming the projection and setting the fill aesthetic
  geom_sf(data = SpDfSf %>% st_transform(projection), aes(geometry = geometry, fill = var * unit.factor), color = NA, linewidth = 10, linetype = 0) +
  # Add the outline of the map using the framing line, transforming the projection and setting the color
  geom_sf(data = SLs1dfSf %>% st_transform(projection), color = "grey5", linewidth = 0.9, fill = NA) +
  # Define the color scale for the fill aesthetic using the specified palette
  scale_fill_stepsn(colours = palette_name,
                    guide = guide_colorbar(title.position = "top",  # Position the title of the color bar at the top
                                           barwidth = 12,           # Set the width of the color bar
                                           barheight = 1,           # Set the height of the color bar
                                           raster = FALSE,          # Disable rasterization of the color bar
                                           frame.colour = "grey6",  # Set the frame color of the color bar
                                           frame.linewidth = 2 / .pt,  # Set the frame linewidth of the color bar
                                           frame.linetype = 1,      # Set the frame linetype of the color bar
                                           ticks = TRUE,            # Enable ticks on the color bar
                                           ticks.colour = "grey6",  # Set the color of the ticks
                                           ticks.linewidth = 2 / .pt),  # Set the linewidth of the ticks
                    breaks = seq(min.value, max.value, intervals),  # Define the breaks for the color scale
                    limits = c(min.value, max.value),  # Set the limits for the color scale
                    na.value = na.colour  # Set the color for NA values
  ) +
  # Apply a minimal theme to the plot
  theme_minimal() +
  # Position the legend at the bottom of the plot
  theme(legend.position = "bottom") +
  # Set the label for the fill aesthetic
  labs(fill = scale.label)

# Return the ggplot object
map
