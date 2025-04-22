#' Generate a Map from NetCDF File
#'
#' This function creates a map visualization from a given NetCDF file. The map can be customized with various parameters including value ranges, intervals, and projection types.
#'
#' @param map Character. Path to the NetCDF file.
#' @param min.value Numeric. Minimum value for the color scale. Default is -6000.
#' @param max.value Numeric. Maximum value for the color scale. Default is 6000.
#' @param intervals Numeric. Intervals for the color scale. Default is 2000.
#' @param continents.outlined Logical. Whether to outline continents. (Parameter not used in the function body)
#' @param scale.label Character. Label for the color scale.
#' @param scale Character. Color scale to use. Default is "viridis".
#' @param projection Character. Projection type for the map. Default is 'ESRI:54012'.
#' @param darkmode Logical. Whether to enable dark mode. Default is FALSE.
#' @param bg.col Character. Background color for the map. Default is "black" when darkmode is TRUE.
#' @param fg.col Character. Foreground color for the map. Default is "white" when darkmode is TRUE.
#'
#' @return A ggplot object representing the map.
#'
#' @import RNetCDF
#' @import dplyr
#' @import sf
#' @import sp
#' @import ggspatial
#' @import reshape2
#' @import ggplot2
#'
#'
#' @export
scotese.map <- function(map, # netcdf file
       min.value = -6000,
       max.value = 6000,
       intervals = 2000,
       continents.outlined,
       scale.label,
       scale = "viridis",
       projection = 'ESRI:54012',
       darkmode = FALSE,
       bg.col = ifelse(darkmode, "black", "white"),
       fg.col = ifelse(darkmode, "white", "black")){
         # Open the NetCDF file
         nc <- RNetCDF::open.nc(paste0(map))

         # Extract latitude and longitude variables from the NetCDF file
         lat <- RNetCDF::var.get.nc(nc, "lat") # units: degrees
         lat.edges <- c(lat - mean(diff(lat)/2), lat[length(lat)] + mean(diff(lat)/2)) # Calculate latitude edges for grid cells
         lon <- RNetCDF::var.get.nc(nc, "lon") # units: degrees
         lon.edges <- c(lon - mean(diff(lon)/2), lon[length(lon)] + mean(diff(lon)/2)) # Calculate longitude edges for grid cells
         z <- RNetCDF::var.get.nc(nc, "z") # units: metres

         # Generate a dataframe with the extracted variables
         df <- as.data.frame(cbind(
            rep(lon, times = length(lat), each = 1),
            rep(lon.edges[1:(length(lon.edges)-1)], times = length(lat), each = 1),
            rep(lon.edges[2:(length(lon.edges))], times = length(lat), each = 1),
            rep(lat, times = 1, each = length(lon)),
            rep(lat.edges[1:(length(lat.edges)-1)], times = 1, each = length(lon)),
            rep(lat.edges[2:(length(lat.edges))], times = 1, each = length(lon)),
            as.data.frame(reshape2::melt(z)$value)))

         # Rename dataframe columns
         names(df) <- c("lon.mid",
             "lon.min",
             "lon.max",
             "lat.mid",
             "lat.min",
             "lat.max",
             "z"
         )

         # Filter out unrealistic latitude and longitude values
         df <- dplyr::filter(df,
             lon.max < 180,
             lon.min > -180,
             lat.max < 90,
             lat.min > -90
             )

         # Initialize lists to store polygons and their names
         poly.list <- list()
         poly.names.list <- list()

         # Loop through each row in the dataframe to create polygons
         for(poly in 1:(nrow(df))){
            polygon.code <- sp::Polygon(cbind(
               c(df$lon.min[poly], df$lon.max[poly], df$lon.max[poly], df$lon.min[poly]),
               c(df$lat.min[poly], df$lat.min[poly], df$lat.max[poly], df$lat.max[poly])))
            assign(paste0("Polygon_", poly), polygon.code)

            polygons.code <- sp::Polygons(list(polygon.code), paste0("p",poly))
            assign(paste0("Polygons_", poly), polygons.code)

            poly.list <- append(poly.list, polygons.code)
            poly.names.list <- append(poly.names.list, paste0("p",poly))
         }

         # Create SpatialPolygons object from the list of polygons
         SpP <- sp::SpatialPolygons(poly.list)

         # Create a dataframe for the polygon attributes
         attr <- data.frame(var = df$z, row.names = paste(poly.names.list))

         # Create a SpatialPolygonsDataFrame object
         SpDf <- sp::SpatialPolygonsDataFrame(SpP, attr)

         # Convert the SpatialPolygonsDataFrame to an sf object
         SpDfSf <- sf::st_as_sf(SpDf)
         sf::st_crs(SpDfSf) = '+proj=longlat +ellps=sphere' # Set the coordinate reference system

         # Create an outline of the map using a framing line
         l1 <- cbind(c(-180, 180, rep(180, 1801), 180, -180, rep(-180, 1801), -180), c(-90, -90, seq(-90,90,0.1),  90, 90, seq(90,-90,-0.1), -90))
         L1 <- sp::Polygon(l1)
         Ls1 <- sp::Polygons(list(L1), ID="a")
         SLs1 <-  sp::SpatialPolygons(list(Ls1))

         # Create a dataframe for the outline attributes
         df1 <- data.frame(rep(2,1), row.names = rep("a",  1))
         names(df1)[1] <- "var"
         SLs1df = sp::SpatialPolygonsDataFrame(SLs1, data = df1)
         SLs1dfSf <- sf::st_as_sf(SLs1df)
         sf::st_crs(SLs1dfSf) = '+proj=longlat +ellps=sphere' # Set the coordinate reference system

         # Create the map plot using ggplot2
         mapplot <- ggplot2::ggplot() +
            ggplot2::geom_sf(data = SpDfSf %>% sf::st_transform(projection), ggplot2::aes(geometry = geometry, fill=var), color = NA, linewidth=10, linetype=0) + # Transform and plot the main data
            ggplot2::geom_sf(data = SLs1dfSf %>% sf::st_transform(projection), ggplot2::aes(geometry = geometry), fill=NA, color = "grey5", linewidth=0.9) + # Transform and plot the outline
            ggplot2::scale_fill_binned(type = scale,
               guide = ggplot2::guide_colorbar(title.position = "top",
                         barwidth = 12,
                         barheight = 1,
                         raster = FALSE,
                         frame.colour = "grey6",
                         frame.linewidth = 2/.pt,
                         frame.linetype = 1,
                         ticks = TRUE,
                         ticks.colour = "grey6",
                         ticks.linewidth = 2/.pt),
               breaks = seq(min.value, max.value, intervals),
               limits=c(min.value, max.value)) + # Set the color scale
            ggplot2::theme_minimal(base_family = "Arial", base_size = 15) + # Use a minimal theme
            ggplot2::theme(
               legend.position="bottom",
               plot.background = ggplot2::element_rect(fill = bg.col, color = NA),
               panel.background = ggplot2::element_rect(fill = bg.col, color = NA),
               panel.grid.major = ggplot2::element_line(color = fg.col),
               panel.grid.minor = ggplot2::element_line(color = fg.col),
               axis.text = ggplot2::element_text(color = fg.col),
               axis.title = ggplot2::element_text(color = fg.col),
               legend.text = ggplot2::element_text(color = fg.col),
               legend.title = ggplot2::element_text(color = fg.col)
            ) + # Customize theme for dark mode
            ggplot2::labs(fill = scale.label) # Set the label for the color scale

         # Return the map plot
         mapplot
}
