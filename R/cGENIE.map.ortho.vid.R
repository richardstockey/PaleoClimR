#' Create an Orthographic Projection Video of cGENIE Map
#'
#' This function generates a video of orthographic projections of cGENIE map data by rotating the longitude.
#'
#' @param experiment Character. The name of the experiment.
#' @param var Character. The variable to be plotted.
#' @param depth.level Numeric. The depth level to be used. Default is 2.
#' @param dims Numeric. The dimensions to be used. Default is 3.
#' @param year Character or Numeric. The year to be used. Default is "default".
#' @param unit.factor Numeric. A factor to convert units. Default is NULL.
#' @param min.value Numeric. The minimum value for the color scale. Default is NULL.
#' @param max.value Numeric. The maximum value for the color scale. Default is NULL.
#' @param intervals Numeric. The intervals for the color scale. Default is NULL.
#' @param continents.outlined Logical. Whether to outline continents. Default is TRUE.
#' @param line.thickness Numeric. The thickness of the outline lines. Default is 0.5.
#' @param line.colour Character. The color of the outline lines. Default is "grey20".
#' @param scale.label Character. The label for the color scale. Default is NULL.
#' @param model Character. The model to be used. Default is "biogem".
#' @param palette_name Character. The name of the color palette to be used. Default is `pals::parula(1000)`.
#' @param darkmode Logical. Whether to use dark mode. Default is FALSE.
#' @param background.color Character. The background color. Default is "black".
#' @param text.color Character. The text color. Default is "white".
#' @param framerate Numeric. The frame rate of the video. Default is 10.
#' @param lon_steps Numeric. The steps for longitude rotation. Default is 1.
#' @param output_file Character. The name of the output video file. Default is "output.mp4".
#'
#' @return None. The function creates a video file as specified by `output_file`.
#
#' @import av
#' @import progress
#' @export

cGENIE.map.ortho.vid <- function(experiment, var, depth.level = 2, dims = 3, year = "default", unit.factor = NULL,
         min.value = NULL, max.value = NULL, intervals = NULL, continents.outlined = TRUE,
         line.thickness = 0.5, line.colour = "white", scale.label = NULL, model = "biogem",
         palette_name = pals::parula(1000), darkmode = TRUE,
         background.color = "black", text.color = "white", framerate = 10, lon_steps = 1,
         output_file = "output.mp4") {

  frames <- list()
  lon_seq <- seq(0, 360, by = lon_steps)
  pb <- progress_bar$new(total = length(lon_seq), format = "[:bar] :percent :eta")

  for (lon in lon_seq) {
  plot <- cGENIE.map.ortho(
    experiment = experiment,
    dims = dims,
    var = var,
    depth.level = depth.level,
    projection = paste0("+proj=ortho +lat_0=0 +lon_0=", lon),
    continents.outlined = continents.outlined,
    darkmode = darkmode,
    year = year,
    unit.factor = unit.factor,
    min.value = min.value,
    max.value = max.value,
    intervals = intervals,
    line.thickness = line.thickness,
    line.colour = line.colour,
    scale.label = scale.label,
    model = model,
    palette_name = palette_name,
    background.color = background.color,
    text.color = text.color
  )
  frames[[length(frames) + 1]] <- plot
  pb$tick()
  }

  # Save frames as images
  img_files <- sapply(seq_along(frames), function(i) {
  img_file <- sprintf("frame_%03d.png", i)
  ggsave(img_file, plot = frames[[i]], width = 8, height = 6)
  img_file
  })

  # Create video from images
  av::av_encode_video(img_files, output = output_file, framerate = framerate)

  # Clean up image files
  file.remove(img_files)
}

# Example usage:
# cGENIE.map.ortho.vid(experiment = "exp1", var = "ocn_O2", framerate = 10, lon_steps = 1, output_file = "output.mp4")
