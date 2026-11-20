#' Extract 2D Data Fields from 3D or 2D cGENIE netCDF Files containing only cells adjacent to land.
#'
#' @param var The variable name to extract from the .nc file.
#' @param experiment Directory containing the experiment's netCDF files.
#' @param depth.level Depth level to extract the data from. Set to NULL by default.
#' @param dims Number of dimensions in the netCDF file. Defaults to 3.
#' @param year Year to extract data for (default is "default", meaning the last time point).
#' @param model The model type; defaults to "biogem".
#' @param wrap.lon Logical; treat longitude as cyclic? Default \code{TRUE}.
#' @param digits Number of decimal places used when matching cell midpoints,
#'   to guard against floating point differences. Default 6.
#' @param keep.nas Logical, shall we keep in the var = NA values? Default \code{TRUE}.
#' @return A data frame containing the extracted 2D lat-lon data field with corresponding grid information.
#' @export
cGENIE.shelf.data <- function(var,
                        experiment,
                        depth.level = NULL,
                        dims = 3,
                        year = "default",
                        model = "biogem", 
                        wrap.lon = TRUE, 
                        digits = 6, 
                        keep.nas = TRUE){

  
  df <- cGENIE.data(var = var,
                    experiment = experiment,
                    depth.level = depth.level,
                    dims = dims,
                    year = year,
                    model = model)
  
  
  
  df.shelf.labelled <- cGENIE.adjacent.land(df = df, var = "var", 
                                            wrap.lon = wrap.lon, 
                                            digits = digits) 
    
  if(keep.nas == TRUE){
    shelf.df <- filter(df.shelf.labelled, (adj.land == "TRUE" | is.na(var)))
                       
  }else{
    shelf.df <- filter(df.shelf.labelled, adj.land == "TRUE")
    
  }
  return(shelf.df)

}
