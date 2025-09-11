#' @title convert_to_CWP_grid
#' @description
#'Function copied from iotc_core_gis_cwp_utils (https://github.com/iotc-secretariat/iotc-lib-core-gis-cwp/blob/master/R/iotc_core_gis_cwp_utils.R)
#'Converts a pair of decimal coordinate into a CWP grid code for a specific grid type
#'@param lon Longitude (decimal coordinates)
#'@param lat Latitude (decimal coordinates)
#'@param grid_type_code The type of CWP grid (one among \code{\link{grid_1x1}}, \code{\link{grid_5x5}}, \code{\link{grid_10x10}}, \code{\link{grid_10x20}}, \code{\link{grid_20x20}} and \code{\link{grid_30x30}})
#'@return The CWP grid code for the provided coordinates and grid type
#'@export
#'@examples convert_to_CWP_grid(20, -10, grid_1x1)
convert_to_CWP_grid = function(lon, lat, grid_type_code = grid_5x5) {
  q = NA

  if     (lon >=0 && lat >=0) q = 1
  else if(lon >=0 && lat < 0) q = 2
  else if(lon < 0 && lat < 0) q = 3
  else if(lon < 0 && lat >=0) q = 4

  lat = floor(abs(lat))
  lon = floor(abs(lon))

  latS = 1
  lonS = 1

  grid = grid_char_1x1

  if     (grid_type_code == grid_5x5)   { lonS = latS =  5;     grid = grid_char_5x5 }
  else if(grid_type_code == grid_10x10) { lonS = latS = 10;     grid = grid_char_10x10 }
  else if(grid_type_code == grid_20x20) { lonS = latS = 20;     grid = grid_char_20x20 }
  else if(grid_type_code == grid_30x30) { lonS = latS = 30;     grid = grid_char_30x30 }
  else if(grid_type_code == grid_10x20) { lonS = 20; latS = 10; grid = grid_char_10x20 }

  lat = floor(lat / latS) * latS
  lon = floor(lon / lonS) * lonS

  return(paste0(grid, q, stri_pad(lat, 2, pad = "0"), stri_pad(lon, 3, pad = "0")))
}
