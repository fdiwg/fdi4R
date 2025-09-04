#' @title convert_CWP_grid
#' @description
#'Function copied from iotc_core_gis_cwp_utils (https://github.com/iotc-secretariat/iotc-lib-core-gis-cwp/blob/master/R/iotc_core_gis_cwp_utils.R)
#'Converts a CWP grid code into another CWP grid code of a given type
#'@param grid_code A CWP grid code
#'@param grid_type_code The type of CWP grid (one among \code{\link{grid_1x1}}, \code{\link{grid_5x5}}, \code{\link{grid_10x10}}, \code{\link{grid_10x20}}, \code{\link{grid_20x20}} and \code{\link{grid_30x30}})
#'@return The CWP grid code for the grid of type \code{grid_type_code} that contains the main corner of the original grid
#'@export
#'@examples convert_CWP_grid("5201123", grid_5x5)
#'@examples convert_CWP_grid("6205125", grid_1x1)
convert_CWP_grid = function(grid_code, target_grid_type_code = grid_1x1) {
  q = as.integer(substr(grid_code, 2, 2))

  qLon = qLat = 1

  if     (q == 2) { qLon =  1; qLat = -1 }
  else if(q == 3) { qLon = -1; qLat = -1 }
  else if(q == 4) { qLon = -1; qLat =  1 }

  lat = qLat * as.integer(substr(grid_code, 3, 4))
  lon = qLon * as.integer(substr(grid_code, 5, 7))

  return (convert_to_CWP_grid(lon, lat, target_grid_type_code))
}
