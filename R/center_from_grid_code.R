#' @title center_from_grid_code
#' @description
#'  Function copied from iotc_core_gis_cwp_utils (https://github.com/iotc-secretariat/iotc-lib-core-gis-cwp/blob/master/R/iotc_core_gis_cwp_utils.R)
#' Returns details about the center of a regular grid provided the grid code.
#' Details include:
#'     the center latitude / longitude (regardless of the fraction of ocean area in the grid)
#'     the grid width and height (in degrees)
#'
#' @param grid_code A grid code
#' @return A named vector containing the center latitude (\code{y}) and longitude (\code{x}) plus the grid
#' width (\code{size_lat}) and height (\code{size_lon}) in degrees
#' @examples
#' center_from_grid_code("5206066")
#' center_from_grid_code("6205065")
#' @export
center_from_grid_code = function(grid_code) {
  #Grid sizes by gridCode[1], i.e.:
  #1 -> 30x30
  #2 -> 10x20
  #3 -> 10x10
  #4 -> 20x20
  #5 ->  1x1
  #6 ->  5x5

  #Height and width (in degrees) by type of grid (i.e. gridCode[0]) sorted by lexicographical first char code
  #(1 = 30x30, 2 = 10x20, 3 = 10x10, 4 = 20x20, 5 = 1x1, 6 = 5x5)
  sizes_lat = c(30, 10, 10, 20, 1, 5)
  sizes_lon = c(30, 20, 10, 20, 1, 5)

  grid_code = toString(grid_code)

  size_code = as.numeric(substring(grid_code, 1, 1))

  quadrant = as.numeric(substring(grid_code, 2, 2))

  lat = as.numeric(substring(grid_code, 3, 4))
  lon = as.numeric(substring(grid_code, 5, 7))

  latitudes.quadrant  = c(  1, -1, -1,  1)
  longitudes.quadrant = c(  1,  1, -1,  1)

  lat = latitudes.quadrant[quadrant]  * ( lat + sizes_lat[size_code] / 2 );
  lon = longitudes.quadrant[quadrant] * ( lon + sizes_lon[size_code] / 2 );

  return (c(x = lon, y = lat, size_lat = sizes_lat[size_code], size_lon = sizes_lon[size_code]))
}
