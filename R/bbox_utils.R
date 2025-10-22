#' @name bbox_to_sf
#' @title bbox_to_sf
#' @description Creates a \pkg{sf} object out of a bounding box
#' 
#' @param xmin xmin
#' @param ymin ymin
#' @param xmax xmax
#' @param ymax ymax
#' @param crs Defaut is 4326
#' @return an object of class \code{sf}
#' 
#' @author Emmanuel Blondel \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
bbox_to_sf <- function(xmin, ymin, xmax, ymax, crs = 4326){
  pts = matrix(c(
    xmin, ymin,
    xmin, ymax,
    xmax, ymax,
    xmax, ymin,
    xmin, ymin
  ), ncol=2, byrow=TRUE)
  poly = sf::st_sf(geom = sf::st_sfc(sf::st_polygon(list(pts)), crs = crs))
  return(poly)
}
