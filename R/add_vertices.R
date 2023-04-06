#' @name add_vertices
#' @title add_vertices
#' @description Densify a spatial polygon by adding vertices
#' 
#' @param sf an object of class \code{sf}
#' @param each the step value to use to create vertices
#' @param parallel run in parallel
#' @param ... parallel options
#' @return an object of class \code{sf}
#' 
#' @author Emmanuel Blondel \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
add_vertices <- function(sf, each = 0.1, parallel = FALSE, ...){
  
  applyHandler <- if(parallel) parallel::mclapply else lapply
  
  sf <- do.call("rbind", applyHandler(1:nrow(sf), function(i){
    p = sf[i,]
    coords <- sf::st_coordinates(p)
    newcoords <- do.call("rbind",lapply(1:(nrow(coords)-1), function(i){
      i_coords <- coords[i:(i+1),]
      out_coords <- data.frame(
        x = seq(from = i_coords[1L,1L], to = i_coords[2L,1L],
                by = ifelse(i_coords[1L,1L]<=i_coords[2L,1L], each, -each)),
        y = seq(from = i_coords[1L,2L], to = i_coords[2L,2L],
                by = ifelse(i_coords[1L,2L]<=i_coords[2L,2L], each, -each))
      )
      if(i<(nrow(coords)-1)) out_coords <- out_coords[,-nrow(out_coords)]
      out_coords <- as.matrix(out_coords)
      return(out_coords)
    }))
    p$geometry <- sf::st_polygon(list(newcoords)) %>% sf::st_sfc(crs = 4326)
    return(p)
  }, ...))
  return(sf)
}