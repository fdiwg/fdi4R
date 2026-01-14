#' @name create_UN_country_tmap
#' @title create_UN_country_tmap
#' @description Creates a UN country tmap
#' 
#' @param iso3 country code
#' @param preview preview as dynamic map
#' @return an object of class \link[tmap]{tmap}
#' 
#' @author Arturo Muñoz Albero
#' @export
#'
create_UN_country_tmap <- function(iso3, preview = FALSE){
  lines = fdi4R::un_boundaries
  polygons = fdi4R::un_countries
  
  # Filter lines
  filtered <- lines[lines$TYPE != 6 & (grepl(iso3, lines$ISO3_CNT1) | grepl(iso3, lines$ISO3_CNT2)), ]
  
  # Split by type
  solid_lines <- filtered[!filtered$TYPE %in% c(3, 4, 9), ]
  dashed_lines <- filtered[filtered$TYPE %in% c(2, 3, 4), ]
  dotted_lines <- filtered[filtered$TYPE %in% c(8, 9), ]
  
  # polygon for selected country (light grey, no border)
  poly_sel <- polygons[polygons$ISO_3 == iso3, ]
  
  # Start with empty map
  map <- NULL
  if(preview) tmap_mode("view")
  
  # Add layers only if not empty
  if (nrow(poly_sel) > 0) {
    map <- map + tm_shape(poly_sel) +
      tm_polygons(col = "grey", border.col = NA, alpha = 0.6)
  }
  if (nrow(solid_lines) > 0) {
    map <- map + tm_shape(solid_lines) + tm_lines(lwd = 1.5, lty = "solid", col = "black")
  }
  if (nrow(dashed_lines) > 0) {
    map <- map + tm_shape(dashed_lines) + tm_lines(lwd = 1.5, lty = "dashed", col = "black")
  }
  if (nrow(dotted_lines) > 0) {
    map <- map + tm_shape(dotted_lines) + tm_lines(lwd = 1.5, lty = "dotted", col = "black")
  }
  
  return(map)
}