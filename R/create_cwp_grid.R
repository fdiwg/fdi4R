#' @name create_cwp_grid
#' @title create_cwp_grid
#' @description Creates a CWP grid spatial object
#' 
#' @param size an integer code corresponding to the grid size (referred as code
#' A in the CWP Handbook)
#' @param res a string matching one of the accepted resolution values. Accepted 
#' resolutions values are '10min_x_10min', '20min_x_20min', '30min_x_30min',
#' '30min_x_1deg', '1deg_x_1deg', '5deg_x_5deg', '10deg_x_10deg', '20deg_x_20deg',
#' '30deg_x_30deg'"
#' @param xmin xmin of the output grid
#' @param ymin ymin of the output grid
#' @param xmax xmax of the output grid
#' @param ymax ymax of the output grid
#' @param parallel run in parallel
#' @param ... parallel options
#' @return an object of class \code{sf}
#' 
#' @references 
#'   CWP Handbook - https://www.fao.org/cwp-on-fishery-statistics/handbook/general-concepts/main-water-areas/fr/#c737133
#' 
#' @author Emmanuel Blondel \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
create_cwp_grid <- function(size = NULL, res = NULL,
                          xmin = NULL, ymin = NULL, xmax = NULL, ymax = NULL,
                          parallel = FALSE, ...){
  
  applyHandler <- if(parallel) parallel::mclapply else lapply
  
  #reference resolutions
  grids <- data.frame(
    size = c(1,2,3,4,5,6,7,8,9),
    lat = c(1/6, 1/3, 0.5, 0.5, 1, 5, 10, 20, 30),
    lon = c(1/6, 1/3, 0.5, 1, 1, 5, 10, 20, 30),
    res = c("10min_x_10min", "20min_x_20min","30min_x_30min", "30min_x_1deg",
            "1deg_x_1deg", "5deg_x_5deg", "10deg_x_10deg", "20deg_x_20deg", "30deg_x_30deg")
  )
  
  #bbbox
  xmin <- if(is.null(xmin)) -180 else xmin
  ymin <- if(is.null(ymin)) -90 else ymin
  xmax <- if(is.null(xmax)) 180 else xmax
  ymax <- if(is.null(ymax)) 90 else ymax
  
  #select grid resolution
  if(!is.null(size)){
    grid <- grids[grids$size == size,]
  }else{
    if(!is.null(res)){
      grid <- grids[grids$res == res,]
    }else{
      stop(sprintf("Please provide either the grid size (CWP A code) or the explicit resolution. Accepted resolutions values are %s",
                   paste(paste0("'",grids$res,"'"), collapse=", ")))
    }
  }
  
  #special case of 20deg resolution
  if(grid$size == 8){
    ymin = -80
    ymax = 80
  }
  
  r <- terra::rast(
    terra::ext(x = c(xmin, ymin, xmax,  ymax), xy = TRUE),
    nrow = length(seq(ymin,ymax, grid$lat))-1, 
    ncol=length(seq(xmin,xmax, grid$lon))-1, 
    crs = "epsg:4326"
  )    
  r[] <- 1:terra::ncell(r)
  sf <- r %>%
    terra::as.polygons() %>%
    sf::st_as_sf()
  
  #densify adding vertices each minute
  sf <- add_vertices(sf, each = 1/60, parallel = parallel, ...)
  
  #attributes (including grid coding)
  idx <- 0
  attrs <- do.call("rbind", applyHandler(1:nrow(sf), function(i){
    poly <- sf[i,]
    pt = suppressWarnings(sf::st_centroid(poly))
    labpt <- pt %>%
      sf::st_coordinates()
    quadrant <- paste0(ifelse(labpt[2]<0,"S","N"), ifelse(labpt[1]<0,"W","E"))
    quadrant_id <- switch(quadrant, "NE" = 1L, "SE" = 2L, "SW" = 3L, "NW" = 4L)
    corner_lon <- sprintf("%03.f", as.integer(min(abs(sf::st_bbox(poly)[c(1,3)]))))
    corner_lat <- sprintf("%02.f", as.integer(min(abs(sf::st_bbox(poly)[c(2,4)]))))
    gridcode <- paste0(grid$size, quadrant_id, corner_lat, corner_lon)
    
    cwp.idx <- NA
    if(grid$size < 5){
	    m.bbox <- sf::st_bbox(poly)
      m <- as.integer(floor(m.bbox))
      if(m[4]==m[2]) m[4] <- m[4]+1
      if(m[3]==m[1]) m[3] <- m[3]+1
      mr <- terra::rast(
        terra::ext(x = m, xy = TRUE),
        nrow = length(seq(m[2],m[4], grid$lat))-1, 
        ncol=length(seq(m[1], m[3], grid$lon))-1, 
        crs = "epsg:4326"
      ) 
      mr[] <- 1:terra::ncell(mr)
      mr.sf <- mr %>%
        terra::as.polygons() %>%
        sf::st_as_sf()
      mr.seq <- mr.sf[[1]]
      mr.seq <- switch(quadrant,
                       "SE" = as.character(mr.seq),
                       "NW" = as.character(rev(mr.seq)),
                       "NE" = as.character(unlist(rev(split(mr.seq, ceiling(seq_along(mr.seq)*grid$lon))))),
                       "SW" = as.character(unlist(rev(split(rev(mr.seq), ceiling(seq_along(rev(mr.seq))*grid$lon))))))
      mr.sf[[1]] <- mr.seq
      cwp.idx <- as.integer(mr.sf[as.integer(sf::st_intersects(pt, mr.sf)),][[1]])
      gridcode <- paste0(gridcode, cwp.idx)
    }
    
    df <- data.frame(GRIDTYPE = grid$res, QUADRANT = quadrant, X_COORD = labpt[1], Y_COORD = labpt[2], 
                     CWP_A = grid$size, CWP_B = quadrant_id, CWP_C = corner_lat, CWP_D = corner_lon, CWP_E = cwp.idx,
                     CWP_CODE = gridcode, SURFACE = as.numeric(sf::st_area(poly)))
    return(df)
  }, ...))
  
  sf <- cbind(sf, attrs)
  sf$lyr.1 = NULL
  return(sf)
}
