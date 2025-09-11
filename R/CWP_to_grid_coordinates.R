#' @title CWP_to_grid_coordinates
#' @description
#'  Function copied from iotc_core_gis_cwp_utils (https://github.com/iotc-secretariat/iotc-lib-core-gis-cwp/blob/master/R/iotc_core_gis_cwp_utils.R)
#'Converts a CWP grid code into its four boundary points (NW, NE, SW and SE)
#'@param grid_code A CWP grid code
#'@return a data table containing the coordinates (LAT, LON) of each of the four boundary points for the grid
#'@export
#'@examples CWP_to_grid_coordinates("5201123")
#'@examples CWP_to_grid_coordinates("6205125")
CWP_to_grid_coordinates = function(grid_code) {
  s = as.integer(substr(grid_code, 1, 1))

  dx = dy = 1

  if     (s == 6) { dx =     dy =  5 }
  else if(s == 3) { dx =     dy = 10 }
  else if(s == 2) { dx = 20; dy = 10 }
  else if(s == 4) { dx =     dy = 20 }
  else if(s == 1) { dx =     dy = 30 }

  q =   as.integer(substr(grid_code, 2, 2))

  lat = as.integer(substr(grid_code, 3, 4))
  lon = as.integer(substr(grid_code, 5, 7))

  points = data.table(POS = character(), LON = numeric(), LAT = numeric())

  if (q == 1) {       #NE quadrant
    points = rbind(points, list("NW",  lon     ,  lat + dy))
    points = rbind(points, list("NE",  lon + dx,  lat + dy))
    points = rbind(points, list("SE",  lon + dx,  lat))
    points = rbind(points, list("SW",  lon     ,  lat))
  } else if(q == 2) { #SE quadrant
    points = rbind(points, list("NW",  lon     , -lat))
    points = rbind(points, list("NE",  lon + dx, -lat))
    points = rbind(points, list("SE",  lon + dx, -lat - dy))
    points = rbind(points, list("SW",  lon     , -lat - dy))
  } else if(q == 3) { #SW quadrant - doesn't really apply to the IO
    points = rbind(points, list("NW", -lon - dx, -lat))
    points = rbind(points, list("NE", -lon     , -lat))
    points = rbind(points, list("SE", -lon     , -lat - dy))
    points = rbind(points, list("SW", -lon - dx, -lat - dy))
  } else if(q == 4) { #NW quadrant - doesn't really apply to the IO
    points = rbind(points, list("NW", -lon - dx,  lat + dy))
    points = rbind(points, list("NE", -lon     ,  lat + dy))
    points = rbind(points, list("SE", -lon     ,  lat))
    points = rbind(points, list("SW", -lon - dx,  lat))
  }

  return(points)
}
