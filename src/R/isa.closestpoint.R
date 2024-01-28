isa.closestpoint <- function(point, locations){
  
  requireNamespace("sf", quietly = TRUE)

  # Create sf objects:
  pointsll.sf <- st_as_sf(points, coords = c("lon","lat"), remove = FALSE)
  st_crs(pointsll.sf) <- 4326
  
  # Convert locations to sf object:
  locationsll.sf <- st_as_sf(locations, coords = c("lon","lat"), remove = FALSE)
  st_crs(locationsll.sf) <- 4326
  
  # Calculate the distance from farmll.sf to each wstn.sf:
  locations$dist <- as.numeric(st_distance(x = pointsll.sf, y = locationsll.sf))
  
  # Return the closest weather station (using the same variable names as AADIS):
  points$id <- locations$id[locations$dist == min(locations$dist)]
  return(points)
}
