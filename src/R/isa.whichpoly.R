isa.whichpoly <- function(points, poly){
  
  requireNamespace("sf", quietly = TRUE)
  
  # Create sf objects:
  pointsll.sf <- st_as_sf(points, coords = c("lon","lat"), remove = FALSE)
  st_crs(pointsll.sf) <- 4326
  
  poly.sf <- st_read(dsn = getwd(), layer = poly)
  st_crs(poly.sf) <- 4326
  
  # Look up attributes from aulgall.sf and add them to farmll.sf
  rval.sf <- st_join(x = pointsll.sf, y = poly.sf, join = st_within)

  rval.df <- st_drop_geometry(rval.sf)
  return(rval.df)
}