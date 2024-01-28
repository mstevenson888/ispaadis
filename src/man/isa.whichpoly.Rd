\name{isa.whichpoly}

\alias{isa.whichpoly}

\title{
In which polygon does a point lie?
}

\description{
Attaches polygon attribute table data to a data frame listing the longitude and latitude of point locations.
}

\usage{
isa.whichpoly(points, poly)
}

\arguments{
  \item{points}{a data frame listing the longitude \code{lon} and latitude \code{lat} of the point location.}
  \item{poly}{the name of an ESRI shape file set defining the polygons to be queried.}
}

\details{
This function would usually be used when one wants to define a new farm or herd location to be included in an AADIS herd data set. The farm-herd location attributes required by AADIS include the identifier of the region, state and local government area (LGA) in which the farm-herd is located. This function can be used to query digital maps (ESRI shape file sets) providing region, state and local government area boundaries. 

Shape file sets must be projected in decimal degress (EPSG 4326).
}

\examples{
\dontrun{
## Create a data frame listing farm-herd locations:
farmsll.df <- data.frame(id = 1, lon = 144.39, lat = -35.97)

## The shape file to be queried is called AU_LGA-ll:
isa.whichpoly(points = farmsll.df, poly = "AU_LGA-ll")

## This farm is in Campaspe LGA (code 165).
}
}