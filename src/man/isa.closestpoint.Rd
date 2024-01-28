\name{isa.closestpoint}

\alias{isa.closestpoint}

\title{
Which point is closest to a given point location?
}

\description{
Returns the point location identifier of the nearest point to a data frame listing the longitude and latitude of point locations.
}

\usage{
isa.closestpoint(point, locations)
}

\arguments{
  \item{point}{a data frame listing the longitude \code{lon} and latitude \code{lat} of the point location.}
  \item{locations}{a data frame listing the identifier \code{id}, the longitude \code{lon} and latitude \code{lat} of point locations to be queried.}
}

\details{
This function would usually be used when one wants to define a new farm or herd location to be included in an AADIS herd data set. The farm-herd location attributes required by AADIS include (among other things) the identifier of the nearest weather station. This function can be used to query a data frame listing the identity and coordinates of weather station to return the nearest weather station to each location listed in data frame \code{point}. 
}

\examples{
\dontrun{
## Create a data frame listing farm-herd locations:
farmsll.df <- data.frame(id = 1, lon = 144.39, lat = -35.97)

## Read in the AADIS weather station location data:
weather_station.df <- read.table("weather_station.csv", header = TRUE, sep = ",")
names(weather_station.df)[c(1,3:4)] <- c("id","lon","lat")

## Query the AADIS weather station location data:
isa.closestpoint(point = farmsll.df, locations = weather_station.df)

## This weather station closest to this farm is 80015.
}
}