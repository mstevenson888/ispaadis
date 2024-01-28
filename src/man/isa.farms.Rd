\name{isa.farms}

\docType{data}

\alias{isa.farms}

\title{
Farm details
}

\description{
This data set provides, for a set of farms within a defined study area, details of location and the number of animals present on each farm.
}

\usage{
data(isa.farms)
}

\format{
  \describe{
  \item{id}{the primary farm identifier.}
  \item{farmid}{the secondary farm identifier.}
  \item{type}{farm type.}
  \item{cattle}{the number of cattle present.}
  \item{other}{the number of other (foot-and-mouth disease susceptible) species present.}
  \item{pigs}{the number of pigs present.}
  \item{xcoord}{easting coordinate of the farm centroid.}
  \item{ycoord}{northing coordinate of the farm centroid.}
}
}

\examples{
data(isa.farms)

## Not run:
library(spatstat); library(RColorBrewer)

data(isa.boundary)
data(isa.farms)

## Create an observation window:
isa.ow <- owin(poly = as.matrix(isa.boundary[172:1,]), 
   unitname = c("metre", "metres"))
## Method for SpatialPolygonsDataFrame object:
## isa.ow <- as(as(isa.boundary, "SpatialPolygonsDataFrame"), "owin")

# Create ppp object:
farms.ppp <- ppp(x = isa.farms$xcoord, y = isa.farms$ycoord, 
   window = isa.ow)

xylims <- rbind(range(isa.farms$xcoord), range(isa.farms$ycoord))
## Method for SpatialPolygonsDataFrame object:
## xylims <- attr(is.boundary, "bbox")
ratio <- (xylims[2,2] - xylims[2,1]) / (xylims[1,2] - xylims[1,1])

x.points <- seq(from = 180000, to = 250000, by = 10000); x.lab <- x.points/1000 
y.points <- seq(from = 360000, to = 420000, by = 10000); y.lab <- y.points/1000 

## Plot farm locations:
par(pin = c(1 * 5, ratio * 5), omi = c(0.5,0,0,0))
plot(x = xylims[1,], y = xylims[2,], xaxt = "n", yaxt = "n", type = "n",
   xlab = "", ylab = "", axes = TRUE)
points(x = isa.farms$xcoord, y = isa.farms$ycoord, col = "gray")
plot(isa.ow, add = TRUE)
axis(side = 1, at = x.points, labels = x.lab, tick = TRUE, cex.axis = 0.80)
axis(side = 2, at = y.points, labels = y.lab, tick = TRUE, cex.axis = 0.80)
mtext("Easting (km)", side = 1, line = 3, cex = 1.0)
mtext("Northing (km)", side = 2, line = 3, cex = 1.0)
legend(x = "topleft", legend = "Farm locations", pch = 1, col = "gray", 
   bty = "n")

## Set the bandwidth and grid dimensions:
sigma <- 1200; dimyx = c(200, 200)

## Kernel smooth:
farms.den <- density(farms.ppp, sigma = sigma, dimyx = dimyx)

## The density.ppp function returns an estimate of the intensity of the 
## underlying point process, that is the expected number of points per unit 
## area (in this case, the number of points per metre). Express results as 
## the number of points per square kilometre (1 square metre = 0.000001 
## square kilometres):
farms.den$v <- farms.den$v / 0.000001
summary(as.vector(farms.den$v))
## Maximum farm density is 7 per farms per square kilometre.

## Image plot of farm density:
breaks <- seq(from = 0, to = 7, length = 5)
col <- brewer.pal(n = 4, name = "Blues")

par(pin = c(1 * 5, ratio * 5), omi = c(0.5,0,0,0))
plot(x = xylims[1,], y = xylims[2,], type = "n", xlab = "Easting (m)", 
   ylab = "Northing (m)", xaxt = "n", yaxt = "n")
image(x = farms.den$xcol, y = farms.den$yrow, z = t(farms.den$v), 
   zlim = c(0, 7), col = col, breaks = breaks, add = TRUE)
plot(isa.ow, add = TRUE)
axis(side = 1, at = x.points, labels = x.lab, tick = TRUE, cex.axis = 0.80)
axis(side = 2, at = y.points, labels = y.lab, tick = TRUE, cex.axis = 0.80)
## metre(xl = xylims[1,1], yb = xylims[2,1], xr = xylims[1,1] + 2000, 
##    yt = xylims[2,1] + 15000, lab = breaks, cols = col, shift = 0, cex = 0.75)
## End(Not run)
}

\keyword{datasets}