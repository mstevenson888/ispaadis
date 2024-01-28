
library(spatstat); library(RColorBrewer); library(sf); library(raster); library(ggplot2)

dat <- read.table("ID_NTB_inf.txt", header = FALSE, sep = "")
head(dat)

source("isa.epicurve.R")

fmd.pred <- isa.epicurve(dat = dat, model = "isp", stats = TRUE, type = "cumulative", quant = c(0.25, 0.75))

  

## Plot results:
plot(c(0, 60), c(0, 60), type = "n", 
     xlab = "Simulation day", ylab = "Cumulative number of infected premises")
lines(fmd.pred[,1], fmd.pred[,4], type = "s", lwd = 2)
lines(fmd.pred[,1], fmd.pred[,5], type = "s", lwd = 1)
lines(fmd.pred[,1], fmd.pred[,6], type = "s", lwd = 1)
lines(act.df$day, cumsum(act.df$n), type = "s", lwd = 2, col = "red")

legend(x = "topleft", legend = c("Actual cumulative number IPs", 
                                 "Predicted cumulative IPs (median of 9 iterations)", 
                                 "25th and 75th prediction quartiles"), 
       col = c("red", "black", "black"), lwd = c(3, 3, 1), bty = "n")


# ==============================================================================


## Create an sf object:
poly <- list(as.matrix(isa.boundary[c(172:1,172),]))
adm0.sf <- st_polygon(poly)

## Create an observation window:
isa.ow <- owin(poly = as.matrix(isa.boundary[172:1,]), 
   unitname = c("metre", "metres"))
## Method for SpatialPolygonsDataFrame object:
## isa.ow <- as(as(isa.boundary, "SpatialPolygonsDataFrame"), "owin")

# Create ppp object:
farms.ppp <- ppp(x = isa.farms$xcoord, y = isa.farms$ycoord, 
   window = isa.ow)

sigma <- 2000; dimyx <- c(300,300)

## Kernel smooth:
farms.im <- density(farms.ppp, sigma = sigma, dimyx = dimyx)

## The density.ppp function returns an estimate of the intensity of the 
## underlying point process, that is the expected number of points per unit 
## area (in this case, the number of points per metre). Express results as 
## the number of points per square kilometre (1 square metre = 0.000001 
## square kilometres):

farms.im$v <- farms.im$v / 0.000001

## Maximum farm density is 5 per farms per square kilometre.

farms.r <- raster(epi.matfix(as.matrix(farms.im$v, nrow = length(xgrid))))
extent(inf.r) <- st_bbox(adm0.sf)
farms.rdf <- data.frame(rasterToPoints(farms.r))

summary(as.vector(farms.im$v))
breaks <- seq(from = 0, to = 6, by = 2)
cols <- brewer.pal(n = 7, name = "Blues")
cols[1] <- "white"

windows(); ggplot() +
  theme_bw() +
  geom_tile(data = farms.rdf, aes(x = x, y = y, fill = layer)) + 
  scale_fill_gradientn(colours = cols, breaks = breaks, name = "Infected desa density", limits = c(-0.01,6)) + 
  # stat_contour(data = inf.rdf, aes(x = x, y = y, z = layer, linetype = 1), breaks = 0.05, linetype = 2, col = "black") +
  geom_sf(data = adm0.sf, fill = "transparent", col = "black") +
  geom_point(data = ehist.df, aes(x = xcoord, y = ycoord), pch = 3, size = 3, col = "black") +
  coord_sf(datum = st_crs(adm1utm.sf), xlim = c(370000,479000), ylim = c(9000000,9100000)) +
  scale_x_continuous(name = "Easting (km)", labels = mformat()) +
  scale_y_continuous(name = "Northing (km)", labels = mformat()) +
  theme(legend.position = "bottom", legend.direction = "horizontal")

