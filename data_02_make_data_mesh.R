# clear environment -------------------------------------------------------

rm(list = ls())



# required functions ----------------------------------------------


# Function to determine UTM zone from longitude
get_utm_zone <- function(lon) {
    zone <- floor((lon + 180) / 6) + 1
    if (zone < 1) {
        zone <- 1
    } else if (zone > 60) {
        zone <- 60
    }
    return(zone)
}

# define raster -----------------------------------------------------------

# Spatial information and mesh
# https://epsg.io/23028
utmCRS <- 23028 ## rather than 32631

orig_crs <- "+proj=longlat +datum=WGS84"
target_crs <- "+proj=utm +zone=28 +datum=WGS84 +units=km"

res0 <- 0.01 # resolution of grid side length in degrees
min_x <- 6.5
min_y <- 53.3
max_x <- 9.3
max_y <- 55.2
utm = get_utm_zone(mean(c(min_x, max_x)))
utm = 28
ext <- ext(min_x, max_x, min_y, max_y)
ras <- rast(ext = ext, crs = orig_crs, resolution = 0.01)
dim(ras)
ras[] <- runif(length(ras))
plot(ras)  
dev.off()
image(ras)

# Extract the coordinates of the raster
tmp <- crds(ras, na.rm=TRUE, df = T )

# Convert the coordinates to a SpatialPoints object
tmp <- SpatialPoints(coords = tmp, proj4string = CRS(crs(ras)))

# Define the new projection (for example, WGS 84)
new_crs <- CRS("+proj=longlat +datum=WGS84 +no_defs")

# Transform the coordinates to the new projection
utm_points <- spTransform(tmp, CRSobj = CRS("+proj=utm +zone=28 +datum=WGS84"))

plot(tmp, pch = ".", axes = T)



# prepare dataset --------------------------------------------------------
df <- read.csv("data/DYFS_data/DYFS_2002-2023_update3.csv")

hist(log(df$CPUE_kg_per_1000m2))
str(df)
df$DateTime <- strptime(df$DateTime, format = "%d.%m.%Y %H:%M")

df$year <- as.numeric(format(df$DateTime, "%Y"))
df$yearf <- as.factor(df$year)
df$TimeOfYear <- as.numeric(format(df$DateTime, "%j"))/365.25
head(df)


# split biomass between small and large crangon
dflen <- read.csv("data/DYFS_data/DYFS_2002-2023_length_update3.csv")
dflen <- subset(dflen, Species == "CRANGON")
head(dflen)

dflen2 <- dflen %>%
  group_by(Station_id, LengthClass) %>% 
  summarize(NumberAtLength = sum(NumberAtLength), .groups = 'drop') %>% 
  pivot_wider(names_from = LengthClass, values_from = NumberAtLength, values_fill = 0)
ord <- order(as.numeric(names(dflen2)[-1]))
names(dflen2)[-1][ord]
dflen2 <- cbind(dflen2[,1], dflen2[,-1][,ord])
dflen2
image(y = as.numeric(names(dflen2)[-1]), 
  z = log(t(apply(as.matrix(dflen2[,-1]), 1, FUN = function(x){x/sum(x)})))
)

matN <- as.matrix(dflen2[,-1], 
  dimnames = list(Station_id = c(dflen2$Station_id), 
    LengthClass = colnames(dflen2)[-1]))
dimnames(matN)

matL <- t(array(as.numeric(colnames(matN)), dim = dim(t(matN))))
matL

LWa <- 4.0625e-6
LWb <- 3.084

matW <- LWa * matL ^ LWb
matB <- matN * matW

col_sm <- which(matL[1,]<50)
col_bg <- which(matL[1,]>=50)

dfspl <- data.frame(Station_id = unique(dflen2$Station_id), 
  p_sm = rowSums(matB[,col_sm], na.rm = TRUE) / rowSums(matB, na.rm = TRUE), 
  p_lg = rowSums(matB[,col_bg], na.rm = TRUE) / rowSums(matB, na.rm = TRUE)
)
head(dfspl)

tmp <- dfspl
tmp$year <- as.numeric(substr(tmp$Station_id, start = 1, stop = 4))

plot(p_lg ~ year, tmp, pch = 16, col = adjustcolor(1,0.2))
fit <- gam(p_lg ~ s(year, k=15), data = tmp, family = quasibinomial(link = "logit"))
newdat <- data.frame(year = seqRan(tmp$year, length.out = 100))
newdat$pred <- predict(fit, newdata = newdat, type = "response")  
lines(pred~year, newdat)  
  
# subset data
spp = "CRANGON" # MERLANGIUS MERLANGUS, CRANGON
yrs = 2012:2023 #sort(unique(df$year))

dfsub <- subset(df, year %in% yrs & Species == spp)[,c(
  "Station_id", "year", "ShootLon", "ShootLat", "Depth", 
  "CPUE_kg_per_1000m2", "SweptArea_m2", "TotKg"
  )]
names(dfsub) <- c(
  "Station_id", "year", "lon", "lat", "z", 
  "cpue", "sweptArea", "b"
  ) # rename variables
head(dfsub)

# merge biomass proportion info
dfsub <- merge(x = dfsub, y = dfspl, all.x = TRUE)


# add presence/absence
dfsub$pa <- as.numeric(dfsub$b > 0)

# add zeros for missing small and large proportions
hit <- which(dfsub$pa == 0)
if(length(hit > 0)){
  dfsub$p_sm[hit] <- 0
  dfsub$p_lg[hit] <- 0
}

# replace missing depth
dfsub$z <- replace(dfsub$z, list = dfsub$z < 0, values = NaN)

# replace missing coordinates
hit <- which(dfsub$lon == -9 | dfsub$lat == -9)
if(length(hit)>0){dfsub <- dfsub[-hit,]}

# remove other rows with NAs
dfsub2 <- na.omit(dfsub)
head(dfsub2)

subset(dfsub2, pa == 0)
sum(dfsub2$pa == 0); sum(dfsub2$p_sm == 0); sum(dfsub2$p_lg == 0); sum(dfsub2$p_sm == 0 | dfsub2$p_lg == 0)

nrow(dfsub); nrow(dfsub2)
sum(dfsub2$pa); nrow(dfsub2); sum(dfsub2$pa)/nrow(dfsub2)

# coordinate system change example ----------------------------------------

pts_orig_crs <- st_as_sf(data.frame(
    # id = 1:3,
    lon = dfsub2$lon,
    lat = dfsub2$lat
), coords = c("lon", "lat"), crs = orig_crs) # Assume original CRS is WGS84 (EPSG:4326)
plot(pts_orig_crs, axes = T)

# transform
pts_targ_crs <- st_transform(pts_orig_crs, crs = target_crs)
plot(pts_targ_crs, axes = T)

# transform back
tmp <- st_transform(pts_targ_crs, crs = orig_crs)
plot(tmp, axes = T)

tmp <- as.data.frame(st_coordinates(pts_targ_crs))
head(tmp)

# add to data
dfsub2 <- cbind(dfsub2, tmp)
head(dfsub2)

# make bathymetry field --------------------------------------------------------

# Bathymetry data from https://www.gebco.net/data-products/gridded-bathymetry-data
bathyOrig <- rast(x = "P:/SFOZE2/datasets/GEBCO_2019_27_Feb_2020_b99ec4d0e93a/gebco_2019_n65.0_s40.0_w-20.0_e15.0.nc")
# plot(bathyOrig)
bathy <- resample(bathyOrig, ras, method = 'bilinear') # interpolate to raster resolution
plot(bathy, col = pals::cividis(100))  


# make land field ---------------------------------------------------------

# Load the shapefile
# coastline data from https://www.eea.europa.eu/ds_resolveuid/06227e40310045408ac8be0d469e1189
shapefile_path <- "P:/SFOZE2/datasets/coastline/European_coastline/Europe_coastline_poly.shp"
coast <- st_read(shapefile_path)
class(coast)
coast <- st_transform(coast, crs = orig_crs)

coast <- st_crop(coast, ext)


# coast <- st_transform(coast, crs = target_crs)

plot(st_geometry(pts_orig_crs))
plot(st_geometry(coast), add = T)

plot(st_geometry(coast))
plot(st_geometry(pts_orig_crs), add = T, pch = 20, cex = 0.5, col = adjustcolor("blue", 0.5))


plot(st_geometry(st_transform(coast, crs = target_crs)))
plot(st_geometry(pts_targ_crs), add = T, pch = 20, cex = 0.5, col = adjustcolor("blue", 0.5))



# Rasterize POLYGON
land <- terra::rasterize(coast, ras, field = 1, fun = "count")
land

# land[dim(land)[1],] <- 1
# land[,dim(land)[2]] <- 1
# land[1,] <- 1
# land[,1] <- 1
image(land)

water <- is.na(land)
image(water)


ras2 <- (bathy < -4) | water # "or" ("|") statement adds a bit of the rivers back
# ras2[,dim(ras2)[2]] <- 0
plot(ras2)


# remove small land clusters ----------------------------------------------

# Set the threshold for the minimum cluster size
threshold <- 5  # Adjust as needed (min # of cells in a clump)

# Identify clusters of cells with the given value
clusters <- terra::patches(as.numeric(!ras2), directions = 8, zeroAsNA = T)
plot(clusters, col = pals::alphabet(26))

# Calculate the size of each cluster
cluster_sizes <- as.data.frame(freq(clusters))


# Identify clusters smaller than the threshold
small_clusters <- cluster_sizes[cluster_sizes$count < threshold, "value"]

# Create a raster mask to remove small clusters
ras3 <- ras2
ras3[clusters %in% small_clusters] <- 1

# Plot the result
plot(ras2)
plot(ras3, main="Raster with Small Clusters Removed")


# remove small water clusters (ponds) ----------------------------------------------

# Set the threshold for the minimum cluster size
threshold <- 10  # Adjust as needed (min # of cells in a clump)

# Identify clusters of cells with the given value
clusters <- terra::patches(as.numeric(ras3), directions = 8, 
  zeroAsNA = T)
plot(clusters, col = pals::alphabet(26))

# Calculate the size of each cluster
cluster_sizes <- as.data.frame(freq(clusters))


# Identify clusters smaller than the threshold
small_clusters <- cluster_sizes[cluster_sizes$count < threshold, "value"]

# Create a raster mask to remove small clusters
ras4 <- ras3
ras4[clusters %in% small_clusters] <- 0

# Plot the result
plot(ras3)
plot(ras4, main="Raster with Small Clusters Removed")




# define outer limit polygon ----------------------------------------------

# x11()
image(bathy, col = pals::ocean.deep(100), zlim = c(min(as.array(bathy)),0))
image(ras4, col = c(8, NaN), add = T)
contour(ras4, add = T, levels = 1)
points(df$ShootLon, df$ShootLat, pch = ".")
# pts <- locator(10) # I did this manually - you need to use x11() for locator to correctly map clicks
# dev.off()

POLY <- as.data.frame(list(x = c(6.5, 7.0666, 7.848, 
  7.960, 7.668, 7.580, 7.709, 
  7.909, 8.012, 8.151), y = c(53.842, 
  53.963, 54.084, 54.189, 54.482, 
  54.696, 54.903, 55.055, 55.137, 
  55.2)))
POLY

tmp <- as.vector(ext)
outerEdge <- data.frame(x = c(tmp[2], tmp[2], tmp[1]), y = c(tmp[4], tmp[3], tmp[3]))
POLY <- rbind(POLY, outerEdge, POLY[1,])

POLY <- st_sf(geometry = st_sfc(st_polygon(list(as.matrix(POLY)))))
plot(POLY,  bg = NA, border = 2, add = T, lwd = 2)

# rasterize polygon to raster area
polygon_raster <- rasterize(POLY, ras, field = 1)
plot(polygon_raster)

# Create a new raster by masking ras4 with the polygon raster
# Cells outside the polygon will be set to 0
ras5 <- mask(x = ras4, mask = polygon_raster, updatevalue = 0)
# Plot the new raster
image(ras5)


# small adj to outer boundary  ----------------------------

# remove outer border of cells
ras5[dim(ras5)[1],] <- 0
ras5[,dim(ras5)[2]] <- 0
ras5[1,] <- 0
ras5[,1] <- 0
image(ras5)

ras6 <- ras5
ras6[ras6 == 0] <- NaN
image(ras6)

# rast to poly and simplify -------------

p1 <-  as.polygons(ras6, dissolve = T)
# p1 <- rasterToContour(ras6, level = 0)
plot(p1)
nrow(p1)
p2 <- st_as_sf(p1, crs = crs(ras))
plot(st_geometry(p2), axes = T)
nrow(p2)


# smooth polygon edges
p4 <- smooth(p2, method = "ksmooth", smoothness = 5)
plot(p4)

# simplify number of points defining the polygon
p5 <- st_simplify(p4, preserveTopology = T, dTolerance = 600)
plot(p5)

# view result
plot_sf(p2, axes = T, xlim = c(8.25, 9), ylim = c(54.5, 55))
plot(p2, add = T, col = NA)
# plot(p3, add = T, col = adjustcolor(2, 0.4))
plot(p4, add = T, col = NA, border = 2)
plot(p5, add = T, col = NA, border = 4)

# make prediction grid ----------------------------------------------------

grd <- crds(ras, df = T)
names(grd) <- c("lon", "lat")
grd$id <- seq(nrow(grd))
grd$bathy <- as.vector(bathy)
grd$z <- grd$bathy * -1
head(grd)

# add alternate coordinate system
tmp <- st_as_sf(grd[,c("lon", "lat")], coords = c("lon", "lat"), crs = orig_crs)
tmp2 <- st_transform(tmp, crs = target_crs)
tmp2 <- as.data.frame(st_coordinates(tmp2))
head(tmp2)
grd <- cbind(grd, tmp2)
head(grd)


# determine which grd is in boundary
# grd$p <- ras5[] # would also be OK, but some points will be outside of bnd
is_within <- st_within(tmp, p4, sparse = F) # determine which grid cells are in p5 polygon
grd$p <- as.numeric(is_within) # change to numeric variable
head(grd)
dim(grd)
sum(grd$p) # number of grids to make predictions


# mesh --------------------------------------------------------------------

# Extract the coordinates as a data.frame
coords <- st_coordinates(pts_targ_crs)
coords_df <- as.data.frame(coords)
head(coords_df)

# spatialPolygons object
# poly.water <- as(p4, "Spatial")
poly.water <- as(st_transform(p4, crs = target_crs), "Spatial")
plot(poly.water)

max.edge = 0.1 * 100 # 1 degrees ~ 100 km ()
# - some chosen constant
# - results should not be sensitive to this (if you have a good mesh)
# - max.edge = diff(range(df$locx))/15
mesh1 = inla.mesh.2d(loc=cbind(coords_df$X, coords_df$Y),
  max.edge = max.edge)
plot(mesh1, main="1st attempt"); points(coords_df$X, coords_df$Y, col="blue")


max.edge = 0.2 * 100
# - as before
bound.outer = 0.5 * 100
# - the outer boundary I want to use for my mesh
# - some chosen constant
# - results should not be sensitive to this
# - bound.outer = diff(range(df$locx))/3
mesh2 = inla.mesh.2d(loc=cbind(coords_df$X, coords_df$Y),
  max.edge = c(1,5)*max.edge,
  # - use 5 times max.edge in the outer extension/offset/boundary
  cutoff = max.edge/5,
  offset = c(max.edge, bound.outer))
plot(mesh2, main="2nd attempt"); points(coords_df$X, coords_df$Y, col="blue")


max.edge = 0.2 * 100
# - as before
bound.outer = 0.2 * 100
# - as before
mesh3 = inla.mesh.2d(boundary = poly.water,
  loc = cbind(coords_df$X, coords_df$Y),
  max.edge = c(1,5)*max.edge,
  # - use 5 times max.edge in the outer extension/offset/boundary
  cutoff = max.edge/5,
  offset = c(max.edge, bound.outer))
plot(mesh3, main="3rd attempt"); points(coords_df$X, coords_df$Y, pch = 20, col = adjustcolor("red", 0.2), cex = 1)


mesh4 <- sdmTMB::make_mesh(coords_df, c("X", "Y"), mesh = mesh3)
plot(mesh4)


# make mesh without boundary
# - as before
meshNoBound = inla.mesh.2d(
  loc = cbind(coords_df$X, coords_df$Y),
  max.edge = c(1,5)*max.edge,
  # - use 5 times max.edge in the outer extension/offset/boundary
  cutoff = max.edge/5,
  offset = c(max.edge, bound.outer))
plot(meshNoBound, main="No Boundary"); points(coords_df$X, coords_df$Y, pch = 20, col = adjustcolor("red", 0.2), cex = 1)


meshNoBound <- sdmTMB::make_mesh(coords_df, c("X", "Y"), mesh = meshNoBound)
plot(meshNoBound)

# save objects ------------------------------------------------------------

dat <- dfsub2
head(dat)
mesh <- mesh4


save(dat, file = "data/dat.Rdata")
save(grd, file = "data/grd.Rdata")

writeRaster(x = bathy, filename = "data/bathy.tif", overwrite=TRUE)
writeRaster(x = ras, filename = "data/ras.tif", overwrite=TRUE)

save(orig_crs, file = "data/orig_crs.Rdata")
save(target_crs, file = "data/target_crs.Rdata")

save(coast, file = "data/coast.Rdata")
save(POLY, file = "data/POLY.Rdata")

save(mesh, file = "data/mesh.Rdata")
save(meshNoBound, file = "data/meshNoBound.Rdata")

