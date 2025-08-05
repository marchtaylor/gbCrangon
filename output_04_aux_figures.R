
# clear environment -------------------------------------------------------

rm(list = ls())


# Study area map -----------------

## load data -----

load(file = "data/dat.Rdata")
load(file = "data/mesh.Rdata")
load(file = "data/grd.Rdata")
# bathy <- rast("data/bathy.tif")
# ras <- rast("data/ras.tif")
load(file = "data/orig_crs.Rdata")
load(file = "data/target_crs.Rdata")
load(file = "data/coast.Rdata")

load(file = "data/orig_crs.Rdata")
load(file = "data/target_crs.Rdata")
load(file = "data/POLY.Rdata")

# coastline data from https://www.eea.europa.eu/ds_resolveuid/06227e40310045408ac8be0d469e1189
shapefile_path <- "P:/SFOZE2/datasets/coastline/European_coastline/Europe_coastline_poly.shp"
coast_eur <- st_read(shapefile_path)
class(coast_eur)

shapefile_path <- "data/Seevermessung_DeutscheSeegrenzen/TerritorialSea.shp"
terr12nm <- st_read(shapefile_path)
class(terr12nm)
terr12nm <- st_transform(terr12nm, crs = orig_crs)

shapefile_path <- "data/Seevermessung_DeutscheSeegrenzen/Baseline.shp"
base <- st_read(shapefile_path)
class(base)
base <- st_transform(base, crs = orig_crs)

shapefile_path <- "data/Seevermessung_DeutscheSeegrenzen/AWZ.shp"
awz <- st_read(shapefile_path)
class(awz)
awz <- st_transform(awz, crs = orig_crs)

shapefile_path <- "data/Seevermessung_DeutscheSeegrenzen/MaritimeBoundaryAWZ.shp"
marbound <- st_read(shapefile_path)
class(marbound)
marbound <- st_transform(marbound, crs = orig_crs)

shapefile_path <- "data/World_12NM_v4_20231025/eez_12nm_v4.shp"
eez12nm <- st_read(shapefile_path)
eez12nm_GER <- eez12nm %>% filter(SOVEREIGN1 == "Germany")
# validity <- st_is_valid(eez12nm_GER, reason = TRUE)
eez12nm_GER <- st_transform(eez12nm_GER, crs = orig_crs)
extent <- st_bbox(c(xmin = 0, ymin = 45, xmax = 9.5, ymax = 60))
# Crop the shapefile using the extent
eez12nm_GER <- st_crop(eez12nm_GER, extent)


EXT <- ext(c(6, 9.3, 53.2, 55.5))

coast_eur <- st_transform(coast_eur, crs = orig_crs)
coast <- st_crop(x = coast_eur, y = EXT)

EXT_lg <- ext(c(-5, 12, 50, 60))
coast_lg <- st_crop(x = coast_eur, y = EXT_lg)

# Bathymetry data from https://www.gebco.net/data-products/gridded-bathymetry-data
bathy <- rast(x = "P:/SFOZE2/datasets/GEBCO_2019_27_Feb_2020_b99ec4d0e93a/gebco_2019_n65.0_s40.0_w-20.0_e15.0.nc")
ras <- rast(ext = EXT, crs = orig_crs)
bathy2 <- resample(bathy, ras, method = 'bilinear') # interpolate to raster resolution
plot(bathy2, col = pals::cividis(100))  

# make bounding box for study area
sa_box <- matrix(c(
  EXT[1], EXT[3],
  EXT[2], EXT[3],
  EXT[2], EXT[4],
  EXT[1], EXT[4],
  EXT[1], EXT[3]  # Close the polygon by repeating the first point
), ncol = 2, byrow = TRUE)

# Create an sf polygon from the coordinates
sa_box <- st_polygon(list(sa_box))

# Create an sf object with the polygon and a CRS (e.g., EPSG:4326)
sa_box <- st_sfc(sa_box, crs = orig_crs)


# adjust lower line of POLY
adjust_coords <- function(geometry) {
  coords <- st_coordinates(geometry)
  coords[coords[,2] == 53.3, 2] <- 53
  st_polygon(list(coords))
}

st_crs(POLY) <- orig_crs

POLY2 <- POLY %>%
  mutate(geometry = st_sfc(lapply(geometry, adjust_coords), crs = st_crs(POLY)))

bathy_df <- as.data.frame(bathy2, xy = TRUE)


land_fill <- "darkseagreen3"
land_col <- "darkgreen"

p1 <- ggplot() +
  geom_raster(data = bathy_df, aes(x = x, y = y, fill = elevation), interpolate = F) +
  geom_hline(yintercept = seq(53, 56, 0.5), linetype = 3, linewidth = 0.5, col = "white") +
  geom_vline(xintercept = seq(6, 10, 1), linetype = 3, linewidth = 0.5, col = "white") +
  geom_sf(data = POLY2, fill = NA, color = "yellow", linewidth = 1) +
  # geom_sf(data = base, fill = NA, color = 4, linewidth = 1) +
  # geom_sf(data = terr12nm, fill = NA, color = 4, linewidth = 1) +
  # geom_sf(data = awz, fill = NA, color = 4, linewidth = 1) +
  # geom_sf(data = marbound, fill = NA, color = 4, linewidth = 1) +
  geom_sf(data = coast, fill = land_fill, color = land_col) +
  geom_point(data = dat, aes(x = lon, y = lat), shape = ".") +
  theme_bw() +
  coord_sf(xlim = EXT[1:2], ylim = c(53.3, EXT[4]), expand = F, crs = orig_crs) +
  labs(title = "German Bight study area") +
  scale_fill_gradient(low = "grey30", high = "grey90", limits = c(NA, 5), name = "Depth [m]") +
  theme(
    text = element_text(size = 10), 
    axis.title = element_blank(),
    axis.text = element_text(size = 8),
    legend.text = element_text(size = 8), 
    legend.title = element_text(size = 8),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1), 
    plot.margin = margin(2,2,2,2)
  )
# print(p1)

p2 <- ggplot() +
  geom_sf(data = coast_lg, fill = land_fill, color = land_col, linewidth = 0.1) +
  geom_sf(data = sa_box, fill = NA, color = "black", linewidth = 0.5) +
  coord_sf(ylim = c(EXT_lg[3]+1, EXT_lg[4]), expand = F, crs = orig_crs) +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    axis.title = element_blank(), 
    axis.text = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.ticks = element_blank(),
    axis.line = element_line(linewidth = 0.25), 
    plot.margin = margin(3,3,0,0), 
    panel.grid = element_line(color = "grey", linewidth = 0.25, linetype = 3)
  )
# print(p2)

p <- p1 +
  inset_element(p2, left = 0.02, bottom = 0.6, right = 0.4, top = 0.99, align_to = "panel")

png("output/study_area.png", width = 5, height = 4.7, units = "in", res = 600)
  print(p)
dev.off()



# Samples by year ----

load("data/dat.Rdata")
df <- as.data.frame(table(dat$year))
mean(df$Freq); range(df$Freq)

p <- ggplot(df) + aes(x = Var1, y = Freq) +
  geom_col(width = 0.8) + 
  labs(x = element_blank(), y = "Frequency", 
    title = "DYFS samples in German Bight by year") +
  theme_bw() + 
  theme(text = element_text(size = 10))

png("output/dyfs_freq_by_year.png", width = 5, height = 3.5, units = "in", res = 1000)
  print(p)
dev.off()


# Samples by date ----

df <- read.csv("data/DYFS_data/DYFS_2002-2023_update3.csv")
df$DateTime <- strptime(df$DateTime, format = "%d.%m.%Y %H:%M")

tmp <- as.Date(df$DateTime)
tmp <- date2yeardec(tmp) %% 1

h <- hist(tmp, plot = F)
df <- data.frame(date = yeardec2date(2000+tmp))
p <- ggplot(df) + aes(date) +
  geom_histogram() +
  labs(x = element_blank(), y = "Frequency", 
    title = "DYFS samples in German Bight by date") +
  theme_bw() + 
  theme(text = element_text(size = 10))

png("output/dyfs_freq_by_date.png", width = 5, height = 3.5, units = "in", res = 1000)
  print(p)
dev.off()

# depth spline -----

load("data/dat.Rdata")

sizeGroups <- c("small", "large", "combined")
L <- vector("list", length(sizeGroups))
names(L) <- sizeGroups
for(size in sizeGroups){
  # size = "small"
  fnameAppendix <- size
  (fname <- file.path("output", paste0("depthEffect", "_", fnameAppendix, ".Rdata")))
 
  load(file = fname, verbose = T)
  p$frac <- size
  L[[size]] <- p

}

df <- do.call("rbind", L)
head(df)


p <- ggplot(df) + 
  aes(x = z, y = exp(est), ymin = exp(est - 1.96 * est_se), ymax = exp(est + 1.96 * est_se)) +
  facet_wrap(~frac, nrow = 1, scales = "free_y") +
  geom_rug(data = dat, mapping = aes(x = z), inherit.aes = F, sides = "b", length = unit(0.02, "npc"), alpha = 0.1, color = 1) +
  geom_line() +
  geom_ribbon(alpha = 0.4) +
  coord_cartesian(ylim = c(0,NA), expand = T) +
  labs(x = "Depth [m]", y = "Biomass density [t/km^2]", title = "Depth effect") +
  theme_bw() + 
  theme(text = element_text(size = 10))
# print(p)

(fname <- file.path("output", paste0("depthEffect.png")))
png(fname, width = 7, height = 3.5, units = "in", res = 400)
  print(p)
dev.off()



# mesh ----
load(file = "data/mesh.Rdata")

png("output/mesh.png", width = 6, height = 5, units = "in", res = 1000)
op <- par(mar = c(3.5,3.5,1,1), mgp = c(2,0.5,0), ps = 10)
plot(mesh)
axis(1)
axis(2)
mtext("Eastings [km]", side = 1, line = 2)
mtext("Northings [km]", side = 2, line = 2)
reltext(relx = 0.02, rely = 0.95, labels = "UTM Zone 28", pos = 4)
par(op)
dev.off()



# spatial prediction (no random effect) ------
amrum <- data.frame(x = (8 + 21/60 + 15/60^2), y = (54 + 42/60 + 34/60^2))
amrum <- st_as_sf(amrum[,c("x", "y")], coords = c("x", "y"), crs = orig_crs)


fname <- "output/bestmod_b_combined.nc"
b <- rast(fname)

b <- app(b, fun="median", na.rm = T) 
plot(b)

ncolor <- 50
breaks <- exp(seq(log(0.1), log(max(as.array(b), na.rm = T)), length = ncolor+1))
breaks[1] <- 0
colors <- adjustcolor(viridis(ncolor), alpha.f = 0.7)
spp <- "Crangon crangon"

tmp <- as.data.frame(b, xy = TRUE)
pts_orig_crs <- st_as_sf(tmp[,c("x", "y")], coords = c("x", "y"), crs = orig_crs)
tmp <- as.data.frame(st_coordinates(pts_orig_crs))
head(tmp)
xyRatio <- earthDist(lon1 = min(tmp$X), lat1 = mean(tmp$Y), lon2 = max(tmp$X), lat2 = mean(tmp$Y)) / 
  earthDist(lon1 = mean(tmp$X), lat1 = min(tmp$Y), lon2 = mean(tmp$X), lat2 = max(tmp$Y))


fname <- file.path("output", "map_median_prediction.png")
png(fname, width = 3.5, height = 3.5/xyRatio, units = "in", res = 600)
op <- par(mar = c(3,3,0.5,0.5), ps = 9, mgp = c(1.75,0.5,0))

image(bathy2, col = NA, asp = NA, xlim = c(6.2, 9.2), ylim = c(53.3, 55.3),
  xlab = "Longitude", ylab = "Latitude")
image(bathy2, add = T, col = grey.colors(100, start = 0.5, end = 1))
image(b, add = TRUE, col = colors, breaks = breaks)
plot(eez12nm_GER$geometry, add = TRUE, col = NA, border = "yellow", lwd = 2)
plot(marbound$geometry, add = TRUE, col = "orange", lwd = 2)
plot(coast, add = TRUE, col = "white", border = NA)
plot(amrum, col = "yellow", cex = 1.5, add = T, pch = 10, lwd = 1.5)

box()
op2 <- par(no.readonly = T)
par(mgp = c(2,0.25,0))
sinkr::embedPlot(expr = {
  sinkr::imageScale(1, col = colors, breaks = breaks, axis.pos = 1,
    xlim = c(breaks[2], breaks[length(breaks)]), log = "x", add.axis = F)
  axis(1, cex.axis = 0.7)
  mtext(bquote("Median biomass ["*t~km^-2*"]"), side = 1, line = 1.25, cex = 0.8)
}, at = c(0.05, 0.4, 0.76, 0.8))
par(op2)

par(op)

dev.off()




# plot of median spatial prediction by depth ------
load("data/grd.Rdata")
  
M <- vector("list", 3)
names(M) <- c("combined", "small", "large")
for(size in names(M)){
  fname <- paste0("output/bestmod_b_", size, ".nc")
  b <- rast(fname)
  
  b <- app(b, fun="median", na.rm = T) 
  # plot(b)

  m <- data.frame(value = as.vector(b))
  m$z <- grd$z
  m$fraction <- size
  
  m <- subset(m, subset = z>0 & z<30)
  
  fit <- gam(value ~ s(z), data = m, family = gaussian(link = "log"))
  pred <- predict(fit, newdata = m, se.fit = T, type = "link")
  
  m$fit <- fit$family$linkinv(pred$fit)
  m$upper <- fit$family$linkinv(pred$fit + 1.96 * pred$se.fit)
  m$lower <- fit$family$linkinv(pred$fit - 1.96 * pred$se.fit)
  m$fitSc <- m$fit/max(m$fit, na.rm = T)

  M[[size]] <- m
}
m <- do.call("rbind", M)



p1 <- ggplot(m) + aes(x = z, y = fit, color = fraction, group = fraction, ymin = lower, ymax = upper) +
  geom_line() + 
  labs(x = "Depth [m]", y = bquote("Biomass [ t"~km^-2~"]")) +
  theme_bw() + 
  guides(color = guide_legend(title = "Size fraction"))
print(p1)

p2 <- ggplot(m) + aes(x = z, y = fitSc, color = fraction, group = fraction, ymin = lower, ymax = upper) +
  geom_line() + 
  labs(x = "Depth [m]", y = bquote("Scaled biomass (max. = 1.0)")) +
  theme_bw() + 
  guides(color = guide_legend(title = "Size fraction"))
print(p2)

p <- (p1 + p2) + patchwork::plot_layout(guides = "collect", axes = "collect_x") &
  theme(text = element_text(size = 10))
p 

png("output/median_prediction~depth.png", width = 7, height = 3, units = "in", res = 1000)
  print(p)
dev.off()



# combined vs added indices -----

load(file = "output/bestmod_pred_index_small.Rdata", verbose = T)
index_small <- index
index_small$frac <- "small"
load(file = "output/bestmod_pred_index_large.Rdata", verbose = T)
index_large <- index
index_large$frac <- "large"
load(file = "output/bestmod_pred_index_combined.Rdata", verbose = T)
index_combined <- index
index_combined$frac <- "combined"


index_added <- index_small
index_added$est <- index_small$est + index_large$est
index_added$lwr <- index_small$lwr + index_large$lwr
index_added$upr <- index_small$upr + index_large$upr
index_added$frac <- "added"


## indices all ----
idx <- rbind(index_small, index_large, index_combined, index_added)
# idx <- subset(idx, frac %in% c("combined", "added"))
idx$frac <- factor(idx$frac, levels = c("combined", "added", "large", "small"), 
  labels = c("Combined", "Added (Small + Large)", "Small", "Large"))

p <- ggplot(idx) + aes(x = year, y = est, group = frac, color = frac, fill = frac, linetype = frac) + 
  geom_line(linewidth = 0.7) +
  # geom_ribbon(mapping = aes(ymin = lwr, ymax = upr), alpha = 0.1, color = NA, linetype = 3) +
  scale_linetype_manual(values = c(1,2,1,1)) +
  # scale_color_manual(values = c(1, "purple", 2, 4)) +
  labs(x = element_blank(), y = "Biomass [t]", color = "Size fraction", linetype = "Size fraction", fill = "Size fraction") +
  theme_bw() + 
  theme(text = element_text(size = 10))

png("output/indices_all.png", width = 7, height = 4.5, units = "in", res = 1000)
  print(p)
dev.off()


## indices combined vs added ----
idx <- rbind(index_small, index_large, index_combined, index_added)
idx <- subset(idx, frac %in% c("combined", "added"))
idx$frac <- factor(idx$frac, levels = c("combined", "added", "large", "small"), 
  labels = c("Combined", "Added (Small + Large)", "Small", "Large"))


p <- ggplot(idx) + aes(x = year, y = est, group = frac, color = frac, fill = frac, linetype = frac) + 
  geom_line(linewidth = 0.7) +
  geom_ribbon(mapping = aes(ymin = lwr, ymax = upr), alpha = 0.2, color = NA, linetype = 3) +
  scale_linetype_manual(values = c(1,2,1,1)) +
  # scale_color_manual(values = c(1, "purple", 2, 4)) +
  labs(x = element_blank(), y = "Biomass [t]", color = "Size fraction", linetype = "Size fraction", fill = "Size fraction") +
  theme_bw() + 
  theme(text = element_text(size = 10))

png("output/indices_combined_vs_added.png", width = 6, height = 3.5, units = "in", res = 1000)
  print(p)
dev.off()



# QQ plots ------
L <- list()
for(size in c("combined", "large", "small")){
  fname <- file.path("model", paste0("bestmod_", size, ".Rdata"))
  load(fname, verbose = T)
  L[[size]] <- data.frame(resid = residuals(bestmod), frac = size)
  rm(bestmod)
}

df <- do.call("rbind", L)

p <- ggplot(df) + aes(sample = resid, group = frac) +
  facet_wrap(~frac, nrow = 1) +
  stat_qq(pch = 1) +
  stat_qq_line(color = 4) +
  labs(title = "Q-Q Plot", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_bw() + 
  theme(text = element_text(size = 10))
p

(fname <- file.path("output", paste0("qqplot_by_fraction.png")))
png(fname, width = 7, height = 3.5, units = "in", res = 1000)
  print(p)
dev.off()


# Reconstruction -----------------------------
load(file = "output/recon.Rdata", verbose = TRUE)
agg <- aggregate(hr~year, data = recon, subset= month %in% c(3:11), FUN = "mean")


p <- ggplot(agg) + aes(x = year, y = hr) + 
  geom_line() +
  geom_point() + 
  coord_cartesian(ylim = c(0,NA)) +
  labs(y = "Mean monthly harvest rate (Mar-Nov)", x = element_blank()) +
  theme_bw() +
  theme(text = element_text(size = 10))

png("output/harvest_rate_mean~year.png", width = 5, height = 3.5, units = "in", res = 1000)
  print(p)
dev.off()



# fleet size --------------------------------------------------------------

df <- readRDS("data/CSH_meshSize.RDS")

p <- ggplot(df) + aes(x = year, y = nEunr) + 
  geom_line() +
  geom_point() + 
  coord_cartesian(ylim = c(0,NA)) +
  scale_x_continuous(breaks = seq(2005, 2020, 5)) +
  labs(y = "Number of vessels targeting brown shrimp", x = element_blank()) +
  theme_bw() +
  theme(text = element_text(size = 10))

png("output/fleet_size~year.png", width = 5, height = 3.5, units = "in", res = 1000)
  print(p)
dev.off()

