

# clear environment -------------------------------------------------------

rm(list = ls())

# settings --------
offsetVar <- "logSweptArea" # NULL to remove




# load data ---------------------------------------------------------------

load(file = "data/mesh.Rdata")
load(file = "data/grd.Rdata")
bathy <- rast("data/bathy.tif")
ras <- rast("data/ras.tif")
load(file = "data/orig_crs.Rdata")
load(file = "data/target_crs.Rdata")
load(file = "data/coast.Rdata")



sizeGroups <- c("small", "large", "combined")
for(size in sizeGroups){

  fnameAppendix <- size
  
  
  load(file = file.path("model", paste0("bestmodcv", "_", fnameAppendix, ".Rdata")))
  load(file = file.path("model", paste0("bestmod", "_", fnameAppendix, ".Rdata")))
  load(file = file.path("model", paste0("modlut", "_", fnameAppendix, ".Rdata")))
  load(file = file.path("model", paste0("clust2", "_", fnameAppendix, ".Rdata")))
  load(file = file.path("model", paste0("dat", "_", fnameAppendix, ".Rdata")))
  
  
  modlut$name[which.max(modlut$logLik)]
  
  # visualize cv blocking structure -----------------------------------------
  
  p <- spatialsample::autoplot(clust2)
  (fname <- file.path("output", paste0("cv_blocks", "_", fnameAppendix, ".png")))
  png(fname, width = 6, height = 5, units = "in", res = 1000)
  print(p)
  dev.off()
  
  
  # checks and marginal effects ---------------------------------------------
  
  AIC(bestmod)
  summary(bestmod)
  sanity(bestmod)
  
  # qqplot
  tmp <- residuals(bestmod) # randomized quantile residuals
  
  (fname <- file.path("output", paste0("qqplot", "_", fnameAppendix, ".png")))
  png(fname, width = 5, height = 4.5, units = "in", res = 1000)
  qqnorm(tmp)
  qqline(tmp)
  dev.off()
  
  
  
  # depth effect
  nd <- data.frame(
    z = seqRan(dat$z, length.out = 100),
    year = 2020,
    logSweptArea = log(1000)
  )
  p <- predict(bestmod, newdata = nd, se_fit = TRUE, re_form = NA, offset = nd$logSweptArea)
  
  p2 <- ggplot(p) + 
    aes(x = z, y = exp(est), 
      ymin = exp(est - 1.96 * est_se),
      ymax = exp(est + 1.96 * est_se)) +
    geom_line() +
    geom_ribbon(alpha = 0.4) +
    scale_x_continuous() +
    coord_cartesian(expand = F) +
    # ylim(0,0.7) +
    labs(x = "Depth [m]", y = "Biomass density [t/km^2]")
  print(p2)
  
  (fname <- file.path("output", paste0("depthEffect", "_", fnameAppendix, ".png")))
  png(fname, width = 5, height = 4.5, units = "in", res = 1000)
    print(p2)
  dev.off()
  
  (fname <- file.path("output", paste0("depthEffect", "_", fnameAppendix, ".Rdata")))
  save(p, file = fname)
  
  
  
  # make prediction to grid -------------------------------------------------
  
  # create unique grid and year combinations
  yrs <- sort(unique(dat$year))
  newdat <- expand.grid(id = sort(unique(subset(grd, p == 1)$id)), year = yrs)
  newdat <- cbind(newdat, grd[newdat$id, c("lon", "lat", "X", "Y", "z")])
  dim(newdat)
  
  # Add area 
  Area <- cellSize(ras, unit="km")
  newdat$area <- c(as.array(Area))[newdat$id]
  
  # Add sweptArea
  newdat$logSweptArea <- log(1000) # to get estimates in kg/1000m^2, equaling t/km^2
  
  if(!is.null(offsetVar)){OFFSET <- newdat[,offsetVar]}else{OFFSET <- NULL}
  
  pred <- predict(bestmod, newdata = newdat, type = "response", return_tmb_object = TRUE, 
    offset = OFFSET)
  head(pred$data)
  
  sim <- predict(bestmod, newdata = newdat, nsim = 500, offset = OFFSET)

  pred$data$lwr <- apply(exp(sim), 1, quantile, probs = 0.025)
  pred$data$upr <- apply(exp(sim), 1, quantile, probs = 0.975)
  pred$data$se <- apply(sim, 1, function(x) sd(x))
  pred$data$sd <- apply(exp(sim), 1, function(x) sd(x))
  pred$data$cv <- apply(exp(sim), 1, function(x) sd(x) / mean(x))  
    
  # test plots
  YR <- 2014
  ggplot(subset(pred$data, year == YR), aes(lon, lat, fill = est)) + 
    geom_raster() +
    scale_fill_viridis_c(trans = "sqrt") +
    geom_point(mapping = aes(size = cpue), data = subset(dat, year == YR), 
      pch = 21, fill = NA, stroke = 1) +
    scale_size_area() +
    coord_cartesian(expand = FALSE) +
    guides(fill = guide_legend(title = "Est. [t/km^2]"), 
      size = guide_legend(title = "Obs. [t/km^2]"))  # set legend titles
  
  vals <- grd$p*NaN
  vals[grd$p==1] <- subset(pred$data, year == YR)$est
  r <- ras
  r <- terra::setValues(r, NaN)
  r <- terra::setValues(r, vals)
  image(sqrt(r), col = pals::viridis(100))
  plot(coast, add = T, col = "grey")
  
  # cv
  vals <- grd$p*NaN
  vals[grd$p==1] <- subset(pred$data, year == YR)$cv
  r <- ras
  r <- terra::setValues(r, NaN)
  r <- terra::setValues(r, vals)
  image(r, col = pals::viridis(100))
  plot(coast, add = T, col = "grey")
  embedPlot(expr = {
    imageScale(as.matrix(r), col = pals::viridis(100), axis.pos = 4)
  }, at = c(0.1,0.2,0.5,0.9))
  
  # se
  vals <- grd$p*NaN
  vals[grd$p==1] <- subset(pred$data, year == YR)$se
  r <- ras
  r <- terra::setValues(r, NaN)
  r <- terra::setValues(r, vals)
  image(r, col = pals::viridis(100))
  plot(coast, add = T, col = "grey")
  embedPlot(expr = {
    imageScale(as.matrix(r), col = pals::viridis(100), axis.pos = 4)
  }, at = c(0.1,0.2,0.5,0.9))
  
  # calculate yearly index --------------------------------------------------
  set.seed(1111)
  index <- get_index(pred, area = newdat$area, bias_correct = TRUE)
  
  # save index and prediction
  (fname <- file.path("output", paste0("bestmod_pred_index", "_", fnameAppendix, ".Rdata")))
  save(pred, index, file = fname)
  
  (fname <- file.path("output", paste0("index_w_ci", "_", fnameAppendix, ".png")))
  png(fname, width = 6, height = 5, units = "in", res = 1000)
  p <- ggplot(index, aes(year, est)) +
    geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "grey70") +
    geom_line(lwd = 1, colour = "blue") +
    labs(x = "Year", y = "Biomass [t]")
  print(p)
  dev.off()
  
  
  
  # make terra brick of predictions ---------------------------------------------------------
  yrs <- sort(unique(dat$year))
  b <- terra::rast(x = ext(ras), 
    nrows = nrow(ras), ncols = ncol(ras), 
    nl = length(yrs))
  names(b) <- as.character(yrs)
  dim(b)
  crs(b) <- crs(ras)
  
  for(i in seq(yrs)){
    grd.i <- grd
    pred.i <- subset(pred$data, year == yrs[i])
    grd.i$pred <- NaN
    grd.i$pred[pred.i$id] <- pred.i$est
    
    b[[i]] <- terra::setValues(b[[i]], grd.i$pred)
    print(i)
  }
  
  names(b) <- as.character(yrs)
  
  # plot(b)
  # plot(b, zlim = c(0,10))
  
  # total biomass
  totalB <- apply(as.array(b*Area), MARGIN = 3, FUN = sum, na.rm = T)
  plot(yrs, totalB, t = "o")
  
  # compare to bias adjusted index
  tmp <- data.frame(year = yrs, totalB = totalB, index = index$est)
  tmp
  
  agg <- aggregate(est*area ~ year, pred$data, FUN = sum)
  agg
  merge(tmp, agg)
  
  plot(totalB, index$est)
  text(totalB, index$est, labels = index$year, pos = 3)
  fit <- lm(index$est ~ totalB)
  abline(fit, lty = 2)
  cor(totalB, index$est)
  summary(fit)
  
  (fname <- file.path("output", paste0("bestmod_b", "_", fnameAppendix, ".nc")))
  # fname <- "output/bestmod_b_large.nc"
  writeCDF(b, 
    filename = fname, 
    overwrite = TRUE)
  
  (fname <- file.path("output", paste0("bestmod_b_names", "_", fnameAppendix, ".Rdata")))
  # fname <- "output/bestmod_b_names_large.nc"
  b_names <- as.character(yrs)
  save(b_names, file = fname)
  
  # make terra brick of prediction error (cv & se) ---------------------------------------------------------
  yrs <- sort(unique(dat$year))
  b_cv <- b_se <- terra::rast(x = ext(ras), 
    nrows = nrow(ras), ncols = ncol(ras), 
    nl = length(yrs))
  names(b_cv) <- as.character(yrs)
  names(b_se) <- as.character(yrs)
  
  dim(b_cv); dim(b_se)
  
  crs(b_cv) <- crs(ras)
  crs(b_se) <- crs(ras)
  
  for(i in seq(yrs)){
    grd.i <- grd
    pred.i <- subset(pred$data, year == yrs[i])
    grd.i$cv <- grd.i$se <- NaN
    grd.i$cv[pred.i$id] <- pred.i$cv
    grd.i$se[pred.i$id] <- pred.i$se
    
    b_cv[[i]] <- terra::setValues(b_cv[[i]], grd.i$cv)
    b_se[[i]] <- terra::setValues(b_se[[i]], grd.i$se)
    
    print(i)
  }
  
  (fname <- file.path("output", paste0("bestmod_b_cv", "_", fnameAppendix, ".nc")))
  # fname <- "output/bestmod_b_large.nc"
  writeCDF(b_cv, 
    filename = fname, 
    overwrite = TRUE)

  (fname <- file.path("output", paste0("bestmod_b_se", "_", fnameAppendix, ".nc")))
  # fname <- "output/bestmod_b_large.nc"
  writeCDF(b_se, 
    filename = fname, 
    overwrite = TRUE)

  
  # plots of spatial prediction -------------------------------------------------------------------
  
  yr <- 2022
  plot(b[[which(an(b_names) == yr)]])
  points(lat~lon, subset(dat, year == yr), pch = ".", cex = 2)
  
  ncolor <- 50
  breaks <- exp(seq(log(0.1), log(max(as.array(b), na.rm = T)), length = ncolor+1))
  breaks[1] <- 0
  colors <- adjustcolor(viridis(ncolor), alpha.f = 0.7)
  spp <- "Crangon crangon"
  
  ## individual plots in pdf ----
  
  (fname <- file.path("output", paste0("pred_maps", "_", fnameAppendix, ".pdf")))
  # fname <- "output/pred_maps_large.pdf"
  pdf(fname, width = 5, height = 5, onefile = T)
  for(i in seq(yrs)){
    yr <- yrs[i]
    
    op <- par(mar = c(3,3,1.5,1), ps = 8, mgp = c(2,0.5,0))
  
    image(bathy, col = grey.colors(100, start = 0.5, end = 1), , asp = NA, xlab = "Longitude", ylab = "Latitude")
    # image(bathy, col = NA)
    image(b[[i]], add = TRUE, col = colors, breaks = breaks)
    plot(coast, add = TRUE, col = "white", border = NA)
    points(lat~lon, data = dat, subset = year == yr, pch = ".", cex = 2, col = 1)
    box()
    mtext(paste0(spp, " (", yr, ")"), side = 3, line = 0.25, adj = 0)
    op2 <- par(no.readonly = T)
    sinkr::embedPlot(expr = {
      sinkr::imageScale(1, col = colors, breaks = breaks, axis.pos = 1, 
        xlim = c(breaks[2], breaks[length(breaks)]), 
        log = "x")
      mtext(bquote("Biomass ["*t~km^-2*"]"), side = 1, line = 1.5)
    }, at = c(0.05,0.4, 0.85, 0.88))
    par(op2)
    reltext(relx = 0.05, rely = 0.95, labels = paste0("Total biomass = ", round(subset(index, year == yr)$est), " [t]" ), 
      pos = 4, font = 2, offset = 0)
    
    par(op)
  }
  dev.off()
  
  
  ## individual plots -----
  (fpath <- file.path("output", paste0("prediction_maps", "_", fnameAppendix)))
  dir.create(fpath, showWarnings = F)
  
  for(i in seq(yrs)){
    yr <- yrs[i]
    
    (fname <- file.path(fpath, paste0("pred", "_", yr, ".png")))
    # fname <- file.path("output", "prediction_maps_large", paste0("pred_", yr, ".png"))
    png(fname, width = 4, height = 5, units = "in", res = 400)
  
    yr <- yrs[i]
    
    op <- par(mar = c(3,3,1.5,1), ps = 8, mgp = c(2,0.5,0))
  
    image(bathy, col = grey.colors(100, start = 0.5, end = 1), asp = NA,
      xlab = "Longitude", ylab = "Latitude")
    # image(bathy, col = NA)
    image(b[[i]], add = TRUE, col = colors, breaks = breaks)
    plot(coast, add = TRUE, col = "white", border = NA)
    points(lat~lon, data = dat, subset = year == yr, pch = ".", cex = 2, col = 1)
    box()
    mtext(paste0(spp, " (", yr, ")"), side = 3, line = 0.25, adj = 0)
    op2 <- par(no.readonly = T)
    par(mgp = c(2,0.25,0))
    sinkr::embedPlot(expr = {
      sinkr::imageScale(1, col = colors, breaks = breaks, axis.pos = 1, 
        xlim = c(breaks[2], breaks[length(breaks)]), log = "x", add.axis = F)
      axis(1, cex.axis = 0.7)
      mtext(bquote("Biomass ["*t~km^-2*"]"), side = 1, line = 1, cex = 1)
    }, at = c(0.05,0.4, 0.85, 0.88))
    par(op2)
    reltext(relx = 0.05, rely = 0.95, labels = paste0("Total biomass = ", round(subset(index, year == yr)$est), " [t]" ), 
      pos = 4, font = 2, offset = 0)
    
    par(op)
    dev.off()
  
  }
  
  
  ## single plot -----
  
  nyrs <- length(yrs)
  nside <- ceiling(nyrs^0.5)
  MAT <- matrix(0, nside, nside)
  MAT[seq(nyrs)] <- seq(nyrs)
  MAT <- t(MAT)
  hit <- apply(MAT, MARGIN = 1, function(x){all(x==0)})
  if(sum(hit)>0){MAT <- MAT[-which(hit),]}
  # MAT <- cbind(MAT, MAT[,1]*0+nyrs+1)
  MAT
  LO <- MAT
  LO
  WIDTHS <- c(rep(1, ncol(LO)),0.3)
  HEIGHTS <- rep(1, nrow(LO))
  
  LO <- cbind(LO, nyrs+1)
  op <- par(no.readonly = T)
  layout(LO, widths = WIDTHS, heights = HEIGHTS)
  layout.show(n = max(LO))
  par(op)
  dev.off()
  
  (fname <- file.path("output", paste0("pred_maps2", "_", fnameAppendix, ".png")))
  # fname <- "output/pred_maps2_large.png"
  png(fname, width = 7, height = 6, units = "in", res = 600)
  
  op <- par(mar = c(0.25,0.25,1,0.25), ps = 9, mgp = c(2,0.5,0), oma = c(2,2,0,2))
  layout(LO, widths = WIDTHS, heights = HEIGHTS)
  # layout.show(n = max(LO))
  par(cex = 1)
  
  for(i in seq(yrs)){
    yr <- yrs[i]
    image(bathy, col = grey.colors(100, start = 0.5, end = 1), xlab = "", ylab = "", 
      axes = F, asp = NA)
    image(b[[i]], add = TRUE, col = colors, breaks = breaks)
    plot(coast, add = TRUE, col = "white", border = NA)
    points(lat~lon, data = dat, subset = year == yr, pch = ".", cex = 1, col = 1)
    box()
    if(i %in% LO[nrow(LO),]){axis(1)}
    if(i %in% LO[,1]){axis(2)}
    mtext(yr, side = 3, line = 0, adj = 0, font = 2, cex = 1.25)
    reltext(relx = 0.98, rely = 0.05, labels = paste0(round(subset(index, year == yr)$est), " t" ), 
      pos = 2, font = 1, offset = 0, cex = 0.9)
  }
  mtext("Longitude", side = 1, outer = T, line = 1)
  mtext("Latitude", side = 2, outer = T, line = 1)
  par(mar = c(8,0.5,8,0.5))
  sinkr::imageScale(1, col = colors, breaks = breaks, axis.pos = 4, 
    ylim = c(breaks[2], breaks[length(breaks)]), log = "y")
  mtext(bquote("Biomass ["*t~km^-2*"]"), side = 4, line = 1.5)
  
  par(op)
  dev.off()
  





  # plots of spatial prediction error -------------------------------------------------------------------
  
  ## single plot -----
  
  ### cv -----
  
  yr <- 2022
  plot(b_cv[[which(an(b_names) == yr)]])
  points(lat~lon, subset(dat, year == yr), pch = ".", cex = 2)
  
  ncolor <- 50
  breaks <- exp(seq(log(0.1), log(max(as.array(b_cv), na.rm = T)), length = ncolor+1))
  breaks[1] <- 0
  colors <- adjustcolor(viridis(ncolor), alpha.f = 0.7)
  spp <- "Crangon crangon"
  
  nyrs <- length(yrs)
  nside <- ceiling(nyrs^0.5)
  MAT <- matrix(0, nside, nside)
  MAT[seq(nyrs)] <- seq(nyrs)
  MAT <- t(MAT)
  hit <- apply(MAT, MARGIN = 1, function(x){all(x==0)})
  if(sum(hit)>0){MAT <- MAT[-which(hit),]}
  # MAT <- cbind(MAT, MAT[,1]*0+nyrs+1)
  MAT
  LO <- MAT
  LO
  WIDTHS <- c(rep(1, ncol(LO)),0.3)
  HEIGHTS <- rep(1, nrow(LO))
  
  LO <- cbind(LO, nyrs+1)
  op <- par(no.readonly = T)
  layout(LO, widths = WIDTHS, heights = HEIGHTS)
  layout.show(n = max(LO))
  par(op)
  dev.off()
  
  (fname <- file.path("output", paste0("cv_maps", "_", fnameAppendix, ".png")))
  # fname <- "output/pred_maps2_large.png"
  png(fname, width = 7, height = 6, units = "in", res = 600)
  
  op <- par(mar = c(0.25,0.25,1,0.25), ps = 9, mgp = c(2,0.5,0), oma = c(2,2,0,2))
  layout(LO, widths = WIDTHS, heights = HEIGHTS)
  # layout.show(n = max(LO))
  par(cex = 1)
  
  for(i in seq(yrs)){
    yr <- yrs[i]
    image(bathy, col = grey.colors(100, start = 0.5, end = 1), xlab = "", ylab = "", 
      axes = F, asp = NA)
    image(b_cv[[i]], add = TRUE, col = colors, breaks = breaks)
    plot(coast, add = TRUE, col = "white", border = NA)
    points(lat~lon, data = dat, subset = year == yr, pch = ".", cex = 1, col = 1)
    box()
    if(i %in% LO[nrow(LO),]){axis(1)}
    if(i %in% LO[,1]){axis(2)}
    mtext(yr, side = 3, line = 0, adj = 0, font = 2, cex = 1.25)
  }
  mtext("Longitude", side = 1, outer = T, line = 1)
  mtext("Latitude", side = 2, outer = T, line = 1)
  par(mar = c(8,0.5,8,0.5))
  sinkr::imageScale(1, col = colors, breaks = breaks, axis.pos = 4, 
    ylim = c(breaks[2], breaks[length(breaks)]), log = "y")
  mtext(bquote("Prediction CV"), side = 4, line = 1.5)
  
  par(op)
  dev.off()

  
  ### se -----
  
  yr <- 2022
  plot(b_se[[which(an(b_names) == yr)]])
  points(lat~lon, subset(dat, year == yr), pch = ".", cex = 2)
  
  ncolor <- 50
  breaks <- exp(seq(log(0.1), log(max(as.array(b_se), na.rm = T)), length = ncolor+1))
  breaks[1] <- 0
  colors <- adjustcolor(viridis(ncolor), alpha.f = 0.7)
  spp <- "Crangon crangon"
  
  nyrs <- length(yrs)
  nside <- ceiling(nyrs^0.5)
  MAT <- matrix(0, nside, nside)
  MAT[seq(nyrs)] <- seq(nyrs)
  MAT <- t(MAT)
  hit <- apply(MAT, MARGIN = 1, function(x){all(x==0)})
  if(sum(hit)>0){MAT <- MAT[-which(hit),]}
  # MAT <- cbind(MAT, MAT[,1]*0+nyrs+1)
  MAT
  LO <- MAT
  LO
  WIDTHS <- c(rep(1, ncol(LO)),0.3)
  HEIGHTS <- rep(1, nrow(LO))
  
  LO <- cbind(LO, nyrs+1)
  op <- par(no.readonly = T)
  layout(LO, widths = WIDTHS, heights = HEIGHTS)
  layout.show(n = max(LO))
  par(op)
  dev.off()
  
  (fname <- file.path("output", paste0("se_maps", "_", fnameAppendix, ".png")))

  png(fname, width = 7, height = 6, units = "in", res = 600)
  
  op <- par(mar = c(0.25,0.25,1,0.25), ps = 9, mgp = c(2,0.5,0), oma = c(2,2,0,2))
  layout(LO, widths = WIDTHS, heights = HEIGHTS)
  # layout.show(n = max(LO))
  par(cex = 1)
  
  for(i in seq(yrs)){
    yr <- yrs[i]
    image(bathy, col = grey.colors(100, start = 0.5, end = 1), xlab = "", ylab = "", 
      axes = F, asp = NA)
    image(b_se[[i]], add = TRUE, col = colors, breaks = breaks)
    plot(coast, add = TRUE, col = "white", border = NA)
    points(lat~lon, data = dat, subset = year == yr, pch = ".", cex = 1, col = 1)
    box()
    if(i %in% LO[nrow(LO),]){axis(1)}
    if(i %in% LO[,1]){axis(2)}
    mtext(yr, side = 3, line = 0, adj = 0, font = 2, cex = 1.25)
  }
  mtext("Longitude", side = 1, outer = T, line = 1)
  mtext("Latitude", side = 2, outer = T, line = 1)
  par(mar = c(8,0.5,8,0.5))
  sinkr::imageScale(1, col = colors, breaks = breaks, axis.pos = 4, 
    ylim = c(breaks[2], breaks[length(breaks)]), log = "y")
  mtext(bquote("Prediction SE"), side = 4, line = 1.5)
  
  par(op)
  dev.off()

  
} # end of size loop

