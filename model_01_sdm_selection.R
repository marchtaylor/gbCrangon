
# clear environment -------------------------------------------------------

rm(list = ls())


# load data ---------------------------------------------------------------

load(file = "data/dat.Rdata")
load(file = "data/mesh.Rdata")
load(file = "data/grd.Rdata")
bathy <- rast("data/bathy.tif")
ras <- rast("data/ras.tif")
load(file = "data/orig_crs.Rdata")
load(file = "data/target_crs.Rdata")
load(file = "data/coast.Rdata")

dat$logSweptArea <- log(dat$sweptArea)
dat$cpue_sm <- dat$cpue * dat$p_sm
dat$cpue_lg <- dat$cpue * dat$p_lg
dat$b_sm <- dat$b * dat$p_sm
dat$b_lg <- dat$b * dat$p_lg
dat$pa_sm <- as.numeric(dat$cpue_sm > 0) # recalculate presence/absence
dat$pa_lg <- as.numeric(dat$cpue_lg > 0) # recalculate presence/absence
dat$pa <- as.numeric(dat$cpue > 0) # recalculate presence/absence

# settings --------
offsetVar <- "logSweptArea" # NULL to remove

sizeGroups <- c("small", "large", "combined")
for(size in sizeGroups){

  fnameAppendix <- size
  
  if(size == "small"){
    dat$B <- dat$b_sm
    dat$PA <- dat$pa_sm
  }
  if(size == "large"){
    dat$B <- dat$b_lg
    dat$PA <- dat$pa_lg
  }
  if(size == "combined"){
    dat$B <- dat$b
    dat$PA <- dat$pa
  }
  
  
  
  
  # define k-folds for CV  ---------------------------------------------------
  
  # Correlogram
  corrRes <- vector("list", length(unique(dat$year)))
  names(corrRes) <- sort(unique(dat$year))
  for(i in seq(corrRes)){
    set.seed(i+1111)
    datsub <- subset(dat, year == an(names(corrRes)[i]))
    corrRes[[i]] <- correlog(x = datsub$X, y = datsub$Y, z = log(datsub$B+1), increment = 2, resamp = 20)
    print(names(corrRes)[i])
  }
  
  plot(correlation ~ mean.of.class, as.data.frame(corrRes[[1]][c("correlation", "mean.of.class")]))
  tmp <- lapply(corrRes, FUN = function(x){as.data.frame(x[c("correlation", "mean.of.class")])})
  for(i in seq(tmp)){
    tmp[[i]]$year <- names(corrRes)[i]
  }
  tmp <- do.call("rbind", tmp)
  plot(correlation ~ mean.of.class, tmp)
  fit <- gam(correlation ~ s(mean.of.class, k = -1)-1, data = tmp)
  tmp <- cbind(tmp, as.data.frame(predict.gam(fit, newdata = tmp, se.fit = T)))
  head(tmp)
  lines(fit ~ mean.of.class, tmp[order(tmp$mean.of.class),], col = 4, lwd = 2)
  lines(fit+1.96*se.fit ~ mean.of.class, tmp[order(tmp$mean.of.class),], col = 4, lwd = 2, lty = 2)
  lines(fit-1.96*se.fit ~ mean.of.class, tmp[order(tmp$mean.of.class),], col = 4, lwd = 2, lty = 2)
  
  
  plot(fit)
  
  
  
  nfold <- 5
  gridSize <- 15 # km
  
  (xGrid <- ceiling(diff(range(dat$X))/gridSize)) # number of grids in lon direction 
  (yGrid <- ceiling(diff(range(dat$Y))/gridSize)) # number of grids in lat direction 
  
  
  dat2 <- sf::st_as_sf(
    dat[,c("X", "Y")],
    # "coords" is in x/y order -- so longitude goes first!
    coords = c("X", "Y"),
    # Set our coordinate reference system to EPSG:4326,
    # the standard WGS84 geodetic coordinate reference system
    crs = target_crs
  )
  
  set.seed(1234)
  # clust1 <- spatialsample::spatial_clustering_cv(dat2, v = 20)
  # autoplot(clust1)
  
  clust2 <- spatialsample::spatial_block_cv(dat2, v = 5, n = c(xGrid, yGrid))
  autoplot(clust2)
  
  
  # extract grid folds
  fold_id_vector <- rep(NA, nrow(dat))
  for (i in seq_along(clust2$splits)) {
    fold_indices <- which(!seq(nrow(dat)) %in% clust2$splits[[i]]$in_id)
    fold_id_vector[fold_indices] <- i
  }
  dat$grid_fold <- fold_id_vector
  
  # randomize spatial fold by year
  dat$grid_fold2 <- NaN
  fold_ids <- sort(unique(dat$grid_fold))
  years <- sort(unique(dat$year))
  for(i in seq(years)){
    year.i <- years[i]
    hit <- which(dat$year == year.i)
    fold1 <- dat$grid_fold[hit]
    tmp <- sample(fold_ids)
    fold2 <- tmp[fold1]
    dat$grid_fold2[hit] <- fold2
  }
  head(dat)
  plot(grid_fold2~grid_fold, dat, pch = 16, col = adjustcolor(1,0.1))
  
  plot(Y~X, data = subset(dat, year == 2021), pch = 16, col = grid_fold2, 
    xlim = range(dat$X), ylim = range(dat$Y))
  
  
  # fit models --------------------------------------------------------------
  ## model info lookup -----
  
  modlut <- as.data.frame(rbind(
    c("mod1cv_tw_s", "CV fit of tweedie with spatial random field"),
    c("mod2cv_tw_st_iid", "CV fit of tweedie with independent and identically distributed (iid) spatiotemporal field"),
    c("mod3cv_tw_st_ar1", "CV fit of tweedie with spatiotemporal random field with stationary, auto-regressive (ar1) process"),
    c("mod4cv_tw_st_rw", "CV fit of tweedie with spatiotemporal random field with random walk (rw) process")
  ))
  names(modlut) <- c("name", "desc")
  modlut
  
  
  
  ## mod1cv_tw_s -------------------------------------------------------------
  
  # future::plan(sequential)
  future::plan(multisession, workers = nfold)
  
  t1 <- Sys.time()
  mod1cv_tw_s <- sdmTMB_cv(
    B ~ s(z, k = 10),
    data = dat, 
    offset = 'logSweptArea',
    mesh = mesh,
    family = tweedie(link = "log"),
    spatial = "on",
    reml = FALSE,
    fold_ids = dat$grid_fold2,
    parallel = TRUE,
    use_initial_fit = TRUE
  )
  t2 <- Sys.time()
  t2-t1
  beepr::beep()
  
  # mod1cv_tw_s$fold_loglik 
  # mod1cv_tw_s$sum_loglik 
  

  ## mod2cv_tw_st_iid -------------------------------------------------------------
  future::plan(multisession, workers = nfold)
  
  t1 <- Sys.time()
  mod2cv_tw_st_iid <- sdmTMB_cv(
    B ~ s(z, k = 10),
    data = dat,
    offset = 'logSweptArea',
    mesh = mesh,
    family = tweedie(link = "log"),
    time = "year", 
    spatiotemporal = "iid",
    spatial = "on",
    reml = FALSE,
    fold_ids = dat$grid_fold2,
    parallel = TRUE,
    use_initial_fit = FALSE # probably only makes sense when nfold >> ncores
  )
  t2 <- Sys.time()
  t2-t1
  beepr::beep()
  
  
  ## mod3cv_tw_st_ar1 -------------------------------------------------------------
  future::plan(multisession, workers = nfold)
  
  t1 <- Sys.time()
  mod3cv_tw_st_ar1 <- sdmTMB_cv(
    B ~ s(z, k = 10),
    data = dat,
    offset = 'logSweptArea',
    mesh = mesh,
    family = tweedie(link = "log"),
    time = "year", 
    spatiotemporal = "ar1",
    spatial = "on",
    reml = FALSE,
    fold_ids = dat$grid_fold2,
    parallel = TRUE,
    use_initial_fit = FALSE # probably only makes sense when nfold >> ncores
  )
  t2 <- Sys.time()
  t2-t1
  beepr::beep()
  
  ## mod4cv_tw_st_rw -------------------------------------------------------------
  future::plan(multisession, workers = nfold)
  
  t1 <- Sys.time()
  mod4cv_tw_st_rw <- sdmTMB_cv(
    B ~ s(z, k = 10),
    data = dat,
    offset = 'logSweptArea',
    mesh = mesh,
    family = tweedie(link = "log"),
    time = "year", 
    spatiotemporal = "rw",
    spatial = "on",
    reml = FALSE,
    fold_ids = dat$grid_fold2,
    parallel = TRUE,
    use_initial_fit = FALSE # probably only makes sense when nfold >> ncores
  )
  t2 <- Sys.time()
  t2-t1
  beepr::beep()

  
  
  # calculate prediction stats ------------------------
  
  MRE <- MARE <-  MAE <- MdAE <- RMSE <- logRMSE <- logLik <- seq(modlut$name)*NaN

  for(i in seq(modlut$name)){
    mod <- get(modlut$name[i])
    nonZero <- which(mod$data$B != 0)
    est <- mod$data$cv_predicted
    obs <- mod$data$B
    rerr <- ((est-obs)/obs) # relative error
    err <- obs-est # residuals
    
    MRE[i] <- median(rerr)
    MARE[i] <- median(abs(rerr))
    MAE[i] <- mean(abs(err))
    MdAE[i] <- median(abs(err))
    
    RMSE[i] <- sqrt(mean((err)^2))
    logRMSE[i] <- sqrt(mean((log(obs+1) - log(est+1))^2))

    logLik[i] <- with(mod, sum_loglik)
  }
  
  modlut$mre <- MRE
  modlut$mare <- MARE
  modlut$mae <- MAE
  modlut$mdae <- MdAE
  
  modlut$rmse <- RMSE
  modlut$logrmse <- logRMSE

  modlut$logLik <- logLik
  modlut
  
  # determine bestmod --------------------------------
  
  (bestModelName <- modlut$name[which.max(modlut$logLik)])
  bestmodcv <- get(bestModelName)
  
  
  
  
  # refit best model --------------------------------------------------------
  
  ST <- strsplit(bestModelName, "_")[[1]][4]
  
  t1 <- Sys.time()
  bestmod <- sdmTMB(
    B ~ s(z, k = 10),
    data = dat,
    offset = "logSweptArea",
    mesh = mesh,
    family = tweedie(link = "log"),
    time = "year", 
    spatiotemporal = ST,
    spatial = "on",
    reml = TRUE
  )
  t2 <- Sys.time()
  t2-t1
  beepr::beep()
  
  # summary(bestmod)
  # sanity(bestmod)
  # tmp <- residuals(bestmod, type = "mle-mvn") # randomized quantile residuals
  # qqnorm(tmp)
  # qqline(tmp)
  
  # sbestmod <- simulate(bestmod, nsim = 500, type = "mle-mvn")
  # 
  # r_bestmod <- dharma_residuals(sbestmod, bestmod, return_DHARMa = TRUE)
  # plot(r_bestmod)
  # tmp <- DHARMa::testResiduals(r_bestmod)
  
  # save outputs ------------------------------------------------------------
  
  save(bestmodcv, file = file.path("model", paste0("bestmodcv", "_", fnameAppendix, ".Rdata")))
  save(bestmod, file = file.path("model", paste0("bestmod", "_", fnameAppendix, ".Rdata")))
  save(modlut, file = file.path("model", paste0("modlut", "_", fnameAppendix, ".Rdata")))
  save(clust2, file = file.path("model", paste0("clust2", "_", fnameAppendix, ".Rdata")))
  save(dat, file = file.path("model", paste0("dat", "_", fnameAppendix, ".Rdata")))

}
