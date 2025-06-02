
# clear environment -------------------------------------------------------

rm(list = ls())



# load data ---------------------------------------------------------------


fnameAppendices <- c("small", "large", "combined")
Lindex <- vector("list", length(fnameAppendices))
# Lpred <- vector("list", length(fnameAppendices))

## read indices
for(i in seq(fnameAppendices)){
  fnameAppendix <- fnameAppendices[i]
  (fname <- file.path("output", paste0("bestmod_pred_index", "_", fnameAppendix, ".Rdata")))
  load(file = fname)
  index$fraction <- fnameAppendix
  Lindex[[i]] <- index
}
names(Lindex) <- fnameAppendices

# read LPUE data
landStats <- readRDS(file = "data/wgcran_adjusted.RDS")
head(landStats)


modDR <- readRDS("output/discardRate_gam.rds")
lutDR <- readRDS("output/prediction_discardRate.rds")
lutDR$year <- lutDR$yeardec %/% 1



# index plot -------------------------------
index_added <- Lindex[["small"]]
index_added$est <- Lindex[["small"]]$est + Lindex[["large"]]$est
index_added$lwr <- Lindex[["small"]]$lwr + Lindex[["large"]]$lwr
index_added$upr <- Lindex[["small"]]$upr + Lindex[["large"]]$upr
index_added$fraction <- "added"
Lindex2 <- Lindex
Lindex2$added <- index_added

dfIdx <- do.call("rbind", Lindex2)

dfIdx$fraction <- factor(dfIdx$fraction, levels = c("combined", "added", "small", "large"))

p <- ggplot(subset(dfIdx, fraction != "added")) + 
  aes(x = year, y = est, ymin = lwr, ymax = upr, group = fraction) +
  geom_ribbon(fill = "grey", alpha = 0.5, ) +
  geom_line(linewidth = 0.7) + 
  facet_wrap(~fraction, ncol = 1, scales = "free_y") +
  theme_bw() +
  labs(y = "Biomass estimate [t]", 
    x = NULL) + 
  scale_x_continuous(breaks = seq(min(dfIdx$year), max(dfIdx$year), by = 2)) +
  theme(
    plot.title = element_text(size = 12),
    legend.title = element_text(size = 8),
    legend.text =  element_text(size = 6),
    legend.position = c(0.8,0.85), 
    # legend.direction = "horizontal",
    text = element_text(size = 7),
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 8), 
    strip.text = element_text(size = 7),
    legend.background = element_blank(),
    legend.key.height = unit(0.7, "lines")
  )
print(p)

png("output/indices_all_wCI.png", width = 3.5, height = 5, units = "in", res = 1000)
print(p)
dev.off()

# ratio of small to large -------------------------------------------------

dftmp <- do.call("rbind", Lindex2)
dftmp <- dftmp[,c("year", "est", "fraction")] |> pivot_wider(names_from = fraction, values_from = est)
dftmp$ratioLarge <- dftmp$large / (dftmp$small + dftmp$large)
dftmp$postMan <- dftmp$year>=2016 # post-Management change year
dftmp

fit0 <- glm(ratioLarge~postMan, data = dftmp, family = binomial(link = "logit"))
fit <- step(fit0)
summary(fit)
dftmp$pred <- predict(fit, newdata = dftmp, type = "response")

p <- ggplot(dftmp) + aes(x = year, y = ratioLarge, color = postMan) +
  geom_line(mapping = aes(x = year, y = pred), color = "grey", linewidth = 1, show.legend = F) +
  geom_point(show.legend = F) + 
  scale_color_brewer(palette = "Dark2", type = "qual") +
  theme_bw() +
  labs(y = "Biomass ratio (large/(small + large)", 
    x = NULL) + 
  scale_x_continuous(breaks = seq(min(dftmp$year), max(dftmp$year), by = 2)) +
  
  theme(
    plot.title = element_text(size = 12),
    legend.title = element_text(size = 8),
    legend.text =  element_text(size = 6),
    legend.position = c(0.8,0.85), 
    # legend.direction = "horizontal",
    text = element_text(size = 7),
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 8), 
    strip.text = element_text(size = 7),
    legend.background = element_blank(),
    legend.key.height = unit(0.7, "lines")
  )
print(p)

png("output/ratioLarge+pred.png", width = 3.5, height = 3, units = "in", res = 400)
print(p)
dev.off()


# lpue~biomass ------------------------------------------------------------


## Sep total -----
mo_incl <- 9
agg <- aggregate(landings ~ year, data = landStats, subset = month %in% mo_incl, FUN = "sum", na.rm = T)
agg <- merge(agg, aggregate(effort ~ year, data = landStats, subset = month %in% mo_incl, FUN = "sum", na.rm = T))
agg$lpue <- agg$landings / agg$effort * 1000
# head(agg)

LindexSep <- lapply(Lindex, FUN = function(x){
  tmp <- merge(x = x, y = agg, all.x = T)
  tmp$q <- tmp$lpue / tmp$est
  tmp$period <- "Sep"
  return(tmp)
})


# fit models
LregrSep <- lapply(LindexSep, FUN = function(x){
  tmp <- x
  fit <- glm(lpue ~ log(est), data = tmp, family = gaussian(link = "log"))
  R2 <- 1 - fit$deviance/fit$null.deviance
  res <- list(fit = fit, R2 = R2)
  return(res)
})

lapply(LregrSep, function(x){x$R2})


## Fall total -----
mo_incl <- 9:12
agg <- aggregate(landings ~ year, data = landStats, subset = month %in% mo_incl, FUN = "sum", na.rm = T)
agg <- merge(agg, aggregate(effort ~ year, data = landStats, subset = month %in% mo_incl, FUN = "sum", na.rm = T))
agg$lpue <- agg$landings / agg$effort * 1000
# head(agg)

LindexFall <- lapply(Lindex, FUN = function(x){
  tmp <- merge(x = x, y = agg, all.x = T)
  tmp$q <- tmp$lpue / tmp$est
  tmp$period <- "Fall"
  tmp$fraction <- 
  return(tmp)
})


# fit models
LregrFall <- lapply(LindexFall, FUN = function(x){
  tmp <- x
  fit <- glm(lpue ~ log(est), data = tmp, family = gaussian(link = "log"))
  R2 <- 1 - fit$deviance/fit$null.deviance
  res <- list(fit = fit, R2 = R2)
  return(res)
})

lapply(LregrFall, function(x){x$R2})

names(LindexSep) <- names(LregrSep) <- 
  names(LindexFall) <- names(LregrFall) <- fnameAppendices


df <- rbind(do.call("rbind", LindexSep),
  do.call("rbind", LindexFall))

df2 <- rbind(
  data.frame(period = "Sep", fraction = names(LregrSep), 
    r2 = unlist(lapply(LregrSep, function(x){
    x$R2
  }))),
  data.frame(period = "Fall", fraction = names(LregrFall), 
  r2 = unlist(lapply(LregrFall, function(x){
    x$R2
  })))
)

df2$label <- paste0("italic(R)^2 == ", sprintf("%.2f", df2$r2))
df2


rlines <- rbind(
  do.call("rbind", lapply(LregrSep, FUN = function(x){
    tmp <- x$fit$data
    df. <- data.frame(period = tmp$period[1], fraction = tmp$fraction[1], 
      est = seqRan(c(1e-8, tmp$est*1.1), 1000))
    
    predictions_link <- predict(x$fit, newdata = df., type = "link", se.fit = TRUE)
    link_fit <- predictions_link$fit
    link_se <- predictions_link$se.fit
    link_lower_ci <- link_fit - 1.96 * link_se
    link_upper_ci <- link_fit + 1.96 * link_se
    response_fit <- x$fit$family$linkinv(link_fit)
    response_lower_ci <- x$fit$family$linkinv(link_lower_ci)
    response_upper_ci <- x$fit$family$linkinv(link_upper_ci)
    
    df.$lpue <- response_fit
    df.$upper <- response_upper_ci
    df.$lower <- response_lower_ci
    df.
  })),
  do.call("rbind", lapply(LregrFall, FUN = function(x){
    tmp <- x$fit$data
    df. <- data.frame(period = tmp$period[1], fraction = tmp$fraction[1], 
      est = seqRan(c(1e-8, tmp$est*1.1), 1000))
    
    predictions_link <- predict(x$fit, newdata = df., type = "link", se.fit = TRUE)
    link_fit <- predictions_link$fit
    link_se <- predictions_link$se.fit
    link_lower_ci <- link_fit - 1.96 * link_se
    link_upper_ci <- link_fit + 1.96 * link_se
    response_fit <- x$fit$family$linkinv(link_fit)
    response_lower_ci <- x$fit$family$linkinv(link_lower_ci)
    response_upper_ci <- x$fit$family$linkinv(link_upper_ci)
    
    df.$lpue <- response_fit
    df.$upper <- response_upper_ci
    df.$lower <- response_lower_ci
    df.
  }))    
)


df$postMan <- df$year >= 2016
df$fraction <- factor(df$fraction, levels = c("small", "large", "combined"))
df$period <- factor(df$period, levels = c("Sep", "Fall"), labels = c("Sep only", "Sep-Dec"))
df2$fraction <- factor(df2$fraction, levels = c("small", "large", "combined"))
df2$period <- factor(df2$period, levels = c("Sep", "Fall"), labels = c("Sep only", "Sep-Dec"))
rlines$fraction <- factor(rlines$fraction, levels = c("small", "large", "combined"))
rlines$period <- factor(rlines$period, levels = c("Sep", "Fall"), labels = c("Sep only", "Sep-Dec"))

set.seed(2)
p <- ggplot(data = df) + aes(x = est, y = lpue) + 
  facet_grid(period ~ fraction, scales = "free") + 
  geom_ribbon(data = rlines, mapping = aes(ymin = lower, ymax = upper), fill = "grey", alpha = 0.3) +
  geom_line(data = rlines, colour = "grey", linewidth = 1) +
  geom_point(show.legend = F) + 
  geom_text_repel(mapping = aes(label = year), size = 2.5, 
    min.segment.length = 0, show.legend = F, color = "grey20") + 
  coord_cartesian(ylim = c(0, NA), xlim = c(0, NA)) +
  geom_label(data = df2, aes(x = 0, y = 1800, label = label), 
    hjust = 0, vjust = 1, 
    parse = TRUE, size = 8, size.unit = "pt", color = "grey20", fontface = "bold") +
  scale_color_brewer(palette = "Dark2", type = "qual") +
  theme_bw() +
  labs(x = "Biomass estimate [t]", 
    y = "Landings per unit effort (LPUE) [t / 1000 days at sea]") + 
  theme(
    plot.title = element_text(size = 12),
    legend.title = element_text(size = 9),
    legend.text =  element_text(size = 9),
    legend.position = "bottom", 
    legend.direction = "horizontal",
    text = element_text(size = 8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 9), 
    strip.text = element_text(size = 9),
  )

# print(p)


png("output/index_vs_lpue_by_period+fraction.png", width = 6.6, height = 4.5, 
  units = "in", res = 1000)
print(p)
dev.off()




# Reconstruction with combined biomass index ----------------------------------------------------------

# full data set plus estimated catch
recon <- landStats
# 
# recon$year <- recon$Year
# recon$month <- recon$Month
# recon$date <- as.Date(paste(recon$Year, recon$Month, 15, sep = "-"))
# recon <- subset(recon, Year >= 2000)[,c("year", "month", "date", "landings", "effort", "lpue")]

## add discard rate ----
recon <- merge(x = recon, y = lutDR[,c("year", "month", "DR")], all.x = TRUE)
recon <- recon[order(recon$date),]

## estimate catch ----
recon$catch <- recon$landings / (1-recon$DR)
recon$cpue <- recon$catch / recon$effort * 1000
plot(catch ~ date, recon, t = "l")
lines(landings ~ date, recon, col = 4)

## model biomass ~ catch -----
df <- subset(recon, month == 9 & year >= 2012)
df <- merge(x = df, y = LindexSep[["combined"]][,c("year", "est")], all.x = TRUE)
bfit <- glm(formula = est ~ log(cpue), family = gaussian(link = "log"), 
  data = df)
summary(bfit)

bfit.inv <- glm(formula = cpue ~ log(est), family = gaussian(link = "log"), 
  data = df)
summary(bfit.inv)
1 - bfit.inv$deviance/bfit.inv$null.deviance

df$pred <- predict(bfit, type = "response")
plot(pred ~ est, df, log = "xy"); abline(0,1)
SSres <- sum(resid(bfit)^2)
SStot <- sum(scale(df$est, scale = F)^2)
R2 <- 1 - bfit$deviance/bfit$null.deviance
R2

df2 <- data.frame(r2 = R2)
df2$label <- paste0("italic(R)^2 == ", sprintf("%.2f", df2$r2))
df2


### plot  -----
tmp <- bfit$data
nd <- data.frame(cpue = seqRan(c(1e-8, tmp$cpue*1.2), 1000))


# Predict with standard errors with type = "link"
predictions_link <- predict(bfit, newdata = nd, type = "link", se.fit = TRUE)

# Calculate the 95% confidence intervals on the link scale
link_fit <- predictions_link$fit
link_se <- predictions_link$se.fit
link_lower_ci <- link_fit - 1.96 * link_se
link_upper_ci <- link_fit + 1.96 * link_se

# Transform the predictions and CIs back to the response scale
response_fit <- bfit$family$linkinv(link_fit)
response_lower_ci <- bfit$family$linkinv(link_lower_ci)
response_upper_ci <- bfit$family$linkinv(link_upper_ci)

nd$est <- response_fit
nd$upper <- response_upper_ci
nd$lower <- response_lower_ci


p <- ggplot(data = df) + aes(x = cpue, y = est) + 
  geom_ribbon(data = nd, mapping = aes(ymin = lower, ymax = upper), fill = "grey", alpha = 0.3) +
  geom_line(data = nd, colour = "grey", linewidth = 1) +
  geom_point() + 
  # 
  # geom_line(data = nd, colour = "grey", linewidth = 1) +
  # geom_point() + 
  geom_text_repel(mapping = aes(label = year), seed = 3, size = 2.5, 
    min.segment.length = 0, show.legend = F, color = "grey20") + 
  coord_cartesian(ylim = c(0, max(df$est)), xlim = c(0, max(df$cpue))) +
  geom_label(data = df2, aes(x = 0, y = 20000, label = label), 
    hjust = 0, vjust = 1, 
    parse = TRUE, size = 8, size.unit = "pt", 
    color = "grey20", fontface = "bold") +
  theme_bw() +
  labs(y = "Biomass estimate [t]", 
    x = "Catch per unit effort (CPUE) [t / 1000 days at sea]") + 
  theme(
    plot.title = element_text(size = 12),
    legend.title = element_text(size = 9),
    legend.text =  element_text(size = 9),
    legend.position = "bottom", 
    legend.direction = "horizontal",
    text = element_text(size = 8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 9), 
    strip.text = element_text(size = 9)
  )

print(p)


png("output/cpueRecon_vs_index.png", width = 3.5, height = 3, 
  units = "in", res = 1000)
print(p)
dev.off()


### catchability -----
dat <- bfit$data
dat$q <- dat$cpue / dat$est

qfit <- glm(q ~ log(est), data = dat, family = gaussian(link = "log"))
summary(qfit)
R2 <- 1 - qfit$deviance/qfit$null.deviance
R2

qfit <- glm(q ~ I(1/est), data = dat, family = gaussian(link = "log"))
summary(qfit)
R2 <- 1 - qfit$deviance/qfit$null.deviance
R2


df2 <- data.frame(r2 = R2)
df2$label <- paste0("italic(R)^2 == ", sprintf("%.2f", df2$r2))
df2


nd <- data.frame(est = seqRan(c(0,dat$est*1.2), length.out = 1000))

# Predict with standard errors with type = "link"
predictions_link <- predict(qfit, newdata = nd, type = "link", se.fit = TRUE)

# Calculate the 95% confidence intervals on the link scale
link_fit <- predictions_link$fit
link_se <- predictions_link$se.fit
link_lower_ci <- link_fit - 1.96 * link_se
link_upper_ci <- link_fit + 1.96 * link_se

# Transform the predictions and CIs back to the response scale
response_fit <- qfit$family$linkinv(link_fit)
response_lower_ci <- qfit$family$linkinv(link_lower_ci)
response_upper_ci <- qfit$family$linkinv(link_upper_ci)

nd$q <- response_fit
nd$upper <- response_upper_ci
nd$lower <- response_lower_ci

plot(q~est, dat, ylim = c(0, max(q)), xlim = c(0,max(est)))
lines(q~est, nd)
lines(upper~est, nd, lty = 2)
lines(lower~est, nd, lty = 2)


set.seed(1)
p <- ggplot(data = dat) + aes(x = est, y = q) + 
  geom_ribbon(data = nd, mapping = aes(ymin = lower, ymax = upper), fill = "grey", alpha = 0.3) +
  geom_line(data = nd, colour = "grey", linewidth = 1) +
  geom_point() + 
  geom_text_repel(mapping = aes(label = year), size = 2.5, 
    min.segment.length = 0, show.legend = F, color = "grey20") + 
  coord_cartesian(ylim = c(0, max(dat$q)), xlim = c(0, max(dat$est))) +
  geom_label(data = df2, aes(x = 0, y = 0.05, label = label), 
    hjust = 0, vjust = 1, 
    parse = TRUE, size = 8, size.unit = "pt", 
    color = "grey20", fontface = "bold") +
  theme_bw() +
  labs(y = "Catchability (q) [t / 1000 days at sea / t]", 
    x = "Biomass [t]") + 
  theme(
    plot.title = element_text(size = 12),
    legend.title = element_text(size = 9),
    legend.text =  element_text(size = 9),
    legend.position = "bottom", 
    legend.direction = "horizontal",
    text = element_text(size = 8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 9), 
    strip.text = element_text(size = 9)
  )

print(p)

png("output/q_vs_index.png", width = 3.5, height = 3.5, 
  units = "in", res = 1000)
print(p)
dev.off()

# predict biomass in full data set -----
recon$biomass <- predict(bfit, newdata = recon, type = "response")

# replace Sept values with sdmTMB estimates
mat <- match(df$date, table = recon$date)
recon$biomass[mat] <- df$est
plot(biomass ~ date, recon, t = "l")

# calculate mortality
recon$hr <- recon$catch / recon$biomass
recon$FMmo <- -log(1-recon$hr)
head(recon)

agg <- aggregate(FMmo~year, data = recon, FUN = "sum")
names(agg) <- c("year", "FMyr")
agg$date <- as.Date(paste(agg$year, "-07-01", sep = ""))
barplot(FMyr ~ year, agg)


# surplus production
recon$dBiomass <- c(recon$biomass[-1] - recon$biomass[-nrow(recon)], NaN)
recon$sp <- c(recon$biomass[-1] - recon$biomass[-nrow(recon)] + recon$catch[-nrow(recon)], NaN)
plot(sp~date, recon, t = "l")
plot(sp~biomass, recon, t = "l")
end <- nrow(recon)
arrows(x0 = recon$biomass[-end], x1 = recon$biomass[-1], y0 = recon$sp[-end], y1 = recon$sp[-1], length = 0.1)

agg1 <- aggregate(sp~year, data = recon, FUN = "sum")
agg2 <- aggregate(biomass~year, data = recon, FUN = "mean")
tmp <- merge(agg1, agg2)
plot(sp~biomass, tmp, t = "l")


## stacked plot  -----

tmp <- recon |> pivot_longer(cols = c("landings", "effort", "lpue", "DR", 
"catch", "cpue", "biomass", "hr", "FMmo"))
tmp

# n_levels <- 2
# COLS <- scales::brewer_pal(palette = "Set1")(n_levels)
# plot(seq(n_levels), col = COLS, cex = 3, pch = 16)
COLS <- c("#0072B2", "#D55E00")# "#E69F00")
x.breaks <- seq.Date(as.Date("2000-01-01"), as.Date("2024-01-01"), by = "1 year")
x.labs <- format(x.breaks, "%Y")
diff(x.breaks)

p1 <- ggplot(data = subset(tmp, name == "catch")) + aes(x = date, y = value) + 
  geom_line(color = COLS[1]) +
  geom_line(data = subset(tmp, name == "landings"), color = COLS[2]) +
  labs(y = "Catch and landings [t]")

p2 <- ggplot(data = subset(tmp, name == "effort")) + aes(x = date, y = value) + 
  geom_line(color = COLS[2]) +
  labs(y = "Effort [DAS]")

p3 <- ggplot(data = subset(tmp, name == "cpue")) + aes(x = date, y = value) + 
  geom_line(color = COLS[1]) +
  geom_line(data = subset(tmp, name == "lpue"), color = COLS[2]) +
  labs(y = "CPUE and LPUE [t/DAS]")

p4 <- ggplot(data = subset(tmp, name == "biomass")) + aes(x = date, y = value) + 
  geom_line(color = COLS[1]) +
  labs(y = "Biomass [t]")

p5 <- ggplot(data = subset(tmp, name == "hr")) + aes(x = date, y = value) + 
  geom_line(color = COLS[1]) +
  labs(y = "Harvest rate [frac.]")

p6 <- ggplot(data = agg) + aes(x = date, y = FMyr) + 
  geom_col(fill = COLS[1], width =  0.8*diff(x.breaks)[1]) +
  labs(y = bquote("Fishing mort. (F) ["*y^-1*"]"))

p <- (p1/p2/p3/p4/p5/p6) + plot_layout(axes = "collect_x") & 
  theme_bw() & 
  theme(text = element_text(size = 10), 
    axis.title.x = element_blank(), 
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) &
  scale_x_continuous(breaks = x.breaks, labels = x.labs, expand =  expansion(mult = 0.01))
p



(fname <- file.path("output", paste0("ts_stacked.png")))
png(fname, width = 7.5, height = 9, units = "in", res = 1000)
  print(p)
dev.off()
#




# save results ----------------------------------------------------------
# data and regressions

# data
save(LindexSep, LindexFall, file = "output/Lindex.Rdata")

# lpue~b
save(LregrSep, LregrFall, file = "output/lpue~biomass_Lregr.Rdata")

# b~cpue
save(bfit, file = "output/biomass~cpue_bfit.Rdata")

# recon
save(recon, file = "output/recon.Rdata")




