
rm(list = ls())

df <- read.csv("data/STECF/STECF German Crangon Discard data 2013- 2023.csv")
df <- as.data.table(df)
df
str(df)
df$DISCARD <- an(df$Total.Discards..tonnes.)
df$TOTWGHTLANDG <- an(df$Total.Live.Weight.Landed..tonnes.)
df$FISHING_TECH <- df$Fishing.Technique
df$MESH_SIZE_RANGE <- df$mesh.Size
df$QUARTER <- df$Quarter
df$YEAR <- df$Year

unique(df$FISHING_TECH)
unique(df$MESH_SIZE_RANGE)
unique(df$QUARTER)

df <- df[FISHING_TECH == "TBB" & MESH_SIZE_RANGE == "16D32"]
df[ , DR := DISCARD/(TOTWGHTLANDG+DISCARD)]
df[, yeardec := YEAR + (QUARTER-1)/4+0.25/2]
df <- df[order(df$yeardec),]
df[,yearfrac := df$yeardec %% 1]
df



fit0 <- gam(
  DR ~ 
    s(YEAR, k = length(unique(df$YEAR))) + 
    s(yearfrac, bs = "cc", k = 4),
  data = df, family = quasibinomial(link = "logit"), 
  knots = list(yearfrac=c(0,1)))
summary(fit0)
plot(fit0, pages = 1)

op <- par(mfrow=c(2,2))
## normal QQ-plot of deviance residuals
qqnorm(residuals(fit0),pch=19,cex=.3)
## Quick QQ-plot of deviance residuals
qq.gam(fit0,pch=19,cex=.3)
## Simulation based QQ-plot with reference bands 
qq.gam(fit0,rep=100,level=.9)
## Simulation based QQ-plot, Pearson resids, all
## simulated reference plots shown...  
qq.gam(fit0,rep=100,level=1,type="pearson",pch=19,cex=.2)
par(op)

# Extract the smooth terms
smooth_terms <- plot(fit0, pages = 1, seWithMean = TRUE)
df_sm <- lapply(smooth_terms, function(x){data.frame(x = x$x, fit = x$fit, se = x$se)})

# make predictions 
df$pred <- predict(fit0, newdata = df, type = "response")
plot(pred~DR, df); abline(0,1)

newdat = as.data.table(expand.grid(YEAR = unique(df$YEAR), month = 1:12, stringsAsFactors = F))
newdat[, yearfrac := (month-0.5)/12,]
newdat[, yeardec := YEAR + yearfrac]
newdat <- newdat[order(newdat$yeardec),]

# pred <- predict(fit0, newdata = newdat, type = "response", se.fit = T)

# Predict with standard errors with type = "link"
predictions_link <- predict(fit0, newdata = newdat, type = "link", se.fit = TRUE)

# Calculate the 95% confidence intervals on the link scale
link_fit <- predictions_link$fit
link_se <- predictions_link$se.fit
link_lower_ci <- link_fit - 1.96 * link_se
link_upper_ci <- link_fit + 1.96 * link_se

# Transform the predictions and CIs back to the response scale
response_fit <- fit0$family$linkinv(link_fit)
response_lower_ci <- fit0$family$linkinv(link_lower_ci)
response_upper_ci <- fit0$family$linkinv(link_upper_ci)


# newdat$fit <- pred$fit
# newdat$se.fit <- pred$se.fit
newdat$DR <- response_fit
# newdat$DRup <- pred$fit + 1.96*pred$se.fit
# newdat$DRlw <- pred$fit - 1.96*pred$se.fit
newdat$DRup <- response_upper_ci
newdat$DRlw <- response_lower_ci

# Predictions pre-2015
newdat2 = as.data.table(expand.grid(YEAR = 2000:(min(df$YEAR)-1), month = 1:12, stringsAsFactors = F))
newdat2[, yearfrac := (month-0.5)/12,]
newdat2[, yeardec := YEAR + yearfrac]
newdat2 <- newdat2[order(newdat2$yeardec),]
lut <- newdat[, .(DR = mean(DR), DRup = NaN, DRlw = NaN), by = .(yearfrac)]
newdat2 <- merge(newdat2, lut)
round(range(lut$DR),2)

# combine
newdat3 <- rbind(newdat[,c("yeardec", "month", "DR", "DRup", "DRlw")], newdat2[,c("yeardec", "month", "DR", "DRup", "DRlw")])


# plot ----

ylim <- range(lapply(df_sm, function(x){range(c(x$fit+x$se, x$fit-x$se), na.rm = T)}))

# term 1
p1 <- ggplot(df_sm[[1]], aes(x = x, y = fit)) +
  geom_ribbon(aes(ymin = fit - se, ymax = fit + se), alpha = 0.2) +
  geom_line() +
  coord_cartesian(ylim = ylim) +
  scale_x_continuous(breaks = seq(2014, 2022, 2)) +
  labs(title = "Annual term", x = element_blank(), y = element_blank()) +
  theme_bw()
p1

# term 2
p2 <- ggplot(df_sm[[2]], aes(x = x, y = fit)) +
  geom_ribbon(aes(ymin = fit - se, ymax = fit + se), alpha = 0.2) +
  geom_line() +
  coord_cartesian(ylim = ylim) +
  scale_x_continuous(
    # name = "Custom X Axis",
    breaks = (0:11)/12,
    labels = month.abb
  ) +
  labs(title = "Seasonal term", x = element_blank(), y = element_blank()) +
  theme_bw()
p2

# prediction
p3 <- ggplot(newdat, aes(x = yeardec, y = DR)) +
  geom_ribbon(data = subset(newdat3, yeardec >= 2013), aes(ymin = DRlw, ymax = DRup), alpha = 0.2) +
  geom_line(data = subset(newdat3, yeardec >= 2013)) +
  geom_line(data = subset(newdat3, yeardec <= 2013.1), linetype = 2, size = 0.5) +
  # geom_ribbon(mapping = aes(ymin = DRlw, ymax = DRup), alpha = 0.2) +
  # geom_line() +
  # geom_line(data = newdat2, linetype = 2, size = 0.5) +
  geom_point(data = df, shape = 1) +
  coord_cartesian(xlim = c(2010,2024)) +
  scale_x_continuous(breaks = seq(2010, 2024, 1)) +
  labs(title = "Long-term prediction", x = element_blank(), y = "Discard rate [fraction]") +
  theme_bw()
p3



p <- ((p1|p2)/p3) & theme(text = element_text(size = 10))
p

png("output/discardRate_GAM.png", width = 7, height = 5, units = "in", res = 400)
print(p)
dev.off()

saveRDS(fit0, file = "output/discardRate_gam.rds")
saveRDS(newdat3, file = "output/prediction_discardRate.rds")
