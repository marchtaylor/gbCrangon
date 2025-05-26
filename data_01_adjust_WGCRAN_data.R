
# clear environment -------------------------------------------------------

rm(list = ls())


# read LPUE data
landStats <- read.csv(file = "data/WGCRAN/WGCRAN2024_All_Summary_Landings_Effort_LPUE_FiTime_1950-2023_update16092024.csv")
landStats <- subset(landStats, Country == "GE" & !is.na(Effort_DAS))

df <- landStats[, c("Year", "Month", "Country", "Landings", "Effort_DAS", "LPUE_DAS")]
names(df) <- c("year", "month", "country", "landings_orig", "effort", "lpue")
df$date <- as.Date(paste(df$year, df$month, 15, sep = "-"))
range(df$year)

# convert all landings back to original boiled weight
df <- df |> mutate(landings_boiled_wt = if_else(year<2009, 
  landings_orig, 
  if_else(year%in%2009:2018, 
    landings_orig/1.18, landings_orig/1.06)))
df <- df[order(df$year, df$month),]

# apply a single conversion factor to get fresh weight
df <- df |> mutate(landings = landings_boiled_wt*1.06)

# recalculate lpue
df <- df |> mutate(lpue = landings / effort * 1000)


plot(landings_boiled_wt ~ landings_orig, df)
plot(landings_boiled_wt ~ date, df, t = "l")
lines(landings_orig ~ date, df, col = 8)

ggplot(df) + aes(x = month, y = landings_boiled_wt) +
  facet_wrap(~year) +
  geom_line(color = 3, linetype = 3) +
  geom_line(aes(y = landings_orig), color = 2, linetype = 2) +
  geom_line(aes(y = landings))

# save output
saveRDS(df, file = "data/wgcran_adjusted.RDS")
  
