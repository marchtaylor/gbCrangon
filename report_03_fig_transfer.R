outPath <- "report/figures_manuscript"
dir.create(outPath, showWarnings = F)


figLut <- data.frame(rbind(
  c("output/study_area.png", file.path(outPath, "figure_01.png")),
  c("output/pred_maps2_combined.png", file.path(outPath, "figure_02.png")),
  c("output/indices_all_wCI.png", file.path(outPath, "figure_03.png")),
  c("output/index_vs_lpue_by_period+fraction.png", file.path(outPath, "figure_04.png")),
  c("output/discardRate_GAM.png", file.path(outPath, "figure_05.png")),
  c("output/cpueRecon_vs_index.png", file.path(outPath, "figure_06.png")),
  c("output/ts_stacked.png", file.path(outPath, "figure_07.png")),
  c("output/map_median_prediction.png", file.path(outPath, "figure_08.png"))
))
names(figLut) <- c("from", "to")
figLut

for(i in seq(nrow(figLut))){
  file.copy(from = figLut$from[i], to = figLut$to[i], overwrite = T)
}