
# gbCrangon

Code accompanying the paper by Taylor et al., “Estimation of brown
shrimp stock and fishery dynamics in the German Bight using a novel
biomass index”

## Reproducing the analysis

The scripts should be run in the following sequence

- *data.R*
  - data_01_adjust_WGCRAN_data.R - prepares fishery-dependent data
  - data_02_make_data_mesh.R - prepares survey-dependent data,
    prediction grid, and mesh for use by sdmTMB
- *model.R*
  - model_01_sdm_selection.R - runs cross validation of sdmTMB models
    and calculates performance metrics
- *output.R*
  - output_01_index_and_pred.R - produces annual biomass indices and
    spatial predictions  
  - output_02_discardRate.R - fits model describing temporal changes to
    discard rates
  - output_03_indexCorr_histRecon.R - evaluates relationship of
    fisheries-dependent indices to biomass estimates, and
    reconstructions monthly biomass and fishery dynamics
  - output_04_aux_figures.R - produces additional figures
- *report.R*
  - report_01_gen_pkg_vers.R - extract package versions used by the
    analysis
  - report_02_supplMat.Rmd - produces the supplementary material
    document
  - report_03_fig_transfer.R - transfers and renames numbered manuscript
    figures

<!-- paste(version$major, version$minor, sep = ".") -->

As of June 2025, this analysis has been run in R Version 4.4.2, with the
following package versions:

    ##  [1] "tidyr (1.3.1)"         "dplyr (1.1.4)"         "data.table (1.17.0)"  
    ##  [4] "pals (1.9)"            "sinkr (0.7.4)"         "ggplot2 (3.5.1)"      
    ##  [7] "ggeffects (2.0.0)"     "ggrepel (0.9.6)"       "patchwork (1.3.0)"    
    ## [10] "zoo (1.8.12)"          "terra (1.8.5)"         "sf (1.0.19)"          
    ## [13] "sp (2.1.4)"            "ncdf4 (1.23)"          "smoothr (1.0.1)"      
    ## [16] "maps (3.4.2.1)"        "mapdata (2.3.1)"       "INLA (24.5.10)"       
    ## [19] "sdmTMB (0.6.0.9010)"   "mgcv (1.9.1)"          "spatialsample (0.6.0)"
    ## [22] "ncf (1.3.2)"           "parallel (4.4.2)"      "future (1.34.0)"      
    ## [25] "rmarkdown (2.29)"
