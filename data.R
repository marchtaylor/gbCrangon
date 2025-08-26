# clear environment -------------------------------------------------------

rm(list = ls())

# make directories -------------------------------------------------------------
dir.create("data", showWarnings = F)
dir.create("model", showWarnings = F)
dir.create("output", showWarnings = F)
dir.create("report", showWarnings = F)

# load packages ----------------------------------------------------------------

# install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE) 

library(tidyr)
library(dplyr)
library(data.table)
library(pals)
library(sinkr)
library(ggplot2)
library(ggeffects)
library(ggrepel)
library(hexbin)
library(patchwork)
library(zoo)

library(terra)
library(sf)
library(sp)
library(ncdf4)
library(smoothr)
library(maps)
library(mapdata)

library(INLA)
library(sdmTMB)
library(mgcv)
library(spatialsample)
library(ncf)

library(parallel)
library(future)

library(rmarkdown)


# run scripts -------------------------------------------------------------

source("data_01_adjust_WGCRAN_data.R") # object preparation for sdmTMB
source("data_02_make_data_mesh.R") # environmental data preparation
