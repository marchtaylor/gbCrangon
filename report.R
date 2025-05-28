# clear environment -------------------------------------------------------

rm(list = ls())


# run scripts -------------------------------------------------------------

source("report_01_gen_pkg_vers.R")



# make supplementary materials --------------------------------------------

rmarkdown::render(
  input="report_02_supplMat.Rmd",
  output_file = "report/Taylor_etal_supplMat.docx"
)



# transfer manuscript figures and rename
source("report_03_fig_transfer.R")

