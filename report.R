# clear environment -------------------------------------------------------

rm(list = ls())


# package versions for Github repo -------------------------------------------------------------

source("report_01_gen_pkg_vers.R")

# make tables --------------------------------------------

rmarkdown::render(
  input = "report_02_tables.Rmd",
  output_file = "report/Taylor_etal_tables.docx"
)

# make supplementary materials --------------------------------------------

rmarkdown::render(
  input="report_03_supplMat.Rmd",
  output_file = "report/Taylor_etal_supplMat.docx"
)



# transfer manuscript figures and rename
source("report_04_fig_transfer.R")

