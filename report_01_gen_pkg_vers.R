generate_package_versions_md <- function(pkgs, md_file = "package_versions.md") {
  # Get installed packages info
  installed <- installed.packages()
  results <- data.frame(
    Package = pkgs,
    Version = sapply(pkgs, function(pkg) {
      if (pkg %in% rownames(installed)) {
        installed[pkg, "Version"]
      } else {
        NA_character_
      }
    }),
    stringsAsFactors = FALSE
  )
  
  # Create markdown table
  md_lines <- c(
    "| Package | Version |",
    "| ------- | ------- |"
  )
  md_lines <- c(md_lines,
                apply(results, 1, function(row) {
                  paste0("| ", row[1], " | ", ifelse(is.na(row[2]), "*Not Installed*", row[2]), " |")
                }))
  
  # Write to file
  writeLines(md_lines, md_file)
  
  # Return markdown content invisibly
  invisible(md_lines)
}



pkgs <- c(
  "tidyr", "dplyr", "data.table", "pals", "sinkr", 
  "ggplot2", "ggeffects", "ggrepel", "patchwork", 
  "zoo", "terra", "sf", "sp", "ncdf4", "smoothr", "maps", "mapdata", 
  "INLA", "sdmTMB", "mgcv", "spatialsample", "ncf", "parallel", "future", "rmarkdown")

generate_package_versions_md(pkgs, md_file = "report/package_versions.md")

