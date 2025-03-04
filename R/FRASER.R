# R version 4.4.1, FRASER v2.0.0

library(FRASER)
fds <- readRDS("data/muscle/fds-object.RDS") 

# Define the new base path
new_base_path <- "/Users/00104561/Library/CloudStorage/OneDrive-UWA/Research/Projects/AIM3_TRANSCRIPTOMICS/app/rnaweb/data/muscle/fraser/"
fds@workingDir <- new_base_path

# List of assay names to update the file paths for
assay_names_1 <- names(fds@assays@data@listData)
assay_names_2 <- names(fds@nonSplicedReads@assays@data@listData)

# Iterate over each assay and update the filepath
for (assay_name in assay_names_1) {
  # Access the seed of the assay and update the filepath
  fds@assays@data@listData[[assay_name]]@seed@seed@filepath <- paste0(new_base_path, assay_name, ".h5")
}

# Iterate over each assay and update the filepath
for (assay_name in assay_names_2) {
  # Access the seed of the assay and update the filepath
  fds@nonSplicedReads@assays@data@listData[[assay_name]]@seed@seed@filepath <- paste0(new_base_path, assay_name, ".h5")
}

# Check one example to verify the filepath has been updated
fds@assays@data@listData$rawCountsJ@seed@seed@filepath

# Check one example to verify the filepath has been updated
fds@nonSplicedReads@assays@data@listData$rawCountsSS@seed@seed@filepath

saveRDS(fds, file = "./data/muscle/fds.Rds")
