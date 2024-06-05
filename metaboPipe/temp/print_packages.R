# Load necessary libraries
library(dplyr)
library(purrr)
library(renv)

# Get unique package names
packages <- renv::dependencies() %>% select(Package) %>% unique()

# Extract package names as a character vector
package_names <- packages$Package

# Create a vector with package versions as characters
package_versions <- sapply(package_names, function(p) as.character(packageVersion(p)))

# Create a dataframe with package names and their versions
package_info <- data.frame(
  Package = package_names,
  Version = package_versions,
  stringsAsFactors = FALSE
)

print(package_info)

# Sort the dataframe by package name
package_info <- package_info[order(package_info$Package), ]

# Save the dataframe to a csv
write.csv(package_info, "package_info.csv", row.names = FALSE)

# Create citations for the packages
write_bib(package_info$Package)
