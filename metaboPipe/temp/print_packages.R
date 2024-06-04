# Load necessary libraries
library(dplyr)
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
