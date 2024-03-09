import_data <- function(file) {
  read_csv(file, col_types = cols()) %>%
    as_tibble() %>%
    filter(!is.na())   
}