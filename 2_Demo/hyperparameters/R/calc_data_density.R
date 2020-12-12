data_density_linear <- function(data, source_type, row_id, vars,
                                weights = 1) {
  #' TODO:
  #' calculate data density as the weighed sum of vars
}

data_density_relative <- function(data, source_type, row_id, vars, supergroup,
                                  scalar = 1000) {
  #' TODO:
  #' calculate linear data density
  #' divide score by max score in supergroup
  #' multiply by scalar
}

calc_data_density <- function(data, source_type, row_id, vars,
                              method = "linear", ...) {

  #' TODO: New structure: get counts of each source type for each location,
  #' rm.na.
  #' then, depending on method type either
  #' linear: perform weighted sum of source types
  #' ratio: sources compared the location with the most sources * scalar

  #ensure unique location-source-year
  data <- unique(data, by = source_id)

  # for each location, count the number of each source type

  # Call data density variant by type "linear" or "ratio"

  # return dataframe with densities
}

ghg <- ggb(mmm)
