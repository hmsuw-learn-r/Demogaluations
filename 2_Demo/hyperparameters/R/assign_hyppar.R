#' Assign Hyperparameters Based on Data Density
#'
#' @description Assign hyperparameters based on data density scores
#' for use in space-time GPR. Density scores can be produced by
#' calc_data_density(). hyperparameters are assigned based on
#' level of data density defined by a data density table
#' (see dt_density_tbl)
#'
#' @param data data.table. contains columns for location_id and assigned
#' densities
#' @param ddt data density table, will use dt_density_tbl by default
#'
#' @return data table containing locations, data density value and
#' corresponding hyperparameters for use in space-time gpr
#' @export
#'
#' @examples
#' data <- calc_data_density(Belgium, "vr", source_type = "source_type")
#' data <- assign_hyppar(data)
assign_hyppar <- function(data, ddt = dt_density_tbl) {

  #check that data has data_density column which contains no NAs
  assertthat::has_name(data, "data_density")
  assertthat::assert_that(sum(is.na(data$data_density)) == 0)

  #check that ddt contains a density_min and density_max column which
  # contain no NAs
  assertthat::has_name(ddt, "density_min")
  assertthat::has_name(ddt, "density_max")
  assertthat::assert_that(sum(is.na(ddt$density_max)) + sum(is.na(ddt$density_min)) == 0)

  # check that the categories are contiguous and start at 0. give warning if not
  start <- ddt$density_min[1]
  inner_1 <- ddt$density_min[2:5]
  inner_2 <- ddt$density_max[1:4]

  if(start != 0 | all(inner_2 != inner_1)) {
    warning("Warning: ddt expects contiguous density categories which begin at 0.
            Expect missingness. To avoid this error, only  use  mod_densities()
            and mod_hyppar() to alter ddt")
  }

  # assign category
  data[ddt,
       `:=`(data_density_category = i.data_density_category),
       on=.(data_density >= density_min, data_density < density_max)]

  # merge on hyperparameters
  data <- data[ddt, on=.(data_density_category), nomatch = 0]

  # clean up columns.
  data <- data[, c("density_min", "density_max", "data_density_category", "data_density") := NULL]
}
