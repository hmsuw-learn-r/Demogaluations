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

  #Remove old vars columns to make cleaning at end nicer
  data <- data[, .(location_id, data_density)]

  # assign category
  data[ddt,
       `:=`(data_density_category = i.data_density_category),
       on=.(data_density >= density_min, data_density < density_max)]

  # merge on hyperparameters
  data[ddt, on=.(data_density_category), nomatch = 0]

  # TODO clean up columns. figure out why this doesn't work.
  # data[, c("density_min", "density_max") := NULL]
}
