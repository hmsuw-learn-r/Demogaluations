#' Modify hyperparameter values
#'
#' @param x char. name of hyperparameter to adjust, or new hyperparameter
#' use names(dt_density_tbl) for default hyperparameters
#' @param new_values vector of numerics which are hyperparameter values
#' @param ddt data density table. Uses the generic dt_density_tbl as default
#'
#' @return data density table with modified hyperparrameter.
#' @export
#'
#' @examples
#' new_ddt <- mod_hyppar("beta", seq(5, 25, 5))
#' new_ddt <- mod_hyppar("zeta", 0.3, new_ddt)
mod_hyppar <- function(x, new_values, ddt = dt_density_tbl) {

  # new values must be numeric. X must be a string and not a data density column
  assertthat::assert_that(is.numeric(new_values))
  assertthat::assert_that(assertthat::is.string(x))
  assertthat::assert_that(!(x %in% c("data_density_category",
                                     "density_min", "density_max")),
                          msg = "to change the data density variables, use mod_densities()")

  # warn if x is unknown hyperparameter
  if (!(x %in% names(ddt))) {
    warning(paste0(x, " is new hyperparameter, adding as new column"))
  }



  modded_ddt <- copy(ddt)
  modded_ddt[, (x) := new_values]

  return(modded_ddt)
}
