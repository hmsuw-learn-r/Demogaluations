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

  # Check if x is a hyperparameter which exists
  if (!(x %in% names(ddt))) {
    warning("x is new hyperparameter, adding as new column")
  }

  # check if x is a numeric vector
  if(!is.numeric(new_values)) {
    stop("hyperparameter values must be numeric")
  }

  modded_ddt <- copy(ddt)
  modded_ddt[, (x) := new_values]

  return(modded_ddt)
}
