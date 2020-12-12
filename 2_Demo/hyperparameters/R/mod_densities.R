#' Modify Data Density Categories
#'
#' @param ddt data table containing data densitys and hyperparams.
#' will use dt_density_tbl if none is provided
#' @param cutpoints numeric vector defining data density categories.
#' lowest value should be 0, largest value defines an open interval
#'
#' @return data density table with updated data density categories.
#' @export
#'
#' @examples
#' new_ddt <- mod_densities(c(0,15,30,40,50))
mod_densities <- function(cutpoints, ddt = dt_density_tbl) {

  # check that cutpoints are numeric and the correct length
  if(!is.numeric(cutpoints) | length(cutpoints) != nrow(ddt)) {
    stop(paste0("Error: Cutpoints must be an atomic vector of length",
                nrow(ddt)))
  }

  # check that the lowest cutpoint is 0
  if(min(cutpoints) != 0) {
    stop("Error: cutpoints should be a sequence starting at 0")
  }

  # create copy of ddt to return
  modded_ddt <- copy(ddt)

  # modify density_min, density_max, and data_density_category according to new
  # cutpoints
  cutpoints = sort(cutpoints)

  modded_ddt[, density_min := cutpoints]
  modded_ddt[, density_max := c(cutpoints[-1], Inf)]
  modded_ddt[, data_density_category := paste0(density_min, "_to_", density_max)]

}
