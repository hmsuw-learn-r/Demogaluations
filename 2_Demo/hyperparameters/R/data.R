#' Data Density Table
#'
#' A dataset containing data density categories used in
#' hyperparameter assignment for ST-GPR
#'
#'
#' @format A data.table with 5 rows and 9 variables:
#' \describe{
#'   \item{data_density_category}{category name for hyperparameters}
#'   \item{density_min & density_max}{bounds for data_density. interval is
#'   min inclusive}
#'   \item{hyperparameters}{parameters for use in ST-GPR. to learn more read
#'   Process Documentation on the HUB}
#'   ...
#' }
#' @source check the HUB
"dt_density_tbl"

#' Belgium
#'
#' A dataset containing years and sources of SRB data in Belgium 2005-2022
#'
#'
#'
#' @format A data.table with 18 rows and 4 variables:
#' \describe{
#'   \item{location_id}{unique id per location}
#'   \item{year_id}{year}
#'   \item{source_id}{unique id per data source}
#'   \item{source_type}{type of source}
#' }
#' @source not important
"Belgium"

#' Belgium
#'
#' A dataset containing years and sources of SRB data in Europe in 2005-2022
#'
#'
#'
#' @format A data.table with 18 rows and 4 variables:
#' \describe{
#'   \item{location_id}{unique id per location}
#'   \item{year_id}{year}
#'   \item{source_id}{unique id per data source}
#'   \item{source_type}{type of source}
#' }
#' @source not important
"sample_reports"
