#' Calculate Data Density
#'
#' @param data data.table which should contain the following columns
#'  - *source_type*: data collection source e.g. contains ("vr", "cbh", "census")
#'  - *location_id*: like ihme_loc_id.
#'  - *year*: years of focus
#'  - *source_id*: unique id for data sources
#'  unique location-source-years will count towards data density. data density is
#'  calculated per location.
#' @param source_type character, name of column which tells type of source
#' @param vars character. types of source which will be added up to calculate data density
#' @param weights vector of numerics. each corresponds with a source_type in vars
#' @param year_id column name detailing years of study
#' @param source_id column name for nid
#' @param location_id unique id for location
#'
#' @return data table with locations and corresponding data density scores
#' @export
#'
#' @examples
#' data <- calc_data_density(sample_reports,
#' "source_type", "location_id", c("vr", "cbh"), c(1, .5))
calc_data_density <- function(data,
                              vars,
                              weights = 1,
                              source_type = "source_type",
                              source_id = "nid",
                              location_id = "location_id",
                              year_id = "year_id") {

  #' check that:
  #' source_type:year_id are strings which are column names in data
  #' vars is type char
  #' weights is numeric and either length 1 or the same length as vars
  assertthat::assert_that(source_type %in% names(data))
  assertthat::assert_that(source_id %in% names(data))
  assertthat::assert_that(location_id %in% names(data))
  assertthat::assert_that(year_id %in% names(data))
  assertthat::assert_that(is.character(vars))
  assertthat::assert_that(is.numeric(weights) &
                            (length(weights) == 1 |
                               length(weights) == length(vars)))

  #ensure rows are unique
  unique(data, by = c(source_id, location_id, year_id))

  # for each location, count the number of each source type, reformat to wide
  data <- data[, .N, by = .((location_id), (source_type))]
  data <- dcast(data, (location_id) ~ (source_type), value.var = "N", fill = 0)

  # Call data density variant by a weighted sum
  data[, data_density := sum(.SD * weights),  .SDcols = vars, by = (location_id)]

  # return datatable with densities
  return(data)
}
