test_that("calc_data_density throws error if columns inputed are not in data", {
  expect_error(calc_data_density(Belgium, "vr")) #default source_id name
                                                  # is nid which toy dataset does not use
})

test_that("calc_data_density outputs correct data", {
  expect_equal(calc_data_density(data = Belgium, vars ="vr",source_id = "source_id"),
              setkey(data.table(location_id = 76,
                    data_density = 11), location_id))
})

test_that("calc_data_density: vars must contains values in source_type column", {
  expect_error(calc_data_density(calc_data_density(data = Belgium, vars ="doop",source_id = "source_id")))
})

test_that("calc_data_density: error if weights are not numeric or correct length", {
  expect_error(calc_data_density(data = Belgium, vars ="vr", weights = c(3,2),
                                 source_id = "source_id"))
  expect_error(calc_data_density(data = Belgium, vars ="vr", weights = c("d","c"),
                                 source_id = "source_id"))
})
