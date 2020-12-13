test_that("calc_data_density throws error if columns inputed are not in data", {
  expect_error(calc_data_density(Belgium, "vr")) #default source_id name
                                                  # is nid which toy dataset does not use
})

test_that("calc_data_density outputs correct data", {
  expect_type(calc_data_density(data = Belgium, vars ="vr",source_id = "source_id"), "list")

  expect_equal(calc_data_density(data = Belgium, vars ="vr",source_id = "source_id"),
               data.table(location_id = 76,
                    `NA` = 3,
                    cbh = 2,
                    sample_registration = 2,
                    vr = 11,
                    data_density = 11))
})
