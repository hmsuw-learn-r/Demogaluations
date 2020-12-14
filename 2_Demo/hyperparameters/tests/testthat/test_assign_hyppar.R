test_that("assign_hypar creates data.table of correct size", {
  expect_equal(dim(assign_hyppar(reports_w_weights)),
               c(26,7))
})

test_that("assign_hypar requires data_density", {
  expect_error(assign_hyppar(data.table(a = c(1:10))))
})

test_that("assign_hyppar: data_density should not contain NAs", {
  expect_error(assign_hyppar(data.table(data_density = c(1:10, NA))))
})

test_that("assign_hyppar: density_min and density max columns needed", {
  expect_error(assign_hyppar(reports_w_weights, ddt = data.table(density_min = NULL)))
  expect_error(assign_hyppar(reports_w_weights, ddt = data.table(density_max = NULL)))
})

test_that("assign_hyppar fails if ddt contains missing values", {
  expect_error(assign_hyppar(reports_w_weights,
                             ddt = data.table(
                               density_min = NA,
                               density_max = 10
                             )))
})

test_that("assign_hyppar gives warning if ddt values are irregular", {
  expect_warning(assign_hyppar(reports_w_weights,
                               ddt = data.table(
                                 density_min = seq(0,25,5),
                                 density_max = seq(6,36,6),
                                 data_density_category = (1:5)
                               )))
})
