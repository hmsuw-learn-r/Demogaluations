test_that("mod_hyppar can assign each row its own value or all the same", {

  expect_equal(mod_hyppar("beta", 15), dt_density_tbl[, beta := 15])
  expect_equal(mod_hyppar("beta", c(5,10,15,20,25)),
               dt_density_tbl[, beta := c(5,10,15,20,25)])
  expect_equal(mod_hyppar("beta", seq(7,35,7)),
               dt_density_tbl[, beta := seq(7,35,7)])
})

test_that("mod_hyppar stops if trying to modify a data density column", {

  expect_error(mod_hyppar("data_density_category", 1),
               "to change the data density variables, use mod_densities()")
  expect_error(mod_hyppar("density_min", 1),
               "to change the data density variables, use mod_densities()")
  expect_error(mod_hyppar("density_max", 1),
               "to change the data density variables, use mod_densities()")
})

test_that("mod_hyppar throws warning, but continues if new hyppar is supplied", {
  expect_warning(mod_hyppar("delta", 100),
                 "delta is new hyperparameter, adding as new column")
})

test_that("mod hyppar stops if new_values are not numeric", {
  expect_error(mod_hyppar("beta", "A"), "new_values is not a numeric or integer vector")
})

test_that("mod_hyppar stops if x is not a string", {
  expect_error(mod_hyppar(1, 10))
})
