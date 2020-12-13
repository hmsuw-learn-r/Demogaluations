test_that("mod_densities assigns values correctly", {

  expect_equal(mod_densities(seq(0,60,15))$data_density_category,
               c("0_to_15", "15_to_30", "30_to_45", "45_to_60", "60_to_Inf"))

  expect_equal(mod_densities(seq(0,60,15))$density_min,
               seq(0,60,15))

  expect_equal(mod_densities(seq(0,60,15))$density_max,
               c(seq(15,60,15), Inf))
})

test_that("mod_densities gives error when cutpoints do not start at 0", {
  expect_error(mod_densities(seq(5,25,5)))
})

test_that("mod_densities gives error when cutpoints are wrong length", {
  expect_error(mod_densities(seq(5,40,5)))
})

test_that("mod_densities gives error when cutpoints are not numeric", {
  expect_error(mod_densities(c("a", "b", "c", "d", "e")))
})
