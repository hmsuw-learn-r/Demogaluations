#######################################################################
#Unit testing for GHDx Checks package
#######################################################################
library(tidyr)
library(dplyr)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Loading test datasets, including data with known errors, to test functions

#Good Data - should function for all tests
good_book <- read.csv("C:\\Users\\camardaj\\Desktop\\hms_520_camardaj\\hms520_final_project\\gen_pop codebook.csv")
good_data <- read.csv("C:\\Users\\camardaj\\Desktop\\hms_520_camardaj\\hms520_final_project\\gen_pop_weighted_data.csv")

#Sample datasets for testing missing_from_data, missing_from_book functions
#These datasets should resolve to FALSE for the appropriate tests
bad_book_mismatchvars1 <- good_book
bad_book_mismatchvars1[1, "variable_name"] <- "imaduckquack"
bad_data_mismatchvars1 <- rename(good_data, c("mage" = "age"))

#Sample datasets for testing is_consistent_question, is_consistent_type functions
#These datasets should resolve to FALSE for the appropriate tests
bad_book_inconsistquest1 <- good_book
bad_book_inconsistquest1[7, "question"] <- "have you heard about the new curduroy pillows? there making headlines everywhere"
bad_book_inconsisttype1 <- good_book
bad_book_inconsisttype1[7, "question_type"] <- "Select 50"

#Sample datasets for testing is_vartype_always_na function
#These datasets should resolve to FALSE for the appropriate tests
bad_book_textnotna1 <- good_book
bad_book_textnotna1[1, "value"] <- "imaduckquack"
bad_book_textnotna2 <- good_book
bad_book_textnotna2[1, "answer_label"] <- "imagooseHONK"

#Sample datasets for testing is_vartype_ever_na function
#These datasets should resolve to FALSE for the appropriate tests
bad_book_seloneisna1 <- good_book
bad_book_seloneisna1[4, "answer_label"] <- "N/A"
bad_book_selmulisna1 <- good_book
bad_book_selmulisna1[64, "value"] <- "N/A"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Testing missing_from_data function
test_that("missing_from_data", {
  expect_equal(missing_from_data(good_book, good_data), TRUE)
  expect_equal(missing_from_data(bad_book_mismatchvars1, good_data), FALSE)
  expect_equal(missing_from_data(bad_data_mismatchvars1, good_data), FALSE)
})
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Testing missing_from_book function
test_that("missing_from_book", {
  expect_equal(missing_from_book(good_book, good_data), TRUE)
  expect_equal(missing_from_book(bad_book_mismatchvars1, good_data), FALSE)
  expect_equal(missing_from_book(bad_data_mismatchvars1, good_data), FALSE)
})
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Testing is_consistent_question function
test_that("is_consistent_question", {
  expect_equal(is_consistent_question(good_book), TRUE)
  expect_equal(is_consistent_question(bad_book_inconsistquest1), FALSE)
  expect_equal(is_consistent_question(bad_book_inconsisttype1), TRUE)
})
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Testing is_consistent_type function
test_that("is_consistent_type", {
  expect_equal(is_consistent_type(good_book), TRUE)
  expect_equal(is_consistent_type(bad_book_inconsistquest1), TRUE)
  expect_equal(is_consistent_type(bad_book_inconsisttype1), FALSE)
})
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Testing is_vartype_always_na function
test_that("is_vartype_always_na", {
  expect_equal(is_vartype_always_na(good_book, "Text"), TRUE)
  expect_equal(is_vartype_always_na(bad_book_textnotna1, "Text"), FALSE)
  expect_equal(is_vartype_always_na(bad_book_textnotna2, "Text"), FALSE)
})
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Testing is_vartype_ever_na function
test_that("is_vartype_ever_na", {
  expect_equal(is_vartype_ever_na(good_book, "Select one"), TRUE)
  expect_equal(is_vartype_ever_na(good_book, "Select multiple"), TRUE)
  expect_equal(is_vartype_ever_na(bad_book_seloneisna1, "Select one"), FALSE)
  expect_equal(is_vartype_ever_na(bad_book_seloneisna1, "Select multiple"), TRUE)
  expect_equal(is_vartype_ever_na(bad_book_selmulisna1, "Select one"), TRUE)
  expect_equal(is_vartype_ever_na(bad_book_selmulisna1, "Select multiple"), FALSE)
})
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
