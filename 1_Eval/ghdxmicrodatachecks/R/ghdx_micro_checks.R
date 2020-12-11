########################################################################
# HMS 520 Final Project
########################################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Overview
#IHME conducts primary data collection during monitoring and evaluation
#projects. When possible, IHME publishes microdata to the GHDx. Prior
#to publication on the GHDx, microdata must be censored to remove any
#sensitive information and reformatted. A codebook must also be produced
#to ease interpretation of data. While the contents of datasets and
#codebooks differ greatly across projects, there are certain structures
#and features of the codebook and dataset which should always be true.

#This code produces a series of standarized functions which can be
#used to confirm that a codebook-dataset pair have the neccesary
#standard feaures. Below is a high-level overview of the checks,
#   1. Comparing variables in codebook and dataset
#        The variables listed in the codebook and the dataset should
#        always perfectly align. These checks confirm that 1) there are
#        no variables listed in the codebook which do not also appear in
#        the dataet, and 2) that there are no variables in the dataset
#        which do no appear in the codebook.
#   2. Checking that question text and question type are consistent
#      across variables
#        Within the codebook, a single variable in the data will appear
#        multiple times depending on the question type and range of values
#        the variable may take. The question text and question type
#        associated with a single variable should never vary. This check
#        confirms that the above characteristic holds true.
#   3. Check that value and response label are either always NA, or never
#      NA, for a specific question type
#        Depending on the type of question, the value and response label
#        in the codebook should either 1) always be NA, or 2) never be NA.
#        This pair of checks allows the user to confirm that the above
#        holds true.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Loading Library
library(dplyr)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. COMPARING VARS IN CODEBOOK AND DATASET

##This function identifies and outputs any variables that appear in the codebook but are missing in the data
#' This function identifies and outputs any variables that appear in the codebook but are missing in the data
#'
#' @param book A dataframe containing the GHDx formatted codebook
#' @param dataset A dataframe containing the GHDx formatted dataset
#'
#' @return TRUE if no mismatched variables are identified. False if otherwise. Also prints messages to the log.
#' @export
#'
#' @examples
missing_from_data <- function(book, dataset){
  #Creating unique list of variables in codebook
  varmatch_book1 <- unique(data.frame(book[ ,1]))
  colnames(varmatch_book1)[1] <- "var_names"
  varmatch_book1$in_book <- TRUE

  #Creating unique list of variables in dataset
  varmatch_data1 <- data.frame(names(dataset))
  colnames(varmatch_data1)[1] <- "var_names"
  varmatch_data1$in_data <- TRUE

  #Joining codebook and dataset names
  varmatch_comb1 <- full_join(varmatch_book1, varmatch_data1, by = "var_names")

  varmatch_comb_book <- varmatch_comb1[is.na(varmatch_comb1$in_data), 1]

  if(length(varmatch_comb_book) > 0) {
    print(varmatch_comb_book)
    resolved <- FALSE
  }
  else {
    print("GREAT JOB! - THERE ARE NO VARIABLES IN THE CODEBOOK WHICH ARE NOT IN THE DATASET")
    resolved <- TRUE
  }
  invisible(resolved)
}

#This function identifies and outputs any variables that appear in the data but are missing in the codebook
#' This function identifies and outputs any variables that appear in the data but are missing in the codebook
#'
#' @param book A dataframe containing the GHDx formatted codebook
#' @param dataset A dataframe containing the GHDx formatted dataset
#'
#' @return TRUE if no mismatched variables are identified. False if otherwise. Also prints messages to the log.
#' @export
#'
#' @examples
missing_from_book <- function(book, dataset){

  #Creating unique list of variables in codebook
  varmatch_book1 <- unique(data.frame(book[ ,1]))
  colnames(varmatch_book1)[1] <- "var_names"
  varmatch_book1$in_book <- TRUE

  #Creating unique list of variables in dataset
  varmatch_data1 <- data.frame(names(dataset))
  colnames(varmatch_data1)[1] <- "var_names"
  varmatch_data1$in_data <- TRUE

  #Joining codebook and dataset names
  varmatch_comb1 <- full_join(varmatch_book1, varmatch_data1, by = "var_names")

  varmatch_comb_data <- varmatch_comb1[is.na(varmatch_comb1$in_book), 1]
  if(length(varmatch_comb_data) > 0) {
    print(varmatch_comb_data)
    resolved <- FALSE
  }
  else {
    print("GREAT JOB! - THERE ARE NO VARIABLES IN THE DATASET WHICH ARE NOT IN THE CODEBOOK")
    resolved <- TRUE
  }
  invisible(resolved)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2. Checking that question text and question type are consistent across variables

#Checking that there are never multiple question texts for a single variable
#' Checking that there are never multiple question texts for a single variable
#'
#' @param book A dataframe containing the GHDx formatted codebook
#'
#' @return TRUE if question text is consistent. False if otherwise. Also prints messages to the log.
#' @export
#'
#' @examples
is_consistent_question <- function(book) {
  book$question <- trimws(book$question)
  consist_question_1 <- unique(book[c("variable_name", "question")])
  consist_question_1 <- data.frame(table(consist_question_1$variable_name))
  consist_question_1 <- consist_question_1[consist_question_1$Freq >1, ]
  if(nrow(consist_question_1) > 0) {
    print("THE FOLLOWING VARIABLES APPEAR IN THE CODEBOOK WITH MULTIPLE DIFFERENT QUESTION TEXTS")
    print(consist_question_1)
    resolved <- FALSE
  }
  else {
    print("GREAT JOB! - ALL VARIABLES HAVE CONSISTENT QUESTION TEXT")
    resolved <- TRUE
  }
  invisible(resolved)
}

#Checking that there are never multiple question types for a single variable
#' Checking that there are never multiple question types for a single variable
#'
#' @param book Checking that there are never multiple question types for a single variable
#'
#' @return TRUE if question type is consistent. False if otherwise. Also prints messages to the log.
#' @export
#'
#' @examples
is_consistent_type <- function(book) {
  book$question_type <- trimws(book$question_type)
  consist_questiontype_1 <- unique(book[c("variable_name", "question_type")])
  consist_questiontype_1 <- data.frame(table(consist_questiontype_1$variable_name))
  consist_questiontype_1 <- consist_questiontype_1[consist_questiontype_1$Freq >1, ]
  if(nrow(consist_questiontype_1) > 0) {
    print("THE FOLLOWING VARIABLES APPEAR IN THE CODEBOOK WITH MULTIPLE DIFFERENT QUESTION TYPES")
    print(consist_questiontype_1)
    resolved <- FALSE
  }
  else {
    print("GREAT JOB! - ALL VARIABLES HAVE CONSISTENT QUESTION TYPE")
    resolved <- TRUE
  }
  invisible(resolved)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3. Check that response value and response label are always/never NA for specified variable type

#This function checks that there is always a value of N/A for the specified variable type
#' This function checks that there is always a value of N/A for the specified variable type
#'
#' @param data Checking that there are never multiple question types for a single variable
#' @param vartype A text string, enclosed in "", that denotes the variable type to be checked
#'
#' @return TRUE if value and answer label are always "N/A". False if otherwise. Also prints messages to the log.
#' @export
#'
#' @examples
is_vartype_always_na <- function(data, vartype) {
  check_val_na1 <- data[(trimws(data$question_type) == vartype) , ]
  if(nrow(check_val_na1) == 0) print("THERE ARE NO ROWS WITH THAT QUESTION TYPE")
  else {
    check_val_na1 <- check_val_na1[(trimws(check_val_na1$value) != "N/A" | trimws(check_val_na1$answer_label) != "N/A") , ]
    if(nrow(check_val_na1) > 0) {
      print("THE FOLLOWING VARIABLES OF THE SPECIFIED TYPE TAKE ON NON NA VALUES FOR EITHER VALUE OR ANSWER_LABEL")
      print(check_val_na1$variable_name)
      resolved <- FALSE
    }
    else {
      print("GREAT JOB! - ALL VARIABLES OF THE SPECIFIED TYPE HAVE NA FOR VALUE AND ANSWER_LABEL")
      resolved <- TRUE
    }
  }
  invisible(resolved)
}

#This function checks if there is ever an N/A value for the specified variable type
#' This function checks if there is ever an N/A value for the specified variable type
#'
#' @param data Checking that there are never multiple question types for a single variable
#' @param vartype A text string, enclosed in "", that denotes the variable type to be checked
#'
#' @return TRUE if value and answer label are never "N/A". False if otherwise. Also prints messages to the log.
#' @export
#'
#' @examples
is_vartype_ever_na <- function(data, vartype) {
  check_val_na2 <- data[(trimws(data$question_type) == vartype) , ]
  if(nrow(check_val_na2) == 0) print("THERE ARE NO ROWS WITH THAT QUESTION TYPE")
  else {
    check_val_na2 <- check_val_na2[(trimws(check_val_na2$value) == "N/A") | (trimws(check_val_na2$answer_label) == "N/A"), ]
    if(nrow(check_val_na2) > 0) {
      print("THE FOLLOWING VARIABLES OF THE SPECIFIED TYPE TAKE ON NA VALUES FOR EITHER VALUE OR ANSWER_LABEL")
      print(check_val_na2$variable_name)
      resolved <- FALSE
    }
    else {
      print("GREAT JOB! - NONE OF THE VARIABLES OF THE SPECIFIED TYPE HAVE NA FOR VALUE OR ANSWER_LABEL")
      resolved <- TRUE
    }
  }
  invisible(resolved)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
