% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ghdx_micro_checks.R
\name{is_vartype_always_na}
\alias{is_vartype_always_na}
\title{This function checks that there is always a value of N/A for the specified variable type}
\usage{
is_vartype_always_na(data, vartype)
}
\arguments{
\item{data}{Checking that there are never multiple question types for a single variable}

\item{vartype}{A text string, enclosed in "", that denotes the variable type to be checked}
}
\value{
TRUE if value and answer label are always "N/A". False if otherwise. Also prints messages to the log.
}
\description{
This function checks that there is always a value of N/A for the specified variable type
}
