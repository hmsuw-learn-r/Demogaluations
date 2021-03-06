% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ghdx_micro_checks.R
\name{is_consistent_question}
\alias{is_consistent_question}
\title{Checking that there are never multiple question texts for a single variable}
\usage{
is_consistent_question(book)
}
\arguments{
\item{book}{A dataframe containing the GHDx formatted codebook}
}
\value{
TRUE if question text is consistent. False if otherwise. Also prints messages to the log.
}
\description{
Checking that there are never multiple question texts for a single variable
}
