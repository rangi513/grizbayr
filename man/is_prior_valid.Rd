% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/is_prior_valid.R
\name{is_prior_valid}
\alias{is_prior_valid}
\title{Is Prior Valid}
\usage{
is_prior_valid(priors_list, valid_prior)
}
\arguments{
\item{priors_list}{A list of valid priors}

\item{valid_prior}{A character string}
}
\value{
Boolean (TRUE/FALSE)
}
\description{
Checks if a single valid prior name is in the list of prior values and if
    that prior value from the list is greater than 0.
}
