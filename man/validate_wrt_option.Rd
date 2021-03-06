% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validate_wrt_option.R
\name{validate_wrt_option}
\alias{validate_wrt_option}
\title{Validate With Respect To Option}
\usage{
validate_wrt_option(wrt_option, posterior_samples)
}
\arguments{
\item{wrt_option}{string name of the option}

\item{posterior_samples}{Tibble returned from sample_from_posterior with 3 columns
`option_name`, `samples`, and `sample_id`.}
}
\value{
None
}
\description{
Verify that the option provided is in the poster_samples dataframe `option_name` column.
Raises error if not TRUE
}
