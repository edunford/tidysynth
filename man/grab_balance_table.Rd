% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grab-methods.R
\name{grab_balance_table}
\alias{grab_balance_table}
\title{grab_balance_table}
\usage{
grab_balance_table(data)
}
\arguments{
\item{data}{nested data of type \code{tbl_df}}
}
\value{
tibble data frame containing balance statistics between the
observed/synthetic unit and the donor pool for each variable used to fit
the synthetic control.
}
\description{
Compare the distributions of the aggregate-level predictors for the observed
intervention unit, the synthetic control, and the donor pool average. Table
helps user compare the the level of balance produced by the synthetic
control.
}
\examples{

\donttest{
data(smoking)
smoking_out <-
smoking \%>\%
synthetic_control(outcome = cigsale,
                  unit = state,
                  time = year,
                  i_unit = "California",
                  i_time = 1988,
                  generate_placebos=FALSE) \%>\%
  generate_predictor(time_window=1980:1988,
                     lnincome = mean(lnincome, na.rm = TRUE),
                     retprice = mean(retprice, na.rm = TRUE),
                     age15to24 = mean(age15to24, na.rm = TRUE)) \%>\%
  generate_predictor(time_window=1984:1988,
                     beer = mean(beer, na.rm = TRUE)) \%>\%
  generate_predictor(time_window=1975,
                     cigsale_1975 = cigsale) \%>\%
  generate_predictor(time_window=1980,
                     cigsale_1980 = cigsale) \%>\%
  generate_predictor(time_window=1988,
                     cigsale_1988 = cigsale) \%>\%
  generate_weights(optimization_window =1970:1988,
                   Margin.ipop=.02,Sigf.ipop=7,Bound.ipop=6) \%>\%
  generate_control()

smoking_out \%>\% grab_balance_table()

}

}
