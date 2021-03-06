% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/percentage_point_gap.R
\name{di_ppg_iterate}
\alias{di_ppg_iterate}
\title{Iteratively calculate disproportionate impact via the percentage point gap (PPG) method for many variables.}
\usage{
di_ppg_iterate(
  data,
  success_vars,
  group_vars,
  cohort_vars,
  reference_groups,
  repeat_by_vars = NULL,
  weight_var = NULL,
  min_moe = 0.03,
  use_prop_in_moe = FALSE,
  prop_sub_0 = 0.5,
  prop_sub_1 = 0.5
)
}
\arguments{
\item{data}{A data frame for which to iterate DI calculation for a set of variables.}

\item{success_vars}{A character vector of success variable names to iterate across.}

\item{group_vars}{A character vector of group (disaggregation) variable names to iterate across.}

\item{cohort_vars}{A character vector of cohort variable names to iterate across.}

\item{reference_groups}{Either 'overall', 'hpg', or a character vector of the same length as `group_vars` that indicates the reference group value for each group variable in `group_vars`.}

\item{repeat_by_vars}{A character vector of variables to repeat DI calculations for across all combination of these variables, including '- All' as a group for each variable.  The reference rate used for DI comparison differs for every combination of the variables listed here.}

\item{weight_var}{A character scalar specifying the weight variable if the input data set is summarized (ie,  the the success variables specified in `success_vars` contain count of successes).  Weight here corresponds to the denominator when calculating the success rate.  Defaults to `NULL` for an input data set where each row describes each individual.}

\item{min_moe}{The minimum margin of error to be used in the PPG calculation, passed to `di_ppg`.}

\item{use_prop_in_moe}{Whether the estimated proportions should be used in the margin of error calculation by the PPG, passed to `di_ppg`.}

\item{prop_sub_0}{Passed to `di_ppg`.}

\item{prop_sub_1}{Passed to `di_ppg`.}
}
\value{
A data frame with all relevant returned fields from `di_ppg` plus `success_variable` (elements of `success_vars`), `disaggregation` (elements of `group_vars`), and `reference_group` (elements of `reference_groups`).
}
\description{
Iteratively calculate disproportionate impact via the percentage point gap (PPG) method for many disaggregation variables.
}
\details{
Iteratively calculate disproportionate impact via the percentage point gap (PPG) method for all combinations of `success_vars`, `group_vars`, and `cohort_vars`, for each combination of subgroups specified by `repeat_by_vars`.
}
\examples{
library(dplyr)
data(student_equity)
# Multiple group variables
di_ppg_iterate(data=student_equity, success_vars=c('Transfer')
  , group_vars=c('Ethnicity', 'Gender'), cohort_vars=c('Cohort')
  , reference_groups='overall')
}
