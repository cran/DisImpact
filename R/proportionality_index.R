##' Calculate disproportionate impact per the proportionality index (PI) method.
##'
##' This function determines disproportionate impact based on the proportionality index (PI) method, as described in \href{http://extranet.cccco.edu/Portals/1/TRIS/Research/Accountability/GUIDELINES\%20FOR\%20MEASURING\%20DISPROPORTIONATE\%20IMPACT\%20IN\%20EQUITY\%20PLANS.pdf}{this} reference from the California Community Colleges Chancellor's Office.  It assumes that a higher rate is good ("success").  For rates that are deemed negative (eg, rate of drop-outs, high is bad), then consider looking at the converse of the non-success (eg, non drop-outs, high is good) instead in order to leverage this function properly.
##' @title Calculate disproportionate impact per the proportionality index (PI) method.
##' @param success A vector of success indicators (\code{1}/\code{0} or \code{TRUE}/\code{FALSE}) or an unquoted reference (name) to a column in \code{data} if it is specified.
##' @param group A vector of group names of the same length as \code{success} or an unquoted reference (name) to a column in \code{data} if it is specified.
##' @param cohort (Optional) A vector of cohort names of the same length as \code{success} or an unquoted reference (name) to a column in \code{data} if it specified.  disproportionate impact is calculated for every group within each cohort.  When \code{cohort} is not specified, then the analysis assumes a single cohort.
##' @param data (Optional) A data frame containing the variables of interest.  If \code{data} is specified, then \code{success}, \code{group}, and \code{cohort} will be searched within it.
##' @return A data frame consisting of: cohort (if used), group, n (sample size), success (number of successes for the cohort-group), pct_success (proportion of successes attributed to the group within the cohort), pct_group (proportion of sample attributed to the group within the cohort), and di_prop_index (ratio of pct_success to pct_group).  When di_prop_index < 1, then there are signs of disproportionate impact.
##' @examples
##' library(dplyr)
##' data(student_equity)
##' di_prop_index(success=Transfer, group=Ethnicity, data=student_equity) %>%
##'   as.data.frame
##' @references California Community Colleges Chancellor's Office (2014).  \href{http://extranet.cccco.edu/Portals/1/TRIS/Research/Accountability/GUIDELINES\%20FOR\%20MEASURING\%20DISPROPORTIONATE\%20IMPACT\%20IN\%20EQUITY\%20PLANS.pdf}{Guidelines for Measuring Disproportionate Impact in Equity Plans}.
##' @export
##' @import dplyr rlang
di_prop_index <- function(success, group, cohort, data) {
  if (!missing(data)) {
    eq_success <- enquo(success)
    success <- data %>% ungroup %>% mutate(success=!!eq_success) %>% select(success) %>% unlist
    eq_group <- enquo(group)
    group <- data %>% ungroup %>% mutate(group=!!eq_group) %>% select(group) %>% unlist
  }
  # Check if success is binary or logical and that there are no NA's
  stopifnot(success %in% c(1, 0))

  # Check if cohort is specified
  if (missing(cohort)) {
    cohort <- 1
    remove_cohort <- TRUE
  } else {
    remove_cohort <- FALSE
    if (!missing(data)) {
      eq_cohort <- enquo(cohort)
      cohort <- data %>% ungroup %>% mutate(cohort=!!eq_cohort) %>% select(cohort) %>% unlist
    }
  }

  # Calculate
  df <- data_frame(cohort, group, success)
  pct_success <- pct_group <- NULL # to resolve CRAN NOTE: no visible binding for global variable
  dResults <- df %>%
    group_by(cohort, group) %>%
    summarize(n=n(), success=sum(success)) %>%
    ungroup %>%
    group_by(cohort) %>% 
    mutate(pct_success=success/sum(success), pct_group=n/sum(n), di_prop_index=pct_success/pct_group) %>% 
    ungroup %>%
    arrange(cohort, group)

  if (remove_cohort) {
    dResults$cohort <- NULL
  }

  return(dResults)
}