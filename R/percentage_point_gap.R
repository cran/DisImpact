##' Calculate the margin of error (MOE) for the percentage point gap (PPG) method.
##'
##' @title Margin of error for the PPG
##' @param n Sample size for the group of interest.
##' @param proportion (Optional) The proportion of successes for the group of interest.  If specified, then the proportion is used in the MOE formula.  Otherwise, a default proportion of 0.50 is used (conservative and yields the maximum MOE).
##' @param min_moe The minimum MOE returned even if the sample size is large.  Defaults to 0.03.  This equates to a minimum threshold gap for declaring disproportionate impact.
##' @param prop_sub_0 For cases where `proportion` is 0, substitute with \code{prop_sub_0} (defaults to 0.5) to account for the zero MOE.
##' @param prop_sub_1 For cases where `proportion` is 1, substitute with \code{prop_sub_1} (defaults to 0.5) to account for the zero MOE.
##' @return The margin of error for the PPG given the specified sample size.
##' @examples
##' ppg_moe(n=800)
##' ppg_moe(n=c(200, 800, 1000, 2000))
##' ppg_moe(n=800, proportion=0.20)
##' ppg_moe(n=800, proportion=0.20, min_moe=0)
##' ppg_moe(n=c(200, 800, 1000, 2000), min_moe=0.01)
##' @references California Community Colleges Chancellor's Office (2017).  \href{https://www.cccco.edu/-/media/CCCCO-Website/About-Us/Divisions/Digital-Innovation-and-Infrastructure/Research/Files/PercentagePointGapMethod2017.ashx}{Percentage Point Gap Method}.
##' @export
ppg_moe <- function(n, proportion, min_moe=0.03, prop_sub_0=0.5, prop_sub_1=0.5) {
  if (missing(proportion)) {
    return(pmax(1.96 * sqrt(0.25/n), min_moe))
  }
  else {
	if (any(proportion == 0)) {
		warning(paste0("The vector `proportion` contains 0.  This will lead to a zero MOE.  `prop_sub_0=", prop_sub_0, "` will be used in calculating the MOE for these cases."))
		proportion[proportion==0] <- prop_sub_0
	}
	if (any(proportion == 1)) {
		warning(paste0("The vector `proportion` contains 1.  This will lead to a zero MOE.  `prop_sub_1=", prop_sub_1, "` will be used in calculating the MOE for these cases."))
		proportion[proportion==1] <- prop_sub_1
	}
    return(pmax(1.96 * sqrt(proportion * (1-proportion)/n), min_moe))
  }
}
##' Calculate disproportionate impact per the percentage point gap (PPG) method.
##'
##' This function determines disproportionate impact based on the percentage point gap (PPG) method, as described in \href{https://www.cccco.edu/-/media/CCCCO-Website/About-Us/Divisions/Digital-Innovation-and-Infrastructure/Research/Files/PercentagePointGapMethod2017.ashx}{this} reference from the California Community Colleges Chancellor's Office.  It assumes that a higher rate is good ("success").  For rates that are deemed negative (eg, rate of drop-outs, high is bad), then consider looking at the converse of the non-success (eg, non drop-outs, high is good) instead in order to leverage this function properly.  Note that the margin of error (MOE) is calculated using using \code{1.96*sqrt(0.25^2/n)}, with a \code{min_moe} used as the minimum by default.
##' @title Calculate disproportionate impact per the percentage point gap (PPG) method.
##' @param success A vector of success indicators (\code{1}/\code{0} or \code{TRUE}/\code{FALSE}) or an unquoted reference (name) to a column in \code{data} if it is specified.  It could also be a vector of counts, in which case \code{weight} (group size) should also be specified.
##' @param group A vector of group names of the same length as \code{success} or an unquoted reference (name) to a column in \code{data} if it is specified.
##' @param cohort (Optional) A vector of cohort names of the same length as \code{success} or an unquoted reference (name) to a column in \code{data} if it is specified.  Disproportionate impact is calculated for every group within each cohort.  When \code{cohort} is not specified, then the analysis assumes a single cohort.
##' @param weight (Optional) A vector of case weights of the same length as \code{success} or an unquoted reference (name) to a column in \code{data} if it is specified.  If \code{success} consists of counts instead of success indicators (1/0), then \code{weight} should also be specified to indicate the group size.
##' @param reference Either \code{'overall'} (default), \code{'hpg'} (highest performing group), \code{'all but current'} (success rate of everyone excluding the comparison group; also known as 'ppg minus 1'), a value from \code{group} (specifying a reference group), a single proportion (eg, 0.50), or a vector of proportions (one for each cohort).  Reference is used as a point of comparison for disproportionate impact for each group.  When \code{cohort} is specified:
##' \itemize{
##'   \item \code{'overall'} will use the overall success rate of each cohort group as the reference;
##'   \item \code{'hpg'} will use the highest performing group in each cohort as reference;
##'   \item  \code{'all but current'} will use the calculated success rate of each cohort group excluding the comparison group
##'   \item the success rate of the specified reference group from \code{group} in each cohort will be used;
##'   \item the specified proportion will be used for all cohorts;
##'   \item the specified vector of proportions will refer to the reference point for each cohort in alphabetical order (so the number of proportions should equal to the number of unique cohorts).
##' }
##' @param data (Optional) A data frame containing the variables of interest.  If \code{data} is specified, then \code{success}, \code{group}, and \code{cohort} will be searched within it.
##' @param min_moe The minimum margin of error (MOE) to be used in the calculation of disproportionate impact and is passed to \link{ppg_moe}.  Defaults to \code{0.03}.
##' @param use_prop_in_moe A logical value indicating whether or not the MOE formula should use the observed success rates (\code{TRUE}).  Defaults to \code{FALSE}, which uses 0.50 as the proportion in the MOE formula.  If \code{TRUE}, the success rates are passed to the \code{proportion} argument of \link{ppg_moe}.
##' @param prop_sub_0 For cases where \code{proportion} is 0, substitute with \code{prop_sub_0} (defaults to 0.5) to account for the zero MOE.  This is relevant only when \code{use_prop_in_moe=TRUE}.
##' @param prop_sub_1 For cases where \code{proportion} is 1, substitute with \code{prop_sub_1} (defaults to 0.5) to account for the zero MOE.  This is relevant only when \code{use_prop_in_moe=TRUE}.
##' @param check_valid_reference Check whether \code{reference} is a valid value; defaults to \code{TRUE}.  This argument exists to be used in \link{di_iterate} as when iterating DI calculations, there may be some scenarios where a specified reference group does not contain any students.
##' @return A data frame consisting of:
##' \itemize{
##'   \item \code{cohort} (if used),
##'   \item \code{group},
##'   \item \code{n} (sample size),
##'   \item \code{success} (number of successes for the cohort-group),
##'   \item \code{pct} (proportion of successes for the cohort-group),
##'   \item \code{reference_group} (reference group used in DI calculation),
##'   \item \code{reference} (reference value used in DI calculation),
##'   \item \code{moe} (margin of error),
##'   \item \code{pct_lo} (lower 95\% confidence limit for pct),
##'   \item \code{pct_hi} (upper 95\% confidence limit for pct),
##'   \item \code{di_indicator} (1 if there is disproportionate impact, ie, when \code{pct_hi <= reference}), 
##'   \item \code{success_needed_not_di} (the number of additional successes needed in order to no longer be considered disproportionately impacted as compared to the reference), and
##'   \item \code{success_needed_full_parity} (the number of additional successes needed in order to achieve full parity with the reference).
##' }
##' @examples
##' library(dplyr)
##' data(student_equity)
##' # Vector
##' di_ppg(success=student_equity$Transfer
##'   , group=student_equity$Ethnicity) %>% as.data.frame
##' # Tidy and column reference
##' di_ppg(success=Transfer, group=Ethnicity, data=student_equity) %>%
##'   as.data.frame
##' # Cohort
##' di_ppg(success=Transfer, group=Ethnicity, cohort=Cohort
##'  , data=student_equity) %>%
##'   as.data.frame
##' # With custom reference (single)
##' di_ppg(success=Transfer, group=Ethnicity, reference=0.54
##'   , data=student_equity) %>%
##'   as.data.frame
##' # With custom reference (multiple)
##' di_ppg(success=Transfer, group=Ethnicity, cohort=Cohort
##'   , reference=c(0.5, 0.55), data=student_equity) %>%
##'   as.data.frame
##' # min_moe
##' di_ppg(success=Transfer, group=Ethnicity, data=student_equity
##'   , min_moe=0.02) %>%
##'   as.data.frame
##' # use_prop_in_moe
##' di_ppg(success=Transfer, group=Ethnicity, data=student_equity
##'   , min_moe=0.02
##'   , use_prop_in_moe=TRUE) %>%
##'   as.data.frame
##' @references California Community Colleges Chancellor's Office (2017).  \href{https://www.cccco.edu/-/media/CCCCO-Website/About-Us/Divisions/Digital-Innovation-and-Infrastructure/Research/Files/PercentagePointGapMethod2017.ashx}{Percentage Point Gap Method}.
##' @export
##' @import dplyr
##' @importFrom rlang !! enquo
di_ppg <- function(success, group, cohort, weight, reference=c('overall', 'hpg', 'all but current', unique(group)), data, min_moe=0.03, use_prop_in_moe=FALSE, prop_sub_0=0.5, prop_sub_1=0.5, check_valid_reference=TRUE) {
  ## require(magrittr)
  ## require(dplyr)
  ## require(rlang)

  if (!missing(data)) {
    eq_success <- enquo(success)
    success <- data %>% ungroup %>% mutate(success=!!eq_success) %>% select(success) %>% unlist
    eq_group <- enquo(group)
    group <- data %>% ungroup %>% mutate(group=!!eq_group) %>% select(group) %>% unlist
  }
  # Check if success is binary or logical and that there are no NA's
  #stopifnot(success %in% c(1, 0))
  stopifnot(!is.na(success), success>=0) # can be counts

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

  # Check if weight is specified
  if (missing(weight)) {
    weight <- rep(1, length(success))
  } else {
    if (!missing(data)) {
      eq_weight <- enquo(weight)
      weight <- data %>% ungroup %>% mutate(weight=!!eq_weight) %>% select(weight) %>% unlist
    } # else: weight should be defined
  }
  
  # Check if reference is specified: overall, hpg, or user-defined references
  if (!is.numeric(reference)) {
    if (check_valid_reference) { # Add this for di_iterate with scenario_by_vars: sometimes a named reference group does not have any students, so will error out because this group doesn't exist
      reference <- match.arg(reference)
    }
    reference_type <- reference
    reference_numeric <- 0
  } else {
    # expecting vector of length 1 or length equal to unique cohort
    #stopifnot(length(reference) == length(unique(cohort)))
    stopifnot(length(reference) == length(sort(unique(cohort), na.last=TRUE))) # remove NA cohort
    reference_type <- 'numeric'
    reference_numeric <- reference
    if (length(reference) > 1) {
      dReference <- tibble(cohort=sort(unique(cohort), na.last=TRUE)
                         , reference
                         , reference_group=reference_type
                               )
    }
  }

  # Calculate
  df <- tibble(cohort, group, success, weight)
  dResults <- df %>%
    group_by(cohort, group) %>%
    summarize(n=sum(weight), success=sum(success), pct=success/n) %>%
    ungroup %>%
    arrange(cohort, group)
  if (!exists('dReference')) {
    dResults <- dResults %>% 
      group_by(cohort) %>%
      mutate(reference=case_when(
               reference_type=='overall' ~ sum(success) / sum(n)
             , reference_type=='hpg' ~ max(pct)
             , reference_type=='all but current' ~ (sum(success) - success) / (sum(n) - n)
             , reference_type!='numeric' ~ if(sum(group==reference_type, na.rm=TRUE) > 0) {success[group==reference_type & !is.na(group)] / n[group==reference_type & !is.na(group)]} else {NA_real_}
             , reference_type=='numeric' ~ reference_numeric
             )
             ## , reference_group=reference_type # Change to next line for ver. 0.0.20 and after so that 'hpg' returns the actual group
             ## , reference_group=case_when( # Following removed because group could be non-character, which would yield an error for case_when
             ##     reference_type == 'hpg' ~ group[pct == max(pct)][1]
             ##   , TRUE ~ reference_type
             ##   )
             , reference_group=if (reference_type == 'hpg') { # this is similar to 80_percent_index
                                 group[pct==max(pct)][1]
                               } else {
                                 reference_type
                               }
             ) %>%
      ungroup
  } else {
    dResults <- dResults %>%
      left_join(dReference, by='cohort')
  }
  pct <- moe <- pct_lo <- pct_hi <- reference_group <- di_indicator <- NULL # to resolve CRAN NOTE: no visible binding for global variable
  dResults <- dResults %>% 
    mutate(## moe=case_when(use_prop_in_moe ~ ppg_moe(n=n, proportion=pct, min_moe=min_moe, prop_sub_0=prop_sub_0, prop_sub_1=prop_sub_1)
           ##               , !use_prop_in_moe ~ ppg_moe(n=n, min_moe=min_moe)
           ##               ) ## this always gets evaluated and warning message pops up from ppg_moe
      moe=if (use_prop_in_moe) { ppg_moe(n=n, proportion=pct, min_moe=min_moe, prop_sub_0=prop_sub_0, prop_sub_1=prop_sub_1) } else { ppg_moe(n=n, min_moe=min_moe) }
         , pct_lo=pct - moe
         , pct_hi=pct + moe
         , di_indicator=ifelse(pct_hi <= reference, 1, 0) %>% coalesce(0)
           ) %>%
    mutate(success_needed_not_di=ifelse(di_indicator==1, ceiling((reference - (pct+moe)) * n), 0)
           , success_needed_full_parity=ifelse(pct < reference, ceiling((reference - pct) * n), 0) %>% coalesce(0)
           ) %>% 
    arrange(cohort, group)
  
  if (remove_cohort) {
    dResults$cohort <- NULL
  }
  
  return(dResults)
}

##' Iteratively calculate disproportionate impact via the percentage point gap (PPG) method for many disaggregation variables.
##' 
##' Iteratively calculate disproportionate impact via the percentage point gap (PPG) method for all combinations of `success_vars`, `group_vars`, and `cohort_vars`, for each combination of subgroups specified by `repeat_by_vars`.
##' @title Iteratively calculate disproportionate impact via the percentage point gap (PPG) method for many variables.
##' @param data A data frame for which to iterate DI calculation for a set of variables.
##' @param success_vars A character vector of success variable names to iterate across.
##' @param group_vars A character vector of group (disaggregation) variable names to iterate across.
##' @param cohort_vars A character vector of cohort variable names to iterate across.
##' @param reference_groups Either 'overall', 'hpg', or a character vector of the same length as `group_vars` that indicates the reference group value for each group variable in `group_vars`.
##' @param repeat_by_vars A character vector of variables to repeat DI calculations for across all combination of these variables, including '- All' as a group for each variable.  The reference rate used for DI comparison differs for every combination of the variables listed here.
##' @param weight_var A character scalar specifying the weight variable if the input data set is summarized (ie,  the the success variables specified in `success_vars` contain count of successes).  Weight here corresponds to the denominator when calculating the success rate.  Defaults to `NULL` for an input data set where each row describes each individual.
##' @param min_moe The minimum margin of error to be used in the PPG calculation, passed to `di_ppg`.
##' @param use_prop_in_moe Whether the estimated proportions should be used in the margin of error calculation by the PPG, passed to `di_ppg`.
##' @param prop_sub_0 Passed to `di_ppg`.
##' @param prop_sub_1 Passed to `di_ppg`.
##' @return A data frame with all relevant returned fields from `di_ppg` plus `success_variable` (elements of `success_vars`), `disaggregation` (elements of `group_vars`), and `reference_group` (elements of `reference_groups`).
##' @examples
##' library(dplyr)
##' data(student_equity)
##' # Multiple group variables
##' di_ppg_iterate(data=student_equity, success_vars=c('Transfer')
##'   , group_vars=c('Ethnicity', 'Gender'), cohort_vars=c('Cohort')
##'   , reference_groups='overall')
##' @import dplyr
##' @importFrom tidyselect everything one_of
##' @importFrom purrr pmap
##' @importFrom tidyr unnest
##' @export
di_ppg_iterate <- function(data, success_vars, group_vars, cohort_vars, reference_groups, repeat_by_vars=NULL, weight_var=NULL, min_moe=0.03, use_prop_in_moe=FALSE, prop_sub_0=0.5, prop_sub_1=0.5) {
  .Deprecated(msg="`di_ppg_iterate` is no longer supported.  Please use `di_iterate` instead, which calculates DI using all 3 methods.  NOTE: `repeat_by_vars` argument is now `scenario_repeat_by_vars`; by default, non-disaggregated results are also returned; and the returned `di_indicator` is now called `di_indicator_ppg`.")
  
  stopifnot(length(group_vars) == length(reference_groups) | length(reference_groups) == 1)
  if (length(unique(sapply(data[, group_vars], class))) > 1) {
    stop("All variables specified in `group_vars` should be of the same class.  Suggestion: set them all as character data using `as.character`.")
  }
  if (!is.null(repeat_by_vars)) {
    if (length(unique(sapply(data[, repeat_by_vars], class))) > 1) {
      stop("All variables specified in `repeat_by_vars` should be of the same class.  Suggestion: set them all as character data.")
    } 
  }
  
  if (is.null(weight_var)) {
    weight_var <- '- Weight'
    ## data[[weight_var]] <- 1

    # Create summarized data set for faster computations down the line
    data <- data %>%
      mutate_at(vars(one_of(success_vars)), .funs=list('NA_FLAG'= ~ is.na(.))) %>% # sum up successes
      group_by_at(vars(one_of(group_vars, cohort_vars, repeat_by_vars, if (length(success_vars)==1) {'NA_FLAG'} else {paste0(success_vars, '_NA_FLAG')}))) %>% # Break out by missingness in the success variables in order to sum separately for valid weights
      mutate(`- Weight`=1) %>%
      summarize_at(vars(success_vars, '- Weight'), .funs=sum) %>%  # sum of success variables and cases (weight)
      ungroup
  } else {
    if (any(is.na(data[[weight_var]]))) {
      stop(paste0("The specified column corresponding to weight_var='", weight_var, "' contain NA values."))
    }
    if (any(data[[weight_var]] <= 0)) {
      stop(paste0("The specified column corresponding to weight_var='", weight_var, "' contain non-positive values."))
    }
  }
 
  # CRAN: no visible binding for global variable
  success_var <- group_var <- cohort_var <- reference_group <- NULL

  # Set up different repeat-by data sets by determining row indices
  if (!is.null(repeat_by_vars)) {
    # All combinations, including '- All'
    dRepeatScenarios0 <- data %>%
      select(one_of(repeat_by_vars)) %>%
      lapply(function(x) c(unique(x), '- All')) %>%
      expand.grid(stringsAsFactors=FALSE)
    
    # For each combination, determine row indices; take only combination with actual observations

    # CRAN: no visible binding for global variable
    row_index <- want_indices <- n_rows <- NULL
    dRepeatScenarios <- lapply(1:nrow(dRepeatScenarios0)
                             , FUN=function(i) {
                               # CRAN: no visible binding for global variable
                               row_index <- want_indices <- n_rows <- NULL
                               
                               vars_specific <- colnames(dRepeatScenarios0)[!(dRepeatScenarios0[i, ] %in% '- All')]
                               vars_all <- colnames(dRepeatScenarios0)[dRepeatScenarios0[i, ] %in% '- All']
                               
                               if (length(vars_specific) != 0) {
                                 # dRepeatScenarios0[i, ] %>% # this gives an error when there is a single variable in repeat_by_vars
                                 d_interm <- dRepeatScenarios0 %>%
                                   slice(i) %>% 
                                   select(one_of(vars_specific)) %>%
                                   left_join(data %>% mutate(row_index=row_number())) %>%
                                   filter(!is.na(row_index)) %>% # No match
                                   group_by_at(vars(one_of(vars_specific))) %>%
                                   summarize(want_indices=list(row_index), n_rows=n()) %>%
                                   ungroup # %>%
                                   # mutate_at(.vars=vars(one_of(vars_all)), .funs=function(x) '- All') ## do this below

                                 d_interm[, vars_all] <- '- All' # No impact if vars_all is empty
                                 d_interm
                               } else { # all variables are '- All'
                                 d_interm <- data %>%
                                   mutate(row_index=row_number()) %>%
                                   summarize(want_indices=list(row_index), n_rows=n()) %>%
                                   ungroup # %>%
                                   # mutate_at(.vars=vars(one_of(vars_all)), .funs=function(x) '- All')
                                 d_interm[, vars_all] <- '- All' # No impact if vars_all is empty
                                 d_interm
                               }
                             }
                             ) %>%
      bind_rows %>%
      filter(n_rows > 0) %>%
      select(-n_rows)
  }
  
  ## if (!is.null(repeat_by_vars)) {
  ##   # Combination of subsets
  ##   dRepeatData1 <- data %>%
  ##     # expand(nesting(one_of(repeat_by_vars)))
  ##     select(one_of(repeat_by_vars)) %>%
  ##     distinct %>%
  ##     left_join(data %>% mutate(row_index=row_number())) %>%
  ##     group_by_at(vars(one_of(repeat_by_vars))) %>%
  ##     summarize(want_indices=list(row_index)) %>%
  ##     ungroup
    
  ##   # Combination of subsets: the 'All' group for each variable
  ##   if (length(repeat_by_vars) > 1) {
  ##     dRepeatData2 <- lapply(seq_along(repeat_by_vars)
  ##                          , FUN=function(i) {
  ##                            cur_var <- repeat_by_vars[i]
  ##                            dRepeatData <- data %>%
  ##                              # expand(nesting(one_of(repeat_by_vars)))
  ##                              select(one_of(repeat_by_vars[-i])) %>%
  ##                              distinct %>%
  ##                              left_join(data %>% mutate(row_index=row_number())) %>%
  ##                              group_by_at(vars(one_of(repeat_by_vars[-i]))) %>%
  ##                              summarize(want_indices=list(row_index)) %>%
  ##                              ungroup %>%
  ##                              mutate(!!cur_var := "- All")
  ##                          }
  ##                          ) %>%
  ##       bind_rows
  ##   } else { # length(repeat_by_vars) == 1
  ##     dRepeatData2 <- data %>%
  ##       mutate(row_index=row_number()) %>% 
  ##       mutate(!!repeat_by_vars := "- All") %>%
  ##       group_by_at(vars(one_of(repeat_by_vars))) %>%
  ##       summarize(want_indices=list(row_index)) %>%
  ##       ungroup
  ##   }
  ##   # Combine
  ##   dRepeatData <- bind_rows(dRepeatData1, dRepeatData2)
  ## } else {

  ## }
  
  # Set up scenarios
  dRef <- data.frame(group_var=group_vars, reference_group=reference_groups, stringsAsFactors=FALSE)
  
  dScenarios <- expand.grid(success_var=success_vars, group_var=group_vars, cohort_var=cohort_vars, min_moe=min_moe, use_prop_in_moe=use_prop_in_moe, prop_sub_0=prop_sub_0, prop_sub_1=prop_sub_1, stringsAsFactors=FALSE) %>%
    left_join(dRef) %>%
    select(success_var, group_var, cohort_var, reference_group, min_moe, use_prop_in_moe, prop_sub_0, prop_sub_1)

  # Function to iterate for each scenario
  iterate <- function(success_var, group_var, cohort_var, reference_group, min_moe, use_prop_in_moe, prop_sub_0, prop_sub_1, subset_idx) {
    
    data <- data[subset_idx, ]

    # Remove rows with missing value for success_var
    data <- data[!is.na(data[[success_var]]), ]

    # If after removing rows with missing data and we end up with 0 cases, return nothing
    if (nrow(data)==0) {
      return(NULL)
    }
    
    if (!(reference_group %in% c('overall', 'hpg', 'all but current'))) {
      # reference_val <- sapply(sort(unique(data[[cohort_var]]), na.last=TRUE), function(cohort) mean(data[[success_var]][data[[group_var]] %in% reference_group & data[[cohort_var]] %in% cohort])) # one for each non-NA cohort; this is for before incorporating weights
      reference_val <- sapply(sort(unique(data[[cohort_var]]), na.last=TRUE), function(cohort) sum(data[[success_var]][data[[group_var]] %in% reference_group & data[[cohort_var]] %in% cohort]) / sum(data[[weight_var]][data[[group_var]] %in% reference_group & data[[cohort_var]] %in% cohort]))
    } else {
      reference_val <- reference_group # overall or hpg or all but current
    }

    # CRAN: no visible binding for global variable
    success_variable <- disaggregation <- NULL

    di_ppg(success=data[[success_var]], group=data[[group_var]], cohort=data[[cohort_var]], weight=data[[weight_var]], reference=reference_val, min_moe=min_moe, use_prop_in_moe=use_prop_in_moe, prop_sub_0=prop_sub_0, prop_sub_1=prop_sub_1) %>%
      mutate(
        success_variable=success_var
      , disaggregation=group_var
      # , reference_group=reference_group # di_ppg returns reference_group, fix below
             ) %>%
      select(-reference_group) %>%
      mutate(reference_group=reference_group) %>% 
      select(success_variable, disaggregation, reference_group, everything())
  }

  # Iterate for all scenarios
  if (is.null(repeat_by_vars)) {
    subset_idx <- 1:nrow(data)
    pmap(dScenarios %>% mutate(subset_idx=list(subset_idx)), iterate) %>%
      bind_rows
  } else {
    dRepeatScenarios$df_results <- lapply(1:nrow(dRepeatScenarios)
                                     , FUN=function(i) {
                                       # data <- data %>% slice(dRepeatScenarios[i, ] %>% select(want_indices) %>% unlist)
                                       # subset_idx <- dRepeatScenarios[i, ] %>% select(want_indices) %>% unlist
                                       subset_idx <- dRepeatScenarios %>% slice(i) %>% select(want_indices) %>% unlist
                                       pmap(dScenarios %>% mutate(subset_idx=list(subset_idx)), iterate) %>%
                                         bind_rows
                                     }
                                     )
    # CRAN: no visible binding for global variable
    df_results <- NULL
    
    dRepeatScenarios %>%
      select(-want_indices) %>%
      unnest(df_results)
  }
}
