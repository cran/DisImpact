## ---- message=FALSE, warning=FALSE--------------------------------------------
library(DisImpact)
library(dplyr) # Ease in manipulations with data frames

## -----------------------------------------------------------------------------
data(ssm_cohort) # provided from DisImpact
dim(ssm_cohort)
# head(ssm_cohort)

## ----echo=FALSE, results='asis'-----------------------------------------------
library(knitr)
kable(ssm_cohort[1:6, !(names(ssm_cohort) %in% c('description', 'categoryLabel', 'source'))], caption='A few rows from the `ssm_cohort` data set.  The following variables are ommitted in this print out: `description`, `categoryLabel`, `source`.')

## -----------------------------------------------------------------------------
d_relevant <- ssm_cohort %>%
  filter(
    categoryLabel %in% c('Completed Both Transfer-Level Math and English Within the District in the First Year Aligned with SCFF'
                       , 'Attained the Vision Goal Definition of Completion'
                       , 'Earned an Associate Degree'
                       , 'Transferred to a Four-Year Postsecondary Institution'
                         )
    , disagg1 %in% c('Ethnicity', 'Foster Youth', 'Veterans')
    , disagg2 == 'None' # There's also Gender
    , missingFlag == 0
    , ferpaFlag == 0
  )
d_relevant %>%
  group_by(disagg1, subgroup1) %>%
  tally

## -----------------------------------------------------------------------------
d_relevant_gender <- ssm_cohort %>%
  filter(
    categoryLabel %in% c('Completed Both Transfer-Level Math and English Within the District in the First Year Aligned with SCFF'
                       , 'Attained the Vision Goal Definition of Completion'
                       , 'Earned an Associate Degree'
                       , 'Transferred to a Four-Year Postsecondary Institution'
                         )
    , disagg1 %in% c('Ethnicity', 'Foster Youth', 'Veterans')
    # , disagg2 == 'None' # There's also Gender
    , disagg2 == 'Gender'
    , missingFlag == 0
    , ferpaFlag == 0
  )
d_relevant_gender %>%
  group_by(disagg1, subgroup1, disagg2, subgroup2) %>%
  tally

## ----warning=FALSE------------------------------------------------------------
# Example 1: By outcome, cohort
di_summ_1 <- di_iterate_on_long(data=d_relevant
                                , num_var='value'
                                , denom_var='denom'
                                , disagg_var_col='disagg1'
                                , group_var_col='subgroup1'
                                , cohort_var_col='academicYear'
                                , summarize_by_vars=c('categoryLabel', 'cohort')
                                , ppg_reference_groups='all but current' # PPG-1
                                , di_80_index_reference_groups='all but current' # Relative rates analogous to PPG-1 for reference group
                                  )
nrow(di_summ_1)
nrow(d_relevant)
di_summ_1 %>%
  head %>%
  as.data.frame

## ----warning=FALSE------------------------------------------------------------
# Example 2: by outcome, collapse cohort academic years
di_summ_2 <- di_iterate_on_long(data=d_relevant
                                , num_var='value'
                                , denom_var='denom'
                                , disagg_var_col='disagg1'
                                , group_var_col='subgroup1'
                                # , cohort_var_col='academicYear'
                                , summarize_by_vars=c('categoryLabel', 'cohort')
                                , ppg_reference_groups='all but current'
                                , di_80_index_reference_groups='all but current'
                                  )
nrow(di_summ_2)
nrow(d_relevant)

di_summ_2 %>%
  head %>%
  as.data.frame

## ----warning=FALSE------------------------------------------------------------
# Example 3: by outcome, intersecting gender
di_summ_3 <- di_iterate_on_long(data=d_relevant_gender
                                , num_var='value'
                                , denom_var='denom'
                                , disagg_var_col='disagg1'
                                , group_var_col='subgroup1'
                                , disagg_var_col_2='disagg2'
                                , group_var_col_2='subgroup2'
                                , cohort_var_col='academicYear'
                                , summarize_by_vars=c('categoryLabel', 'cohort')
                                , ppg_reference_groups='overall'
                                , di_80_index_reference_groups='all but current'
                                  )
nrow(di_summ_3)
nrow(d_relevant_gender)

di_summ_3 %>%
  head %>%
  as.data.frame

## ----warning=FALSE------------------------------------------------------------
# Example 4: By outcome, cohort; custom reference groups
di_summ_4 <- di_iterate_on_long(data=d_relevant %>%
                                  filter(subgroup1 != 'All Masked Values') %>% # some foster youth and vetans disaggregation have just a single All Masked Values row; removing these scenarios for purpose of illustration
                                  mutate(custom_reference=ifelse(subgroup1 %in% c('White','Not Foster Youth', 'Not Veteran'), 1, 0)) # create a variable that flags the reference groups
                                , num_var='value'
                                , denom_var='denom'
                                , disagg_var_col='disagg1'
                                , group_var_col='subgroup1'
                                , cohort_var_col='academicYear'
                                , summarize_by_vars=c('categoryLabel', 'cohort')
                                , custom_reference_group_flag_var='custom_reference' # Specify variable/flag for custom reference groups
                                  )
nrow(di_summ_4)

di_summ_4 %>%
  head %>%
  as.data.frame

## -----------------------------------------------------------------------------
sessionInfo()

