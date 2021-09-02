## ---- message=FALSE, warning=FALSE--------------------------------------------
library(DisImpact)
library(dplyr) # Ease in manipulations with data frames

## -----------------------------------------------------------------------------
# Load fake data set
data(student_equity)

# Print first few observations
head(student_equity)

# For description of data set
## ?student_equity

## -----------------------------------------------------------------------------
# Summarize toy data
dim(student_equity)
dSumm <- student_equity %>%
  group_by(Cohort, Ethnicity) %>%
  summarize(n=n(), Transfer_Rate=mean(Transfer))
dSumm ## This is a summarized version of the data set

## -----------------------------------------------------------------------------
# Vector
di_ppg(success=student_equity$Transfer, group=student_equity$Ethnicity) %>% as.data.frame
# Tidy and column reference
di_ppg(success=Transfer, group=Ethnicity, data=student_equity) %>%
  as.data.frame

## -----------------------------------------------------------------------------
# Cohort
di_ppg(success=Transfer, group=Ethnicity, cohort=Cohort, data=student_equity) %>%
  as.data.frame

## -----------------------------------------------------------------------------
di_ppg(success=Transfer_Rate*n, group=Ethnicity, cohort=Cohort, weight=n, data=dSumm) %>%
  as.data.frame

## -----------------------------------------------------------------------------
# Reference: Highest performing group
di_ppg(success=Transfer, group=Ethnicity, cohort=Cohort, reference='hpg', data=student_equity) %>%
  as.data.frame

# Reference: All but current (PPG minus 1)
di_ppg(success=Transfer, group=Ethnicity, cohort=Cohort, reference='all but current', data=student_equity) %>%
  as.data.frame

# Reference: custom group
di_ppg(success=Transfer, group=Ethnicity, cohort=Cohort, reference='White', data=student_equity) %>%
  as.data.frame
di_ppg(success=Transfer, group=Ethnicity, cohort=Cohort, reference='Asian', data=student_equity) %>%
  as.data.frame

## -----------------------------------------------------------------------------
# With custom reference (single)
di_ppg(success=Transfer, group=Ethnicity, reference=0.54, data=student_equity) %>%
  as.data.frame

# With custom reference (multiple)
di_ppg(success=Transfer, group=Ethnicity, cohort=Cohort, reference=c(0.5, 0.55), data=student_equity) %>%
  as.data.frame

## -----------------------------------------------------------------------------
# min_moe
di_ppg(success=Transfer, group=Ethnicity, data=student_equity, min_moe=0.02) %>%
  as.data.frame
# use_prop_in_moe
di_ppg(success=Transfer, group=Ethnicity, data=student_equity, min_moe=0.02, use_prop_in_moe=TRUE) %>%
  as.data.frame

## -----------------------------------------------------------------------------
# Set Native American to have have zero transfers and see what the results
di_ppg(success=Transfer, group=Ethnicity, data=student_equity %>% mutate(Transfer=ifelse(Ethnicity=='Native American', 0, Transfer)), use_prop_in_moe=TRUE, prop_sub_0=0.1, prop_sub_1=0.9) %>%
  as.data.frame

## -----------------------------------------------------------------------------
# Without cohort
## Vector
di_prop_index(success=student_equity$Transfer, group=student_equity$Ethnicity) %>% as.data.frame
## Tidy and column reference
di_prop_index(success=Transfer, group=Ethnicity, data=student_equity) %>%
  as.data.frame

# With cohort
## Vector
di_prop_index(success=student_equity$Transfer, group=student_equity$Ethnicity, cohort=student_equity$Cohort) %>% as.data.frame
## Tidy and column reference
di_prop_index(success=Transfer, group=Ethnicity, cohort=Cohort, data=student_equity) %>%
  as.data.frame

## -----------------------------------------------------------------------------
# Changing threshold for DI
di_prop_index(success=student_equity$Transfer, group=student_equity$Ethnicity, cohort=student_equity$Cohort, di_prop_index_cutoff=0.5) %>% as.data.frame

## -----------------------------------------------------------------------------
# Without cohort
## Vector
di_80_index(success=student_equity$Transfer, group=student_equity$Ethnicity) %>% as.data.frame
## Tidy and column reference
di_80_index(success=Transfer, group=Ethnicity, data=student_equity) %>%
  as.data.frame

# With cohort
## Vector
di_80_index(success=student_equity$Transfer, group=student_equity$Ethnicity, cohort=student_equity$Cohort) %>% as.data.frame
## Tidy and column reference
di_80_index(success=Transfer, group=Ethnicity, cohort=Cohort, data=student_equity) %>%
  as.data.frame

## -----------------------------------------------------------------------------
# Changing reference group
di_80_index(success=student_equity$Transfer, group=student_equity$Ethnicity, cohort=student_equity$Cohort, reference_group='White') %>% as.data.frame

## -----------------------------------------------------------------------------
# Changing threshold for DI
di_80_index(success=student_equity$Transfer, group=student_equity$Ethnicity, cohort=student_equity$Cohort, di_80_index_cutoff=0.50) %>% as.data.frame

## -----------------------------------------------------------------------------
## di_ppg(success=!Probation, group=Ethnicity, data=student_equity) %>%
##   as.data.frame ## If there were a Probation variable
di_ppg(success=!Transfer, group=Ethnicity, data=student_equity) %>%
  as.data.frame ## Illustrating the point with `!`

## -----------------------------------------------------------------------------
# Transform success
a <- sample(0:1, size=nrow(student_equity), replace=TRUE, prob=c(0.95, 0.05))
mean(a)
di_ppg(success=pmax(Transfer, a), group=Ethnicity, data=student_equity) %>%
  as.data.frame

# Collapse Black and Hispanic
di_ppg(success=Transfer, group=ifelse(Ethnicity %in% c('Black', 'Hispanic'), 'Black/Hispanic', Ethnicity), data=student_equity) %>% as.data.frame

## -----------------------------------------------------------------------------
# Multiple group variables
di_iterate(data=student_equity, success_vars=c('Transfer'), group_vars=c('Ethnicity', 'Gender'), cohort_vars=c('Cohort'), ppg_reference_groups='overall') %>% as.data.frame

# Multiple group variables and different reference groups

bind_rows(
  di_iterate(data=student_equity, success_vars=c('Transfer'), group_vars=c('Ethnicity', 'Gender'), cohort_vars=c('Cohort'), ppg_reference_groups='overall')
  , di_iterate(data=student_equity, success_vars=c('Transfer'), group_vars=c('Ethnicity', 'Gender'), cohort_vars=c('Cohort'), ppg_reference_groups=c('White', 'Male'), include_non_disagg_results=FALSE) # include_non_disagg_results = FALSE: Already have this scenario in Overall run
)

## -----------------------------------------------------------------------------
sessionInfo()

