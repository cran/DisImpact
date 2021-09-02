## ---- warning=FALSE-----------------------------------------------------------
# Load some necessary packages
library(dplyr)
library(stringr)
library(ggplot2)
library(scales)
library(forcats)
library(DisImpact)

# Load student equity data set
data(student_equity)

# Caclulate DI over several scenarios
df_di_summary <- di_iterate(data=student_equity
                          , success_vars=c('Math', 'English', 'Transfer')
                          , group_vars=c('Ethnicity', 'Gender')
                          , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                          , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                            )

## -----------------------------------------------------------------------------
head(student_equity)
## # Correlation to show overlap
## cor(student_equity[, str_detect(names(student_equity), 'EthnicityFlag')])

## -----------------------------------------------------------------------------
# Identify the ethnicity flag variables
want_vars <- names(student_equity)[str_detect(names(student_equity), '^EthnicityFlag')]
want_vars <- want_vars[!str_detect(want_vars, 'Unknown')] # Remove Unknown
want_vars <- want_vars[!str_detect(want_vars, 'Two')] # Remove Two or More Races
want_vars # Ethnicity Flags of interest


# Number of students
## Total
student_equity %>%
  group_by(Cohort) %>%
  tally

## Each group
student_equity %>%
  select(Cohort, one_of(want_vars)) %>% 
  group_by(Cohort) %>%
  summarize_all(.funs=sum) %>%
  as.data.frame
## Observation: students can be in more than 1 group

# Convert the ethnicity flags to character as required by di_iterate
for (varname in want_vars) {
  student_equity[[varname]] <- as.character(student_equity[[varname]])
}

# DI analysis
df_di_summary_mult_eth <- di_iterate(data=student_equity
                          , success_vars=c('Math', 'English', 'Transfer')
                          , group_vars=want_vars # specify the list of ethnicity flag variables
                          , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                          , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                          , di_80_index_reference_groups='all but current'
                            ) %>%
  filter(group=='1') %>% # Ethnicity flags have 1's and 0's; filter on just the 1 group as that is of interest
  # filter((group=='1') | (disaggregation=='- None' & group=='- All')) %>% 
  mutate(group=str_replace(disaggregation, 'EthnicityFlag_', '') %>% gsub(pattern='([A-Z])', replacement=' \\1', x=.) %>% str_replace('^ ', '') %>% str_replace('A A N A P I', 'AANAPI')# Rather than show '1', identify the ethnicity group names and assign them to group
       , disaggregation='Multi-Ethnicity' # Originally is a list of variable names corresponding to the various ethnicity flags; call this disaggregation 'Multi-Ethnicity'
         )

# Check if re-assignments are correct
table(df_di_summary_mult_eth$disaggregation, useNA='ifany')
table(df_di_summary_mult_eth$group, useNA='ifany')

# Illustration: the group proportions add up to more than 100% since a student could be counted in more than 1 group
df_di_summary_mult_eth %>%
  filter(Ed_Goal=='- All', College_Status=='- All', success_variable=='Transfer', cohort=='2018') %>%
  select(group, n) %>%
  mutate(Proportion=n / sum(student_equity$Cohort=='2018')) %>%
  mutate(Sum_Proportion=sum(Proportion))

## -----------------------------------------------------------------------------
# Combine
df_di_summary_combined <- bind_rows(
  df_di_summary
  , df_di_summary_mult_eth # Could first filter on rows of interest (eg, just the categorizations of interest to the institution)
)

# Disaggregation: Ethnicity
df_di_summary_combined %>%
  filter(Ed_Goal=='- All', College_Status=='- All', success_variable=='Math', disaggregation=='Ethnicity') %>%
  select(cohort, group, n, pct, di_indicator_ppg, di_indicator_prop_index, di_indicator_80_index) %>%
  as.data.frame

# Disaggregation: Multi-Ethnicity
df_di_summary_combined %>%
  filter(Ed_Goal=='- All', College_Status=='- All', success_variable=='Math', disaggregation=='Multi-Ethnicity') %>%
  select(cohort, group, n, pct, di_indicator_ppg, di_indicator_prop_index, di_indicator_80_index) %>%
  as.data.frame

## ---- fig.width=9, fig.height=5-----------------------------------------------
# Disaggregation: Ethnicity
df_di_summary_combined %>%
  filter(Ed_Goal=='- All', College_Status=='- All', success_variable=='Math', disaggregation=='Ethnicity') %>%
  select(cohort, group, n, pct, di_indicator_ppg, di_indicator_prop_index, di_indicator_80_index) %>%
  mutate(group=factor(group) %>% fct_reorder(desc(pct))) %>% 
  ggplot(data=., mapping=aes(x=factor(cohort), y=pct, group=group, color=group)) +
  geom_point(aes(size=factor(di_indicator_ppg, levels=c(0, 1), labels=c('Not DI', 'DI')))) +
  ## geom_point(aes(size=factor(di_indicator_80_index, levels=c(0, 1), labels=c('Not DI', 'DI')))) +
  geom_line() +
  xlab('Cohort') +
  ylab('Rate') +
  theme_bw() +
  scale_color_manual(values=c('#1b9e77', '#d95f02', '#7570b3', '#e7298a', '#66a61e', '#e6ab02'), name='Ethnicity') +
  labs(size='Disproportionate Impact') +
  scale_y_continuous(labels = percent, limits=c(0, 1)) +
  ggtitle('Dashboard drop-down selections:', subtitle=paste0("Ed Goal = '- All' | College Status = '- All' | Outcome = 'Math' | Disaggregation = 'Ethnicity'"))

## ---- fig.width=9, fig.height=5-----------------------------------------------
# Disaggregation: Multi-Ethnicity
df_di_summary_combined %>%
  filter(Ed_Goal=='- All', College_Status=='- All', success_variable=='Math', disaggregation=='Multi-Ethnicity') %>%
  select(cohort, group, n, pct, di_indicator_ppg, di_indicator_prop_index, di_indicator_80_index) %>%
  mutate(group=factor(group) %>% fct_reorder(desc(pct))) %>% 
  ggplot(data=., mapping=aes(x=factor(cohort), y=pct, group=group, color=group)) +
  geom_point(aes(size=factor(di_indicator_ppg, levels=c(0, 1), labels=c('Not DI', 'DI')))) +
  ## geom_point(aes(size=factor(di_indicator_80_index, levels=c(0, 1), labels=c('Not DI', 'DI')))) +
  geom_line() +
  xlab('Cohort') +
  ylab('Rate') +
  theme_bw() +
  scale_color_manual(values=c('#a6cee3', '#1f78b4', '#b2df8a', '#33a02c', '#fb9a99', '#e31a1c', '#fdbf6f', '#ff7f00', '#cab2d6', '#6a3d9a', '#ffff99'), name='Multi-Ethnicity') +
  labs(size='Disproportionate Impact') +
  scale_y_continuous(labels = percent, limits=c(0, 1)) +
  ggtitle('Dashboard drop-down selections:', subtitle=paste0("Ed Goal = '- All' | College Status = '- All' | Outcome = 'Math' | Disaggregation = 'Multi-Ethnicity'"))

## -----------------------------------------------------------------------------
sessionInfo()

