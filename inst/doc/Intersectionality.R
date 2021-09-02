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
# Create new variable
student_equity_intersection <- student_equity %>%
  mutate(`Ethnicity + Gender`=paste0(Ethnicity, ', ', Gender))

# Check
table(student_equity_intersection$`Ethnicity + Gender`, useNA='ifany')

# Run DI, then selet rows of interest (for Ethnicity + Gender, remove the Other gender)
df_di_summary_intersection <- di_iterate(data=student_equity_intersection # Specify new data set
                          , success_vars=c('Math', 'English', 'Transfer')
                          , group_vars=c('Ethnicity', 'Gender', 'Ethnicity + Gender') # Add new column name
                          , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                          , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                            ) %>%
  filter(!(disaggregation=='Ethnicity + Gender') | !str_detect(group, ', Other')) # Remove Ethnicity + Gender groups that correspond to 

## -----------------------------------------------------------------------------
# Disaggregation: Ethnicity
df_di_summary_intersection %>%
  filter(Ed_Goal=='- All', College_Status=='- All', success_variable=='Math', disaggregation=='Ethnicity') %>%
  select(cohort, group, n, pct, di_indicator_ppg, di_indicator_prop_index, di_indicator_80_index) %>%
  as.data.frame

# Disaggregation: Gender
df_di_summary_intersection %>%
  filter(Ed_Goal=='- All', College_Status=='- All', success_variable=='Math', disaggregation=='Ethnicity') %>%
  select(cohort, group, n, pct, di_indicator_ppg, di_indicator_prop_index, di_indicator_80_index) %>%
  as.data.frame

# Disaggregation: Ethnicity + Gender
df_di_summary_intersection %>%
  filter(Ed_Goal=='- All', College_Status=='- All', success_variable=='Math', disaggregation=='Ethnicity + Gender') %>%
  select(cohort, group, n, pct, di_indicator_ppg, di_indicator_prop_index, di_indicator_80_index) %>%
  as.data.frame

## ---- fig.width=9, fig.height=5-----------------------------------------------
# Disaggregation: Ethnicity
df_di_summary_intersection %>%
  filter(Ed_Goal=='- All', College_Status=='- All', success_variable=='Math', disaggregation=='Ethnicity') %>%
  select(cohort, group, n, pct, di_indicator_ppg, di_indicator_prop_index, di_indicator_80_index) %>%
  mutate(group=factor(group) %>% fct_reorder(desc(pct))) %>% 
  ggplot(data=., mapping=aes(x=factor(cohort), y=pct, group=group, color=group)) +
  geom_point(aes(size=factor(di_indicator_ppg, levels=c(0, 1), labels=c('Not DI', 'DI')))) +
  geom_line() +
  xlab('Cohort') +
  ylab('Rate') +
  theme_bw() +
  scale_color_manual(values=c('#1b9e77', '#d95f02', '#7570b3', '#e7298a', '#66a61e', '#e6ab02'), name='Ethnicity') +
  labs(size='Disproportionate Impact') +
  scale_y_continuous(labels = percent, limits=c(0, 1)) +
  ggtitle('Dashboard drop-down selections:', subtitle=paste0("Ed Goal = '- All' | College Status = '- All' | Outcome = 'Math' | Disaggregation = 'Ethnicity'"))

# Disaggregation: Gender
df_di_summary_intersection %>%
  filter(Ed_Goal=='- All', College_Status=='- All', success_variable=='Math', disaggregation=='Gender') %>%
  select(cohort, group, n, pct, di_indicator_ppg, di_indicator_prop_index, di_indicator_80_index) %>%
  mutate(group=factor(group) %>% fct_reorder(desc(pct))) %>% 
  ggplot(data=., mapping=aes(x=factor(cohort), y=pct, group=group, color=group)) +
  geom_point(aes(size=factor(di_indicator_ppg, levels=c(0, 1), labels=c('Not DI', 'DI')))) +
  geom_line() +
  xlab('Cohort') +
  ylab('Rate') +
  theme_bw() +
  scale_color_manual(values=c('#e7298a', '#66a61e', '#e6ab02'), name='Gender') +
  labs(size='Disproportionate Impact') +
  scale_y_continuous(labels = percent, limits=c(0, 1)) +
  ggtitle('Dashboard drop-down selections:', subtitle=paste0("Ed Goal = '- All' | College Status = '- All' | Outcome = 'Math' | Disaggregation = 'Gender'"))

# Disaggregation: Ethnicity + Gender
df_di_summary_intersection %>%
  filter(Ed_Goal=='- All', College_Status=='- All', success_variable=='Math', disaggregation=='Ethnicity + Gender') %>%
  select(cohort, group, n, pct, di_indicator_ppg, di_indicator_prop_index, di_indicator_80_index) %>%
  mutate(group=factor(group) %>% fct_reorder(desc(pct))) %>% 
  ggplot(data=., mapping=aes(x=factor(cohort), y=pct, group=group, color=group)) +
  geom_point(aes(size=factor(di_indicator_ppg, levels=c(0, 1), labels=c('Not DI', 'DI')))) +
  geom_line() +
  xlab('Cohort') +
  ylab('Rate') +
  theme_bw() +
  scale_color_manual(values=c('#a6cee3', '#1f78b4', '#b2df8a', '#33a02c', '#fb9a99', '#e31a1c', '#fdbf6f', '#ff7f00', '#cab2d6', '#6a3d9a', '#ffff99', '#b15928'), name='Ethnicity + Gender') +
  labs(size='Disproportionate Impact') +
  scale_y_continuous(labels = percent, limits=c(0, 1)) +
  ggtitle('Dashboard drop-down selections:', subtitle=paste0("Ed Goal = '- All' | College Status = '- All' | Outcome = 'Math' | Disaggregation = 'Ethnicity + Gender'"))

## -----------------------------------------------------------------------------
sessionInfo()

