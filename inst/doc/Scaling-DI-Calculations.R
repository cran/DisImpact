## ---- message=FALSE, warning=FALSE---------------------------------------
library(DisImpact)
library(dplyr) # Ease in manipulations with data frames

## ------------------------------------------------------------------------
data(student_equity) # provided from DisImpact
dim(student_equity)
# head(student_equity)

## ----echo=FALSE, results='asis'------------------------------------------
library(knitr)
kable(student_equity[1:6, ], caption='A few rows from the `student_equity` data set.')

## ----warning=FALSE-------------------------------------------------------
df_di_summary <- di_iterate(data=student_equity
                          , success_vars=c('Math', 'English', 'Transfer')
                          , group_vars=c('Ethnicity', 'Gender')
                          , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                          , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                            )

## df_di_summary <- di_iterate(data=student_equity, success_vars=c('Math', 'English', 'Transfer'), group_vars=c('Ethnicity', 'Gender'), cohort_vars=c('Cohort', 'Cohort', 'Cohort'), scenario_repeat_by_vars=c('Ed_Goal', 'College_Status'))

## df_di_summary <- di_iterate(data=student_equity, success_vars=c('Math', 'English', 'Transfer'), group_vars=c('Ethnicity', 'Gender'), scenario_repeat_by_vars=c('Ed_Goal', 'College_Status'))

## df_di_summary <- di_iterate(data=student_equity, success_vars=c('Math', 'English', 'Transfer'), group_vars=c('Ethnicity', 'Gender'), cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort'), scenario_repeat_by_vars=c('Ed_Goal', 'College_Status'), ppg_reference_groups=c('White', 'Male'), di_80_index_reference_groups=c('White', 'Male'))

## df_di_summary <- di_iterate(data=student_equity, success_vars=c('Math', 'English', 'Transfer'), group_vars=c('Ethnicity', 'Gender'), cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort'), scenario_repeat_by_vars=c('Ed_Goal', 'College_Status'), ppg_reference_groups=c('all but current'), di_80_index_reference_groups=c('White', 'Male'))

## ------------------------------------------------------------------------
dim(df_di_summary)
df_di_summary %>% head %>% as.data.frame # first few rows


## ------------------------------------------------------------------------
table(df_di_summary$Ed_Goal)
table(df_di_summary$College_Status)

## ------------------------------------------------------------------------
table(df_di_summary$disaggregation)

## ----results=FALSE-------------------------------------------------------
# No Disaggregation
df_di_summary %>%
  filter(Ed_Goal=='- All', College_Status=='- All', disaggregation=='- None') %>%
  as.data.frame

## ----echo=FALSE, results='asis'------------------------------------------
df_di_summary %>%
  filter(Ed_Goal=='- All', College_Status=='- All', disaggregation=='- None') %>%
  as.data.frame %>% 
  kable

## ----results=FALSE-------------------------------------------------------
# No Disaggregation
df_di_summary %>%
  filter(Ed_Goal=='- All', College_Status=='- All', success_variable=='Math', disaggregation=='- None') %>%
  as.data.frame

## ----include=FALSE-------------------------------------------------------
run_plots <- FALSE
# run_plots <- TRUE
if (run_plots) {
  library(ggplot2)
  library(forcats)
  library(scales)

  # No Disaggregation
  df_di_summary %>%
    filter(Ed_Goal=='- All', College_Status=='- All', success_variable=='Math', disaggregation=='- None') %>%
    select(cohort, group, n, pct, di_indicator_ppg, di_indicator_prop_index, di_indicator_80_index) %>%
    mutate(group=factor(group) %>% fct_reorder(desc(pct))) %>% 
    ggplot(data=., mapping=aes(x=factor(cohort), y=pct, group=group, color=group)) +
    geom_point() +
    geom_line() +
    xlab('Cohort') +
    ylab('Rate') +
    theme_bw() +
    scale_color_manual(values=c('#1b9e77'), name='Group') +
                                        # labs(size='Disproportionate Impact') +
  scale_y_continuous(labels = percent, limits=c(0, 1)) +
    ggtitle('Dashboard drop-down selections:', subtitle=paste0("Ed Goal = '- All' | College Status = '- All' | Outcome = 'Math' | Disaggregation = '- None'"))
  ggsave('Dashboard_1.png', height=5, width=9)
}

## ------------------------------------------------------------------------
# Disaggregation: Ethnicity
df_di_summary %>%
  filter(Ed_Goal=='- All', College_Status=='- All', success_variable=='Math', disaggregation=='Ethnicity') %>%
  select(cohort, group, n, pct, di_indicator_ppg, di_indicator_prop_index, di_indicator_80_index) %>%
  as.data.frame

## ----include=FALSE-------------------------------------------------------
if (run_plots) {
  # Disaggregation: Ethnicity
  df_di_summary %>%
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
  ggsave('Dashboard_2.png', height=5, width=9)
}

## ------------------------------------------------------------------------
# Disaggregation: Ethnicity; Deg/Transfer
df_di_summary %>%
  filter(Ed_Goal=='Deg/Transfer', College_Status=='- All', success_variable=='Math', disaggregation=='Ethnicity') %>%
  select(cohort, group, n, pct, di_indicator_ppg, di_indicator_prop_index, di_indicator_80_index) %>%
  as.data.frame

## ----include=FALSE-------------------------------------------------------
if (run_plots) {
  # Disaggregation: Ethnicity; Deg/Transfer
  df_di_summary %>%
    filter(Ed_Goal=='Deg/Transfer', College_Status=='- All', success_variable=='Math', disaggregation=='Ethnicity') %>%
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
    ggtitle('Dashboard drop-down selections:', subtitle=paste0("Ed Goal = 'Deg/Transfer' | College Status = '- All' | Outcome = 'Math' | Disaggregation = 'Ethnicity'"))
  ggsave('Dashboard_3.png', height=5, width=9)
}

## ------------------------------------------------------------------------
# Disaggregation: Gender; Deg/Transfer; English
df_di_summary %>%
  filter(Ed_Goal=='Deg/Transfer', College_Status=='- All', success_variable=='English', disaggregation=='Gender') %>%
  as.data.frame

## ----include=FALSE-------------------------------------------------------
if (run_plots) {
  # Disaggregation: Gender; Deg/Transfer; English
  df_di_summary %>%
    filter(Ed_Goal=='Deg/Transfer', College_Status=='- All', success_variable=='English', disaggregation=='Gender') %>%
    select(cohort, group, n, pct, di_indicator_ppg, di_indicator_prop_index, di_indicator_80_index) %>%
    mutate(group=factor(group) %>% fct_reorder(desc(pct))) %>% 
    ggplot(data=., mapping=aes(x=factor(cohort), y=pct, group=group, color=group)) +
    geom_point(aes(size=factor(di_indicator_ppg, levels=c(0, 1), labels=c('Not DI', 'DI')))) +
    geom_line() +
    xlab('Cohort') +
    ylab('Rate') +
    theme_bw() +
    scale_color_manual(values=c('#1b9e77', '#d95f02', '#7570b3', '#e7298a', '#66a61e', '#e6ab02'), name='Gender') +
    labs(size='Disproportionate Impact') +
    scale_y_continuous(labels = percent, limits=c(0, 1)) +
    ggtitle('Dashboard drop-down selections:', subtitle=paste0("Ed Goal = 'Deg/Transfer' | College Status = '- All' | Outcome = 'English' | Disaggregation = 'Gender'"))
  ggsave('Dashboard_4.png', height=5, width=9)
}

