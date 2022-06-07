## ---- message=FALSE, warning=FALSE--------------------------------------------
library(DisImpact)
library(dplyr) # Ease in manipulations with data frames

## -----------------------------------------------------------------------------
data(student_equity) # provided from DisImpact
dim(student_equity)
# head(student_equity)

## ----echo=FALSE, results='asis'-----------------------------------------------
library(knitr)
kable(student_equity[1:6, ], caption='A few rows from the `student_equity` data set.')

## ----warning=FALSE------------------------------------------------------------
df_di_summary <- di_iterate(data=student_equity
                          , success_vars=c('Math', 'English', 'Transfer')
                          , group_vars=c('Ethnicity', 'Gender')
                          , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                          , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                            )

## df_di_summary <- di_iterate(data=student_equity, success_vars=c('Math', 'English', 'Transfer'), group_vars=c('Ethnicity', 'Gender'), cohort_vars=c('Cohort', 'Cohort', 'Cohort'), scenario_repeat_by_vars=c('Ed_Goal', 'College_Status'))

## df_di_summary <- di_iterate(data=student_equity, success_vars=c('Math', 'English', 'Transfer'), group_vars=c('Ethnicity', 'Gender'), scenario_repeat_by_vars=c('Ed_Goal', 'College_Status'))

## df_di_summary_2 <- di_iterate(data=student_equity, success_vars=c('Math', 'English', 'Transfer'), group_vars=c('Ethnicity', 'Gender'), cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort'), scenario_repeat_by_vars=c('Ed_Goal', 'College_Status'), ppg_reference_groups=c('White', 'Male'), di_80_index_reference_groups=c('White', 'Male'))

## df_di_summary <- di_iterate(data=student_equity, success_vars=c('Math', 'English', 'Transfer'), group_vars=c('Ethnicity', 'Gender'), cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort'), scenario_repeat_by_vars=c('Ed_Goal', 'College_Status'), ppg_reference_groups=c('all but current'), di_80_index_reference_groups=c('White', 'Male'))

## -----------------------------------------------------------------------------
dim(df_di_summary)
df_di_summary %>% head %>% as.data.frame # first few rows


## -----------------------------------------------------------------------------
table(df_di_summary$Ed_Goal)
table(df_di_summary$College_Status)

## -----------------------------------------------------------------------------
table(df_di_summary$disaggregation)

## ----results=FALSE------------------------------------------------------------
# No Disaggregation
df_di_summary %>%
  filter(Ed_Goal=='- All', College_Status=='- All', disaggregation=='- None') %>%
  as.data.frame

## ----echo=FALSE, results='asis'-----------------------------------------------
df_di_summary %>%
  filter(Ed_Goal=='- All', College_Status=='- All', disaggregation=='- None') %>%
  as.data.frame %>% 
  kable

## -----------------------------------------------------------------------------
# No Disaggregation
df_di_summary %>%
  filter(Ed_Goal=='- All', College_Status=='- All', success_variable=='Math', disaggregation=='- None') %>%
  as.data.frame

## ---- fig.width=9, fig.height=5-----------------------------------------------
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

## -----------------------------------------------------------------------------
# Disaggregation: Ethnicity
df_di_summary %>%
  filter(Ed_Goal=='- All', College_Status=='- All', success_variable=='Math', disaggregation=='Ethnicity') %>%
  select(cohort, group, n, pct, di_indicator_ppg, di_indicator_prop_index, di_indicator_80_index) %>%
  as.data.frame

## ---- fig.width=9, fig.height=5-----------------------------------------------
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

## -----------------------------------------------------------------------------
# Disaggregation: Ethnicity; Deg/Transfer
df_di_summary %>%
  filter(Ed_Goal=='Deg/Transfer', College_Status=='- All', success_variable=='Math', disaggregation=='Ethnicity') %>%
  select(cohort, group, n, pct, di_indicator_ppg, di_indicator_prop_index, di_indicator_80_index) %>%
  as.data.frame

## ---- fig.width=9, fig.height=5-----------------------------------------------
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

## -----------------------------------------------------------------------------
# Disaggregation: Gender; Deg/Transfer; English
df_di_summary %>%
  filter(Ed_Goal=='Deg/Transfer', College_Status=='- All', success_variable=='English', disaggregation=='Gender') %>%
  as.data.frame

## ---- fig.width=9, fig.height=5-----------------------------------------------
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

## -----------------------------------------------------------------------------
args(di_iterate)

## -----------------------------------------------------------------------------
dim(student_equity)

## Example summarized data set
student_equity_summ <- student_equity %>%
  group_by(Ethnicity, Gender, Cohort, Cohort_Math, Cohort_English, Ed_Goal, College_Status) %>%
  summarize(N=n() %>% as.numeric # not needed, for all.equal()
            , Math=sum(Math, na.rm=TRUE)
            , English=sum(English, na.rm=TRUE)
            , Transfer=sum(Transfer, na.rm=TRUE)
            ) %>%
  ungroup

dim(student_equity_summ) # same number of columns, less number of rows

student_equity_summ %>% head %>% as.data.frame # first few rows

## Run on summarized data set
df_di_summary_2 <- di_iterate(data=student_equity_summ
                          , success_vars=c('Math', 'English', 'Transfer')
                          , group_vars=c('Ethnicity', 'Gender')
                          , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                          , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                          , weight_var='N' # SET THIS
                            )
dim(df_di_summary)  ## original results
dim(df_di_summary_2) # more rows?  because of NA cohort from Cohort_English and Cohort_Math
dim(df_di_summary_2 %>% filter(!is.na(cohort)))

## ## if user wants to see the extra rows
## extra_rows <- df_di_summary_2 %>%
##   anti_join(df_di_summary %>% select(Ed_Goal, College_Status, success_variable, cohort_variable, cohort, disaggregation, group))
## difference %>% head %>% as.data.frame  

all.equal(df_di_summary
        , df_di_summary_2 %>% filter(!is.na(cohort))
          ) # returned results are the same


## -----------------------------------------------------------------------------
df_di_summary_2 <- di_iterate(data=student_equity
                          , success_vars=c('Math', 'English', 'Transfer')
                          , group_vars=c('Ethnicity', 'Gender')
                          , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                          , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                          , include_non_disagg_results=FALSE ## SET THIS
                            )
dim(df_di_summary)
dim(df_di_summary_2) ## less rows because no longer have disaggregated results
table(df_di_summary$disaggregation)
table(df_di_summary_2$disaggregation) # No more '- None'

## -----------------------------------------------------------------------------
# Highest performing group as reference
df_di_summary_2 <- di_iterate(data=student_equity
                            , success_vars=c('Math', 'English', 'Transfer')
                            , group_vars=c('Ethnicity', 'Gender')
                            , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                            , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                            , ppg_reference_groups='hpg' ## SET THIS
                              )

# Reference: all other groups except group of interest (PPG minus 1)
df_di_summary_2 <- di_iterate(data=student_equity
                            , success_vars=c('Math', 'English', 'Transfer')
                            , group_vars=c('Ethnicity', 'Gender')
                            , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                            , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                            , ppg_reference_groups='all but current' ## SET THIS
                              )

# Reference: custom groups
df_di_summary_2 <- di_iterate(data=student_equity
                            , success_vars=c('Math', 'English', 'Transfer')
                            , group_vars=c('Ethnicity', 'Gender')
                            , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                            , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                            , ppg_reference_groups=c('White', 'Male') ## corresponds to each variable in group_vars
                              )

## -----------------------------------------------------------------------------
df_di_summary_2 <- di_iterate(data=student_equity
                            , success_vars=c('Math', 'English', 'Transfer')
                            , group_vars=c('Ethnicity', 'Gender')
                            , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                            , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                            , di_prop_index_cutoff=0.9 # Easier to declare DI using PI
                              )

## -----------------------------------------------------------------------------
# Custom reference groups
df_di_summary_2 <- di_iterate(data=student_equity
                            , success_vars=c('Math', 'English', 'Transfer')
                            , group_vars=c('Ethnicity', 'Gender')
                            , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                            , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                            , di_80_index_reference_groups=c('White', 'Male') ## corresponds to each variable in group_vars
                              )

## -----------------------------------------------------------------------------
df_di_summary_2 <- di_iterate(data=student_equity
                            , success_vars=c('Math', 'English', 'Transfer')
                            , group_vars=c('Ethnicity', 'Gender')
                            , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                            , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                            , di_80_index_cutoff=0.5 # Harder to declare DI using 80% index
                              )

## -----------------------------------------------------------------------------
# Multiple group variables and different reference groups
df_di_summary_long <- bind_rows(
  di_iterate(data=student_equity
           , success_vars=c('Math', 'English', 'Transfer')
           , group_vars=c('Ethnicity', 'Gender')
           , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
           , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
             )
  , di_iterate(data=student_equity
           , success_vars=c('Math', 'English', 'Transfer')
           , group_vars=c('Ethnicity', 'Gender')
           , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
           , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
           , ppg_reference_groups=c('White', 'Male') ## corresponds to each variable in group_vars
           , include_non_disagg_results = FALSE # Already have non-disaggregated results in the first run
             )
)

dim(df_di_summary_long)

## -----------------------------------------------------------------------------
## df_di_summary %>%
##   mutate(FERPA_Block=ifelse(n < 10, 1, 0)) %>%
##   filter(FERPA_Block == 0)

## -----------------------------------------------------------------------------
df_di_summary <- di_iterate(data=student_equity
                          , success_vars=c('Math', 'English', 'Transfer')
                          , group_vars=c('Ethnicity', 'Gender')
                          , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                          , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                            ) %>%
  suppressMessages

## ----eval=FALSE---------------------------------------------------------------
#  df_di_summary <- di_iterate(data=student_equity
#                            , success_vars=c('Math', 'English', 'Transfer')
#                            , group_vars=c('Ethnicity', 'Gender')
#                            , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
#                            , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
#                            , parallel=TRUE
#                            , parallel_n_cores=4
#                              )

## -----------------------------------------------------------------------------
# Create a very large student data set
n_college <- 200
student_equity_big <- do.call('rbind', replicate(n_college, student_equity, simplify=FALSE)) # repeat student_equity data set n_college times
student_equity_big$college <- rep(paste0('College ', 1:n_college), each=nrow(student_equity)) # College Name
dim(student_equity_big)
table(student_equity_big$college)

## ----eval=FALSE---------------------------------------------------------------
#  # Not run: user test on their own system
#  # User should try to increase n_college to 800 or another large number if the system has a lot of memory
#  # User should wrap following call with system.time() to time execution: parallel=FALSE, then parallel=TRUE, then parallel=TRUE and parallel_split_to_disk=TRUE
#  
#  # Parallel execution + writing to disk
#  big_di_summary <- di_iterate(data=student_equity_big
#                            , success_vars=c('Math', 'English', 'Transfer')
#                            , group_vars=c('Ethnicity', 'Gender')
#                            , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
#                            , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status', 'college') # Add college
#                            , parallel=TRUE
#                            # , parallel_n_cores=4 # when not specified, use the max number of cores
#                            , parallel_split_to_disk=TRUE
#                              )
#  

## -----------------------------------------------------------------------------
sessionInfo()

