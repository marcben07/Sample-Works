library(tidysynth)
library(tidyverse)
library(readxl)

state_income_raw = read_excel('Median_Income.xls')
pps_raw = read_excel('Spending_per_pupil.xlsx')
raw = read.csv('school_scores.csv')
raw = as_tibble(raw)


# Data Cleaning -----------------------------------------------------------
state_income = state_income_raw %>% 
  pivot_longer(cols=-1, names_to = 'State', values_to = 'median') %>% 
  mutate(Year = as.integer(Year))

pps = pps_raw %>% 
  pivot_longer(cols = -1, names_to = 'Year', values_to = 'PPS') %>% 
  select(Year, State, PPS) %>%  arrange(State, Year) %>% 
  mutate(Year = as.integer(Year))

control_states = c('OR','CA','NV','UT', 'AZ', 'CO', 'NM', 'KS', 'SD', 'WV', 'MO', 'AR', 'LA',
                   'MS', 'AL', 'GA', 'FL', 'TN', 'IA', 'IL', 'WI', 'MI', 'OH', 'KY', 'NC', 'NY', 'PA', 
                   'VT', 'NH', 'MA', 'RI', 'CT', 'NJ', 'DE', 'MD', 'HI')

treat_state = 'VA'

data_school_math = raw %>% 
  select(Year:State.Name, Academic.Subjects.Mathematics.Average.GPA, Gender.Male.Test.takers, Gender.Female.Test.takers) %>% 
  left_join(state_income, by = c('State.Name' = 'State', 'Year'))%>% 
  left_join(pps, by = c('State.Name' = 'State', 'Year')) %>% 
  filter(State.Code %in% control_states | State.Code == treat_state) %>% 
  filter(Year >= 2007)

data_school_english = raw %>% 
  select(Year:State.Name, Academic.Subjects.English.Average.GPA, Gender.Male.Test.takers, Gender.Female.Test.takers) %>% 
  left_join(state_income, by = c('State.Name' = 'State', 'Year'))%>% 
  left_join(pps, by = c('State.Name' = 'State', 'Year')) %>% 
  filter(State.Code %in% control_states | State.Code == treat_state) %>% 
  filter(Year >= 2007)
#filter(Year) to remove NA values in 2005, 2006

# synthetic control math -------------------------------------------------------

gpa_synth_math = data_school_math %>% 
  synthetic_control(outcome = Academic.Subjects.Mathematics.Average.GPA, unit = State.Code,
                    time = Year,
                    i_unit = treat_state,
                    i_time = 2010,
                    generate_placebos= TRUE)%>% 
  generate_predictor(time_window=2007:2010,
                     median = mean(median, na.rm = TRUE),
                     Gender.Male.Test.takers = mean(Gender.Male.Test.takers, na.rm = TRUE),
                     Gender.Female.Test.takers = mean(Gender.Female.Test.takers, na.rm = TRUE),
                     PPS = mean(PPS, na.rm = TRUE))%>% 
  generate_predictor(time_window = 2007,
                     gpa_2007 = mean(Academic.Subjects.Mathematics.Average.GPA, na.rm = TRUE)) %>%
  generate_predictor(time_window = 2008,
                     gpa_2008 = mean(Academic.Subjects.Mathematics.Average.GPA, na.rm = TRUE)) %>% 
  generate_predictor(time_window = 2009,
                     gpa_2009 = mean(Academic.Subjects.Mathematics.Average.GPA, na.rm = TRUE)) %>%
  generate_predictor(time_window = 2010, 
                     gpa_2010 = mean(Academic.Subjects.Mathematics.Average.GPA, na.rm = TRUE)) %>% 
  generate_weights(optimization_window =2007:2010,
                   Margin.ipop=.02,Sigf.ipop=7,Bound.ipop=6) %>% 
  generate_control()

#Results and weight
gpa_synth_math %>% plot_trends(2007:2015)
gpa_synth_math %>% grab_balance_table()
gpa_synth_math %>% grab_predictor_weights() %>% arrange(desc(weight))
gpa_synth_math %>% grab_unit_weights() %>% arrange(desc(weight))
gpa_synth_math %>% plot_weights()

#Inference, MSPE
gpa_synth_math %>% plot_placebos(time_window = 2007:2015)
gpa_synth_math %>% plot_mspe_ratio()

gpa_synth_math %>% grab_signficance(time_window = 2007:2015)
gpa_synth_math %>% grab_loss() #loss of MSPE due to optimization values


# synthetic control english -----------------------------------------------

gpa_synth_english = data_school_english %>% 
  synthetic_control(outcome = Academic.Subjects.English.Average.GPA, unit = State.Code,
                    time = Year,
                    i_unit = treat_state,
                    i_time = 2010,
                    generate_placebos= TRUE)%>%
  generate_predictor(time_window=2007:2010,
                     median = mean(median, na.rm = TRUE),
                     Gender.Male.Test.takers = mean(Gender.Male.Test.takers, na.rm = TRUE),
                     Gender.Female.Test.takers = mean(Gender.Female.Test.takers, na.rm = TRUE),
                     PPS = mean(PPS, na.rm = TRUE))%>% 
  generate_predictor(time_window = 2007,
                     gpa_2007 = mean(Academic.Subjects.English.Average.GPA, na.rm = TRUE)) %>%
  generate_predictor(time_window = 2008,
                     gpa_2008 = mean(Academic.Subjects.English.Average.GPA, na.rm = TRUE)) %>% 
  generate_predictor(time_window = 2009,
                     gpa_2009 = mean(Academic.Subjects.English.Average.GPA, na.rm = TRUE)) %>% 
  generate_predictor(time_window = 2010, 
                     gpa_2010 = mean(Academic.Subjects.English.Average.GPA, na.rm = TRUE)) %>% 
  generate_weights(optimization_window =2007:2010,
                   Margin.ipop=.02,Sigf.ipop=7,Bound.ipop=6) %>% 
  generate_control()

#Results and weights
gpa_synth_english %>% plot_trends(2007:2015)
gpa_synth_english %>% grab_balance_table()
gpa_synth_english %>% grab_predictor_weights() %>% arrange(desc(weight))
gpa_synth_english %>% grab_unit_weights() %>% arrange(desc(weight))
gpa_synth_english %>% plot_weights()

#Inference, MSPE
gpa_synth_english %>% plot_placebos(time_window = 2007:2015) 
gpa_synth_english %>% grab_signficance(time_window = 2007:2015)
gpa_synth_english %>% grab_loss()
gpa_synth_english %>% plot_mspe_ratio()




#NOTE: change generate_placebos = FALSE for results and weights only, for faster computing
