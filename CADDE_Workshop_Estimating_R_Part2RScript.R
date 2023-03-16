# Load required libraries
library(EpiEstim); library(ggplot2); library(here)

# Loading incidence data and estimating Rt using default weekly sliding windows
covid_case_incidence <- readRDS("UK_covid_incidence.rds") # load COVID-19 incidence
output_weekly <- estimate_R(incid = covid_case_incidence,       # incidence dataframe
                            method = "parametric_si",          
                            config = make_config(list(
                               mean_si = 6.3,                   # mean of the serial interval
                               std_si = 4.2)))                  # sd of the serial interval
plot(output_weekly)

# Estimating Rt using single day window
t_start_daily <- seq(2, nrow(covid_case_incidence)) 
t_end_daily <- t_start_daily 
output_daily <- EpiEstim::estimate_R(incid = covid_case_incidence, # incidence dataframe
                                     method = "parametric_si",    
                                     config = make_config(list(
                                       mean_si = 6.3,              # mean of the serial interval
                                       std_si = 4.2,               # sd of the serial interval
                                       t_start = t_start_daily,    # daily sliding window start time
                                       t_end = t_end_daily)))      # daily sliding window end time
plot(output_daily)

# Comparing and plotting Rt estimates for weekly vs daily
dates_weekly_plotting <- covid_case_incidence$dates[output_weekly$R$t_end]
Rt_weekly_df <- data.frame(Rt = output_weekly$R$`Mean(R)`, window_size = "weekly",
                           date = dates_weekly_plotting)
dates_daily_plotting <- covid_case_incidence$dates[output_daily$R$t_end]
Rt_daily_df <- data.frame(Rt = output_daily$R$`Mean(R)`, window_size = "daily",
                          date = covid_case_incidence$dates[output_daily$R$t_end])
Rt_df <- rbind(Rt_weekly_df, Rt_daily_df)
ggplot(Rt_df, aes(x = date, y = Rt, col = window_size)) +
  geom_line()

# Estimating Rt to explore impact of national lockdown on 25th March
lockdown_date_index <- which(covid_case_incidence$dates == as.Date("2020-03-25"))
t_start_npi <- c(2, lockdown_date_index)
t_end_npi <- c(lockdown_date_index - 1, nrow(covid_case_incidence))
output_npi <- EpiEstim::estimate_R(incid = covid_case_incidence,  # incidence dataframe
                                   method = "parametric_si",      
                                   config = make_config(list(
                                     mean_si = 6.3,               # mean of the serial interval
                                     std_si = 4.2,                # sd of the serial interval
                                     t_start = t_start_npi,       # time-window start times
                                     t_end = t_end_npi)))         # time-window end times
plot(output_npi)
