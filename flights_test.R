library(tidyr)
library(dplyr)
library(nycflights13)

data(weather)
#month - day - hour
#find avg temp (min temp, max temp) by day, month, and year

#Unique identifier: origin
#Time identifiers: year, month, day, hour

#Would need a function to work with an arbitrary number of observation identifiers (eg, origin), 
# hierarchical identifiers (eg, year, month, day, hour), and
# functions to apply, possibly with arguments

#Direct approach: group and mutate
df <- weather %>%
  group_by(origin, year, month, day) %>% 
  mutate(day_avg_temp = mean(temp),
         day_min_temp = min(temp),
         day_max_temp = max(temp)) %>% 
  group_by(origin, year, month) %>%
  mutate(month_avg_temp = mean(temp),
         month_min_temp = min(temp),
         month_max_temp = max(temp)) %>%
  group_by(origin, year) %>%
  mutate(year_avg_temp = mean(temp),
         year_min_temp = min(temp),
         year_max_temp = max(temp)) %>%
  ungroup()
  

##Approach 2: create separate dataframes and join
df_day <- weather %>%
  select(origin, year, month, day, temp) %>%
  group_by(origin, year, month, day) %>%
  summarize(mean_temp = mean(temp),
            min_temp = min(temp),
            max_temp = max(temp))

df_month <- weather %>%
  select(origin, year, month, day, temp) %>%
  group_by(origin, year, month) %>%
  summarize(mean_temp = mean(temp),
            min_temp = min(temp),
            max_temp = max(temp))

df_year <- weather %>%
  select(origin, year, month, day, temp) %>%
  group_by(origin, year) %>%
  summarize(mean_temp = mean(temp, na.rm = TRUE),
            min_temp = min(temp, na.rm = TRUE),
            max_temp = max(temp, na.rm = TRUE))

dfweather <- weather %>%
  left_join(df_day, by = c("origin", "year", "month", "day"), suffix = c("", ".day")) %>%
  left_join(df_month, by = c("origin", "year", "month"), suffix = c(".day", ".month")) %>%
  left_join(df_year, by = c("origin", "year"), suffix = c(".month", ".year"))