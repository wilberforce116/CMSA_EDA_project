library(tidyverse)
library(patchwork)
library(maps)
source("clean_data.R")

country_data <- cricket_cleaned %>% 
  group_by(region, country) %>% 
  na.omit() %>% 
  summarise(num_players = n(),
            initial_year = min(start),
            team_innings = sum(innings),
            team_wickets = sum(wickets),
            team_maiden_ratio = mean(maiden_ratio),
            team_avg = mean(average[is.finite(average)]),
            team_economy = mean(economy),
            team_strike_rate = mean(strike_rate[is.finite(strike_rate)]))


### We would expect players with more experience to perform better. Does the evidence bear this out?
### Hypothesis: 

by_years_active <- cricket_cleaned %>% 
  na.omit() %>% 
  mutate(years_active = end - start + 1) %>% 
  group_by(years_active) %>% 
  summarise(mean_avg = mean(strike_rate[is.finite(strike_rate)]),
            mean_economy = mean(economy[is.finite(economy)]),
            mean_strike_rate = mean(strike_rate[is.finite(strike_rate)]))

plot_econ <- by_years_active %>% 
  ggplot(aes(years_active, mean_economy)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, col = "gold") +
  xlab("Years Active") +
  ylab("Economy Rate") +
  theme_bw()

plot_avg <- by_years_active %>% 
  ggplot(aes(years_active, mean_avg)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, col = "gold") +
  xlab("Years Active") +
  ylab("Bowling Average") +
  theme_bw()

plot_sr <- by_years_active %>% 
  ggplot(aes(years_active, mean_strike_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, col = "gold") +
  xlab("Years Active") +
  ylab("Strike Rate") +
  theme_bw()

(plot_econ + plot_avg) / plot_sr +
  plot_layout(heights = c(1,2)) +
  plot_annotation(title = "Years of Experience vs Bowling Success",
                  subtitle = "All plotted values are conditional means")

### According to Wikipedia, an economy rate below seven is considered to be good in T20 cricket.
### Is this a reasonable benchmark value for women's T20 international cricket?
### Hypothesis: 

cricket_cleaned %>% 
  na.omit() %>% 
  ggplot(aes(economy)) +
  stat_ecdf() +
  geom_vline(xintercept = 7, linetype = "dashed", color = "red") +
  labs(title = "ECDF of Economy Rate") +
  xlab("Economy") +
  ylab("F(x)") +
  theme_bw()

cricket_cleaned %>% 
  na.omit() %>% 
  ggplot(aes(economy)) +
  geom_histogram() +
  geom_density() +
  geom_vline(xintercept = 7, linetype = "dashed", color = "red") +
  theme_bw()

cricket_cleaned %>% 
  na.omit() %>% 
  mutate(under_7 = case_when(economy > 7 ~ "Over",
                            economy <= 7 ~ "Under")) %>% 
  group_by(under_7) %>% 
  summarise_all(mean) %>% 
  select(under_7, usage_rate, matches, overs, maidens, runs, wickets)




### Maps
world_map <- map_data("world")

left_join(world_map, country_data, by = c("region" = "country")) %>% 
  ggplot() +
  geom_polygon(aes(long, lat, group = group, fill = team_avg)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 100) +
  theme_bw()

left_join(world_map, country_data, by = c("region" = "country")) %>% 
  ggplot() +
  geom_polygon(aes(long, lat, group = group, fill = team_economy)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 9) +
  theme_bw() +
  labs(fill = "Average Career Economy",
       title = "Economy by country")

left_join(world_map, country_data, by = c("region" = "country")) %>% 
  ggplot() +
  geom_polygon(aes(long, lat, group = group, fill = team_strike_rate)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 80) +
  theme_bw()



youth_country_data <- cricket_cleaned %>% 
  filter(start > 2017) %>% 
  group_by(region, country) %>% 
  na.omit() %>% 
  summarise(num_players = n(),
            initial_year = min(start),
            team_innings = sum(innings),
            team_wickets = sum(wickets),
            team_maiden_ratio = mean(maiden_ratio),
            team_avg = mean(average),
            team_economy = mean(economy),
            team_strike_rate = mean(strike_rate))


left_join(world_map, youth_country_data, by = c("region" = "country")) %>% View()
  ggplot() +
  geom_polygon(aes(long, lat, group = group, fill = team_economy)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = sum(range(youth_country_data$team_economy))/2) +
  theme_bw() +
  labs(fill = "Average Career Economy",
       title = "Economy by country")
  
### Scratch Paper
cricket_cleaned %>% 
  na.omit() %>% 
  filter(is.finite(strike_rate)) %>% 
  ggplot(aes(x = usage_rate, y = strike_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  theme_bw()

ggplot((country_data %>% filter(team_avg < 100))) +
  geom_point(aes(x = initial_year, y = team_strike_rate)) +
  theme_bw()

ggplot((country_data %>% filter(team_avg < 100))) +
  geom_histogram(aes(team_strike_rate), bins = 20)+
  theme_bw()


ggplot((country_data %>% filter(team_avg < 150)), aes(team_economy, team_strike_rate))+
  geom_point()+
  geom_smooth(method = "lm", se = F) +
  theme_bw()

ggplot(cricket_cleaned)+
  geom_histogram(aes(average, fill = region)) +
  theme_bw()


ggplot(country_data)+
  geom_jitter(aes(x = 1, y = initial_year, col = region), width = 0)


cricket_cleaned %>% 
  na.omit() %>% 
  ggplot(aes(economy)) +
  geom_histogram(aes(y = ..density..)) + 
  geom_density() +
  geom_rug() +
  theme_bw()


### K-Means Clustering

cricket_scaled <- cricket_cleaned %>% 
  na.omit() %>% 
  mutate(years_active = end - start + 1) %>% 
  select_if(is.numeric) %>% 
  select(-c(start, end)) %>% 
  scale()

kmeans()
