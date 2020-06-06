library(tidyverse)

# Read in the file
cricket_data <- read_csv("http://www.stat.cmu.edu/cmsac/sure/materials/data/eda_projects/womens_cricket_bowling.csv")


cricket_cleaned <- cricket_data %>% 
  filter(player != "CR Seneviratna (SL-W/UAE-W)") %>%           # Remove player who has caps for more than one nation (Sahana, thoughts on what to do with this observation?)
  select(-"X1") %>% 
  mutate(player = gsub(' \\(\\)$', '', cricket_cleaned$player), # Remove trailing parentheses
         country = fct_recode(country,                          # Replace country abbreviations
                              Thailand = "THI",
                              Indonesia = "INA",
                              China = "CHN",
                              Malawi = "MLW",
                              Uganda = "UGA",
                              Nepal = "NEP",
                              Mozabique = "MOZ",
                              Botswana = "BOT",
                              Vanuatu = "VAN",
                              Brazil = "BRA",
                              Rwanda = "RWA",
                              Myanmar = "MYA",
                              Nigeria = "NGA",
                              Chile = "CHI",
                              Kenya = "KEN",
                              Tanzania = "TZN",
                              Malaysia = "MAL",
                              Korea = "KOR",
                              Singapore = "SIN",
                              Belize = "BLZ",
                              France = "FRA",
                              Mexico = "MEX",
                              Oman = "OMA",
                              Jersey = "JEY",
                              "Sierra Leone" = "Sri LankaE",
                              Kuwait = "KUW",
                              Germany = "GER",
                              Peru = "PER",
                              Argentina = "ARG",
                              Austria = "AUT",
                              Fiji = "FJI",
                              Norway = "NOR",
                              "Costa Rica" = "CRC",
                              Bhutan = "BHU",
                              Philippines = "PHI",
                              Mali = "MLI",
                              Guernsey = "GUN",
                              Maldives = "MDV",
                              Samoa = "South AfricaMwn"))

