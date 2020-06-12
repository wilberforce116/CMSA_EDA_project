library(tidyverse)

# Read in the file
cricket_data <- read_csv("http://www.stat.cmu.edu/cmsac/sure/materials/data/eda_projects/womens_cricket_bowling.csv")


cricket_cleaned <- cricket_data %>% 
  filter(player != "CR Seneviratna (SL-W/UAE-W)") %>%           # Remove player who has caps for more than one nation (Sahana, thoughts on what to do with this observation?)
  select(-"X1") %>% 
  mutate(player = gsub(' \\(\\)$', '', player), # Remove trailing parentheses
         country = fct_recode(country,                          # Replace country abbreviations
                              Thailand = "THI",
                              Indonesia = "INA",
                              China = "CHN",
                              Malawi = "MLW",
                              Uganda = "UGA",
                              Nepal = "NEP",
                              Mozambique = "MOZ",
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
                              Samoa = "South AfricaMwn",
                              "Papua New Guinea" = "Papau New Guinea",
                              USA = "United States of America"),
         country = fct_explicit_na(country,                   # Remaining missing values are from the Qatar national team
                                   na_level = "Qatar"),
         region = case_when(country %in% c("Argentina", "Belize", "Brazil", "Canada", "Chile", "Costa Rica", "Mexico", "Peru", "United States of America", "West Indies") ~ "Americas",
                            country %in% c("South Africa", "Zimbabwe", "Namibia", "Botswana", "Kenya", "Malawi", "Mali", "Mozambique", "Nigeria", "Rwanda", "Sierra Leone", "Tanzania", "Uganda") ~ "Africa",
                            country %in% c("India", "Pakistan", "Sri Lanka", "Hong Kong", "Bangladesh", "Bhutan", "China", "Kuwait", "Malaysia", "Maldives", "Myanmar", "Nepal", "Oman", "Qatar", "Singapore", "Thailand", "United Arab Emirates") ~ "Asia",
                            country %in% c("Australia", "New Zealand", "Papau New Guinea", "Fiji", "Indonesia", "Japan", "Philippines", "Samoa", "Korea", "Vanuatu") ~ "East Asia - Pacific",
                            country %in% c("England", "Ireland", "Scotland", "Netherlands", "France", "Jersey", "Germany", "Austria", "Norway", "Guernsey") ~ "Europe"),
         maiden_ratio = maidens/overs,
         usage_rate = overs/innings,
         years_active = end - start + 1) %>% 
  select(player, country, region, everything())
