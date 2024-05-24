#----------------------------------Data Preparation----------------------------#
#-Author: Francisca Castro ----------------------------- Created: May 07, 2024-#
#-R Version: 4.4.0 ------------------------------------- Revised: May 23, 2024-#

# 0) Load Packages

pacman::p_load(magrittr, dplyr, readxl, tidyr)

# 1) Load data

#- The first data to be loaded is the VDem data. This data will be used to 
#- identify which countries have gone through democcratic transitions or 
#- autocratization processes since 1970.

# 1.1) Load VDem data

vdem_v14 <- readRDS("00-data/raw-data/vdem_14/vdem_v14.rds")

vdem_v14 %<>%
  filter(year >= 1970, e_p_polity >= -10) # filter by year and NAs for polity combined score

# 2) Identify transitional countries

#- Considering the e_p_polity variable, we identify those countries that present
#- significant differences in time in this index. 

#- Calculate the standard deviation of e_p_polity for each country

country_changes <- vdem_v14 %>%
  group_by(country_name) %>%
  summarize(std_dev_polity = sd(e_p_polity, na.rm = TRUE),  # Calculate standard deviation, ignoring NA values
            mean_polity = mean(e_p_polity, na.rm = TRUE),   # Mean for additional context
            n = n(),  # Count of observations per country
            transition = if_else(std_dev_polity > 2, 1, 0)) %>%  # Create a binary variable for significant transitions
  ungroup() 

#- 95 countries have gone through some sort of transition
#- Now we have to identify transition periods in the full dataset

#- Calculate the standard deviation of e_p_polity for each country
country_std_dev <- vdem_v14 %>%
  group_by(country_name) %>%
  summarize(std_dev_polity = sd(e_p_polity, na.rm = TRUE), .groups = 'drop')

#- Calculate year-to-year changes in e_p_polity
transitional_countries <- vdem_v14 %>%
  arrange(country_name, year) %>%
  group_by(country_name) %>%
  mutate(
    previous_polity = lag(e_p_polity),  # Get the previous year's e_p_polity
    change_in_polity = e_p_polity - lag(e_p_polity),
    std_dev_polity = sd(e_p_polity, na.rm = TRUE),
    # Democratization occurs if the previous year was negative and the current year is positive
    democratization = if_else(previous_polity < 0 & e_p_polity > 0, 1, 0),
    # Autocratization occurs if the previous year was positive and the current year is negative
    autocratization = if_else(previous_polity > 0 & e_p_polity < 0, 1, 0)
  ) %>%
  ungroup() %>%
  select(country_name, year, e_p_polity, std_dev_polity, change_in_polity, democratization, autocratization)


#- Remove countries that have never gone through democratization process
transitional_countries_filtered <- transitional_countries %>%
  group_by(country_name) %>%
  # Sum the transition flags across years for each country
  mutate(total_democratization = sum(democratization, na.rm = TRUE),
         total_autocratization = sum(autocratization, na.rm = TRUE)) %>%
  ungroup() %>%
  # Keep only countries with at least one democratization or autocratization event
  filter(total_democratization > 0 | total_autocratization > 0) %>%
  # Optionally, select only the necessary columns to remove the total columns added for filtering
  select(-total_democratization, -total_autocratization)


# 3) Include TJ mechanisms

#- Include data from Olsen, Payne & Ritter (2010)

authoritarian_tj <- read_excel("00-data/raw-data/replication_olsen_et_al.xlsx", sheet = 4)
civil_wars_tj <- read_excel("00-data/raw-data/replication_olsen_et_al.xlsx", sheet = 6)

authoritarian_tj <- rename(authoritarian_tj, year = Year, country_name = Country)
civil_wars_tj <- rename(civil_wars_tj, year = Year, country_name = Country)

authoritarian_tj %<>%
  rename_all(tolower)

civil_wars_tj %<>%
  rename_all(tolower)

#- Merge the datasets
#- Merge transitional_countries_filtered with authoritarian_tj
tj_countries_authoritarian <- left_join(transitional_countries_filtered, authoritarian_tj, by = c("country_name", "year"))
names(tj_countries_authoritarian)

# Merge the result with civil_wars_tv
tj_countries_civil_war <- left_join(transitional_countries_filtered, civil_wars_tj, by = c("country_name", "year"))
names(tj_countries_civil_war)

tj_countries_full <- tj_countries_authoritarian %>%
    full_join(tj_countries_civil_war, by = c("country_name", "year", "e_p_polity", "std_dev_polity", "change_in_polity", "democratization", "autocratization")) %>%
    mutate(
      transition_year = `transition year`,
      trial = ifelse(is.na(trial.x) & !is.na(trial.y), trial.y, ifelse(is.na(trial.y), trial.x, pmax(trial.x, trial.y, na.rm = TRUE))),
      tc = ifelse(is.na(tc.x) & !is.na(tc.y), tc.y, ifelse(is.na(tc.y), tc.x, pmax(tc.x, tc.y, na.rm = TRUE))),
      amnesty = ifelse(is.na(amnesty.x) & !is.na(amnesty.y), amnesty.y, ifelse(is.na(amnesty.y), amnesty.x, pmax(amnesty.x, amnesty.y, na.rm = TRUE))),
      reparation = ifelse(is.na(reparation.x) & !is.na(reparation.y), reparation.y, ifelse(is.na(reparation.y), reparation.x, pmax(reparation.x, reparation.y, na.rm = TRUE))),
      lustration = ifelse(is.na(lustration.x) & !is.na(lustration.y), lustration.y, ifelse(is.na(lustration.y), lustration.x, pmax(lustration.x, lustration.y, na.rm = TRUE)))
    ) %>%
    select(country_name, year, e_p_polity, std_dev_polity, change_in_polity, democratization, autocratization, transition_year, trial, tc, amnesty, reparation, lustration) 


#- Replace NAs with 0 (no transitional justice mechanisms took place)
tj_countries_full %<>%
  mutate(
    transition_year = replace_na(transition_year, 0),
    trial = replace_na(trial, 0),
    tc = replace_na(tc, 0),
    amnesty = replace_na(amnesty, 0),
    reparation = replace_na(reparation, 0),
    lustration = replace_na(lustration, 0))
  
names(tj_countries_full)

#- Add PA-X database

pax_data <- read_excel("00-data/raw-data/pax_data.xlsx")

#- Aggregate pax_data to ensure single row per country-year combination
pax_data_aggregated <- pax_data %>%
  group_by(country, Year) %>%
  summarize(
    amnesty = max(amnesty, na.rm = TRUE),
    trial = max(trial, na.rm = TRUE),
    tc = max(tc, na.rm = TRUE),
    lustration = max(lustration, na.rm = TRUE),
    reparation = max(reparation, na.rm = TRUE),
    .groups = 'drop')

pax_data_transformed <- pax_data_aggregated %>%
  mutate(
    amnesty = if_else(amnesty > 0, 1, 0),
    trial = if_else(trial > 0, 1, 0),
    tc = if_else(tc > 0, 1, 0),
    lustration = if_else(lustration > 0, 1, 0),
    reparation = if_else(reparation > 0, 1, 0)
  ) %>%
  select(country, year = Year, amnesty, trial, tc, lustration, reparation)

#- Merge with merged_tj_countries
tj_countries_full %<>%
  left_join(pax_data_transformed, by = c("country_name" = "country", "year" = "year")) %>%
  mutate(
    amnesty = if_else(!is.na(amnesty.y) & (amnesty.x == 0 | is.na(amnesty.x)), 1, coalesce(amnesty.x, amnesty.y)),
    trial = if_else(!is.na(trial.y) & (trial.x == 0 | is.na(trial.x)), 1, coalesce(trial.x, trial.y)),
    tc = if_else(!is.na(tc.y) & (tc.x == 0 | is.na(tc.x)), 1, coalesce(tc.x, tc.y)),
    lustration = if_else(!is.na(lustration.y) & (lustration.x == 0 | is.na(lustration.x)), 1, coalesce(lustration.x, lustration.y)),
    reparation = if_else(!is.na(reparation.y) & (reparation.x == 0 | is.na(reparation.x)), 1, coalesce(reparation.x, reparation.y))
  ) %>%
  select(country_name, year, e_p_polity, std_dev_polity, change_in_polity, democratization, autocratization, transition_year, trial, tc, amnesty, reparation, lustration)
