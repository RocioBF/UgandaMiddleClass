# In this script the varibales as used in the main regression of the paper are created.


# SHOCKS:

# Creating a data frame with variables that denotes the shocks:
Health_shock <- Shocks_2013_original %>%
  #Restrict sample to those shocks affecting the health of any member of the hh
  filter(Shock_code %in% 110:111) %>%
  #Group by HHID
  group_by(HHID) %>%
  #Creat a dummy = 1 if the hh suffered a shock last year
  mutate(health_shock = ifelse(shock_lastyear == 1, 1, 0)) %>%
  #Select the important variables
  dplyr::select(HHID, health_shock) %>%
  #Those variables contains two health shocks variables: for the HoH and rest of members.
  #I want to capture if the hh suffered the shock in general.
  #It is possible both(none) (HoH and the rest of members) suffered a shock
  #(then I will have two times health_shock = 1(0) for the same hh),
  #then I delete the repeated ones.
  unique() %>%
  #It could be that the HoH suffered a shock and the rest did not (or viceverse),
  #so we can have for the same HHID health_shock = 1 and health_shock = 0.
  #For me this dummy should only take the value 1, that is why I sum the two rows obtaining a 1
  #in the dummy and only one observation per household
  summarise(health_shock = sum(health_shock))

# The same approach, as described above, is used to calculate the rest of the shocks.
Death_shock <- Shocks_2013_original %>%
  group_by(HHID) %>%
  filter(Shock_code %in% 112:113) %>%
  mutate(death_shock = ifelse(shock_lastyear == 1, 1, 0)) %>%
  dplyr::select(HHID, death_shock) %>%
  unique() %>%
  summarise(death_shock = sum(death_shock))

Climate_shock <- Shocks_2013_original %>%
  group_by(HHID) %>%
  filter(Shock_code == 102 |
           Shock_code %in% 1011:1032) %>% #Take a look
  mutate(climate_shock_1 = ifelse(shock_lastyear == 1, 1, 0)) %>%
  dplyr::select(HHID, climate_shock_1) %>%
  unique() %>%
  summarise(climate_shock = sum(climate_shock_1))

Dweling_shock <- Shocks_2013_original %>%
  group_by(HHID) %>%
  filter(Shock_code %in% 114:117) %>%
  mutate(dweling_shock = ifelse(shock_lastyear == 1, 1, 0)) %>%
  dplyr::select(HHID, dweling_shock) %>%
  unique() %>%
  summarise(dweling_shock = sum(dweling_shock))

Economic_shock <- Shocks_2013_original %>%
  group_by(HHID) %>%
  filter(Shock_code %in% 106:109) %>%
  mutate(economic_shock = ifelse(shock_lastyear == 1, 1, 0)) %>%
  dplyr::select(HHID, economic_shock) %>%
  unique() %>%
  summarise(economic_shock = sum(economic_shock))

Agricultural_shock <- Shocks_2013_original %>%
  group_by(HHID) %>%
  filter(Shock_code %in% 104:105) %>%
  mutate(agricultural_shock_1 = ifelse(shock_lastyear == 1, 1, 0)) %>%
  dplyr::select(HHID, agricultural_shock_1) %>%
  unique() %>%
  summarise(agricultural_shock = sum(agricultural_shock_1))

Other_shock <- Shocks_2013_original %>%
  group_by(HHID) %>%
  filter(Shock_code == 118) %>%
  mutate(other_shock = ifelse(shock_lastyear == 1, 1, 0)) %>%
  dplyr::select(HHID, other_shock) %>%
  unique() %>%
  summarise(other_shock = sum(other_shock))


shocks_13 <-
  list(
    Health_shock,
    Death_shock,
    Climate_shock,
    Dweling_shock,
    Economic_shock,
    Agricultural_shock,
    Other_shock
  ) %>%
  reduce(inner_join, by = "HHID")

#Joinning the data base we had with the shocks variables:
Main_Df <- left_join(Main_Df, shocks_13, by = "HHID")

rm(Shocks_2013_original, shocks_13)

#Change in number of workers and change in number of persons living in hh:
Main_Df$change_n_resident <- Main_Df$hsize_13 - Main_Df$hsize_11
Main_Df$change_n_working <-
  Main_Df$n_working_13 - Main_Df$n_working_11


#logaritm of expenditure:
Main_Df$ln_expenditure_11 <- log(Main_Df$welfare_11)
Main_Df$ln_expenditure_13 <- log(Main_Df$welfare_13)

# Clear namespace of varibles which are not used in future analysis steps.
# To see intermediate results change the code below accordingly.
rm(
  Agricultural_shock,
  Bicycle,
  Climate_shock,
  Death_shock,
  Dweling_shock,
  HH_appliances,
  hh_size,
  mobile_phone,
  ownership,
  TV,
  Vehicle,
  Economic_shock,
  Health_shock,
  Motorcycle,
  Other_shock,
  Radio
)
