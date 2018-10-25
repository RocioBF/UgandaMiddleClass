# Table: Characteristics of hh in 2011

## Eduation of the head of the household:
Table_educ_2011 <- regression_df_clean %>%
  summarize(
    no_schooling = mean(education_level_11 == 0),
    some = mean(education_level_11 == 1),
    primary = mean(education_level_11 == 2),
    Junior = mean(education_level_11 == 3),
    secondary = mean(education_level_11 == 4),
    post_primary = mean(education_level_11 == 5),
    post_secondary = mean(education_level_11 == 6),
    university = mean(education_level_11 == 7)
  )

# Sector of labor activity of the head household:
Table_sector_2011 <- regression_df_clean %>%
  summarize(
    primary = mean(sector_11 == 1),
    secondary = mean(sector_11 == 2),
    services = mean(sector_11 == 3)
  )

# Marital status of the head of the household: 
Table_marital_status_2011 <- regression_df_clean %>%
  summarize(
    monogamy = mean(marital_status_11 == 1),
    polygamy = mean(marital_status_11 == 2),
    single = mean(marital_status_11 == 3),
    widowed = mean(marital_status_11 == 4),
    divorced = mean(marital_status_11 == 5)
  )


# Other characteristics of the head of the household:
Table_other_Hoh_char_2011 <- regression_df_clean %>%
  summarize(age = mean(age_11),
            male = mean(male_11 == 1))

# Dwelling characteristics of the household
# (sanitation and assets hold):
Table_dwelling_2011 <- regression_df_clean %>%
  summarize(
    urban = mean(urban_11 == 1),
    n_residents = mean(hsize_11),
    indep = mean(indep_hh_11 == 1),
    own_residence = mean(ownership_house_11 == 1),
    rooms = mean(rooms_11),
    electricity = mean(electricity_11 == 1),
    water = mean(piped_water_11 == 1),
    flush_toilet = mean(flush_toilet_11 == 1),
    hand_wash = mean(hand_washing_11 == 1),
    hh_appliances = mean(ownership_hh_appliances_11 == 1),
    tv = mean(ownership_TV_11 == 1),
    radio = mean(ownership_radio_11 == 1),
    bycicle = mean(ownership_bicycle_11 == 1),
    motorbike = mean(ownership_motorcycle_11 == 1),
    vehicle = mean(ownership_vehicle_11 == 1),
    mobilephone = mean(ownership_mobile_phone_11 == 1)
  )

# Shocks suffered during the last year by the hh:
Table_shocks_2011 <- regression_df_clean %>%
  summarize(
    health_shock = mean(health_shock == 1),
    death_shock = mean(death_shock == 1),
    dweling_shock = mean(dweling_shock == 1),
    agric_shock = mean(agricultural_shock == 1),
    eco_shock = mean(economic_shock == 1),
    clima_shock = mean(climate_shock == 1),
    other_shock = mean(other_shock == 1),
    change_n_working = mean(change_n_working),
    change_n_resident = mean(change_n_resident)
  )


# Clear namespace of varibles which are not used in future analysis steps.
# To see intermediate results change the code below accordingly.
rm(
  Table_dwelling_2011,
  Table_educ_2011,
  Table_shocks_2011,
  Table_sector_2011,
  Table_marital_status_2011,
  Table_other_Hoh_char_2011
)
