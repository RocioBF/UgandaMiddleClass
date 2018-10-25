## Creating control variables for the year 2011 (vars ended in "_11"):

# Variables related with Head Information:

#EDUCATION
# TODO: Clean up the code s.t. the data cleaning and creation part is structured in data_11 and data_13
#Schooling is a categorical variable = 1 (Never attended) 2 (attended in the past) 3 (currently attending)
# if = 2 then the level is in the variable schooling level, if = 3 then the information is in sch_level_lastyear
equal <- function(in_col, equal_to) {
  (in_col == equal_to) & (!is.na(in_col))
}
in_it <- function(in_col, equal_to){
  (in_col %in% equal_to) & (!is.na(in_col))
  
}


Main_Df$education_level_11 <- with(Main_Df, ifelse(equal(schooling_11,1) | equal(sch_level_lastyear_11,88) | equal(sch_level_lastyear_11,99) | equal(schooling_level_11,99) , 0,
                                                  ifelse(equal(sch_level_lastyear_11,1) | equal(schooling_level_11,10), 1,
                                                         ifelse(in_it(sch_level_lastyear_11,10:16) | in_it(schooling_level_11,11:17), 2,
                                                                ifelse(in_it(sch_level_lastyear_11,21:23) | in_it(schooling_level_11, 21:23), 3,
                                                                       ifelse(in_it(sch_level_lastyear_11,30:35) | in_it(schooling_level_11,31:36), 4,
                                                                              ifelse(equal(sch_level_lastyear_11,40) | equal(schooling_level_11,41), 5,
                                                                                     ifelse(equal(sch_level_lastyear_11,50) | equal(schooling_level_11, 51), 6,
                                                                                            ifelse(equal(sch_level_lastyear_11 ,61) | equal(schooling_level_11, 61), 7,
                                                                                                   ifelse(is.na(sch_level_lastyear_11) & is.na(schooling_level_11), NA, NA
                                                                                                   ))))))))))


# SEX

Main_Df$male_11 <- with(Main_Df, ifelse(equal(sex_11, 1), 1, 0))


# Variables for HH conditions:
Main_Df$indep_hh_11 <- with(Main_Df, ifelse(equal(type_residence_11, 1) | equal(type_residence_11,3), 1, 0
                                            )
                            )
Main_Df$piped_water_11 <- with(Main_Df, ifelse(equal(water_source_11, 1), 1, 0
                                               )
                               )
Main_Df$ownership_house_11 <- with(Main_Df, ifelse(equal(tenure_status_11, 1) | equal(tenure_status_11, 2) | equal(tenure_status_11, 3), 1,0 
                                                   )
                                   )
Main_Df$flush_toilet_11 <- with(Main_Df, ifelse(equal(toilet_11, 6) | equal(toilet_11, 7), 1, 0
                                                )
                                )
Main_Df$hand_washing_11 = with(Main_Df, ifelse(equal(hand_washing_toilet_11,2) | equal(hand_washing_toilet_11, 3),1,0
                                               )
                               )
Main_Df$d_electricity_11 <- with(Main_Df, ifelse(equal(electricity_11, 1),1,0
                                                 )
                                 )



#Shocks:
#Creating a data frame with variables that denotes the shocks:
# TODO. Document this in depth.
# TODO: Change it as in the comment below. Check if the results remain consistent
# TODO: Check how the new approach handels NAs

Health_shock <- Shocks_2013_original %>%
  filter(Shock_code %in% 110:111) %>%
  group_by(HHID) %>%
  mutate(health_shock = ifelse(shock_lastyear == 1, 1, 0)) %>%
  select( HHID, health_shock) %>%
  unique() %>%
  summarise(health_shock = sum(health_shock))

'Health_shock_new <- Shocks_2013_original %>%
filter(Shock_code %in% 110:111) %>%
group_by(HHID) %>%
mutate(health_shock = ifelse(1 %in% shock_lastyear, 1, 0)) %>%
select( HHID, health_shock)

'
Death_shock <- Shocks_2013_original %>%
  group_by(HHID) %>%
  filter(Shock_code %in% 112:113) %>%
  mutate(death_shock = ifelse(shock_lastyear == 1, 1, 0)) %>%
  select( HHID, death_shock) %>%
  unique() %>%
  summarise(death_shock = sum(death_shock))



Climate_shock <- Shocks_2013_original %>%
  group_by(HHID) %>%
  filter(Shock_code == 102 | Shock_code %in% 1011:1032) %>% #Take a look
  mutate(climate_shock_1 = ifelse(shock_lastyear == 1, 1, 0)) %>%
  select( HHID, climate_shock_1) %>%
  unique() %>%
  summarise(climate_shock = sum(climate_shock_1))

Dweling_shock <- Shocks_2013_original %>%
  group_by(HHID) %>%
  filter(Shock_code %in% 114:117) %>%
  mutate(dweling_shock = ifelse(shock_lastyear == 1, 1, 0)) %>%
  select( HHID, dweling_shock) %>%
  unique() %>%
  summarise(dweling_shock = sum(dweling_shock))


Economic_shock <- Shocks_2013_original %>%
  group_by(HHID) %>%
  filter(Shock_code %in% 106:109) %>%
  mutate(economic_shock = ifelse(shock_lastyear == 1, 1, 0)) %>%
  select( HHID, economic_shock) %>%
  unique() %>%
  summarise(economic_shock = sum(economic_shock))



Agricultural_shock <- Shocks_2013_original %>%
  group_by(HHID) %>%
  filter(Shock_code %in% 104:105) %>%
  mutate(agricultural_shock_1 = ifelse(shock_lastyear == 1, 1, 0)) %>%
  select( HHID, agricultural_shock_1) %>%
  unique() %>%
  summarise(agricultural_shock = sum(agricultural_shock_1))

Other_shock <- Shocks_2013_original %>%
  group_by(HHID) %>%
  filter(Shock_code == 118) %>%
  mutate(other_shock = ifelse( shock_lastyear == 1, 1, 0)) %>%
  select( HHID, other_shock)%>%
  unique() %>%
  summarise(other_shock = sum(other_shock))


shocks_13 <- list(Health_shock, Death_shock, Climate_shock, Dweling_shock, Economic_shock, 
                  Agricultural_shock, Other_shock) %>%
  reduce(inner_join, by="HHID")
'Data_with_shocks_13 <- merge(data_13, shocks_13, by = "HHID")
Data_with_shocks_13 <- select(Data_with_shocks_13, HHID, health_shock, death_shock, climate_shock, 
                              dweling_shock, economic_shock, agricultural_shock, other_shock)'

#We join the data base we had with the shocks variables:
Main_Df <- left_join(Main_Df, shocks_13, by = "HHID")

#Change in number of workers and change in number of persons living in hh:
Main_Df$change_n_resident <- Main_Df$hsize_13 - Main_Df$hsize_11
Main_Df$change_n_working <- Main_Df$n_working_13 - Main_Df$n_working_11

#logaritm of expenditure:
Main_Df$ln_expenditure_11 <- log(Main_Df$welfare_11)
Main_Df$ln_expenditure_13 <- log(Main_Df$welfare_13)
