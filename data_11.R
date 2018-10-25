# In this script the data frame for the year 2011 is created. 

############################# SECTION-1 ######################################
# Head information:
# In this section the df *Head_info_11* will be created and it will contain the variables
# related with head information:

hh_roster_2011_vars <-
  c("HHID_old",
    "PID",
    "sex",
    "relationship_head",
    "age",
    "marital_status")
hh_size <- hh_roster_2011_original %>%
  group_by(HHID_old) %>%
  summarize(hsize = n())
# Add variable hsize to HH information

hh_roster_11 <- hh_roster_2011_original %>%
  dplyr::select(hh_roster_2011_vars) %>%
  filter(relationship_head == 1) %>%
  mutate(age_sq = age ^ 2) %>%
  #There are some households who have two heads, but one of them has NA age.
  #Cleaning NA from age so duplicated *HHID_old* are deleted:
  filter(!is.na(age))

# Education:
# Selecting useful variables for my purpose:
educ_2011_vars <-
  c("HHID_old",
    "PID",
    "literate",
    "schooling",
    "schooling_level",
    "highest_class")
educ_11 <- dplyr::select(educ_2011_original, educ_2011_vars)
educ_11$sch_level_lastyear <- educ_11$highest_class


Head_info_11 <-
  merge(hh_roster_11, educ_11, by = c("HHID_old", "PID"))

# labor variables:
equal <- function(in_col, equal_to) {
  (in_col == equal_to) & (!is.na(in_col))
}

in_it <- function(in_col, equal_to) {
  (in_col %in% equal_to) & (!is.na(in_col))
}

#TODO: Check labor status construction
labor_2011$labor_status <-
  with(labor_2011,
       ifelse(
         equal(work_lastweek, 1) | equal(hhfarm_lastWeek, 1) |
           equal(nopaid_work_lastweek, 1) |
           equal(business_lastweek, 1) |
           equal(apprentice_lastweek, 1),
         1,
         ifelse(
           is.na(hhfarm_lastWeek) &
             is.na(nopaid_work_lastweek) &
             is.na(work_lastweek) &
             is.na(business_lastweek) &
             is.na(apprentice_lastweek),
           NA,
           0
         )
       ))

labor_2011$MainJob_sectorcode <-
  as.numeric(labor_2011$MainJob_sectorcode)
labor_2011 <-
  mutate(labor_2011, sector = ifelse(in_it(MainJob_sectorcode, 1:3), 1,
                                     ifelse(
                                       equal(MainJob_sectorcode, 4), 2,
                                       ifelse(in_it(MainJob_sectorcode, 5:17), 3, NA)
                                     )))

labor_workers_11 <- labor_2011 %>%
  group_by(HHID_old) %>%
  #Add this variable later in the HH information section:
  summarize(n_working = sum(labor_status == 1))

labor_11 <-
  dplyr::select(labor_2011, HHID_old, PID, labor_status, sector, MainJob_code)
Head_info_11 <-
  merge(Head_info_11, labor_11, by = c("HHID_old", "PID"))

############################################ SECTION-2 ###############################################

# HH information:
# In this section it is created a dataframe with all information regarding the household

vars_UNPS_11 <-
  c("HHID_old", "poor", "cpexp30", "equiv", "welfare", "spline")
UNPS_2011_select <- UNPS_2011[vars_UNPS_11]
data_11 <- merge(hh_id_2011, UNPS_2011_select, by = "HHID_old")
data_11 <- merge(data_11, hh_size, by = "HHID_old")

#I have three outliers in cpexp30 I don't rely so I will delete them:
data_11 <- filter(data_11,!(cpexp30 > 9999999))

# hh condition variables:
hh_cond_11 <- Housing_cond_2011_original %>%
  dplyr::select(
    HHID_old,
    type_residence,
    tenure_status,
    rooms,
    material_roof,
    material_wall,
    material_floor,
    water_source,
    reason_not_protected_water,
    water_paid,
    money_spend_water,
    how_make_safer_water,
    water_stored,
    water_covered,
    toilet,
    hand_washing_toilet
  )

data_11 <- merge(data_11, hh_cond_11, by = "HHID_old")

# Energy variables:
energy_11 <- Energy_2011_original_1 %>%
  dplyr::select(HHID_old,
                electricity,
                electricity_expenditure,
                generator,
                most_used_stove,
                chimney)

data_11 <- merge(data_11, energy_11, by = "HHID_old")


#we created *labor_workers_11* in head information section *SECTION-1*
data_11 <- merge(data_11, labor_workers_11, by = "HHID_old")

# Assets variables:

HH_appliances <- Assets_2011_original %>%
  filter(Asset_code == 5) %>%
  group_by(HHID_old) %>%
  mutate(ownership_hh_appliances = ifelse(own_asset == 1, 1, 0)) %>%
  dplyr::select(HHID_old, ownership_hh_appliances)

TV <- Assets_2011_original %>%
  filter(Asset_code == 6) %>%
  group_by(HHID_old) %>%
  mutate(ownership_TV = ifelse(own_asset == 1, 1, 0)) %>%
  dplyr::select(HHID_old, ownership_TV)

Radio <- Assets_2011_original %>%
  filter(Asset_code == 7) %>%
  group_by(HHID_old) %>%
  mutate(ownership_radio = ifelse(own_asset == 1, 1, 0)) %>%
  dplyr::select(HHID_old, ownership_radio)

Bicycle <- Assets_2011_original %>%
  filter(Asset_code == 10) %>%
  group_by(HHID_old) %>%
  mutate(ownership_bicycle = ifelse(own_asset == 1, 1, 0)) %>%
  dplyr::select(HHID_old, ownership_bicycle)

Motorcycle <- Assets_2011_original %>%
  filter(Asset_code == 11) %>%
  group_by(HHID_old) %>%
  mutate(ownership_motorcycle = ifelse(own_asset == 1, 1, 0)) %>%
  dplyr::select(HHID_old, ownership_motorcycle)

Vehicle <- Assets_2011_original %>%
  filter(Asset_code == 12) %>%
  group_by(HHID_old) %>%
  mutate(ownership_vehicle = ifelse(own_asset == 1, 1, 0)) %>%
  dplyr::select(HHID_old, ownership_vehicle)

mobile_phone <- Assets_2011_original %>%
  filter(Asset_code == 16) %>%
  group_by(HHID_old) %>%
  mutate(ownership_mobile_phone = ifelse(own_asset == 1, 1, 0)) %>%
  dplyr::select(HHID_old, ownership_mobile_phone)

ownership <-
  list(HH_appliances,
       TV,
       Radio,
       Bicycle,
       Motorcycle,
       Vehicle,
       mobile_phone) %>%
  reduce(inner_join, by = "HHID_old")

data_11 <- merge(data_11, ownership, by = "HHID_old")



#Combining head information with household information:
data_11 <- merge(data_11, Head_info_11, by = "HHID_old")

### Control variables:
#EDUCATION
# Schooling is a categorical variable = 1 (Never attended) 2 (attended in the past) 3 (currently attending)
# if = 2 then the level of education is in the variable schooling level, if = 3 then the information is in sch_level_lastyear

data_11$education_level <- with(data_11, ifelse(
  equal(schooling, 1) |
    equal(sch_level_lastyear, 88) |
    equal(sch_level_lastyear, 99) | equal(schooling_level, 99) ,
  0,
  ifelse(
    equal(sch_level_lastyear, 1) | equal(schooling_level, 10),
    1,
    ifelse(
      in_it(sch_level_lastyear, 10:16) | in_it(schooling_level, 11:17),
      2,
      ifelse(
        in_it(sch_level_lastyear, 21:23) | in_it(schooling_level, 21:23),
        3,
        ifelse(
          in_it(sch_level_lastyear, 30:35) | in_it(schooling_level, 31:36),
          4,
          ifelse(
            equal(sch_level_lastyear, 40) | equal(schooling_level, 41),
            5,
            ifelse(
              equal(sch_level_lastyear, 50) | equal(schooling_level, 51),
              6,
              ifelse(
                equal(sch_level_lastyear , 61) | equal(schooling_level, 61),
                7,
                ifelse(is.na(sch_level_lastyear) &
                         is.na(schooling_level), NA, NA)
              )
            )
          )
        )
      )
    )
  )
))


# SEX
data_11$male <- with(data_11, ifelse(equal(sex, 1), 1, 0))

# Variables for HH sanitation conditions:
data_11$indep_hh <-
  with(data_11, ifelse(equal(type_residence, 1) |
                         equal(type_residence, 3), 1, 0))
data_11$piped_water <-
  with(data_11, ifelse(equal(water_source, 1), 1, 0))
data_11$ownership_house <-
  with(data_11, ifelse(
    equal(tenure_status, 1) |
      equal(tenure_status, 2) | equal(tenure_status, 3),
    1,
    0
  ))
data_11$flush_toilet <-
  with(data_11, ifelse(equal(toilet, 6) | equal(toilet, 7), 1, 0))
data_11$hand_washing <- with(data_11, ifelse(
  equal(hand_washing_toilet, 2) | equal(hand_washing_toilet, 3),
  1,
  0
))
data_11$d_electricity <-
  with(data_11, ifelse(equal(electricity, 1), 1, 0))


rm(
  Assets_2011_original,
  educ_2011_original,
  educ_11,
  Energy_2011_original_1,
  Energy_2011_original_2,
  Energy_2011_original_3,
  energy_11,
  Head_info_11,
  hh_id_2011,
  hh_id_2011_1,
  hh_id_2011_original,
  hh_roster_11,
  hh_roster_2011_names,
  hh_roster_2011_original,
  Housing_cond_2011_original,
  hh_cond_11,
  labor_2011,
  labor_2011_original,
  labor_11,
  labor_workers_11,
  Shocks_2011_original,
  UNPS_2011_original,
  UNPS_2011,
  UNPS_2011_select
)
