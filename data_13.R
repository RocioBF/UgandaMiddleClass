# In this script the data frame for the year 2013 is created.


# Head information:
hh_roster_2013_vars <-
  c("HHID",
    "PID",
    "sex",
    "relationship_head",
    "age",
    "marital_status")
hh_roster_13 <- hh_roster_2013_original %>%
  dplyr::select(hh_roster_2013_vars) %>%
  filter(relationship_head == 1) %>%
  mutate(age_sq = age ^ 2) %>%
  # One household has two heads of the household. I keep the older one:
  filter(!(PID == "P06107-002"))


educ_2013_vars <-
  c("HHID",
    "PID",
    "literate",
    "schooling",
    "schooling_level",
    "highest_class")
educ_13 <- dplyr::select(educ_2013_original, educ_2013_vars)
educ_13$sch_level_lastyear <- educ_13$highest_class

# Joinning the all obs that appears in df1 AND df2 (merge = inner_join by default):
Head_info_13 <- merge(hh_roster_13, educ_13, by = c("HHID", "PID"))


#creating variables for labor:

equal <- function(in_col, equal_to) {
  (in_col == equal_to) & (!is.na(in_col))
}

in_it <- function(in_col, equal_to) {
  (in_col %in% equal_to) & (!is.na(in_col))
}


labor_2013$labor_status <-
  with(labor_2013,
       ifelse(
         equal(work_lastweek, 1) | equal(hhfarm_lastWeek, 1) |
           equal(nopaid_work_lastweek, 1) |
           equal(business_lastweek, 1) |
           equal(apprentice_lastweek, 1),
         1,
         ifelse(
           equal(hhfarm_lastWeek, 2) & equal(nopaid_work_lastweek, 2) &
             equal(work_lastweek, 2) &
             equal(business_lastweek, 2) &
             equal(apprentice_lastweek, 2),
           0,
           NA
         )
       ))

labor_2013$MainJob_sectorcode <-
  as.numeric(labor_2013$MainJob_sectorcode, na.rm = TRUE)


labor_2013 <-
  mutate(labor_2013, sector = ifelse(
    in_it(MainJob_sectorcode, 111:990),
    1,
    ifelse(in_it(MainJob_sectorcode, 1010:3320), 2,
           ifelse(
             in_it(MainJob_sectorcode, 3510:9900), 3, NA
           ))
  ))

#This variable will be added in the household information sector:
labor_workers_13 <- labor_2013 %>%
  group_by(HHID) %>%
  summarize(n_working = sum(labor_status == 1, na.rm = TRUE))


labor_13 <-
  dplyr::select(labor_2013, HHID, PID, labor_status, sector, MainJob_code)

Head_info_13 <- merge(Head_info_13, labor_13, by = c("HHID", "PID"))

##########################################################################################
# HH information

vars_UNPS_13 <-
  c("HHID",
    "hsize",
    "poor_13",
    "cpexp30",
    "equiv",
    "welfare",
    "spline")
UNPS_2013_select <- UNPS_2013_original[vars_UNPS_13]
#one hh is repeated:
UNPS_2013_select <- unique(UNPS_2013_select)

data_13 <-
  merge(hh_id_2013, UNPS_2013_select, by = "HHID")

#housing condition variables:

hh_cond_13 <- Housing_cond_2013_original %>%
  #select the variables that are useful for my purpose among all variables available:
  dplyr::select(
    HHID,
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

data_13 <- merge(data_13, hh_cond_13, by = "HHID")

#Energy related variables:

energy_13 <- Energy_2013_original_1 %>%
  dplyr::select(HHID, electricity, electricity_expenditure, generator, chimney)
data_13 <- merge(data_13, energy_13, by = "HHID")
#we have two observations less in energy (two hh that do not appear and we loose after the merge)

#Assets information:
# TODO: control by NA ("add is.na = TRUE")
HH_appliances <- Assets_2013_original %>%
  filter(Asset_code == 5) %>%
  group_by(HHID) %>%
  mutate(ownership_hh_appliances = ifelse(own_asset == 1, 1, 0)) %>%
  dplyr::select(HHID, ownership_hh_appliances)

TV <- Assets_2013_original %>%
  filter(Asset_code == 6) %>%
  group_by(HHID) %>%
  mutate(ownership_TV = ifelse(own_asset == 1, 1, 0)) %>%
  dplyr::select(HHID, ownership_TV)

Radio <- Assets_2013_original %>%
  filter(Asset_code == 7) %>%
  group_by(HHID) %>%
  mutate(ownership_radio = ifelse(own_asset == 1, 1, 0)) %>%
  dplyr::select(HHID, ownership_radio)

Bicycle <- Assets_2013_original %>%
  filter(Asset_code == 10) %>%
  group_by(HHID) %>%
  mutate(ownership_bicycle = ifelse(own_asset == 1, 1, 0)) %>%
  dplyr::select(HHID, ownership_bicycle)

Motorcycle <- Assets_2013_original %>%
  filter(Asset_code == 11) %>%
  group_by(HHID) %>%
  mutate(ownership_motorcycle = ifelse(own_asset == 1, 1, 0)) %>%
  dplyr::select(HHID, ownership_motorcycle)

Vehicle <- Assets_2013_original %>%
  filter(Asset_code == 12) %>%
  group_by(HHID) %>%
  mutate(ownership_vehicle = ifelse(own_asset == 1, 1, 0)) %>%
  dplyr::select(HHID, ownership_vehicle)

mobile_phone <- Assets_2013_original %>%
  filter(Asset_code == 16) %>%
  group_by(HHID) %>%
  mutate(ownership_mobile_phone = ifelse(own_asset == 1, 1, 0)) %>%
  dplyr::select(HHID, ownership_mobile_phone)

ownership <-
  list(HH_appliances,
       TV,
       Radio,
       Bicycle,
       Motorcycle,
       Vehicle,
       mobile_phone) %>%
  reduce(inner_join, by = "HHID")

data_13 <- merge(data_13, ownership, by = "HHID")



# merge the variable n_workers created in the head information section:
data_13 <- merge(data_13, labor_workers_13, by = "HHID")


#Combining head information with household information:
data_13 <- merge(data_13, Head_info_13, by = "HHID")

#EDUCATION
# Schooling is a categorical variable = 1 (Never attended) 2 (attended in the past) 3 (currently attending)
# if = 2 then the level of education is in the variable schooling level, if = 3 then the information is in sch_level_lastyear

data_13$education_level <- with(data_13, ifelse(
  equal(schooling, 1) |
    equal(sch_level_lastyear, 88) |
    equal(sch_level_lastyear, 99) | equal(schooling_level, 99),
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
                equal(sch_level_lastyear, 61) | equal(schooling_level, 61),
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
data_13$male <- with(data_13, ifelse(equal(sex, 1), 1, 0))

# Variables for HH sanitation conditions:
data_13$indep_hh <-
  with(data_13, ifelse(equal(type_residence, 1) |
                         equal(type_residence, 3), 1, 0))
data_13$piped_water <-
  with(data_13, ifelse(equal(water_source, 1), 1, 0))
data_13$ownership_house <-
  with(data_13, ifelse(
    equal(tenure_status, 1) |
      equal(tenure_status, 2) | equal(tenure_status, 3),
    1,
    0
  ))
data_13$flush_toilet <-
  with(data_13, ifelse(equal(toilet, 6) | equal(toilet, 7), 1, 0))
data_13$hand_washing <- with(data_13, ifelse(
  equal(hand_washing_toilet, 2) | equal(hand_washing_toilet, 3),
  1,
  0
))
data_13$d_electricity <-
  with(data_13, ifelse(equal(electricity, 1), 1, 0))

# Clear namespace of varibles which are not used in future analysis steps.
# To see intermediate results change the code below accordingly.
rm(
  Assets_2013_original,
  educ_2013_original,
  educ_13,
  Energy_2013_original_1,
  Energy_2013_original_2,
  Energy_2013_original_3,
  energy_13,
  Head_info_13,
  hh_id_2013,
  hh_id_2013_original,
  hh_roster_13,
  hh_roster_2013_names,
  hh_roster_2013_original,
  Housing_cond_2013_original,
  hh_cond_13,
  labor_2013,
  labor_2013_original,
  labor_13,
  labor_workers_13,
  UNPS_2013_original,
  UNPS_2013_select
)