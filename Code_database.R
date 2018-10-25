# This script is used to call all data that is related to the regression analysis.
# Furthermore, varibles are renamed to be used in the analysis.

#############################################################################
# HH Identification: This section contains the variables needed to identify 
# households. 
# HHID is the variable used to identy the households in 2013, 
# and HHID_old is an identifier that appears in both databases 2011 and 2013.

# Load HH for 2013
hh_id_2013_original <-
  read_csv("./raw_data/UGA_2013_UNPS_v01_M_CSV/gsec1.csv", na = "NA")

# Select subset of columns from hh_id_2013_original
my_vars <- c("HHID", "HHID_old", "region", "urban", "wgt_X", "year")
hh_id_2013 <- hh_id_2013_original[my_vars]

# Load HH for 2011
hh_id_2011_original <-
  read_stata("./raw_data/UGA_2011_UNPS_v01_M_Stata8/gsec1.dta")

# Read HHID as numeric
hh_id_2011_1 <-
  transform(hh_id_2011_original, HHID_old = as.numeric(HHID))

# Select subset subset of columns from hh_id_2011 with transformed *HHID_old* that matches
# *HHID_old* in hh_id_2013.

my_vars_2 <- c("region", "urban", "year", "HHID", "HHID_old")
hh_id_2011 <- hh_id_2011_1[my_vars_2]

#############################################################################
# Household Roster: This section contains variables that identifies the 
# members of the household. PID and PID_old are the identifiers of each person 
# in the household. When relationship_head = 1, the person is the head. 

hh_roster_2013_original <-
  read_csv("./raw_data/UGA_2013_UNPS_v01_M_CSV/gsec2.csv", na = "NA")
hh_roster_2013_names <- setnames(
  hh_roster_2013_original,
  old = c(
    "h2q1",
    "h2q3",
    "h2q4",
    "h2q5",
    "h2q6",
    "h2q7",
    "h2q8",
    "h2q9a",
    "h2q9b",
    "h2q9c",
    "h2q10",
    "h2q11",
    "h2q13_1",
    "h2q13_2"
  ),
  new = c(
    "members",
    "sex",
    "relationship_head",
    "months_in_hh",
    "reason_absence",
    "residential_status",
    "age",
    "day_birht",
    "month_birth",
    "year_birth",
    "marital_status",
    "member_visit2",
    "went_Region",
    "went_Country"
  )
)

hh_roster_2011_original <-
  read_stata("./raw_data/UGA_2011_UNPS_v01_M_Stata8/gsec2.dta")

hh_roster_2011_names <- setnames(
  hh_roster_2011_original,
  old = c(
    "h2q1",
    "h2q3",
    "h2q4",
    "h2q5",
    "h2q6",
    "h2q7",
    "h2q8",
    "h2q9a",
    "h2q9b",
    "h2q9c",
    "h2q10",
    "h2q11",
    "h2q13_1",
    "h2q13_2"
  ),
  new = c(
    "members",
    "sex",
    "relationship_head",
    "months_in_hh",
    "reason_absence",
    "residential_status",
    "age",
    "day_birht",
    "month_birth",
    "year_birth",
    "marital_status",
    "member_visit2",
    "went_Region",
    "went_Country"
  )
)

hh_roster_2011_original <-
  transform(hh_roster_2011_names, HHID_old = as.numeric(HHID))

###################################################################
# EDUCATION: This section provides information regarding the level of
# education and schooling of the members of the household.

educ_2013_original <-
  read_csv("./raw_data/UGA_2013_UNPS_v01_M_CSV/gsec4.csv", na = "NA")

names_educ_13 <-
  c(
    "HHID",
    "PID",
    "answer_by_themselves",
    "responder",
    "literate",
    "schooling",
    "reason_no_sch",
    "schooling_level",
    "reason_left_school",
    "highest_class",
    "class_actual",
    "name_school",
    "management_school",
    "type_school",
    "section_sch",
    "distance_sch",
    "time_sch",
    "transport_sch",
    "registration_fees",
    "spend_uniform",
    "spend_books",
    "spend_travel",
    "boarding_fees",
    "other_spend_Sch",
    "total_cost_sch",
    "scholarship",
    "source_scholarship",
    "meals_sch",
    "wgt_all"
  )
colnames(educ_2013_original) <- names_educ_13

educ_2011_original <-
  read_stata("./raw_data/UGA_2011_UNPS_v01_M_Stata8/gsec4.dta")
names_educ_11 <-
  c(
    "HHID",
    "PID",
    "members",
    "answer_by_themselves",
    "responder",
    "literate",
    "schooling",
    "reason_no_sch",
    "schooling_level",
    "reason_left_school",
    "highest_class",
    "class_actual",
    "management_school",
    "boarding_school",
    "type_school",
    "distance_sch",
    "time_sch",
    "registration_fees",
    "spend_uniform",
    "spend_books",
    "spend_travel",
    "boarding_fees",
    "other_spend_Sch",
    "total_cost_sch",
    "scholarship",
    "source_scholarship",
    "meals_sch"
  )
colnames(educ_2011_original) <- names_educ_11
educ_2011_original <-
  transform(educ_2011_original, HHID_old = as.numeric(HHID))


#######################################################################
# LABOR FORCE STATUS: This section contains the variables regarding the 
# labor status of each household member.

labor_2013_original <-
  read_csv("./raw_data/UGA_2013_UNPS_v01_M_CSV/gsec8_1.csv", na = "NA")
labor_2013 <-
  dplyr::select(labor_2013_original,-("h8q36a":"h8q36g"),-"h8q46",-"h8q51a")

names_labor_13 <-
  c(
    "HHID",
    "PID",
    "answer_by_themselves",
    "responder",
    "work_lastweek",
    "work_lastyear",
    "business_lastweek",
    "business_lastyear",
    "nopaid_work_lastweek",
    "nopaid_work_year",
    "apprentice_lastweek",
    "apprentice_lastyear",
    "hhfarm_lastWeek",
    "hhfarm_lastyear",
    "nohelp_nopay",
    "look_for_lastmonth",
    "Action1",
    "Action2",
    "trybussiness_lastmonth",
    "member_situation",
    "MainJob_description",
    "MainJob_code",
    "MainJob_Sector",
    "MainJob_sectorcode",
    "MainJob_year_start",
    "MainJob_month_start",
    "MainJob_class_lastweek",
    "MainJob_pension_fund",
    "MainJob_paid_leave",
    "MainJob_medical_benefits",
    "MainJob_tax",
    "MainJob_employment_agreetment",
    "MainJob_position",
    "MainJob_duration_contract",
    "MainJob_months_lastyear",
    "MainJob_avg_weekly_hours",
    "MainJob_cash",
    "MainJob_salary",
    "MainJob_controlmoney",
    "MainJob_controlmoney2",
    "MainJob_business_registered_VAT",
    "MainJob_business_registered_tax",
    "MainJob_apprentice_class",
    "MainJob_apprentice2_class",
    "MainJob_employer",
    "SecondJob_lastweek",
    "SecondJob_description",
    "SecondJob_jobcode",
    "SecondJob_sector",
    "SecondJob_sectorcode",
    "SecondJob_start_year",
    "SecondJob_start_month",
    "SecondJob_class",
    "SecondJob_employer",
    "SecondJob_hours_lastweek",
    "SecondJob_months_peryear",
    "SecondJob_weeks_permonth",
    "SecondJob_cash",
    "SecondJob_salary",
    "SecondJob_period_payment",
    "SecondJob_controlmoney",
    "SecondJob_controlmoney2",
    "usual_activity",
    "year_usual",
    "month_usual",
    "employer_usual",
    "position_usual",
    "duration_usual",
    "description_usual",
    "usual_jobcode",
    "sector_usual",
    "usual_sectorcode",
    "secondAct_lastyear",
    "Usual_class",
    "Usual_months_in_year",
    "Usual_weeks_in_month",
    "Usual_hours_in_week",
    "Usual_cash",
    "Usual_salary",
    "Usual_Time_payment",
    "SencondAct_description",
    "SecondAct_sectorcode",
    "SecondAct_sector",
    "SecondAct_start_year",
    "SecondAct_start_month",
    "SecondAct_class",
    "SecondAct_employer",
    "SecondAct_months_lastyear",
    "SecondAct_weeks_lastmonth",
    "SecondAct_hours_lastweek",
    "cash_SecondAct",
    "salary_SecondAct",
    "Period_payment_SecondAct",
    "SecondAct_controlmoney",
    "SecondAct_controlmoney2",
    "NM_firewood",
    "NM_water",
    "NM_construction",
    "NM_repairs",
    "NM_milling",
    "NM_handicrafts",
    "NM_agriculture",
    "NM_hunting",
    "NM_domesticAct",
    "MainJob_lastweek",
    "wgt_all"
  )
colnames(labor_2013) <- names_labor_13

labor_2013 <-
  dplyr::select(labor_2013,  1:46, "sector_usual", "usual_sectorcode")


labor_2011_original <-
  read_dta("./raw_data/UGA_2011_UNPS_v01_M_Stata8/gsec8.dta")
labor_2011 <-
  dplyr::select(labor_2011_original, 1:43, "h8q50a", "h8q50b")
names_labor_11 <-
  c(
    "HHID",
    "PID",
    "members",
    "responder",
    "answer_by_themselves",
    "work_lastweek",
    "work_lastyear",
    "business_lastweek",
    "business_lastyear",
    "nopaid_work_lastweek",
    "nopaid_work_lastyear",
    "apprentice_lastweek",
    "apprentice_lastyear",
    "hhfarm_lastWeek",
    "hhfarm_lastyear",
    "nohelp_nopay",
    "look_for_lastmonth",
    "try_business_lastmonth",
    "situation",
    "Action1",
    "Action2",
    "MainJob_code",
    "MainJob_description",
    "MainJob_sectorcode",
    "MainJob_sector",
    "MainJob_year_start",
    "MainJob_month_start",
    "MainJob_class_lastweek",
    "MainJob_pension_fund",
    "MainJob_paid_leave",
    "MainJob_medical_benefits",
    "MainJob_tax",
    "MainJob_employment_agreetment",
    "MainJob_position",
    "MainJob_duration_contract",
    "MainJob_cash",
    "MainJob_salary",
    "MainJob_periodpayment",
    "MainJob_business_registered_VAT",
    "MainJob_business_registered_tax",
    "MainJob_apprentice_class",
    "MainJob_apprentice2_class",
    "MainJob_employer",
    "sector_usual",
    "usual_sectorcode"
  )
colnames(labor_2011) <- names_labor_11

labor_2011 <- transform(labor_2011, HHID_old = as.numeric(HHID))


########################################################
#Housing Conditions, Water and Sanitation. 

Housing_cond_2013_original <-
  read_csv("./raw_data/UGA_2013_UNPS_v01_M_CSV/gsec9_1.csv", na = "NA")
names_housing_cond_13 <-
  c(
    "HHID",
    "same_residence",
    "type_residence",
    "tenure_status",
    "rooms",
    "material_roof",
    "material_wall",
    "material_floor",
    "water_source",
    "reason_not_protected_water",
    "TravelTime_water",
    "WaitingTime_water",
    "far_water_residence",
    "water_used_perday_Liters",
    "water_paid",
    "payment_purpose",
    "money_spend_water",
    "person1_collects_water",
    "person2_collects_water",
    "person3_collects_water",
    "user_committees",
    "how_make_safer_water",
    "water_stored",
    "water_covered",
    "changed_safer_water",
    "constraints_safe_water",
    "toilet",
    "shared_toilet",
    "n_hh_shared_toilet",
    "hand_washing_toilet",
    "wgt_all"
  )
colnames(Housing_cond_2013_original) <- names_housing_cond_13

Housing_cond_2011_original <-
  read_dta("./raw_data/UGA_2011_UNPS_v01_M_Stata8/gsec9A.dta")
names_housing_cond_11 <-
  c(
    "HHID",
    "type_residence",
    "tenure_status",
    "rooms",
    "material_roof",
    "material_wall",
    "material_floor",
    "water_source",
    "reason_not_protected_water",
    "TravelTime_water",
    "WaitingTime_water",
    "far_water_residence",
    "water_used_perday_UNIT",
    "water_used_perday_QUANTITY",
    "water_paid",
    "payment_purpose",
    "money_spend_water",
    "user_committees",
    "how_make_safer_water",
    "water_stored",
    "water_covered",
    "changed_safer_water",
    "constraints_safe_water",
    "toilet",
    "hand_washing_toilet"
  )
colnames(Housing_cond_2011_original) <- names_housing_cond_11
Housing_cond_2011_original <-
  transform(Housing_cond_2011_original, HHID_old = as.numeric(HHID))


#################################################################################
#ENERGY USE: This section contains the information regarding the energy used 
# in the household

Energy_2013_original_1 <-
  read_csv("./raw_data/UGA_2013_UNPS_v01_M_CSV/gsec10_1.csv", na = "NA")
names_Energy_13_1 <-
  c(
    "HHID",
    "electricity",
    "hours_power",
    "HOW_pay_for_electricity",
    "q_electricity",
    "electricity_expenditure",
    "days_covered_billing",
    "generator",
    "pay_diesel_generator",
    "diesel_litres",
    "pay_petrol_generator",
    "petrol_litres",
    "stove",
    "chimney",
    "hours_stove_perday",
    "location_stove",
    "wgt_all"
  )
colnames(Energy_2013_original_1) <- names_Energy_13_1

Energy_2013_original_2 <-
  read_csv("./raw_data/UGA_2013_UNPS_v01_M_CSV/gsec10_2.csv", na = "NA")
names_Energy_13_2 <- c("HHID", "stove_id", "stove_type", "wgt_all")
colnames(Energy_2013_original_2) <- names_Energy_13_2

Energy_2013_original_3 <-
  read_csv("./raw_data/UGA_2013_UNPS_v01_M_CSV/gsec10_3.csv", na = "NA")
names_Energy_13_3 <-
  c(
    "HHID",
    "Fuel_id",
    "Fuel_id_Yes_no",
    "fuel_cooking",
    "fuel_lighting",
    "fuel_heating",
    "where_get_fuel",
    "shillings_fuel",
    "quantity_fuel",
    "unit_fuel",
    "wgt_all"
  )
colnames(Energy_2013_original_3) <- names_Energy_13_3


Energy_2011_original_1 <-
  read_dta("./raw_data/UGA_2011_UNPS_v01_M_Stata8/gsec10A.dta")
Energy_2011_original_1 <-
  dplyr::select(Energy_2011_original_1,-"h10q9")
names_Energy_11_1 <-
  c(
    "HHID",
    "electricity",
    "hours_power",
    "HOW_pay_for_electricity",
    "q_electricity",
    "electricity_expenditure",
    "electricity_expenditure_2",
    "generator",
    "pay_diesel_generator",
    "diesel_litres",
    "pay_petrol_generator",
    "petrol_litres",
    "chimney",
    "hours_stove_perday",
    "location_stove",
    "most_used_stove"
  )
colnames(Energy_2011_original_1) <- names_Energy_11_1
Energy_2011_original_1 <-
  transform(Energy_2011_original_1, HHID_old = as.numeric(HHID))


Energy_2011_original_2 <-
  read_dta("./raw_data/UGA_2011_UNPS_v01_M_Stata8/gsec10B.dta")
names_Energy_11_2 <- c("HHID", "stove_id", "stove_type")
colnames(Energy_2011_original_2) <- names_Energy_11_2
Energy_2011_original_2 <-
  transform(Energy_2011_original_2, HHID_old = as.numeric(HHID))


Energy_2011_original_3 <-
  read_dta("./raw_data/UGA_2011_UNPS_v01_M_Stata8/gsec10C.dta")
names_Energy_11_3 <-
  c(
    "HHID",
    "Fuel_id",
    "Fuel_id_Yes_no",
    "fuel_cooking",
    "fuel_lighting",
    "fuel_heating",
    "where_get_fuel",
    "shillings_fuel",
    "quantity_fuel",
    "unit_fuel",
    "result_code"
  )
colnames(Energy_2011_original_3) <- names_Energy_11_3
Energy_2011_original_3 <-
  transform(Energy_2011_original_3, HHID_old = as.numeric(HHID))


################################################################################
#HH ASSETS: information about the assets holded by the housheold

Assets_2013_original <-
  read_csv("./raw_data/UGA_2013_UNPS_v01_M_CSV/gsec14A.csv", na = "NA")
names_assets_13 <-
  c(
    "HHID",
    "Asset_code",
    "own_asset",
    "number_assets",
    "primary_owner",
    "secondary_owner",
    "value_assets",
    "number_assets_wave1",
    "number_assets_lastvisit",
    "value_assets_wage1",
    "why_less",
    "why_more",
    "wgt_all"
  )
colnames(Assets_2013_original) <- names_assets_13

Assets_2011_original <-
  read_dta("./raw_data/UGA_2011_UNPS_v01_M_Stata8/gsec14.dta")
names_assets_11 <-
  c(
    "HHID",
    "Asset_code",
    "own_asset",
    "number_assets",
    "value_assets",
    "number_assets_wave1",
    "value_assets_wage1",
    "why_less",
    "why_more",
    "assets_last_year",
    "result_code"
  )
colnames(Assets_2011_original) <- names_assets_11
Assets_2011_original <-
  transform(Assets_2011_original, HHID_old = as.numeric(HHID))


################################################################################

# SHOCKS AND COPING STRATEGIES

Shocks_2013_original <-
  read_csv("./raw_data/UGA_2013_UNPS_v01_M_CSV/gsec16.csv", na = "NA")
names_shocks_13 <-
  c(
    "HHID",
    "Shock_code",
    "shock_lastyear",
    "month_shock",
    "year_shock",
    "how_long_shock",
    "decline_income_shock",
    "decline_assets_shock",
    "decline_food_shock",
    "decline_food_purch_shock",
    "solution_shock_1",
    "solution_shock_2",
    "solution_shock_3",
    "wgt_all"
  )
colnames(Shocks_2013_original) <- names_shocks_13

Shocks_2011_original <-
  read_dta("./raw_data/UGA_2011_UNPS_v01_M_Stata8/gsec16.dta")
names_shocks_11 <-
  c(
    "HHID",
    "Shock_code",
    "shock_lastyear",
    "month_shock",
    "year_shock",
    "how_long_shock",
    "decline_income_shock",
    "decline_assets_shock",
    "decline_food_shock",
    "decline_food_purch_shock",
    "solution_shock_1",
    "solution_shock_2",
    "solution_shock_3"
  )
colnames(Shocks_2011_original) <- names_shocks_11
Shocks_2011_original <-
  transform(Shocks_2011_original, HHID_old = as.numeric(HHID))



###############################################################################
# UNPS CONSUMPTION AGGREGATE: Indicators calculated by the Uganda National 
# Panel Survey. Here there is information of the poverty line and the 
# aggregate consumption of each household.


UNPS_2013_original <-
  read_csv("./raw_data/UGA_2013_UNPS_v01_M_CSV/unps 2013-14 consumption aggregate.csv",
           na = "NA")


UNPS_2011_original <-
  read_dta("./raw_data/UGA_2011_UNPS_v01_M_Stata8/UNPS 2011-12 Consumption Aggregate.dta")
UNPS_2011 <-
  transform(UNPS_2011_original, HHID_old = as.numeric(HHID))

##################################################################################
# Data frame for Uganda's background: 

## Data for GNP per capita:
GNP_pc <-
  read_excel("./raw_data/background_uganda_data/ADIEXCEL.xlsx", sheet = 3, col_names = TRUE)

## Data for GPD growth per capita
gdp_growth <-
  read_excel("./raw_data/background_uganda_data/gdp_growth.xlsx", sheet = 4, col_names = TRUE)

## Data for primary enrollment ratio:
Primary_enrollment <-
  read_excel("./raw_data//background_uganda_data/Primary_enrollment.xlsx",
             sheet = 2,
             col_names = TRUE)

