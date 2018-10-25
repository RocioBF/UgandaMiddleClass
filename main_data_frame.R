# In this section  I append the data frames from 2011 and 2013.

# For that the data frames need to have the same variables and type:
data_13$poor <- data_13$poor_13
data_13 <- dplyr::select(data_13, -poor_13,-wgt_X)
data_11 <- dplyr::select(data_11, -most_used_stove, -HHID)

# Merging data_11 & data_13
Main_Df <-
  merge(data_11,
        data_13,
        by = "HHID_old",
        suffixes = c("_11", "_13"))

#Merging data_11 and data_13 in panel data frame:
#Delete HHID from data_13 so we have the same number of columns that data_11
new_data_13 <- dplyr::select(data_13, -HHID)
panel_df <- rbind(data_11, new_data_13)
panel_df <-
  dplyr::select(
    panel_df,
    HHID_old,
    year,
    welfare,
    welfare,
    poor,
    poor,
    male,
    age,
    age_sq,
    education_level,
    marital_status,
    hsize,
    indep_hh,
    piped_water,
    flush_toilet,
    hand_washing,
    ownership_house,
    rooms,
    electricity,
    urban,
    region,
    d_electricity,
    MainJob_code,
    sector,
    labor_status,
    ownership_hh_appliances,
    ownership_TV,
    ownership_radio,
    ownership_bicycle,
    ownership_motorcycle,
    ownership_vehicle,
    ownership_mobile_phone
  )
panel_df_clean <- panel_df[complete.cases(panel_df), ]

#Deleting intermediate data frames:
rm(data_11, data_13, new_data_13)