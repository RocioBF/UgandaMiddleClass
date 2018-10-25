# RESULTS:

# Kernel Density (expenditure in 2011 and 2013):

kernel_density_2011 <-
  density(
    regression_df_clean$ln_expenditure_11,
    bw = "nrd0",
    adjust = 1,
    kernel = "gaussian",
    weights = NULL,
    window = kernel,
    width,
    give.Rkern = FALSE,
    n = 1532,
    cut = 3
  )


kernel_density_2013 <-
  density(
    regression_df_clean$ln_expenditure_13,
    bw = "nrd0",
    adjust = 1,
    kernel = "gaussian",
    weights = NULL,
    window = kernel,
    width,
    give.Rkern = FALSE,
    n = 1532,
    cut = 3
  )

## Plotting the two expenditure densities:

plot(
  kernel_density_2013,
  col = "#00BFC4",
  main = "",
  bty = 'l',
  cex.axis = 1,
  cex.lab = 1,
  lwd = 2
)
lines(kernel_density_2011, col = "#F8766D", lwd = 2)
abline(v = 10.2, type = "l", lty = 3) #poverty line
mtext("$1.5", at = 10, cex = 1)
abline(v = 10.7, type = "l", lty = 3)
mtext("$2.4", at = 10.8, cex = 1)
abline(v = 11.6, type = "l", lty = 3)
mtext("$5.6", at = 11.6, cex = 1)
legend(
  "topright",
  c("2011", "2013"),
  bty = "n",
  lty = c(1, 1),
  col = c("#F8766D", "#00BFC4"),
  cex = 1
)



#Gini and Lorenz Curve:

ineq(regression_df_clean$welfare_11,
     type = "Gini")
ineq(regression_df_clean$welfare_13,
     type = "Gini")

plot(
  Lc(regression_df_clean$welfare_13),
  col = "#00BFC4",
  lwd = 2,
  main = "",
  bty = 'l',
  xlab = "percentages",
  ylab = "Lorenz curve"
)
lines(Lc(regression_df_clean$welfare_11),
      col = "#F8766D",
      lwd = 2)
legend(
  "bottomright",
  c("2011", "2013"),
  bty = "n",
  lty = c(1, 1),
  col = c("#F8766D", "#00BFC4"),
  cex = 1
)

#Table Share of population:
Share_population <- regression_df_clean %>%
  summarise(
    total = n(),
    PC_share_11 = sum(ln_expenditure_11 < log_poverty_line) / total,
    PC_share_13 = sum(ln_expenditure_13 < log_poverty_line) / total,
    VC_share_11 = sum(
      ln_expenditure_11 < lower &
        ln_expenditure_11 >= log_poverty_line
    ) / total,
    VC_share_13 = sum(
      ln_expenditure_13 < lower &
        ln_expenditure_13 >= log_poverty_line
    ) / total,
    MC_share_11 = sum(ln_expenditure_11 > lower &
                        ln_expenditure_11 < upper$upper) / total,
    MC_share_13 = sum(ln_expenditure_13 > lower &
                        ln_expenditure_13 < upper$upper) / total,
    EC_share_11 = sum(ln_expenditure_11 > upper$upper) / total,
    EC_share_13 = sum(ln_expenditure_13 > upper$upper) / total
  )

#logaritm of expenditure:
panel_df_clean$ln_expenditure <- log(panel_df_clean$welfare)

#Dummies for social class: Poor class (PC), Vulnerable class (VC), Middle class (MC):
panel_df_clean$PC <-
  with(panel_df_clean,
       ifelse(ln_expenditure <= log_poverty_line, 1, 0))
panel_df_clean$VC <-
  with(
    panel_df_clean,
    ifelse(ln_expenditure <= lower &
             ln_expenditure > log_poverty_line, 1, 0)
  )
panel_df_clean$MC <-
  with(panel_df_clean,
       ifelse(ln_expenditure > lower & ln_expenditure <= upper$upper, 1, 0))
panel_df_clean$EC <-
  with(panel_df_clean, ifelse(ln_expenditure > upper$upper, 1, 0))
panel_df_clean$social_class <-
  with(panel_df_clean,
       ifelse(
         ln_expenditure < log_poverty_line,
         1,
         ifelse (
           ln_expenditure > log_poverty_line & ln_expenditure <= lower,
           2,
           ifelse(ln_expenditure > lower &
                    ln_expenditure <= upper$upper, 3, 4)
         )
       ))
panel_df_clean$period <-
  with(panel_df_clean, ifelse(year == 2011 | year == 2012, 1, 2))

panel_df_clean <-
  panel_df_clean %>% mutate(category_class = cut(
    social_class,
    breaks = c(0, 1, 2, 3, 4),
    labels = c("poor", "vulnerable", "middle", "elite")
  )) %>%
  mutate(category_period = cut(
    period,
    breaks = c(0, 1, 2),
    labels = c("2011", "2013")
  ))

# Plotting the different shares of income by social class:

step_1 <- panel_df_clean %>%
  group_by(period) %>%
  mutate(total_expend_by_year = sum(welfare))

share_income_11 <- step_1 %>%
  group_by(category_class) %>%
  filter(period == 1) %>%
  mutate(welfare_class = sum(welfare)) %>%
  mutate(Share_income = welfare_class / total_expend_by_year)

share_income_13 <- step_1 %>%
  group_by(category_class) %>%
  filter(period == 2) %>%
  mutate(welfare_class = sum(welfare)) %>%
  mutate(Share_income = welfare_class / total_expend_by_year)

share_df <- rbind(share_income_11, share_income_13)
rm(share_income_11, share_income_13, step_1)

ggplot(data = share_df,
       aes(x = category_class,  y = Share_income, fill = category_period)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = sprintf("%.2f", Share_income)),
            position = position_dodge(width = 0.9),
            vjust = -0.25) +
  scale_y_continuous(limits = c(0, 1)) +
  guides(fill = guide_legend(title = "Year")) +
  xlab('Social Class') +
  ylab('Share of expenditure') +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
  )


##########################################################
# Differences among groups:

## EDUCATION:

Education_by_class <- panel_df_clean %>%
  group_by(category_class) %>%
  filter(period == 1) %>%
  summarize(
    no_schooling = mean(education_level == 0),
    some = mean(education_level == 1),
    primary = mean(education_level == 2),
    Junior = mean(education_level == 3),
    secondary = mean(education_level == 4),
    post_primary = mean(education_level == 5),
    post_secondary = mean(education_level == 6),
    university = mean(education_level == 7)
  )

# Comparison between MC and PC:
MC_vs_PC <- filter(panel_df_clean, PC == 1 | MC == 1)
t.test((education_level == 0) ~ MC, data = MC_vs_PC)
t.test((education_level == 1) ~ MC, data = MC_vs_PC)
t.test((education_level == 2) ~ MC, data = MC_vs_PC)
t.test((education_level == 3) ~ MC, data = MC_vs_PC)
t.test((education_level == 4) ~ MC, data = MC_vs_PC)
t.test((education_level == 5) ~ MC, data = MC_vs_PC)
t.test((education_level == 6) ~ MC, data = MC_vs_PC)
t.test((education_level == 7) ~ MC, data = MC_vs_PC)

# Comparison between MC and VC:
MC_vs_VC <- filter(panel_df_clean, VC == 1 | MC == 1)
t.test((education_level == 0) ~ MC, data = MC_vs_VC)
t.test((education_level == 1) ~ MC, data = MC_vs_VC)
t.test((education_level == 2) ~ MC, data = MC_vs_VC)
t.test((education_level == 3) ~ MC, data = MC_vs_VC)
t.test((education_level == 4) ~ MC, data = MC_vs_VC)
t.test((education_level == 5) ~ MC, data = MC_vs_VC)
t.test((education_level == 6) ~ MC, data = MC_vs_VC)
t.test((education_level == 7) ~ MC, data = MC_vs_VC)

# Comparison between MC and EC:
MC_vs_EC <- filter(panel_df_clean, EC == 1 | MC == 1)
t.test((education_level == 0) ~ MC, data = MC_vs_EC)
t.test((education_level == 1) ~ MC, data = MC_vs_EC)
t.test((education_level == 2) ~ MC, data = MC_vs_EC)
t.test((education_level == 3) ~ MC, data = MC_vs_EC)
t.test((education_level == 4) ~ MC, data = MC_vs_EC)
t.test((education_level == 5) ~ MC, data = MC_vs_EC)
t.test((education_level == 6) ~ MC, data = MC_vs_EC)
t.test((education_level == 7) ~ MC, data = MC_vs_EC)



## LABOR ACTIVITY:
Labor_by_class <- panel_df_clean %>%
  group_by(category_class) %>%
  filter(period == 1) %>%
  summarize(
    primary = mean(sector == 1),
    secondary = mean(sector == 2),
    services = mean(sector == 3)
  )

# Comparison between MC and PC:

t.test((sector == 1) ~ MC, data = MC_vs_PC)
t.test((sector == 2) ~ MC, data = MC_vs_PC)
t.test((sector == 3) ~ MC, data = MC_vs_PC)

# Comparison between MC and VC:

t.test((sector == 1) ~ MC, data = MC_vs_VC)
t.test((sector == 2) ~ MC, data = MC_vs_VC)
t.test((sector == 3) ~ MC, data = MC_vs_VC)


# Comparison between MC and EC:

t.test((sector == 1) ~ MC, data = MC_vs_EC)
t.test((sector == 2) ~ MC, data = MC_vs_EC)
t.test((sector == 3) ~ MC, data = MC_vs_EC)


# DWELLING AND SANITATION CHARACTERISTICS:
Dwelling_by_class <- panel_df_clean %>%
  group_by(category_class) %>%
  filter(period == 1) %>%
  summarize(
    urban = mean(urban == 1),
    n_residents = mean(hsize),
    indep = mean(indep_hh == 1),
    own_residence = mean(ownership_house == 1),
    rooms = mean(rooms),
    electricity = mean(electricity == 1),
    water = mean(piped_water == 1),
    flush_toilet = mean(flush_toilet == 1),
    hand_wash = mean(hand_washing == 1),
    hh_appliances = mean(ownership_hh_appliances == 1),
    tv = mean(ownership_TV == 1),
    radio = mean(ownership_radio == 1),
    bycicle = mean(ownership_bicycle == 1),
    motorbike = mean(ownership_motorcycle == 1),
    vehicle = mean(ownership_vehicle == 1),
    mobilephone = mean(ownership_mobile_phone == 1)
  )

# Comparison between MC and PC:

t.test((urban == 1) ~ MC, data = MC_vs_PC)
t.test(hsize ~ MC, data = MC_vs_PC)
t.test((indep_hh == 1) ~ MC, data = MC_vs_PC)
t.test((ownership_house == 1) ~ MC, data = MC_vs_PC)
t.test((rooms) ~ MC, data = MC_vs_PC)
t.test((electricity == 1) ~ MC, data = MC_vs_PC)
t.test((piped_water == 1) ~ MC, data = MC_vs_PC)
t.test((flush_toilet == 1) ~ MC, data = MC_vs_PC)
t.test((hand_washing == 1) ~ MC, data = MC_vs_PC)
t.test((ownership_hh_appliances == 1) ~ MC, data = MC_vs_PC)
t.test((ownership_TV == 1) ~ MC, data = MC_vs_PC)
t.test((ownership_radio == 1) ~ MC, data = MC_vs_PC)
t.test((ownership_bicycle == 1) ~ MC, data = MC_vs_PC)
t.test((ownership_motorcycle == 1) ~ MC, data = MC_vs_PC)
t.test((ownership_vehicle == 1) ~ MC, data = MC_vs_PC)
t.test((ownership_mobile_phone == 1) ~ MC, data = MC_vs_PC)

# Comparison between MC and VC:

t.test((urban == 1) ~ MC, data = MC_vs_VC)
t.test(hsize ~ MC, data = MC_vs_VC)
t.test((indep_hh == 1) ~ MC, data = MC_vs_VC)
t.test((ownership_house == 1) ~ MC, data = MC_vs_VC)
t.test((rooms) ~ MC, data = MC_vs_VC)
t.test((electricity == 1) ~ MC, data = MC_vs_VC)
t.test((piped_water == 1) ~ MC, data = MC_vs_VC)
t.test((flush_toilet == 1) ~ MC, data = MC_vs_VC)
t.test((hand_washing == 1) ~ MC, data = MC_vs_VC)
t.test((ownership_hh_appliances == 1) ~ MC, data = MC_vs_VC)
t.test((ownership_TV == 1) ~ MC, data = MC_vs_VC)
t.test((ownership_radio == 1) ~ MC, data = MC_vs_VC)
t.test((ownership_bicycle == 1) ~ MC, data = MC_vs_VC)
t.test((ownership_motorcycle == 1) ~ MC, data = MC_vs_VC)
t.test((ownership_vehicle == 1) ~ MC, data = MC_vs_VC)
t.test((ownership_mobile_phone == 1) ~ MC, data = MC_vs_VC)

# Comparison between MC and EC:

t.test((urban == 1) ~ MC, data = MC_vs_EC)
t.test(hsize ~ MC, data = MC_vs_EC)
t.test((indep_hh == 1) ~ MC, data = MC_vs_EC)
t.test((ownership_house == 1) ~ MC, data = MC_vs_EC)
t.test((rooms) ~ MC, data = MC_vs_EC)
t.test((electricity == 1) ~ MC, data = MC_vs_EC)
t.test((piped_water == 1) ~ MC, data = MC_vs_EC)
t.test((flush_toilet == 1) ~ MC, data = MC_vs_EC)
t.test((hand_washing == 1) ~ MC, data = MC_vs_EC)
t.test((ownership_hh_appliances == 1) ~ MC, data = MC_vs_EC)
t.test((ownership_TV == 1) ~ MC, data = MC_vs_EC)
t.test((ownership_radio == 1) ~ MC, data = MC_vs_EC)
t.test((ownership_bicycle == 1) ~ MC, data = MC_vs_EC)
t.test((ownership_motorcycle == 1) ~ MC, data = MC_vs_EC)
t.test((ownership_vehicle == 1) ~ MC, data = MC_vs_EC)
t.test((ownership_mobile_phone == 1) ~ MC, data = MC_vs_EC)

## SHOCKS
Df_2011 <- panel_df_clean %>%
  filter(period == 1)

shocks_df <- regression_df_clean %>%
  dplyr::select(
    HHID_old,
    health_shock,
    death_shock,
    climate_shock,
    dweling_shock,
    economic_shock,
    agricultural_shock,
    other_shock,
    change_n_resident,
    change_n_working
  )
Df_with_shocks <- merge(Df_2011, shocks_df, by = "HHID_old")

shocks_by_class <- Df_with_shocks %>%
  group_by(category_class) %>%
  filter(period == 1) %>%
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

# Comparison between MC and PC:
MC_vs_PC_shock <- filter(Df_with_shocks, PC == 1 | MC == 1)

t.test((health_shock == 1) ~ MC, data = MC_vs_PC_shock)
t.test((death_shock == 1) ~ MC, data = MC_vs_PC_shock)
t.test((dweling_shock == 1) ~ MC, data = MC_vs_PC_shock)
t.test((agricultural_shock == 1) ~ MC, data = MC_vs_PC_shock)
t.test((economic_shock == 1) ~ MC, data = MC_vs_PC_shock)
t.test((climate_shock == 1) ~ MC, data = MC_vs_PC_shock)
t.test((other_shock == 1) ~ MC, data = MC_vs_PC_shock)
t.test(change_n_working ~ MC, data = MC_vs_PC_shock)
t.test(change_n_resident ~ MC, data = MC_vs_PC_shock)

# Comparison between MC and VC:

MC_vs_VC_shock <- filter(Df_with_shocks, VC == 1 | MC == 1)

t.test((health_shock == 1) ~ MC, data = MC_vs_VC_shock)
t.test((death_shock == 1) ~ MC, data = MC_vs_VC_shock)
t.test((dweling_shock == 1) ~ MC, data = MC_vs_VC_shock)
t.test((agricultural_shock == 1) ~ MC, data = MC_vs_VC_shock)
t.test((economic_shock == 1) ~ MC, data = MC_vs_VC_shock)
t.test((climate_shock == 1) ~ MC, data = MC_vs_VC_shock)
t.test((other_shock == 1) ~ MC, data = MC_vs_VC_shock)
t.test(change_n_working ~ MC, data = MC_vs_VC_shock)
t.test(change_n_resident ~ MC, data = MC_vs_VC_shock)

# Comparison between MC and VC:

MC_vs_EC_shock <- filter(Df_with_shocks, EC == 1 | MC == 1)

t.test((health_shock == 1) ~ MC, data = MC_vs_EC_shock)
t.test((death_shock == 1) ~ MC, data = MC_vs_EC_shock)
t.test((dweling_shock == 1) ~ MC, data = MC_vs_EC_shock)
t.test((agricultural_shock == 1) ~ MC, data = MC_vs_EC_shock)
t.test((economic_shock == 1) ~ MC, data = MC_vs_EC_shock)
t.test((climate_shock == 1) ~ MC, data = MC_vs_EC_shock)
t.test((other_shock == 1) ~ MC, data = MC_vs_EC_shock)
t.test(change_n_working ~ MC, data = MC_vs_EC_shock)
t.test(change_n_resident ~ MC, data = MC_vs_EC_shock)

# Clear namespace of varibles which are not used in future analysis steps.
# To see intermediate results change the code below accordingly.
rm(
  Education_by_class,
  Labor_by_class,
  Dwelling_by_class,
  shocks_by_class,
  MC_vs_EC,
  MC_vs_EC_shock,
  MC_vs_PC,
  MC_vs_PC_shock,
  MC_vs_VC,
  MC_vs_VC_shock,
  shocks_df,
  Df_2011,
  Df_with_shocks
)
