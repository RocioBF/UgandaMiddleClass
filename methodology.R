# In this script the classification of households according with to their vulnerability
# is made. Also, you can find the vulnerability analysis of the methodology choices. 

# Data Frame for Regressions:

regression_df <-
  dplyr::select(
    Main_Df,
    HHID_old,
    ln_expenditure_13,
    ln_expenditure_11,
    welfare_11,
    welfare_13,
    poor_11,
    poor_13,
    male_11,
    age_11,
    age_sq_11,
    education_level_11,
    marital_status_11,
    hsize_11,
    indep_hh_11,
    piped_water_11,
    flush_toilet_11,
    hand_washing_11,
    ownership_house_11,
    rooms_11,
    electricity_11,
    urban_11,
    region_11,
    d_electricity_11,
    MainJob_code_11,
    sector_11,
    labor_status_11,
    ownership_hh_appliances_11,
    ownership_TV_11,
    ownership_radio_11,
    ownership_bicycle_11,
    ownership_motorcycle_11,
    ownership_vehicle_11,
    ownership_mobile_phone_11,
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

regression_df_clean <-
  regression_df[complete.cases(regression_df), ]
rm(Main_Df, regression_df)

#####################################################################
# This section is the classification of households in four categories:
# Poor_poor
# nonpoor_poor
# poor_nonpoor
# nonpoor_nonpoor

#Exchange rate in 2005: 619.64 UGX per 1 USD
ex_rate <- 619.64

# Different poverty lines that can be used to classify the households:
poverty_line_WB <- 1.25
poverty_line <- 1.5
poverty_line_upr <- 1.7
poverty_line_BD <- 2

#Change poverty_line here to see the different results with different poverty lines:
poverty_line_month <- poverty_line * ex_rate * 30
log_poverty_line <- log(poverty_line_month)

regression_df_clean$new_poor_11 <-
  with(regression_df_clean,
       ifelse(welfare_11 <= poverty_line_month, 1, 0))

regression_df_clean$new_poor_13 <-
  with(regression_df_clean,
       ifelse(welfare_13 <= poverty_line_month, 1, 0))

regression_df_clean %>%
  filter(new_poor_11 == 1) %>%
  summarize(poor_poor = mean(new_poor_13 == 1),
            poor_rich = mean(new_poor_13 == 0))
regression_df_clean %>%
  filter(new_poor_11 == 0) %>%
  summarize(rich_poor = mean(new_poor_13 == 1),
            rich_rich = mean(new_poor_13 == 0))


#####################################################################
# This section present the different regressions:

## Linear regression:
regression_lm <-
  lm(
    ln_expenditure_11 ~ male_11 + age_11 + age_sq_11 + education_level_11 + factor(sector_11) +
      factor(marital_status_11) + hsize_11 + indep_hh_11 + piped_water_11 + flush_toilet_11 +
      hand_washing_11 + ownership_house_11 + rooms_11 + urban_11 + d_electricity_11 +
      ownership_hh_appliances_11 + ownership_TV_11 + ownership_radio_11 +
      ownership_bicycle_11 + ownership_motorcycle_11 + ownership_vehicle_11 +
      ownership_mobile_phone_11  + death_shock + health_shock + climate_shock + dweling_shock +
      economic_shock + agricultural_shock + other_shock + change_n_resident +
      change_n_working + factor(region_11),
    data = regression_df_clean
  )
# Uncomment next line to see the coefficients:
# summary(regression_lm)

# Probit regression:
regression_probit <-
  glm(
    new_poor_13 ~ male_11 + age_11 + age_sq_11 + education_level_11 +
      factor(sector_11)  + factor(marital_status_11) + hsize_11 +
      indep_hh_11 + piped_water_11 + flush_toilet_11 + hand_washing_11 +
      ownership_house_11 + rooms_11 + urban_11 + d_electricity_11 +
      ownership_hh_appliances_11 + ownership_TV_11 + ownership_radio_11 +
      ownership_bicycle_11 + ownership_motorcycle_11 +
      ownership_vehicle_11 + ownership_mobile_phone_11  +
      death_shock + health_shock + climate_shock + dweling_shock + economic_shock +
      agricultural_shock + other_shock + change_n_resident +
      change_n_working + factor(region_11),
    family = binomial(link = "probit"),
    data = regression_df_clean
  )

# Uncomment next line to see the coefficients:
# summary(regression_probit)


# Uncomment this to create the table for LaTex
# stargazer(regression_probit, regression_lm, title="Results", align=TRUE,
#           no.space=TRUE,  dep.var.labels=c("Prob. Poverty","log(Expenditure)"),
#           covariate.labels=c("Sex (male=1)","Age", "Squared Age", "Education",
#                              "HoH in secondary sector", "HoH in services",
#                              "HoH polygamously married",
#                              "HoH single", "HoH widowed", "HoH divorced",
#                              "Number of residents", "Living in independent HH",
#                              "HH has pipped water",  "HH has flush toilet",
#                              "HH has hand-washing facilities",
#                              "HH owns their dwelling", "Number of rooms",
#                              "Urban area",
#                              "HH has electricity", "HH owns appliances",
#                              "HH owns a TV", "HH owns a radio",
#                              "HH owns a bicycle","HH owns a motorcycle",
#                              "HH owns a vehicle","HH owns a mobile phone",
#                              "HH suffered a death shock",
#                              "HH suffered a health shock",
#                              "HH suffered a climate shock",
#                              "HH suffered a dwelling shock",
#                              "HH suffered a economic shock",
#                              "HH suffered an agricultural shock",
#                              "HH suffered other shock",
#                              "Change in the number of residents",
#                              "Change in the working members",
#                              "East", "North", "West"
#          ), single.row=TRUE)

# Use the Variance inflation factor Test to check for Multicol.
vif(regression_probit)
vif(regression_lm)
# Rule of thumb: VIF > 5 -> multicolinearity


# Baseline Model: Model use in the analysis:
## Linear final model:
lm_b <-
  lm(
    ln_expenditure_11 ~ male_11 + age_11 + age_sq_11 + education_level_11 + factor(sector_11) +
      factor(marital_status_11) + hsize_11 + indep_hh_11 + piped_water_11 +
      hand_washing_11 + ownership_house_11 + rooms_11 + urban_11 +
      ownership_hh_appliances_11 + ownership_TV_11 + ownership_radio_11 +
      ownership_bicycle_11 + ownership_motorcycle_11 +
      ownership_mobile_phone_11  + death_shock + health_shock + climate_shock + dweling_shock +
      economic_shock + agricultural_shock + other_shock + change_n_resident +
      change_n_working + factor(region_11),
    data = regression_df_clean
  )

# Uncomment next line to see the coefficients:
# summary(lm_b)


## Probit final model:
probit_b <-
  glm(
    new_poor_13 ~ male_11 + age_11 + age_sq_11 + education_level_11 +
      factor(sector_11)  + factor(marital_status_11) + hsize_11 +
      indep_hh_11 + piped_water_11 + hand_washing_11 +
      ownership_house_11 + rooms_11 + urban_11 +
      ownership_hh_appliances_11 + ownership_TV_11 + ownership_radio_11 +
      ownership_bicycle_11 + ownership_motorcycle_11 +
      ownership_mobile_phone_11  +
      death_shock + health_shock + climate_shock + dweling_shock + economic_shock +
      agricultural_shock + other_shock + change_n_resident +
      change_n_working + factor(region_11),
    family = binomial(link = "probit"),
    data = regression_df_clean
  )

# Uncomment next line to see the coefficients:
#summary(probit_b)

# Use the Variance inflation factor Test to check for Multicol.
vif(lm_b)
vif(probit_b)
# Rule of thumb: VIF > 5 -> multicolinearity

# Uncomment this to create the table for LaTex
# stargazer(probit_b, lm_b, title="Results", align=TRUE,
#           no.space=TRUE,  dep.var.labels=c("Prob. Poverty","log(Expenditure)"),
#           covariate.labels=c("Sex (male=1)","Age", "Squared Age", "Education",
#                              "HoH in secondary sector", "HoH in services",
#                              "HoH polygamously married",
#                              "HoH single", "HoH widowed", "HoH divorced",
#                              "Number of residents", "Living in independent HH",
#                               "HH has pipped water",
#                              "HH has hand-washing facilities",
#                              "HH owns their dwelling",
#                              "Number of rooms",
#                              "Urban area",
#                             "HH owns appliances",
#                              "HH owns a TV", "HH owns a radio",
#                              "HH owns a bicycle","HH owns a motorcycle",
#                              "HH owns a mobile phone",
#                              "HH suffered a death shock",
#                             "HH suffered a health shock",
#                             "HH suffered a climate shock",
#                              "HH suffered a dwelling shock",
#                              "HH suffered a economic shock",
#                              "HH suffered an agricultural shock",
#                              "HH suffered other shock",
#                              "Change in the number of residents",
#                              "Change in the working members",
#                             "East", "North", "West"
#           ), single.row=TRUE)

#####################################################################
# In this section, I match the estimated prob with the estimated exp 
# using a local linear regression approach:

## Getting the needed values:
prob_hut <- probit_b[["fitted.values"]]
ln_exp_hut <- lm_b[["fitted.values"]]
actual_ln_exp <- regression_df_clean[["ln_expenditure_11"]]
actual_exp <- regression_df_clean[["welfare_11"]]

## Creating df with the needed values:
estimations <-
  cbind(prob_hut, actual_exp, ln_exp_hut, actual_ln_exp)
estimations <- data.frame(estimations)
estimations <- mutate(estimations, exp_hut = exp(ln_exp_hut))

## Setting the Kernel distribution and the bandwitch for the 
## local linear regression
kernel <- gaussK
bw <- bw.nrd0(prob_hut) #Silverman's ‘rule of thumb’

# local linear estimations:
fit <-
  locpol(
    ln_exp_hut ~ prob_hut,
    estimations,
    weig = rep(1, nrow(estimations)),
    bw = bw,
    kernel,
    deg = 1,
    xeval = NULL,
    xevalLen = 100
  )

# lower bound for the MC: 
lwr_bound <-
  locpol(
    ln_exp_hut ~ prob_hut,
    estimations,
    weig = rep(1, nrow(estimations)),
    kernel,
    bw = bw,
    deg = 1,
    xeval = c(0.1)
  )

lower <- lwr_bound$lpFit$ln_exp_hut


monthly_lwr_PPP <- exp(lower) / ex_rate
daily_lwr_PPP <- monthly_lwr_PPP / 30


# Upper bound for the MC: 
upper <- estimations %>%
  filter(prob_hut < 0.01) %>%
  summarize(upper = mean(ln_exp_hut))

monthly_upr_PPP <- exp(upper$upper) / ex_rate
daily_upr_PPP <- monthly_upr_PPP / 30




# PLOTS FOR THE LOCAL LINEAR ESTIMATION RESULTS:
fit_df <- fit$lpFit
fit_df <- as.data.frame(fit_df)

# Plotting the Local Linear result:
ggplot(fit_df, aes(prob_hut, ln_exp_hut)) + geom_point() +
  geom_vline(aes(xintercept = 0.10, color = "10% "), size = 1) +
  geom_hline(aes(yintercept = 10.7, color = "$2.4"), size = 1) +
  xlab("risk of poverty") +
  ylab("household expenditure (in logs)") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
  )

## Data and polynomial fit:
ggplot(estimations, aes(x = prob_hut, y = ln_exp_hut)) + geom_point(shape =
                                                                      3) +
  geom_line(
    data = fit_df,
    aes(x = prob_hut, y = ln_exp_hut),
    color = "#F8766D",
    size = 1
  ) +
  xlab('risk of poverty') +
  ylab('household expenditure (in logs)') +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
  )

## Density:
ggplot(fit_df, aes(y = xDen, x = prob_hut)) + geom_line() +
  xlab('risk of poverty') +
  ylab('Density') +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
  )

## Variance:
ggplot(fit_df, aes(y = var, x = prob_hut)) + geom_line() +
  xlab('risk of poverty') +
  ylab('variance') +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
  )


#####################################################################
# In this section I classify households in four groups: 
# poor, vulnerable, middle class and elite. 
# Tables for the population distirbution and transition to poverty 
# can be found also in this section.

#Dummies for Social Class:
regression_df_clean <- regression_df_clean %>%
  mutate(
    Vulnerable_11 = ifelse(
      ln_expenditure_11 < lower &
        ln_expenditure_11 >= log_poverty_line,
      1,
      0
    )
  ) %>%
  mutate(
    Vulnerable_13 = ifelse(
      ln_expenditure_13 < lower &
        ln_expenditure_13 >= log_poverty_line,
      1,
      0
    )
  ) %>%
  mutate(Middle_11 = ifelse(ln_expenditure_11 > lower &
                              ln_expenditure_11 < upper$upper, 1, 0)) %>%
  mutate(Middle_13 = ifelse(ln_expenditure_13 > lower &
                              ln_expenditure_13 < upper$upper, 1, 0)) %>%
  mutate(Elite_11 = ifelse(ln_expenditure_11 > upper$upper, 1, 0)) %>%
  mutate(Elite_13 = ifelse(ln_expenditure_13 > upper$upper, 1, 0))


# Transition to poverty:
## Households were poor in 2011:
regression_df_clean %>%
  filter(new_poor_11 == 1) %>%
  summarize(
    poor = mean(new_poor_13 == 1),
    vuln = mean(Vulnerable_13 == 1),
    middle = mean(Middle_13 == 1),
    elite = mean(Elite_13 == 1)
  )
## Households were non poor in 2011:
regression_df_clean %>%
  filter(new_poor_11 == 0) %>%
  summarize(
    poor = mean(new_poor_13 == 1),
    vuln = mean(Vulnerable_13 == 1),
    middle = mean(Middle_13 == 1),
    elite = mean(Elite_13 == 1)
  )

#Movements across classes:
regression_df_clean %>%
  filter(new_poor_11 == 1) %>%
  summarize(
    poor_poor = mean(new_poor_13 == 1),
    poor_vulnerable = mean(Vulnerable_13 == 1),
    poor_middle = mean(Middle_13 == 1),
    poor__elite = mean(Elite_13 == 1)
  )
regression_df_clean %>%
  filter(Vulnerable_11 == 1) %>%
  summarize(
    vulnerable_poor = mean(new_poor_13 == 1),
    vulnerable_vulnerable = mean(Vulnerable_13 == 1),
    vulnerable_middle = mean(Middle_13 == 1),
    vulnerable__elite = mean(Elite_13 == 1)
  )
regression_df_clean %>%
  filter(Middle_11 == 1) %>%
  summarize(
    middle_poor = mean(new_poor_13 == 1),
    middle_vulnerable = mean(Vulnerable_13 == 1),
    middle_middle = mean(Middle_13 == 1),
    middle_elite = mean(Elite_13 == 1)
  )
regression_df_clean %>%
  filter(Elite_11 == 1) %>%
  summarize(
    elite_poor = mean(new_poor_13 == 1),
    elite_vulnerable = mean(Vulnerable_13 == 1),
    elite_middle = mean(Middle_13 == 1),
    elite_elite = mean(Elite_13 == 1)
  )



#####################################################################
# In this section there is the sensibility analysis of the metholodogy
# choices:

## 1. PROBABILITY CHOICE: 
### Probabilities possible: 0.03, 0.05, 0.1, 0.15, 0.20, 0.30, 0.40
lwr_prob <-
  locpol(
    ln_exp_hut ~ prob_hut,
    estimations,
    bw = 0.05,
    xeval = c(0.03, 0.05, 0.1, 0.15, 0.20, 0.30, 0.40)
  )
results_lwr <- lwr_prob$lpFit


# Middle class size and growth when prob = 0.03
mc_11_1 <-
  mean(
    regression_df_clean$ln_expenditure_11 > results_lwr[1, "ln_exp_hut"]
    &
      regression_df_clean$ln_expenditure_11 < upper$upper
  )
mc_13_1 <-
  mean(
    regression_df_clean$ln_expenditure_13 > results_lwr[1, "ln_exp_hut"]
    &
      regression_df_clean$ln_expenditure_13 < upper$upper
  )
growth_mc_1 <- mc_13_1 - mc_11_1

daily_bound_1 <- exp(results_lwr[1, "ln_exp_hut"]) / ex_rate / 30

# Middle class size and growth whenprob = 0.05
mc_11_2 <-
  mean(
    regression_df_clean$ln_expenditure_11 > results_lwr[2, "ln_exp_hut"]
    &
      regression_df_clean$ln_expenditure_11 < upper$upper
  )
mc_13_2 <-
  mean(
    regression_df_clean$ln_expenditure_13 > results_lwr[2, "ln_exp_hut"]
    &
      regression_df_clean$ln_expenditure_13 < upper$upper
  )
growth_mc_2 <- mc_13_2 - mc_11_2

daily_bound_2 <- exp(results_lwr[2, "ln_exp_hut"]) / ex_rate / 30

#prob = 0.1
mc_11_real <- mean(
  regression_df_clean$ln_expenditure_11 > lower
  &
    regression_df_clean$ln_expenditure_11 < upper$upper
)
mc_13_real <- mean(
  regression_df_clean$ln_expenditure_13 > lower
  &
    regression_df_clean$ln_expenditure_13 < upper$upper
)
growth_mc_real <- mc_13_real - mc_11_real




# Middle class size and growth when prob = 0.15
lower_3 <- estimations %>%
  filter(prob_hut > 0.14 & prob_hut < 0.16) %>%
  summarize(lower = mean(ln_exp_hut))
mc_11_3 <-
  mean(
    regression_df_clean$ln_expenditure_11 > results_lwr[4, "ln_exp_hut"]
    &
      regression_df_clean$ln_expenditure_11 < upper$upper
  )
mc_13_3 <-
  mean(
    regression_df_clean$ln_expenditure_13 > results_lwr[4, "ln_exp_hut"]
    &
      regression_df_clean$ln_expenditure_13 < upper$upper
  )
growth_mc_3 <- mc_13_3 - mc_11_3

daily_bound_3 <- exp(results_lwr[4, "ln_exp_hut"]) / ex_rate / 30

# Middle class size and growth when prob = 0.2
lower_4 <- estimations %>%
  filter(prob_hut > 0.19 & prob_hut < 0.21) %>%
  summarize(lower = mean(ln_exp_hut))
mc_11_4 <-
  mean(
    regression_df_clean$ln_expenditure_11 > results_lwr[5, "ln_exp_hut"]
    &
      regression_df_clean$ln_expenditure_11 < upper$upper
  )
mc_13_4 <-
  mean(
    regression_df_clean$ln_expenditure_13 > results_lwr[5, "ln_exp_hut"]
    &
      regression_df_clean$ln_expenditure_13 < upper$upper
  )
growth_mc_4 <- mc_13_4 - mc_11_4

daily_bound_4 <- exp(results_lwr[5, "ln_exp_hut"]) / ex_rate / 30

# Middle class size and growth when prob = 0.3
lower_5 <- estimations %>%
  filter(prob_hut > 0.29 & prob_hut < 0.31) %>%
  summarize(lower = mean(ln_exp_hut))
mc_11_5 <-
  mean(
    regression_df_clean$ln_expenditure_11 > results_lwr[6, "ln_exp_hut"]
    &
      regression_df_clean$ln_expenditure_11 < upper$upper
  )
mc_13_5 <-
  mean(
    regression_df_clean$ln_expenditure_13 > results_lwr[6, "ln_exp_hut"]
    &
      regression_df_clean$ln_expenditure_13 < upper$upper
  )
growth_mc_5 <- mc_13_5 - mc_11_5

daily_bound_5 <- exp(results_lwr[6, "ln_exp_hut"]) / ex_rate / 30

# Middle class size and growth when prob = 0.4
lower_6 <- estimations %>%
  filter(prob_hut > 0.39 & prob_hut < 0.41) %>%
  summarize(lower = mean(ln_exp_hut))
mc_11_6 <-
  mean(
    regression_df_clean$ln_expenditure_11 > results_lwr[7, "ln_exp_hut"]
    &
      regression_df_clean$ln_expenditure_11 < upper$upper
  )
mc_13_6 <-
  mean(
    regression_df_clean$ln_expenditure_13 > results_lwr[7, "ln_exp_hut"]
    &
      regression_df_clean$ln_expenditure_13 < upper$upper
  )
growth_mc_6 <- mc_13_6 - mc_11_6

daily_bound_6 <- exp(results_lwr[7, "ln_exp_hut"]) / ex_rate / 30


## UPPER BOUND: 
# possible upper bounds of the risk of poverty: 0, 0.001, 0.01 
# 0.01 is the case consider in the baseline model so I do not contemplate it here

# Middle class size and growth when no upper bound
mc_11_a <- mean(regression_df_clean$ln_expenditure_11 > lower)
mc_13_a <- mean(regression_df_clean$ln_expenditure_13 > lower)
growth_mc_a <- mc_13_a - mc_11_a

# Middle class size and growth when prob.=0.001
upper_b <- estimations %>%
  filter(prob_hut < 0.001) %>%
  summarize(upper = mean(ln_exp_hut))
upr_bound_day_b <- exp(upper_b$upper) / ex_rate / 30

mc_11_b <- mean(
  regression_df_clean$ln_expenditure_11 > lower
  &
    regression_df_clean$ln_expenditure_11 < upper_b$upper
)
mc_13_b <- mean(
  regression_df_clean$ln_expenditure_13 > lower
  &
    regression_df_clean$ln_expenditure_13 < upper_b$upper
)
growth_mc_b <- mc_13_b - mc_11_b

upr_bound_day <- exp(upper$upper) / ex_rate / 30
mc_11_c <- mean(
  regression_df_clean$ln_expenditure_11 > lower
  &
    regression_df_clean$ln_expenditure_11 < upper$upper
)
mc_13_c <- mean(
  regression_df_clean$ln_expenditure_13 > lower
  &
    regression_df_clean$ln_expenditure_13 < upper$upper
)

growth_mc_c <- mc_13_c - mc_11_c

#####################################################################
# In this section there is a robustness check using the Principal 
# Component Analysis for the variables of hh living standards and shocks

# PRINCIPAL COMPONENT ANALYSIS
## For Living standards and Sanitation:
indicators_sanitation <-
  dplyr::select(
    regression_df_clean,
    piped_water_11,
    hand_washing_11,
    ownership_hh_appliances_11,
    ownership_TV_11,
    ownership_radio_11,
    ownership_bicycle_11,
    ownership_motorcycle_11,
    ownership_mobile_phone_11
  )

pca_sanitation <-
  prcomp(indicators_sanitation, center = TRUE, scale. = TRUE)

# Uncomment here to see the results from the PCA:
# summary(pca_sanitation)
# str(pca_sanitation)
# biplot(pca_sanitation)
# plot(pca_sanitation)

## For Shocks:
indicators_shock <-
  dplyr::select(
    regression_df_clean,
    health_shock,
    death_shock,
    climate_shock,
    dweling_shock,
    economic_shock,
    agricultural_shock,
    other_shock
  )

pca_shock <- prcomp(indicators_shock, center = TRUE, scale. = TRUE)

# Uncomment to see the results from the PCA
# summary(pca_shock)
# str(pca_shock)
# biplot(pca_shock)
# plot(pca_shock)

## Regressions with PCAs: 
### First, only considering the PCA for living standards and sanitation:
lm_pca_sanitation <-
  lm(
    ln_expenditure_11 ~ male_11 + age_11 + age_sq_11 + education_level_11 +
      factor(sector_11) + factor(marital_status_11) + rooms_11 + ownership_house_11 + pca_sanitation$x[, 1] + hsize_11
    + urban_11 + death_shock + health_shock + climate_shock + dweling_shock +
      economic_shock + agricultural_shock + other_shock + change_n_resident +
      change_n_working + factor(region_11),
    data = regression_df_clean
  )

probit_pca_sanitation <-
  glm(
    new_poor_13 ~ male_11 + age_11 + age_sq_11 + education_level_11 +
      factor(sector_11) + factor(marital_status_11) + rooms_11 + ownership_house_11 +
      pca_sanitation$x[, 1] + hsize_11
    + urban_11 + death_shock + health_shock + climate_shock + dweling_shock +
      economic_shock + agricultural_shock + other_shock + change_n_resident +
      change_n_working + factor(region_11),
    family = binomial(link = "probit"),
    data = regression_df_clean
  )

# Uncomment to create the tables for LaTex
# stargazer(probit_pca_sanitation, lm_pca_sanitation, title="Results (PCA with standards of living)", align=TRUE,
#                     no.space=TRUE,  dep.var.labels=c("Prob. Poverty","log(Expenditure)"),
#                     covariate.labels=c("Sex (male=1)","Age", "Squared Age", "Education",
#                                        "HoH in secondary sector", "HoH in services",
#                                        "HoH polygamously married",
#                                        "HoH single", "HoH widowed", "HoH divorced",
#                                        "Number of rooms", "HH owns their dwelling",
#                                        "PCA living standard", "number of residents",
#                                        "Urban area",
#                                        "HH suffered a death shock",
#                                       "HH suffered a health shock",
#                                       "HH suffered a climate shock",
#                                        "HH suffered a dwelling shock",
#                                        "HH suffered a economic shock",
#                                        "HH suffered an agricultural shock",
#                                        "HH suffered other shock",
#                                        "Change in the number of residents",
#                                        "Change in the working members",
#                                       "East", "North", "West"
#                     ), single.row=TRUE)


# matching the estimated prob with the estimated exp:

prob_hut_pca_1 <- probit_pca_sanitation[["fitted.values"]]
ln_exp_hut_pca_1 <- lm_pca_sanitation[["fitted.values"]]


estimations_pca_1 <- cbind(prob_hut_pca_1, ln_exp_hut_pca_1)
estimations_pca_1 <- data.frame(estimations_pca_1)

# Calculating the lower bound of the MC:
lwr_bound_pca_1 <-
  locpol(
    ln_exp_hut_pca_1 ~ prob_hut_pca_1,
    estimations_pca_1,
    weig = rep(1, nrow(estimations)),
    kernel,
    bw = bw.nrd0(prob_hut_pca_1),
    deg = 1,
    xeval = c(0.1)
  )


lower_pca_1 <- lwr_bound_pca_1$lpFit$ln_exp_hut_pca_1


monthly_lwr_PPP_pca_1 <- exp(lower_pca_1) / ex_rate
daily_lwr_PPP_pca_1 <- monthly_lwr_PPP_pca_1 / 30

### Second, regressions considering both PCA with standard of living and shokcs:

lm_pca <-
  lm(
    ln_expenditure_11 ~ male_11 + age_11 + age_sq_11 + education_level_11 +
      factor(sector_11)  + factor(marital_status_11) + rooms_11 + ownership_house_11 +
      pca_sanitation$x[, 1] + hsize_11
    + urban_11 + pca_shock$x[, 1] +change_n_resident +
      change_n_working + factor(region_11),
    data = regression_df_clean
  )

probit_pca <-
  glm(
    new_poor_13 ~ male_11 + age_11 + age_sq_11 + education_level_11 +
      factor(sector_11) + factor(marital_status_11) + rooms_11 +
      ownership_house_11 + pca_sanitation$x[, 1] + hsize_11
    + urban_11 + pca_shock$x[, 1] +change_n_resident +
      change_n_working + factor(region_11),
    family = binomial(link = "probit"),
    data = regression_df_clean
  )

# Uncomment to create the tables for LaTex
# stargazer(probit_pca, lm_pca, title="Results (PCAs)", align=TRUE,
#           no.space=TRUE,  dep.var.labels=c("Prob. Poverty","log(Expenditure)"),
#           covariate.labels=c("Sex (male=1)","Age", "Squared Age", "Education",
#                              "HoH in secondary sector", "HoH in services",
#                              "HoH polygamously married",
#                              "HoH single", "HoH widowed", "HoH divorced",
#                              "Number of rooms", "HH owns their dwelling",
#                              "PCA living standard", "number of residents",
#                              "Urban area",
#                              "PCA shocks",
#                              "Change in the number of residents",
#                              "Change in the working members",
#                              "East", "North", "West"
#           ), single.row=TRUE)


# matching the estimated prob with the estimated exp:

prob_hut_pca_2 <- probit_pca[["fitted.values"]]
ln_exp_hut_pca_2 <- lm_pca[["fitted.values"]]


estimations_pca_2 <- cbind(prob_hut_pca_2, ln_exp_hut_pca_2)
estimations_pca_2 <- data.frame(estimations_pca_2)

# Calculating the lower bound for the MC:
lwr_bound_pca_2 <-
  locpol(
    ln_exp_hut_pca_2 ~ prob_hut_pca_2,
    estimations_pca_2,
    weig = rep(1, nrow(estimations)),
    kernel,
    bw = bw.nrd0(prob_hut_pca_2),
    deg = 1,
    xeval = c(0.1)
  )

lower_pca_2 <- lwr_bound_pca_2$lpFit$ln_exp_hut_pca_2


monthly_lwr_PPP_pca_2 <- exp(lower_pca_2) / ex_rate
daily_lwr_PPP_pca_2 <- monthly_lwr_PPP_pca_2 / 30


#####################################################################
# In this section I made a robustness check taking into account 
# different model specifications:

#MODEL SPECIFICATIONS:
## Model specification (1): Base
## Model specification (2): (1) + controls for standard of living
## Model specification (3): (2) + variables measuring changes
###

## Model specification (1): Base
lm_1 <-
  lm(
    ln_expenditure_11 ~ male_11 + age_11 + age_sq_11 + education_level_11 +
      factor(sector_11) + factor(marital_status_11) + factor(region_11),
    data = regression_df_clean
  )
# Uncomment next line to see the coefficients:
# summary(lm_1)

probit_1 <-
  glm(
    new_poor_13 ~ male_11 + age_11 + age_sq_11 + education_level_11 +
      factor(sector_11) + factor(marital_status_11) + factor(region_11),
    family = binomial(link = "probit"),
    data = regression_df_clean
  )
# Uncomment next line to see the coefficients:
# summary(probit_1)

# matching the estimated prob with the estimated exp:
prob_hut_1 <- probit_1[["fitted.values"]]
ln_exp_hut_1 <- lm_1[["fitted.values"]]


estimations_1 <- cbind(prob_hut_1, ln_exp_hut_1)
estimations_1 <- data.frame(estimations_1)

# Calculating lower bound for the MC:
lwr_bound_1 <-
  locpol(
    ln_exp_hut_1 ~ prob_hut_1,
    estimations_1,
    weig = rep(1, nrow(estimations)),
    kernel,
    bw = bw.nrd0(prob_hut_1),
    deg = 1,
    xeval = c(0.1)
  )

lwr_bound_1$lpFit
lower_1 <- lwr_bound_1$lpFit$ln_exp_hut_1

monthly_lwr_PPP_1 <- exp(lower_1) / ex_rate
daily_lwr_PPP_1 <- monthly_lwr_PPP_1 / 30

## Model specification (2): (1) + controls for standard of living
lm_2 <-
  lm(
    ln_expenditure_11 ~ male_11 + age_11 + age_sq_11 + education_level_11 + factor(sector_11) +
      factor(marital_status_11) + hsize_11 + indep_hh_11 + piped_water_11  +
      hand_washing_11 + ownership_house_11 + rooms_11 + urban_11 +
      ownership_hh_appliances_11 + ownership_TV_11 + ownership_radio_11 +
      ownership_bicycle_11 + ownership_motorcycle_11 +
      ownership_mobile_phone_11 + factor(region_11),
    data = regression_df_clean
  )
# Uncomment next line to see the coefficients:
#summary(lm_2)

probit_2 <-
  glm(
    new_poor_13 ~ male_11 + age_11 + age_sq_11 + education_level_11 +
      factor(sector_11)  + factor(marital_status_11) + hsize_11 +
      indep_hh_11 + piped_water_11 + hand_washing_11 +
      ownership_house_11 + rooms_11 + urban_11 +
      ownership_hh_appliances_11 + ownership_TV_11 + ownership_radio_11 +
      ownership_bicycle_11 + ownership_motorcycle_11 +
      ownership_mobile_phone_11  + factor(region_11),
    family = binomial(link = "probit"),
    data = regression_df_clean
  )
# Uncomment next line to see the coefficients:
#summary(probit_2)

# matching the estimated prob with the estimated exp:
prob_hut_2 <- probit_2[["fitted.values"]]
ln_exp_hut_2 <- lm_2[["fitted.values"]]


estimations_2 <- cbind(prob_hut_2, ln_exp_hut_2)
estimations_2 <- data.frame(estimations_2)


# Calculating lower bound for the MC:
lwr_bound_2 <-
  locpol(
    ln_exp_hut_2 ~ prob_hut_2,
    estimations_2,
    weig = rep(1, nrow(estimations)),
    kernel,
    bw = bw.nrd0(prob_hut_2),
    deg = 1,
    xeval = c(0.1)
  )

lower_2 <- lwr_bound_2$lpFit$ln_exp_hut_2


monthly_lwr_PPP_2 <- exp(lower_2) / ex_rate
daily_lwr_PPP_2 <- monthly_lwr_PPP_2 / 30

## Model specification (3): (2) + variables measuring changes
lm_3 <-
  lm(
    ln_expenditure_11 ~ male_11 + age_11 + age_sq_11 + education_level_11 + factor(sector_11) +
      factor(marital_status_11) + hsize_11 + indep_hh_11 + piped_water_11 +
      hand_washing_11 + ownership_house_11 + rooms_11 + urban_11 +
      ownership_hh_appliances_11 + ownership_TV_11 + ownership_radio_11 +
      ownership_bicycle_11 + ownership_motorcycle_11 +
      ownership_mobile_phone_11 + change_n_resident +
      change_n_working + factor(region_11),
    data = regression_df_clean
  )
# Uncomment next line to see the coefficients:
#summary(lm_3)

probit_3 <-
  glm(
    new_poor_13 ~ male_11 + age_11 + age_sq_11 + education_level_11 +
      factor(sector_11)  + factor(marital_status_11) + hsize_11 +
      indep_hh_11 + piped_water_11 + hand_washing_11 +
      ownership_house_11 + rooms_11 + urban_11 +
      ownership_hh_appliances_11 + ownership_TV_11 + ownership_radio_11 +
      ownership_bicycle_11 + ownership_motorcycle_11 +
      ownership_mobile_phone_11  +
      change_n_resident + change_n_working + factor(region_11),
    family = binomial(link = "probit"),
    data = regression_df_clean
  )
# Uncomment next line to see the coefficients:
#summary(probit_3)

# matching the estimated prob with the estimated exp:
prob_hut_3 <- probit_3[["fitted.values"]]
ln_exp_hut_3 <- lm_3[["fitted.values"]]


estimations_3 <- cbind(prob_hut_3, ln_exp_hut_3)
estimations_3 <- data.frame(estimations_3)

# Calculating lower bound for the MC:
lwr_bound_3 <-
  locpol(
    ln_exp_hut_3 ~ prob_hut_3,
    estimations_3,
    weig = rep(1, nrow(estimations)),
    kernel,
    bw =  bw.nrd0(prob_hut_3),
    deg = 1,
    xeval = c(0.1)
  )

lower_3 <- lwr_bound_3$lpFit$ln_exp_hut_3


monthly_lwr_PPP_3 <- exp(lower_3) / ex_rate
daily_lwr_PPP_3 <- monthly_lwr_PPP_3 / 30

# Uncomment to create the tables for LaTex
# stargazer(lm_1, lm_2, lm_3, lm_b, title="Model specification (linear regression)", align=FALSE,
#           no.space=TRUE,  dep.var.labels=c("log(Expenditure)", "log(Expenditure)", "log(Expenditure)","log(Expenditure)"),
#           covariate.labels=c("Sex (male=1)","Age", "Squared Age", "Education",
#                              "HoH in secondary sector", "HoH in services",
#                              "HoH polygamously married",
#                              "HoH single", "HoH widowed", "HoH divorced",
#                              "Number of residents", "Living in independent HH",
#                               "HH has pipped water",
#                              "HH has hand-washing facilities",
#                              "HH owns their dwelling",
#                              "Number of rooms",
#                              "Urban area",
#                             "HH owns appliances",
#                              "HH owns a TV", "HH owns a radio",
#                              "HH owns a bicycle","HH owns a motorcycle",
#                              "HH owns a mobile phone",
#                              "HH suffered a death shock",
#                             "HH suffered a health shock",
#                             "HH suffered a climate shock",
#                              "HH suffered a dwelling shock",
#                              "HH suffered a economic shock",
#                              "HH suffered an agricultural shock",
#                              "HH suffered other shock",
#                              "Change in the number of residents",
#                              "Change in the working members",
#                             "East", "North", "West"
#           ), single.row=TRUE)

# Uncomment to create the tables for LaTex
# stargazer(probit_1, probit_2, probit_3, probit_b, title="Model specification (probit)", align=FALSE,
#           no.space=TRUE,  dep.var.labels=c("Prob. Poverty", "Prob. Poverty", "Prob. Poverty","Prob. Poverty"),
#           covariate.labels=c("Sex (male=1)","Age", "Squared Age", "Education",
#                              "HoH in secondary sector", "HoH in services",
#                              "HoH polygamously married",
#                              "HoH single", "HoH widowed", "HoH divorced",
#                              "Number of residents", "Living in independent HH",
#                              "HH has pipped water",
#                              "HH has hand-washing facilities",
#                              "HH owns their dwelling",
#                              "Number of rooms",
#                              "Urban area",
#                              "HH owns appliances",
#                              "HH owns a TV", "HH owns a radio",
#                              "HH owns a bicycle","HH owns a motorcycle",
#                              "HH owns a mobile phone",
#                              "HH suffered a death shock",
#                              "HH suffered a health shock",
#                              "HH suffered a climate shock",
#                              "HH suffered a dwelling shock",
#                              "HH suffered a economic shock",
#                              "HH suffered an agricultural shock",
#                              "HH suffered other shock",
#                              "Change in the number of residents",
#                              "Change in the working members",
#                              "East", "North", "West"
#           ), single.row=TRUE)


#####################################################################
# Clear namespace of varibles which are not used in future analysis steps.
# To see intermediate results change the code below accordingly.
rm(
  estimations_1,
  estimations_2,
  estimations_3,
  estimations_pca_1,
  estimations_pca_2,
  fit,
  lm_1,
  lm_2,
  lm_3,
  lm_b,
  lm_pca_sanitation,
  lm_pca,
  lower_4,
  lower_5,
  lower_6,
  lwr_bound,
  lwr_bound_1,
  lwr_bound_2,
  lwr_bound_3,
  lwr_prob,
  probit_1,
  probit_2,
  probit_3,
  probit_b,
  probit_pca,
  probit_pca_sanitation,
  regression_lm,
  regression_probit,
  results_lwr,
  pca_sanitation,
  pca_shock,
  indicators_sanitation,
  indicators_shock,
  lwr_bound_pca_1,
  lwr_bound_pca_2
)
