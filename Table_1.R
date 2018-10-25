#TABLE : Share of population in Uganda using different methods

## For the wave 2011:
regression_df_clean$daily_consump_11 <-
  regression_df_clean$welfare_11 / 30
regression_df_clean$annual_consump_11 <-
  regression_df_clean$welfare_11 * 12

regression_df_clean$daily_c_USD_11 <-
  regression_df_clean$daily_consump_11 / ex_rate
table_absolute_methods_2011 <- regression_df_clean %>%
  summarize(
    total = n(),
    share_BD = sum(daily_consump_11 >= 2 * ex_rate &
                     daily_consump_11 <= 10 * ex_rate) / total * 100,
    share_AfBD = sum(daily_consump_11 >= 2 * ex_rate &
                       daily_consump_11 <= 20 * ex_rate) / total * 100,
    share_Ravallion = sum(daily_consump_11 >= 2 * ex_rate &
                            daily_consump_11 <= 13 * ex_rate) / total * 100,
    share_Kharas = sum(daily_consump_11 >= 10 * ex_rate &
                         daily_consump_11 <= 100 * ex_rate) / total * 100,
    share_Kohut = sum(daily_consump_11 >= 10 * ex_rate) / total *
      100,
    share_CN = sum(daily_consump_11 >= 16 * ex_rate &
                     daily_consump_11 <= 82 * ex_rate) / total * 100,
    share_VP = sum(daily_consump_11 >= 8 * ex_rate &
                     daily_consump_11 <= 58 * ex_rate) / total * 100,
    share_Nomura = sum(
      annual_consump_11 >= 6000 * ex_rate &
        annual_consump_11 <= 25000 * ex_rate
    ) / total * 100,
    share_Bhalla = sum(annual_consump_11 >= 3900 * ex_rate) / total *
      100
  )

table_relative_methods_2011 <- regression_df_clean %>%
  summarize(
    total = n(),
    mean = mean(daily_consump_11),
    q_20 = quantile(daily_consump_11, prob = c(0.20)),
    q_30 = quantile(daily_consump_11, prob = c(0.30)),
    q_40 = quantile(daily_consump_11, prob = c(0.40)),
    q_50 = quantile(daily_consump_11, prob = c(0.50)),
    q_60 = quantile(daily_consump_11, prob = c(0.60)),
    q_70 = quantile(daily_consump_11, prob = c(0.70)),
    q_75 = quantile(daily_consump_11, prob = c(0.75)),
    q_80 = quantile(daily_consump_11, prob = c(0.80)),
    q_90 = quantile(daily_consump_11, prob = c(0.90)),
    q_95 = quantile(daily_consump_11, prob = c(0.95)),
    share_B_GP = sum(0.75 * q_50 <= daily_consump_11 &
                       daily_consump_11 <= 1.25 * q_50) / total * 100,
    share_BB = sum(0.6 * q_50 <= daily_consump_11 &
                     daily_consump_11 <= 2.25 * q_50) / total * 100,
    share_DH = sum(0.5 * q_50 <= daily_consump_11 &
                     daily_consump_11 <= 1.5 * q_50) / total * 100,
    share_Wolfson = sum(0.75 * q_50 <= daily_consump_11 &
                          daily_consump_11 <= 1.25 * q_50) / total * 100,
    share_L_F = sum(q_40 <= daily_consump_11 &
                      daily_consump_11 <= q_70) / total * 100,
    share_Easterly = sum(q_20 <= daily_consump_11 &
                           daily_consump_11 <= q_80) / total * 100,
    share_Palma = sum(q_50 <= daily_consump_11 &
                        daily_consump_11 <= q_90) / total * 100,
    share_Solimano = sum(q_30 <= daily_consump_11 &
                           daily_consump_11 <= q_90) / total * 100,
    share_Partridge = sum(q_50 <= daily_consump_11 &
                            daily_consump_11 <= q_60) / total * 100,
    share_AP = sum(q_50 <= daily_consump_11 &
                     daily_consump_11 <= q_80) / total * 100,
    share_Birdsall = sum(10 * ex_rate <= daily_consump_11 &
                           daily_consump_11 <= q_95) / total * 100
  )

## For the wage 2013:
regression_df_clean$daily_consump_13 <-
  regression_df_clean$welfare_13 / 30
regression_df_clean$annual_consump_13 <-
  regression_df_clean$welfare_13 * 12


regression_df_clean$daily_c_USD_13 <-
  regression_df_clean$daily_consump_13 / ex_rate
table_absolute_methods_2013 <- regression_df_clean %>%
  summarize(
    total = n(),
    share_BD = sum(daily_consump_13 >= 2 * ex_rate &
                     daily_consump_13 <= 10 * ex_rate) / total * 100,
    share_AfBD = sum(daily_consump_13 >= 2 * ex_rate &
                       daily_consump_13 <= 20 * ex_rate) / total * 100,
    share_Ravallion = sum(daily_consump_13 >= 2 * ex_rate &
                            daily_consump_13 <= 13 * ex_rate) / total * 100,
    share_Kharas = sum(daily_consump_13 >= 10 * ex_rate &
                         daily_consump_13 <= 100 * ex_rate) / total * 100,
    share_Kohut = sum(daily_consump_13 >= 10 * ex_rate) / total *
      100,
    share_CN = sum(daily_consump_13 >= 16 * ex_rate &
                     daily_consump_13 <= 82 * ex_rate) / total * 100,
    share_VP = sum(daily_consump_13 >= 8 * ex_rate &
                     daily_consump_13 <= 58 * ex_rate) / total * 100,
    share_Nomura = sum(
      annual_consump_13 >= 6000 * ex_rate &
        annual_consump_13 <= 25000 * ex_rate
    ) / total * 100,
    share_Bhalla = sum(annual_consump_13 >= 3900 * ex_rate) / total *
      100
  )

table_relative_methods_2013 <- regression_df_clean %>%
  summarize(
    total = n(),
    mean = mean(daily_consump_13),
    q_20 = quantile(daily_consump_13, prob = c(0.20)),
    q_30 = quantile(daily_consump_13, prob = c(0.30)),
    q_40 = quantile(daily_consump_13, prob = c(0.40)),
    q_50 = quantile(daily_consump_13, prob = c(0.50)),
    q_60 = quantile(daily_consump_13, prob = c(0.60)),
    q_70 = quantile(daily_consump_13, prob = c(0.70)),
    q_75 = quantile(daily_consump_13, prob = c(0.75)),
    q_80 = quantile(daily_consump_13, prob = c(0.80)),
    q_90 = quantile(daily_consump_13, prob = c(0.90)),
    q_95 = quantile(daily_consump_13, prob = c(0.95)),
    share_B_GP = sum(0.75 * q_50 <= daily_consump_13 &
                       daily_consump_13 <= 1.25 * q_50) / total * 100,
    share_BB = sum(0.6 * q_50 <= daily_consump_13 &
                     daily_consump_13 <= 2.25 * q_50) / total * 100,
    share_VP = sum(0.5 * q_50 <= daily_consump_13 &
                     daily_consump_13 <= 1.5 * q_50) / total * 100,
    share_Wolfson = sum(0.75 * q_50 <= daily_consump_13 &
                          daily_consump_13 <= 1.25 * q_50) / total * 100,
    share_L_F = sum(q_40 <= daily_consump_13 &
                      daily_consump_13 <= q_70) / total * 100,
    share_Easterly = sum(q_20 <= daily_consump_13 &
                           daily_consump_13 <= q_80) / total * 100,
    share_Palma = sum(q_50 <= daily_consump_13 &
                        daily_consump_13 <= q_90) / total * 100,
    share_Solimano = sum(q_30 <= daily_consump_13 &
                           daily_consump_13 <= q_90) / total * 100,
    share_Partridge = sum(q_50 <= daily_consump_13 &
                            daily_consump_13 <= q_60) / total * 100,
    share_AP = sum(q_50 <= daily_consump_13 &
                     daily_consump_13 <= q_80) / total * 100,
    share_Birdsall = sum(10 * ex_rate <= daily_consump_13 &
                           daily_consump_13 <= q_95) / total * 100
  )
# Clear namespace of varibles which are not used in future analysis steps.
# To see intermediate results change the code below accordingly.
rm(
  table_absolute_methods_2011,
  table_absolute_methods_2013,
  table_relative_methods_2011,
  table_relative_methods_2013
)
